/* Copyright (C) 2004 MySQL AB

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA */

#include "MCrontab.h"
#include "myx_public_interface.h"
#include "myg_utils.h"
#include <errno.h>

bool MCrontab::parse_line(const std::string &line, Entry &entry)
{
  char *tmp= g_strdup(line.c_str());
  char *tok;
  
  tok= strtok(tmp, " ");
  if (!tok)
    goto error;
  if (*tok == '*')
    entry.minute= -1;
  else
    entry.minute= atoi(tok);

  tok= strtok(NULL, " ");
  if (!tok)
    goto error;
  if (*tok == '*')
    entry.hour= -1;
  else
    entry.hour= atoi(tok);

  tok= strtok(NULL, " ");
  if (!tok)
    goto error;
  if (*tok == '*')
    entry.day="";
  else
    entry.day= tok;

  tok= strtok(NULL, " ");
  if (!tok)
    goto error;
  if (*tok == '*')
    entry.month= -1;
  else
    entry.month= atoi(tok);

  tok= strtok(NULL, " ");
  if (!tok)
    goto error;
  if (*tok == '*')
    entry.weekday= 0;
  else
  {
    char **tokens= g_strsplit(tok, ",", -1);
    entry.weekday= 0;
    for (int i=0; tokens[i]; i++)
    {
      entry.weekday|= (1<<atoi(tokens[i]));
    }
    g_strfreev(tokens);
  }

  tok= strtok(NULL, "\n");
  if (!tok)
    goto error;
  entry.command= tok;

  g_free(tmp);
  return true;

error:
  g_free(tmp);
  return false;
}


std::string MCrontab::format_entry(const Entry &entry)
{
  std::string line;
  
  if (entry.minute < 0)
    line= "*";
  else
    line= tostr(entry.minute);
  
  if (entry.hour < 0)
    line+= " *";
  else
    line+= " "+tostr(entry.hour);
  
  if (entry.day.empty())
    line+= " *";
  else
  {
    line+= " "+entry.day;
  }
  
  if (entry.month < 0)
    line+= " *";
  else
    line+= " "+tostr(entry.month);
  
  if (entry.weekday == 0)
    line+= " *";
  else
  {
    std::string wdays;
    for (int i=0; i < 7; i++)
    {
      if (entry.weekday & (1<<i))
      {
        if (wdays.empty())
          wdays= tostr(i);
        else
          wdays+= ","+tostr(i);
      }
    }
    line+= " "+wdays;
  }

  line+= " "+entry.command;

  return line;
}


bool MCrontab::find_entry_by_comment(const std::string &comment, Entry &entry)
{
  bool next_is_the_one= false;

  for (std::list<std::string>::const_iterator iter= _lines.begin();
       iter != _lines.end(); ++iter)
  {
    if ((*iter).empty()) continue;

    if ((*iter)[0]=='#')
    {
      if (iter->find(comment)!=std::string::npos)
        next_is_the_one= true;
    }
    else
    {
      if (next_is_the_one)
        return parse_line(*iter, entry);
    }
  }

  return false;
}


bool MCrontab::remove_entry_with_comment(const std::string &comment,
                                         const std::string &command)
{
  bool next_is_the_one= false;
  std::list<std::string>::iterator comment_iter;

  for (std::list<std::string>::iterator iter= _lines.begin();
       iter != _lines.end(); ++iter)
  {
    if ((*iter).empty()) continue;

    if ((*iter)[0]=='#')
    {
      if (iter->find(comment)!=std::string::npos)
      {
        next_is_the_one= true;
        comment_iter= iter;
      }
    }
    else
    {
      if (next_is_the_one)
      {
        Entry entry;
        if (parse_line(*iter, entry))
        {
          // check if command below the comment is what we expect
          if (command.empty() || Glib::str_has_prefix(command, entry.command))
          {
            // remove comment and command from list
            _lines.erase(iter);
            _lines.erase(comment_iter);
          }
          return true;
        }
      }
    }
  }
  return false;
}


void MCrontab::add_entry(Entry &entry, const std::string &comment)
{
  _lines.push_back("#"+comment);
  _lines.push_back(format_entry(entry));
}


bool MCrontab::load()
{
  char *cmdline= "/usr/bin/crontab -l";
  char *stdout_text= NULL;
  char *stderr_text= NULL;
  GError *error= NULL;
  gint exit_status;
  bool got_error= false;

  if (!g_spawn_command_line_sync(cmdline, &stdout_text, &stderr_text, &exit_status,
                                 &error) || exit_status != 0)
  {
    if (!stderr_text || !strstr(stderr_text, "no crontab"))
    {
      got_error= true;

      if (stderr_text)
        g_printerr("%s", stderr_text);
    
    
      if (error && error->message)
        g_warning("could not get crontab list: %s",
                  error->message);
      else
        g_warning("could not get crontab list");
    }
  }

  if (!got_error)
  {
    char *tok= strtok(stdout_text, "\n");
    while (tok)
    {
      _lines.push_back(tok);
      tok= strtok(NULL, "\n");
    }
  }
  if (stdout_text)
    g_free(stdout_text);
  if (stderr_text)
    g_free(stderr_text);
  if (error)
    g_error_free(error);

  return true;
}


bool MCrontab::install()
{
  char fname[32];
  int fd;
  bool ok= true;
  
  
  // 1st write a tmp file with the crontab contents
  
  strcpy(fname, "/tmp/cron.XXXXXX");
  
  // carefull here, we dont want to allow someone else to write in our
  // tmp file and schedule stuff on our behalf
  fd= g_mkstemp(fname);
  if (fd < 0)
  {
    g_warning("could not create tmp file %s", fname);
    return false;
  }

  for (std::list<std::string>::const_iterator iter= _lines.begin();
       iter != _lines.end(); ++iter)
  {
    if (write(fd, iter->c_str(), iter->length()) < 0
        || write(fd, "\n", 1) < 0)
    {
      g_warning("error writing to tmp file %s: %s", fname,
                 g_strerror(errno));
      ok= false;
      break;
    }
  }

  close(fd);
  
  if (ok)
  {
    char *cmdline= g_strdup_printf("/usr/bin/crontab %s", fname);
    GError *error= NULL;
    gint exit_status;

    // now install the cron file
    if (!g_spawn_command_line_sync(cmdline, NULL, NULL, &exit_status, &error)
        || exit_status != 0)
    {
      if (error && error->message)
        g_warning("could not install crontab: %s",
                   error->message);
      else
        g_warning("could not install crontab");
      if (error)
        g_error_free(error);

      ok= false;
    }
  }
  
  // delete tmp file
  unlink(fname);

  return ok;
}
