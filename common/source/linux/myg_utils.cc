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


#include <stdarg.h>

#include <gtkmm/checkbutton.h>
#include <gtkmm/entry.h>
#include "MGGladeXML.h"
#include "mygpriv.h"
#include "myg_utils.h"


std::string myg_datadir;


bool myg_fetch_advanced_options(MGGladeXML *xml, MGAdvancedOption *options,
                                std::list<std::string> &advanced_options)
{
  advanced_options.clear();

  for (int i= 0; options[i].name; i++)
  {
    std::string line;
    std::string value;

    line= options[i].name;
    line+= "=";

    switch (options[i].type)
    {
    case 'B':
      {
        Gtk::CheckButton *check= (Gtk::CheckButton*)xml->get_widget(options[i].widget);
        if (check->get_active())
        {
          line+= "Y";
          advanced_options.push_back(line);
        }
      }
      break;
    case 'S':
      value= ((Gtk::Entry*)xml->get_widget(options[i].widget))->get_text();
      if (!value.empty())
      {
        line+= value;
        advanced_options.push_back(line);
      }
      break;
    }
  }

  return true;
}


bool myg_show_advanced_options(MGGladeXML *xml, MGAdvancedOption *options,
                               const std::list<std::string> &advanced_options)
  
{
  for (int j= 0; options[j].name; j++)
  {
    bool value= false;
    std::string svalue;

    for (std::list<std::string>::const_iterator iter= advanced_options.begin();
         iter!=advanced_options.end(); ++iter)
    {
      std::string::size_type pos= iter->find('=');
      if (pos == std::string::npos) continue;

      std::string name(*iter, 0, pos);

      if (strcasecmp(name.c_str(), options[j].name)==0)
      {
        svalue= std::string(*iter, pos+1);

        switch (options[j].type)
        {
        case 'B':
          if (svalue[0] == 'y' || svalue[0] == 'Y' || svalue[0] == '1')
            value= true;
          break;
        case 'S':
          break;
        }
        break;
      }
    }

    switch (options[j].type)
    {
    case 'B':
      ((Gtk::ToggleButton*)xml->get_widget(options[j].widget))->set_active(value);
      break;
    case 'S':
      ((Gtk::Entry*)xml->get_widget(options[j].widget))->set_text(svalue);
      break;
    }
  }

  return true;
}


void myg_convert_string_list(const std::list<std::string> &slist,
                             char **&list, unsigned int &list_num)
{
  if (slist.empty())
  {
    list= NULL;
    list_num= 0;
    return;
  }
  
  list= (char**)malloc(sizeof(char*)*slist.size());
  list_num= slist.size();
  
  std::list<std::string>::const_iterator it= slist.begin();
  for (unsigned int i= 0; it != slist.end(); ++it, ++i)
  {
    list[i]= strdup(it->c_str());
  }
}


void myg_convert_string_list(char **list, unsigned int list_num,
                             std::list<std::string> &slist)
{
  slist.clear();

  for (unsigned int i= 0; i < list_num; i++)
  {
    slist.push_back(list[i]);
  }
}



Glib::ustring ufmt(const gchar *msg, ...)
{
  va_list val;
  gchar *text;
  
  va_start(val, msg);
  
  text= g_strdup_vprintf(msg, val);
  Glib::ustring str(text);
  g_free(text);

  va_end(val);

  return str;
}


std::string tostr(int num)
{
  char buf[32];
  
  g_snprintf(buf, sizeof(buf), "%i", num);
  
  return buf;
}


void myg_set_datadir(const std::string &path)
{
  myg_datadir= path;
}


Glib::ustring myg_message_for_xlib_error(MYX_LIB_ERROR err)
{    
  static char *msgs[]= {
    "",
    N_("Can't open file."),
    N_("Can't connect to server instance."),
    N_("Error parsing XML file."),
    N_("Error parsing XML file (bad document)."),
    N_("Error parsing XML file (empty document)."),
    N_("Error executing SQL command."),
    N_("Executing stopped."),
    N_("Internal error in libxml (could not change memory allocators)."),
    N_("The object was not found in the database."),
    N_("Cannot read from file."),
    N_("Error during character set conversion."),
    N_("Invalid character set specified."),
    N_("Memory limit reached while fetching query results."),
    N_("Out of virtual memory."),
  };

  return msgs[(int)err];
}


Glib::ustring strjoin(const std::list<Glib::ustring> &list)
{
  Glib::ustring s;
  
  for (std::list<Glib::ustring>::const_iterator i= list.begin();
       i != list.end(); ++i)
  {
    if (s.empty())
      s= *i;
    else
      s= s+","+*i;
  }
  return s;
}


Glib::ustring strjoin(const std::vector<Glib::ustring> &list)
{
  Glib::ustring s;
  
  for (std::vector<Glib::ustring>::const_iterator i= list.begin();
       i != list.end(); ++i)
  {
    if (s.empty())
      s= *i;
    else
      s= s+","+*i;
  }
  return s;
}


Glib::ustring strreplace(const Glib::ustring &str,const Glib::ustring &from,const Glib::ustring &to)
{
  Glib::ustring::size_type p, p0= 0;
  Glib::ustring s;

  if (str.find(from, p0) == Glib::ustring::npos)
  {
    s= str;
  }
  else
  {
    while ((p= str.find(from, p0))!=Glib::ustring::npos)
    {
      s+= str.substr(p0, p-p0);
      s+= to;
      p0= p+from.length();
    }
  }
  return s;
}

