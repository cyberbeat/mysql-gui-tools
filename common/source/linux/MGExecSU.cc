

#include "MGExecSU.h"
#include "myg_utils.h"
#include "myg_gtkutils.h"
#include "mygpriv.h"
#include <sys/signal.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <pty.h>
#include <fcntl.h>


#define TIMEOUT_INTERVAL 500


MGExecSU::MGExecSU(const Glib::ustring &description, const Glib::ustring &command)
  : _command(command), _timeout_seconds(0), _timeout_counter(0)
{
  _xml= new MGGladeXML(myg_datadir+"/auth_dialog.glade");
  
  _xml->get_label("label")->set_markup(Glib::ustring(_("<b>Administrative Rights Required</b>"))+"\n\n"+description);
}


bool MGExecSU::authenticate()
{
  Gtk::Dialog *dlg;
  
  dlg= (Gtk::Dialog*)_xml->get_widget("auth_dialog");

  if (dlg->run() == Gtk::RESPONSE_OK)
  {
    Glib::ustring password;
    dlg->hide();
    password= _xml->get_entry("entry")->get_text();
    _xml->get_entry("entry")->set_text("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
    _xml->get_entry("entry")->set_text("");
    write_line(password+"\n");
    password.assign("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
    return true;
  }
  dlg->hide();
  return false;
}


bool MGExecSU::wait_readable(int timeout)
{
  bool ready= false;

  // wait until something to read
  while (_status == RUNFINISHED)
  {
    fd_set fds;
    struct timeval tv;
    int rc;
    
    if (timeout == 0)
    {
      tv.tv_sec= 0;
      tv.tv_usec= 10000;
    }
    else
    {
      tv.tv_sec= timeout;
      tv.tv_usec= 0;
    }

    FD_ZERO(&fds);
    FD_SET(fileno(_rfd), &fds);
    if ((rc= select(fileno(_rfd)+1, &fds, NULL, NULL, &tv)) > 0)
    {
      ready= true;
      break;
    }
    else if (rc == 0 && timeout > 0)
      break;

    while (Gtk::Main::instance()->events_pending())
      Gtk::Main::instance()->iteration();
  }

  return ready;
}


bool MGExecSU::write_line(const Glib::ustring &line)
{
  if (!fwrite(line.c_str(), 1, line.size(), _wfd))
    return false;
  fflush(_wfd);
  return true;
}


bool MGExecSU::read_line(Glib::ustring &line, bool full_line)
{
  char buffer[1024];

  if (!full_line)
  {
    const char *tokens[]= { "\n", NULL };

    return read_until(tokens, line);
  }
  else
  {
    if (fgets(buffer, sizeof(buffer), _rfd))
    {
      line= buffer;
      return true;
    }
  }
  return false;
}


bool MGExecSU::read_until(const char **tokens, Glib::ustring &line)
{ 
  int i;
  int c;

  line= "";

  for (;;)
  {
    for (i= 0; tokens[i]; i++)
    {
      if (strstr(line.c_str(), tokens[i]))
        return true;
    }

    c= fgetc(_rfd);
    if (c != EOF)
      line.push_back((char)c);
    else
      break;
  }
  return false;
}


Glib::ustring MGExecSU::read_to_end()
{
  Glib::ustring text;

  for (;;)
  {
    int c;

    if (!wait_readable(1))
      break;
    c= fgetc(_rfd);
    if (c == EOF)
      break;
    text.push_back((char)c);
  }

  return text;
}


int MGExecSU::check_needs_password()
{
  Glib::ustring line;
  const char *tokens[]= {"assword:", "\n", NULL};
 
  if (!wait_readable())
    return -1;

  if (!read_until(tokens, line))
    return -1;

  if (!strstr(line.c_str(), "assword:"))
    return 0;

  return 1;
}


bool MGExecSU::timeout()
{
  int status;

  if (waitpid(_pid, &status, WNOHANG) <= 0)
  {
    _timeout_counter+= 1;
    if (_timeout_seconds > 0 && (_timeout_counter*TIMEOUT_INTERVAL)/1000 >= _timeout_seconds)
    {
      _status= RTIMEOUT;
      _finished_signal.emit(_status);      
      return false;
    }
    return true;
  }

  if (WIFEXITED(status) && WEXITSTATUS(status) == 255)
  {
    if (_needs_su)
    {
      myg_show_error(_("<b>Error executing '/bin/su' command</b>\n\nMake sure it is executable."));
      _status= RERROR;
    }
    else
    {
      myg_show_error(_("<b>Error executing command</b>\n\nMake sure installation is correct."));
      _status= RERROR;
    }
    _finished_signal.emit(_status);
    return false;
  }
  else if (WIFEXITED(status) && WEXITSTATUS(status) > 0)
  {
    if (_needs_su)
    {
      myg_show_error(_("<b>Invalid password</b>\n\nCould not execute the requested command."));
      _status= RBADPASSWORD;
    }
    else
      _status= RSUCCESS;
    _finished_signal.emit(_status);
    return false;
  }
  else if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
  {
    _status= RSUCCESS;
    _finished_signal.emit(_status);
    return false;
  }
  else
  {
    if (_needs_su)
      myg_show_error(_("Unexpected error executing 'su' command."));
    else
      myg_show_error(_("Unexpected error executing command."));
    _status= RERROR;
    _finished_signal.emit(_status);
    return false;
  }
}

bool MGExecSU::handle_io(Glib::IOCondition cond)
{
  if (cond == Glib::IO_IN)
  {
    Glib::ustring data;
    read_line(data, false);
  }
  else
    return false;
  return true;
}


bool MGExecSU::run_as_current()
{
  int rfd[2];
  int wfd[2];
  const char *args[5];

  _status= RUNFINISHED;

  pipe(wfd);
  pipe(rfd);

  _pid= fork();
  
  if (_pid < 0)
  {
    myg_show_sys_error(_("Error executing command."), errno);
    return false;
  }
  else if (_pid == 0)
  {
    args[0]= "/bin/sh";
    args[1]= "-c";
    args[2]= _command.c_str();
    args[3]= NULL;
    
    dup2(rfd[1], 1);
    dup2(rfd[1], 2);
    close(rfd[0]);

    // so that we don't get wierd output for the Password: prompt
    unsetenv("LANG");
    unsetenv("LANGUAGE");
    unsetenv("LC_ALL");
    unsetenv("LC_MESSAGES");
    execv(args[0], (char* const*)args);
    exit(255);
  }
  else
  {
    close(wfd[0]);
    close(rfd[1]);
    _wfd= fdopen(wfd[1], "w");
    _rfd= fdopen(rfd[0], "r");
    setvbuf(_rfd, NULL, _IONBF, 0);
  }

  Glib::signal_timeout().connect(sigc::mem_fun(*this, &MGExecSU::timeout),
                                 TIMEOUT_INTERVAL);
  
  Glib::signal_io().connect(sigc::mem_fun(*this, &MGExecSU::handle_io),
                            Glib::IOChannel::create_from_fd(fileno(_rfd)),
                            Glib::IO_IN | Glib::IO_HUP);

  return true;
}


bool MGExecSU::run_as_root()
{
  int wfd;
  int rfd[2];
  const char *args[5];

  args[0]= "/bin/su";
  args[1]= "root";
  args[2]= "-c";
  args[3]= _command.c_str();
  args[4]= NULL;

  _status= RUNFINISHED;
  
  pipe(rfd);

  _pid= forkpty(&wfd, NULL, NULL, NULL);
  
  if (_pid < 0)
  {
    myg_show_sys_error(_("<b>Error executing privileged command</b>\nCould not create pseudo-terminal"), errno);
    
    return false;
  }

  if (_pid == 0)
  {    
    dup2(rfd[1], 1);
    dup2(rfd[1], 2);
    close(rfd[0]);
    for (int i= 3; i < 255; i++)
      close(i);

    // so that we don't get wierd output for the Password: prompt
    unsetenv("LANG");
    unsetenv("LANGUAGE");
    unsetenv("LC_ALL");
    unsetenv("LC_MESSAGES");
    execv(args[0], (char* const*)args);
    exit(255);
  }
  else
  {
    close(rfd[1]);
    _wfd= fdopen(wfd, "w");
    _rfd= fdopen(rfd[0], "r");

    setvbuf(_rfd, NULL, _IONBF, 0);
  }

  Glib::signal_timeout().connect(sigc::mem_fun(*this, &MGExecSU::timeout),
                                 TIMEOUT_INTERVAL);
  
  int r;
  if ((r= check_needs_password()) == 1)
  {
    if (!authenticate())
      return false;
  }
  else if (r < 0)
    return false;

  Glib::signal_io().connect(sigc::mem_fun(*this, &MGExecSU::handle_io),
                            Glib::IOChannel::create_from_fd(fileno(_rfd)),
                            Glib::IO_IN | Glib::IO_HUP);

  return true;
}


void MGExecSU::set_timeout(int timeout)
{
  _timeout_seconds= timeout;
}


bool MGExecSU::run()
{
  if (getuid() == 0)
  {
    _needs_su= false;
    return run_as_current();
  }
  else
  {
    _needs_su= true;
    return run_as_root();
  }
}


void MGExecSU::cancel()
{
  if (kill(_pid, SIGTERM) < 0)
    perror("Cancel command");
}



#ifdef TEST
bool handle_output(const Glib::ustring &str)
{
  g_message("OUTPUT: %s", str.c_str());
  return true;
}


void handle_end(MGExecSU::Status status, MGExecSU *exs)
{
  g_message("THE END: %s", exs->read_to_end().c_str());
  
  Gtk::Main::instance()->quit();
}


int main(int argc, char **argv)
{
  Gtk::Main app(argc, argv, true);

  myg_datadir= "../../res/linux";
  
  MGExecSU *exsu= new MGExecSU("testing", "/etc/init.d/mysql stop");

  exsu->signal_output().connect(sigc::ptr_fun(&handle_output));
  exsu->signal_finished().connect(sigc::bind<MGExecSU*>(sigc::ptr_fun(&handle_end), exsu));
  
  if (!exsu->run())
    puts("error");
  else
    puts("...");
  app.run();
}
#endif


