

#include "myadmin.h"

#include "util.h"
#include <glib.h>
#include <unistd.h>

#include <myx_library.h>

#include "myg_utils.h"


int confirm_change(Gtk::Window &parent, const Glib::ustring &msg)
{
  Gtk::MessageDialog dlg(parent, msg, false,
                         Gtk::MESSAGE_QUESTION,
                         Gtk::BUTTONS_NONE,
                         true);

  Gtk::Button btn(Gtk::Stock::CANCEL);
  btn.show();
  dlg.add_action_widget(btn, Gtk::RESPONSE_CANCEL);

  dlg.add_button(_("_Discard changes"), Gtk::RESPONSE_NO);
  dlg.add_button(_("_Save changes"), Gtk::RESPONSE_APPLY);

  return dlg.run();
}


void show_adminlib_error(Gtk::Window &parent, const Glib::ustring &msg, MYX_ADMIN_LIB_ERROR err)
{
  static char *msgs[]= {
    "",
    N_("Can't open file."),
    N_("Error parsing XML file."),
    N_("Error parsing XML file (bad document)."),
    N_("Error parsing XML file (empty document)."),
    N_("Error parsing INI file."),
    N_("General error."),
    N_("SQL error.")
  };
  Gtk::MessageDialog dlg(parent, ufmt("%s\n%s", msg.c_str(), _(msgs[(int)err])), false,
                         Gtk::MESSAGE_ERROR,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.run();
}



Glib::RefPtr<Gdk::Pixbuf> make_pixbuf_from_data(void *data, int length)
{
  Glib::RefPtr<Gdk::Pixbuf> pbuf;
  std::string fname;
  int f= Glib::file_open_tmp(fname);
  if (f>=0)
  {
    write(f, data, length);
    close(f);

    pbuf= Gdk::Pixbuf::create_from_file(fname);
    unlink(fname.c_str());
  }
  return pbuf;
}


Glib::ustring format_value(long long value)
{
  char buffer[64];

  if (value > 1024*1024*1024*1024LL) // tera
    sprintf(buffer, "%.2f T", value/(1024*1024*1024*1024.0));
  else if (value > 1024*1024*1024LL) // giga
    sprintf(buffer, "%.2f G", value/(1024*1024*1024.0));
  else if (value > 1024*1024.0) // mega
    sprintf(buffer, "%.2f M", value/(1024*1024.0));
  else if (value > 1024.0) // kilo
    sprintf(buffer, "%.2f k", value/1024.0);
  else
    sprintf(buffer, "%lli", value);
  
  return Glib::ustring(buffer);
}

