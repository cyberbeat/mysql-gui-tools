

#ifndef _UTIL_H_
#define _UTIL_H_


#include <glibmm.h>
#include <gtkmm.h>
#include <mysql.h>

#include "myx_public_interface.h"
#include "myx_admin_public_interface.h"


Glib::ustring format_value(long long value);



Glib::RefPtr<Gdk::Pixbuf> make_pixbuf_from_data(void *data, int length);

// returns RESPONSE_APPLY, RESPONSE_NO, RESPONSE_CANCEL
int confirm_change(Gtk::Window &parent, const Glib::ustring &msg);

void show_adminlib_error(Gtk::Window &parent, const Glib::ustring &msg, MYX_ADMIN_LIB_ERROR err);


#endif /* _UTIL_H_ */
