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


#include "MGGladeXML.h"

#include <gtk/gtk.h>



MGGladeXML::MGGladeXML(const std::string &file, const Glib::ustring &root, 
                        const std::string &translation_domain)
  : _xml(0)
{
  static bool initialized= false;
  
  if (!initialized)
  {
    initialized= true;
    glade_init();
  }

  if (translation_domain.empty())
    _xml= glade_xml_new(file.c_str(), root.empty()?NULL:root.c_str(), NULL);
  else
    _xml= glade_xml_new(file.c_str(), root.empty()?NULL:root.c_str(), translation_domain.c_str());
  
  if (!_xml)
  {
    throw Error("could not open glade file "+Glib::filename_to_utf8(file));
  }
}


MGGladeXML::~MGGladeXML()
{
  g_object_unref(_xml);
}


Gtk::Widget *MGGladeXML::get_widget(const Glib::ustring &name) const
{
  GtkWidget *w= glade_xml_get_widget(_xml, name.c_str());
  if (!w)
  {
    g_warning("request for bad widget '%s' in glade object", name.c_str());
    return 0;
  }
  
  return Glib::wrap(w);
}


Gtk::Window *MGGladeXML::get_window(const Glib::ustring &name) const
{
  Gtk::Window *w= static_cast<Gtk::Window*>(get_widget(name));
  if (!GTK_IS_WINDOW(w->gobj()))
  {
    g_warning("request for bad widget '%s' in glade object", name.c_str());
    return 0;
  }
  return w;
}


Gtk::Entry *MGGladeXML::get_entry(const Glib::ustring &name) const
{
  Gtk::Entry *w= static_cast<Gtk::Entry*>(get_widget(name));
  
  if (!GTK_IS_ENTRY(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::ToggleButton *MGGladeXML::get_toggle(const Glib::ustring &name) const
{
  Gtk::ToggleButton *w= static_cast<Gtk::ToggleButton*>(get_widget(name));
  
  if (!GTK_IS_TOGGLE_BUTTON(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::TreeView *MGGladeXML::get_tree(const Glib::ustring &name) const
{
  Gtk::TreeView *w= static_cast<Gtk::TreeView*>(get_widget(name));
  
  if (!GTK_IS_TREE_VIEW(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::Button *MGGladeXML::get_button(const Glib::ustring &name) const
{
  Gtk::Button *w= static_cast<Gtk::Button*>(get_widget(name));
  
  if (!GTK_IS_BUTTON(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::Label *MGGladeXML::get_label(const Glib::ustring &name) const
{
  Gtk::Label *w= static_cast<Gtk::Label*>(get_widget(name));
  
  if (!GTK_IS_LABEL(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::SpinButton *MGGladeXML::get_spin(const Glib::ustring &name) const
{
  Gtk::SpinButton *w= static_cast<Gtk::SpinButton*>(get_widget(name));
  
  if (!GTK_IS_SPIN_BUTTON(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}

Gtk::TextView *MGGladeXML::get_text(const Glib::ustring &name) const
{
  Gtk::TextView *w= static_cast<Gtk::TextView*>(get_widget(name));
  
  if (!GTK_IS_TEXT_VIEW(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}



Gtk::OptionMenu *MGGladeXML::get_option(const Glib::ustring &name) const
{
  Gtk::OptionMenu *w= static_cast<Gtk::OptionMenu*>(get_widget(name));

  if (!GTK_IS_OPTION_MENU(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  return w;
}


Gtk::ComboBox *MGGladeXML::get_combo(const Glib::ustring &name) const
{
  Gtk::ComboBox *w= static_cast<Gtk::ComboBox*>(get_widget(name));
  
  if (!GTK_IS_COMBO_BOX(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }  
  return w;
}

Gtk::ComboBoxEntry *MGGladeXML::get_combo_entry(const Glib::ustring &name) const
{
  Gtk::ComboBoxEntry *w= static_cast<Gtk::ComboBoxEntry*>(get_widget(name));
  
  if (!GTK_IS_COMBO_BOX_ENTRY(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }  
  return w;
}

Gtk::Entry *MGGladeXML::get_combo_entry_entry(const Glib::ustring &name) const
{
  Gtk::ComboBoxEntry *w= static_cast<Gtk::ComboBoxEntry*>(get_widget(name));
  
  if (!GTK_IS_COMBO_BOX_ENTRY(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  return (Gtk::Entry*)w->get_child();
}

Gtk::Container *MGGladeXML::get_container(const Glib::ustring &name) const
{
  Gtk::Container *w= static_cast<Gtk::Container*>(get_widget(name));
  
  if (!GTK_IS_CONTAINER(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}

Gtk::Notebook *MGGladeXML::get_note(const Glib::ustring &name) const
{
  Gtk::Notebook *w= static_cast<Gtk::Notebook*>(get_widget(name));
  
  if (!GTK_IS_NOTEBOOK(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::MenuItem *MGGladeXML::get_menu_item(const Glib::ustring &name) const
{
  Gtk::MenuItem *w= static_cast<Gtk::MenuItem*>(get_widget(name));
  
  if (!GTK_IS_MENU_ITEM(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::CheckMenuItem *MGGladeXML::get_check_menu_item(const Glib::ustring &name) const
{
  Gtk::CheckMenuItem *w= static_cast<Gtk::CheckMenuItem*>(get_widget(name));
  
  if (!GTK_IS_CHECK_MENU_ITEM(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  
  return w;
}


Gtk::Paned *MGGladeXML::get_paned(const Glib::ustring &name) const
{
  Gtk::Paned *w= static_cast<Gtk::Paned*>(get_widget(name));

  if (!GTK_IS_PANED(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }

  return w;
}


Gtk::Box *MGGladeXML::get_box(const Glib::ustring &name) const
{
  Gtk::Box *w= static_cast<Gtk::Box*>(get_widget(name));

  if (!GTK_IS_BOX(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }

  return w;
}



/*
Gtk::IconView *MGGladeXML::get_icon_view(const Glib::ustring &name) const
{
  Gtk::IconView *w= static_cast<Gtk::IconView*>(get_widget(name));

  if (!GTK_IS_ICON_VIEW(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  return w;
}
*/


Gtk::Image *MGGladeXML::get_image(const Glib::ustring &name) const
{
  Gtk::Image *w= static_cast<Gtk::Image*>(get_widget(name));

  if (!GTK_IS_IMAGE(w->gobj()))
  {
    g_warning("requested widget '%s' with the wrong type", name.c_str());
    return 0;
  }
  return w;
}


void MGGladeXML::bind_accelerators(const Glib::RefPtr<Gtk::AccelGroup> &group,
                                   const AccelList &accels)
{
  for (AccelList::const_iterator iter= accels.begin(); iter != accels.end(); ++iter)
  {
    guint accel;
    Gdk::ModifierType mods;

    Gtk::AccelGroup::parse(iter->second, accel, mods);

    get_menu_item(iter->first)->add_accelerator("activate",
                                                group,
                                                accel, mods, 
                                                Gtk::ACCEL_VISIBLE);
  }
}
