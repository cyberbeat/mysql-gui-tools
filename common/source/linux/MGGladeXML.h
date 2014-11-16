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


#ifndef _MGGLADEXML_H_
#define _MGGLADEXML_H_

#include <glade/glade.h>
#include <gtkmm.h>


class MGGladeXML : public Glib::ObjectBase {
  public:    
    class Error : public Glib::Exception {
        Glib::ustring _msg;
      public:
        Error(const Glib::ustring &error) : _msg(error) {};
        virtual ~Error() throw() {};
        virtual Glib::ustring what() const { return _msg; };
    };
    
    typedef std::pair<Glib::ustring,Glib::ustring> AccelPair;
    typedef std::list<AccelPair> AccelList;

  private:
    
    GladeXML *_xml;

  public:
    MGGladeXML(const std::string &file, const Glib::ustring &root=Glib::ustring(), const std::string &domain="");
    virtual ~MGGladeXML();
    
    Gtk::Widget *get_widget(const Glib::ustring &name) const;
    Gtk::Window *get_window(const Glib::ustring &name) const;
    Gtk::Entry *get_entry(const Glib::ustring &name) const;
    Gtk::ToggleButton *get_toggle(const Glib::ustring &name) const;
    Gtk::TreeView *get_tree(const Glib::ustring &name) const;
    Gtk::Button *get_button(const Glib::ustring &name) const;
    Gtk::Label *get_label(const Glib::ustring &name) const;
    Gtk::SpinButton *get_spin(const Glib::ustring &name) const;
    Gtk::TextView *get_text(const Glib::ustring &name) const;
    Gtk::OptionMenu *get_option(const Glib::ustring &name) const;
    Gtk::ComboBox *get_combo(const Glib::ustring &name) const;
    Gtk::ComboBoxEntry *get_combo_entry(const Glib::ustring &name) const;
    Gtk::Entry *get_combo_entry_entry(const Glib::ustring &name) const;
    Gtk::Container *get_container(const Glib::ustring &name) const;
    Gtk::Notebook *get_note(const Glib::ustring &name) const;
    Gtk::MenuItem *get_menu_item(const Glib::ustring &name) const;
    Gtk::CheckMenuItem *get_check_menu_item(const Glib::ustring &name) const;
    Gtk::Paned *get_paned(const Glib::ustring &name) const;
    Gtk::Box *get_box(const Glib::ustring &name) const;
// gtkmm-2.6 only    Gtk::IconView *get_icon_view(const Glib::ustring &name) const;
    Gtk::Image *get_image(const Glib::ustring &name) const;

    void bind_accelerators(const Glib::RefPtr<Gtk::AccelGroup> &group,
                           const AccelList &accels);
    
    template<class T> 
      void get_widget_derived(const Glib::ustring &name, T *&widget)
      {
          typedef typename T::BaseObjectType cwidget_type;
          cwidget_type *w= (cwidget_type*)glade_xml_get_widget(_xml, name.c_str());
          if (!w)
          {
            g_error("error retrieving widget %s from glade xml", name.c_str());
          }
          widget= new T(w);
      };

    template<class T> 
      void get_widget_derived2(const Glib::ustring &name, T *&widget)
      {
          typedef typename T::BaseObjectType cwidget_type;
          cwidget_type *w= (cwidget_type*)glade_xml_get_widget(_xml, name.c_str());
          if (!w)
          {
            g_error("error retrieving widget %s from glade xml", name.c_str());
          }
          widget= new T(w, this);
      };
};


#endif /* _MGGLADEXML_H_ */
