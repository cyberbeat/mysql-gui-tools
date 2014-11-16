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

#ifndef _MGCELLRENDERERBLOB_H_
#define _MGCELLRENDERERBLOB_H_


#include <gdkmm.h>
#include <gtkmm/cellrenderertext.h>
#include <gtkmm/window.h>
#include <gtkmm/scrolledwindow.h>
#include <gtkmm/box.h>
#include <gtkmm/textview.h>
#include <gtkmm/eventbox.h>



// I don't like this, but until I find a better alternative, it'll stay like this
inline void str2data(const std::string &str, gpointer *data, gsize *size)
{
  sscanf(str.c_str(), "%p %lu", data, size);
}


inline std::string data2str(gpointer data, gsize size)
{
  char buffer[128];
  sprintf(buffer, "%p %lu", data, size);
  return buffer;
}


class CellRendererBlob;

class MGBlobDelegate 
{
  public:
    virtual void blob_edit(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)= 0;
    virtual void blob_save(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)= 0;
    virtual void blob_load(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)= 0;
    virtual void blob_clear(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)= 0;
    virtual void blob_view(CellRendererBlob *cell, Gtk::Widget *widget, const Glib::ustring &path, int column)= 0;
};

class MGBlobEditor;

class CellRendererBlob : public Gtk::CellRenderer
{
  private:
    typedef CellRendererBlob Self;

    // property_field doesn't really contain the data. it's a hack containing
    // a string with the pointer and size of the data
    mutable Glib::Property<std::string> _property_field;
    Glib::Property<bool> _property_editable;
    Glib::Property<std::string> _property_background;
    Glib::Property<int> _property_column;

    MGBlobDelegate *_delegate;
    
    guint _max_text_length;

    mutable guint _max_text_width;
    mutable int _text_char_width;
    mutable int _text_height;

    bool _binary;
    
    Glib::RefPtr<Gdk::GC> _gc;
    Glib::RefPtr<Gdk::Pixbuf> _blob_icon;
    mutable Glib::RefPtr<Pango::Layout> _layout;
    
    Glib::RefPtr<Gdk::Pixbuf> _clear_icon;
    Glib::RefPtr<Gdk::Pixbuf> _edit_icon;
    Glib::RefPtr<Gdk::Pixbuf> _load_icon;
    Glib::RefPtr<Gdk::Pixbuf> _save_icon;
    Glib::RefPtr<Gdk::Pixbuf> _view_icon;
    Glib::RefPtr<Gdk::Pixbuf> _null_icon;

    void clear_clicked(Gtk::Widget *parent, const Glib::ustring &path);
    void save_clicked(Gtk::Widget *parent, const Glib::ustring &path);
    void load_clicked(Gtk::Widget *parent, const Glib::ustring &path);

  public:
    void edit_clicked(Gtk::Widget *parent, const Glib::ustring &path);
    void view_clicked(Gtk::Widget *parent, const Glib::ustring &path);

  protected:
    virtual void get_size_vfunc(Gtk::Widget& widget,
                                const Gdk::Rectangle* cell_area,
                                int* x_offset, int* y_offset,
                                int* width,    int* height) const;
    
  
    //virtual void render_vfunc(const Glib::RefPtr<Gdk::Window>& window, Gtk::Widget& widget, 
    //                          const Gdk::Rectangle& background_area, const Gdk::Rectangle& cell_area, 
    //                          const Gdk::Rectangle& expose_area, Gtk::CellRendererState flags);
    virtual void render_vfunc(const Glib::RefPtr<Gdk::Drawable>& window, Gtk::Widget& widget, 
                              const Gdk::Rectangle& background_area, const Gdk::Rectangle& cell_area, 
                              const Gdk::Rectangle& expose_area, Gtk::CellRendererState flags);

    virtual bool activate_vfunc(GdkEvent* event,
                                Gtk::Widget& widget,
                                const Glib::ustring& path,
                                const Gdk::Rectangle& background_area,
                                const Gdk::Rectangle& cell_area,
                                Gtk::CellRendererState flags);
    
    //  virtual void on_show_popup(const Glib::ustring& path, int x1, int y1, int x2, int y2);
    //  virtual void on_hide_popup();
    //

  public:
    CellRendererBlob(bool binary);
    virtual ~CellRendererBlob();

    void set_max_text_width(guint width);
    
    Glib::PropertyProxy<std::string> property_field() const; // should be built with data2str()
    Glib::PropertyProxy<bool> property_editable();
    Glib::PropertyProxy<std::string> property_background();
    Glib::PropertyProxy<int> property_column();
    
    void edit(const Glib::ustring &path);
    void view(const Glib::ustring &path);

    bool get_edited_data(gpointer &data, guint &size);
    
    void set_blob_icon(const Glib::RefPtr<Gdk::Pixbuf> &icon);
    void set_overlay_icons(const Glib::RefPtr<Gdk::Pixbuf> &clear_icon,
                           const Glib::RefPtr<Gdk::Pixbuf> &edit_icon,
                           const Glib::RefPtr<Gdk::Pixbuf> &load_icon,
                           const Glib::RefPtr<Gdk::Pixbuf> &save_icon,
                           const Glib::RefPtr<Gdk::Pixbuf> &view_icon,
                           const Glib::RefPtr<Gdk::Pixbuf> &null_icon);

    void set_delegate(MGBlobDelegate *delegate) { _delegate= delegate; };
    
    bool is_binary() { return _binary; };
};


#endif /* _MGCELLRENDERERBLOB_H_ */
