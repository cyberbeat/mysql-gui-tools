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

#include "MGBlobEditor.h"
#include <gtkmm.h>

#include "myg_gtkutils.h"
#include "myg_utils.h"
#include "mygpriv.h"


class MGBlobListStore : public Glib::Object, public Gtk::TreeModel
{
  protected:
    virtual bool iter_is_valid(const Gtk::TreeModel::iterator& iter) const;
    virtual bool iter_next_vfunc(const Gtk::TreeModel::iterator& iter, Gtk::TreeModel::iterator& iter_next) const;

    virtual bool iter_children_vfunc(const Gtk::TreeModel::iterator& parent, Gtk::TreeModel::iterator& iter) const;
    virtual bool iter_has_child_vfunc(const Gtk::TreeModel::iterator& iter) const;
    virtual int iter_n_children_vfunc(const Gtk::TreeModel::iterator& iter) const;
    virtual int iter_n_root_children_vfunc() const;
    virtual bool iter_nth_child_vfunc(const Gtk::TreeModel::iterator& parent, int n, Gtk::TreeModel::iterator& iter) const;
    virtual bool iter_nth_root_child_vfunc(int n, Gtk::TreeModel::iterator& iter) const;
    virtual bool iter_parent_vfunc(const Gtk::TreeModel::iterator& child, Gtk::TreeModel::iterator& iter) const;

    virtual Gtk::TreeModel::Path get_path_vfunc(const Gtk::TreeModel::iterator& iter) const;
    virtual bool get_iter_vfunc(const Gtk::TreeModel::Path& path, Gtk::TreeModel::iterator& iter) const;

    virtual Gtk::TreeModelFlags get_flags_vfunc() const;
    virtual int get_n_columns_vfunc() const;
    virtual GType get_column_type_vfunc(int index) const;
    virtual void get_value_vfunc(const Gtk::TreeModel::iterator& iter, int column, Glib::ValueBase& value) const;

    guchar *_data;
    gsize _size;

    gint _bytes_per_row;
    int _stamp;

    MGBlobListStore(gint bytes_per_row);

  public:
    static Glib::RefPtr<MGBlobListStore> create(gint bpr);
    
    void set_data(char *data, gsize size);
};


Glib::RefPtr<MGBlobListStore> MGBlobListStore::create(gint bpr)
{
  MGBlobListStore *store;

  store= new MGBlobListStore(bpr);

  return Glib::RefPtr<MGBlobListStore>(store);
}


MGBlobListStore::MGBlobListStore(gint bytes_per_row)
  : Glib::ObjectBase(typeid(MGBlobListStore)),
    Glib::Object(),
   _bytes_per_row(bytes_per_row),
   _stamp(1)
{
}


void MGBlobListStore::set_data(char *data, gsize size)
{
  _data= (guchar*)data;
  _size= size;
  Gtk::TreeModel::Path p(Glib::ustring("0"));
  row_changed(p, get_iter(p));
  row_inserted(p, get_iter(p));
}


Gtk::TreeModel::Path MGBlobListStore::get_path_vfunc(const Gtk::TreeModel::iterator& iter) const
{
  Path path;
  path.push_back((long)iter.gobj()->user_data);
    
  return path;
}


bool MGBlobListStore::iter_next_vfunc(const Gtk::TreeModel::iterator& iter, Gtk::TreeModel::iterator& iter_next) const
{
  long next_row= 0;

  iter_next= Gtk::TreeModel::iterator();
  if (iter_is_valid(iter))
  {
    next_row= (long)iter.gobj()->user_data+1;
    if (next_row < iter_n_root_children_vfunc())
    {
      iter_next.set_stamp(_stamp);
      iter_next.gobj()->user_data= (gpointer)next_row;
      return true;
    }
  }
  return false;
}


bool MGBlobListStore::iter_has_child_vfunc(const Gtk::TreeModel::iterator& iter) const
{
  return false;
}


bool MGBlobListStore::iter_parent_vfunc(const Gtk::TreeModel::iterator& child, Gtk::TreeModel::iterator& iter) const
{
  iter= Gtk::TreeModel::iterator();
  return false;
}

bool MGBlobListStore::iter_is_valid(const Gtk::TreeModel::iterator& iter) const
{
  return iter.get_stamp() == _stamp;
}

bool MGBlobListStore::get_iter_vfunc(const Gtk::TreeModel::Path& path, Gtk::TreeModel::iterator& iter) const
{
  iter= Gtk::TreeModel::iterator();
  if (!path.empty())
  {
    long row_index= path[0];
    if (row_index < iter_n_root_children_vfunc())
    {
      iter.set_stamp(_stamp);
      iter.gobj()->user_data= (gpointer)row_index;
      return true;
    }
  }
  return false;
}

bool MGBlobListStore::iter_nth_root_child_vfunc(int n, Gtk::TreeModel::iterator& iter) const
{
  iter= Gtk::TreeModel::iterator();
  if (n < iter_n_root_children_vfunc())
  {
    iter.set_stamp(_stamp);
    iter.gobj()->user_data= (gpointer)(long)n;
    return true;
  }
  return false;
}


bool MGBlobListStore::iter_nth_child_vfunc(const Gtk::TreeModel::iterator& parent, int n, Gtk::TreeModel::iterator& iter) const
{
  return iter_nth_root_child_vfunc(n, iter);
}


int MGBlobListStore::iter_n_root_children_vfunc() const
{
  return _size / _bytes_per_row + (_size % _bytes_per_row > 0 ? 1 : 0);
}


int MGBlobListStore::iter_n_children_vfunc(const Gtk::TreeModel::iterator& iter) const
{
  return 0;
}

bool MGBlobListStore::iter_children_vfunc(const Gtk::TreeModel::iterator& parent, Gtk::TreeModel::iterator& iter) const
{
  return iter_nth_root_child_vfunc(0, iter);
}

Gtk::TreeModelFlags MGBlobListStore::get_flags_vfunc() const
{
  return Gtk::TreeModelFlags(0);
}

int MGBlobListStore::get_n_columns_vfunc() const
{
  return 21;
}

GType MGBlobListStore::get_column_type_vfunc(int index) const
{
  return G_TYPE_STRING;
}

void MGBlobListStore::get_value_vfunc(const Gtk::TreeModel::iterator& iter, int column, Glib::ValueBase& value) const
{
  char buffer[_bytes_per_row * 4];
  unsigned int line_num= (unsigned long)iter.gobj()->user_data;

  if (!iter_is_valid(iter))
  {
    value.init(G_TYPE_STRING);
    return;
  }

  Glib::Value<Glib::ustring> nvalue;
  nvalue.init(G_TYPE_STRING);
  if (column == 0)
  {
    sprintf(buffer, "%.8x", line_num * _bytes_per_row);
    nvalue.set(buffer);
  }
  else if (column >= _bytes_per_row + 1)
  {
    int i;
    buffer[0]= 0;
    for (i= 0; i < _bytes_per_row/4; i++)
    {
      guint offs= line_num * _bytes_per_row + i + (column-_bytes_per_row-1)*4;
      if (offs >= _size)
        strcat(buffer, " ");
      else if (isprint(_data[offs]))
        strncat(buffer, (char*)_data+offs, 1);
      else
      {
        switch (_data[offs])
        {
          /* no common fixed width fonts have these chars
        case 0:
          strcat(buffer, "\xE2\x90\x80");
          break;
        case '\n':
          strcat(buffer, "\xE2\x90\x8A");
          break;
        case '\r':
          strcat(buffer, "\xE2\x90\x8D");
          break;
           */
        default:
          strcat(buffer, "\xC2\xB7");
        }
      }
    }
    nvalue.set(buffer);
  }
  else
  {
    guint offs= line_num * _bytes_per_row + column - 1;
    
    if (offs >= _size)
    {
      nvalue.set("  ");
    }
    else
    {
      sprintf(buffer, "%.2x", _data[offs]);
      nvalue.set(buffer);
    }
  }
  value.init(Glib::Value<Glib::ustring>::value_type());
  value= nvalue;
}
  



//----------------------------------------------------------------------


MGBlobEditor::MGBlobEditor(MGBlobEditor::ViewType vtypes, bool edit)
  : Gtk::Window(Gtk::WINDOW_TOPLEVEL), _topbox(false, 8),
   _bbox(Gtk::BUTTONBOX_END, 8), _button1(0), _button2(0),
   _vtypes(vtypes), _data(0), _size(0)
{
  _delete_on_close= false;

  set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  set_default_size(600, 400);
  
  signal_delete_event().connect(sigc::mem_fun(*this,&MGBlobEditor::delete_event));
  
  _note.signal_switch_page().connect(sigc::mem_fun(*this,&MGBlobEditor::note_page_switched));
    
  add(_topbox);
  _topbox.pack_start(_note);
  set_border_width(12);

  set_title(_("Field Viewer"));

  if (vtypes & (VText|VBinary))
  {
    Gtk::ScrolledWindow *swin= Gtk::manage(new Gtk::ScrolledWindow());
    _text= Gtk::manage(new Gtk::TextView());
    
    swin->set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
    swin->set_shadow_type(Gtk::SHADOW_IN);

    _text->set_editable(edit);
    swin->add(*_text);

    _note.append_page(*swin, _("Text"));
  }
  else
    _text= 0;

  if (vtypes & VImage)
  {
    Gtk::VBox *vb= Gtk::manage(new Gtk::VBox(false, 8));
    Gtk::ScrolledWindow *scroll= Gtk::manage(new Gtk::ScrolledWindow());

    _image= Gtk::manage(new Gtk::Image());
    _image_label= Gtk::manage(new Gtk::Label());

    scroll->add(*_image);
    
    vb->pack_start(*scroll, true, true);
    vb->pack_start(*_image_label, false, true);

    _note.append_page(*vb, _("Image"));
  }

  if (vtypes & VBinary)
  {
    Gtk::ScrolledWindow *swin= Gtk::manage(new Gtk::ScrolledWindow());

    swin->set_policy(Gtk::POLICY_NEVER, Gtk::POLICY_AUTOMATIC);
    swin->set_shadow_type(Gtk::SHADOW_IN);

    
    _tree_store= MGBlobListStore::create(16);

    _tree= Gtk::manage(new Gtk::TreeView());
    swin->add(*_tree);

    _tree->set_headers_visible(false);
    _tree->append_column("", _columns.offset);
    ((Gtk::CellRendererText*)_tree->get_column(0)->get_first_cell_renderer())->set_fixed_height_from_font(1);
    _tree->get_column(0)->set_fixed_width(100);
    
    // separator
    {
      Gtk::TreeViewColumn *col= Gtk::manage(new Gtk::TreeViewColumn(""));
      Gtk::CellRendererText *rend= Gtk::manage(new Gtk::CellRendererText());
      rend->set_fixed_height_from_font(1);
      rend->property_background()= "#000000";
      col->pack_start(*rend, false);
      col->set_fixed_width(1);
      col->set_max_width(1);
      _tree->append_column(*col);
    }

    // hex view
    for (int i= 0; i < 4; i++)
    {
      Gtk::TreeViewColumn *col= Gtk::manage(new Gtk::TreeViewColumn(""));

      for (int j= 0; j < 4; j++)
      {
        Gtk::CellRendererText *rend= Gtk::manage(new Gtk::CellRendererText());
        rend->set_fixed_height_from_font(1);
        if (!(i&1))
          rend->property_background()= "#eeeef0";
        col->pack_start(*rend, false);
        col->add_attribute(rend->property_text(), _columns.data[i*4+j]);
      }
      _tree->append_column(*col);
    }

    // separator
    {
      Gtk::TreeViewColumn *col= Gtk::manage(new Gtk::TreeViewColumn(""));
      Gtk::CellRendererText *rend= Gtk::manage(new Gtk::CellRendererText());
      rend->set_fixed_height_from_font(1);
      rend->property_background()= "#000000";
      col->pack_start(*rend, false);
      col->set_fixed_width(1);
      col->set_max_width(1);
      _tree->append_column(*col);
    }

    // ascii view
    Gtk::TreeViewColumn *col= Gtk::manage(new Gtk::TreeViewColumn(""));
    for (int i= 0; i < 4; i++)
    {
      Gtk::CellRendererText *rend= Gtk::manage(new Gtk::CellRendererText());
      col->pack_start(*rend, false);
      rend->set_fixed_height_from_font(1);
      rend->property_family()= "lucidatypewriter";
      if (!(i&1))
        rend->property_background()= "#eeeef0";
      col->add_attribute(rend->property_text(), _columns.text[i]);
    }
    _tree->append_column(*col);
    
    _note.append_page(*swin, _("Binary"));
  }

  _summary.set_alignment(0.0);
  _topbox.pack_start(_summary, false, true);

  _topbox.pack_start(_bbox, false, true);


  if (edit)
  {
    _button2= Gtk::manage(new Gtk::Button(Gtk::Stock::CANCEL));
    _button2->signal_clicked().connect(sigc::mem_fun(*this,&MGBlobEditor::close));
    _bbox.pack_start(*_button2, false, false);
  }
  
  _button1= Gtk::manage(new Gtk::Button(Gtk::Stock::OK));
  if (edit)
    _button1->signal_clicked().connect(sigc::mem_fun(*this,&MGBlobEditor::save));
  else
    _button1->signal_clicked().connect(sigc::mem_fun(*this,&MGBlobEditor::close));
  _bbox.pack_start(*_button1, false, false);

  show_all_children();
}

void MGBlobEditor::note_page_switched(GtkNotebookPage* page, guint)
{
  if((_text != NULL) && (_note.get_current()->get_tab_label_text().compare("Binary") == 0))
  {
    char* c= g_strdup(_text->get_buffer()->get_text().c_str());
    _tree->set_model(Glib::RefPtr<Gtk::ListStore>());
    _tree_store->set_data(c, strlen(c));
    _tree->set_model(_tree_store);
  }
}

MGBlobEditor::~MGBlobEditor()
{
}


void MGBlobEditor::save()
{
  _save_signal.emit();
  
  close();
}

void MGBlobEditor::close()
{
  hide();
  if (_delete_on_close)
    delete this;
}


bool MGBlobEditor::delete_event(GdkEventAny *ev)
{
  close();
  return true;
}


void MGBlobEditor::set_delete_on_close(bool flag)
{
  _delete_on_close= flag;
}


void MGBlobEditor::get_data(gpointer &data, gsize &size)
{
  if (_vtypes & VText)
  {
    Glib::ustring text= _text->get_buffer()->get_text();

    size= text.bytes();
    data= g_strdup(text.c_str());
  }
  else
  {
    size= _size;
    data= g_memdup(_data, _size);
  }
}


static bool can_show_as_text(const char *data, gsize size)
{
  const gchar *end;
  
  if (g_utf8_validate(data, size, &end))
    return true;
  return false;
  /*
    gsize i;
  for (i= 0; i < size; i++)
  {
    if ((unsigned char)data[i] > 127 || data[i]==0)
      return false;
  }
  return true;
   */
}


void MGBlobEditor::set_data(gpointer data, gsize size)
{
  _data= data;
  _size= size;

  _summary.set_label(ufmt(_("Total Length: %i bytes"), size));

  if (_vtypes & VImage)
  {
    try
    {
      std::string fname="/tmp/qXXXXXX";
      int fd= Glib::mkstemp(fname);
      if (fd < 0)
      {
        g_message("could not create temporary data file for displaying image");
      }
      else
      {
        const char *format;
        
        ::write(fd, _data, _size);
        ::close(fd);
        
        Glib::RefPtr<Gdk::Pixbuf> pixbuf= Gdk::Pixbuf::create_from_file(fname);
        
        _image->set(pixbuf);
        
      switch (myx_guess_image_format((char*)_data, _size))
        {
        case MYX_IMG_JPEG:
          format= "JPEG";
          break;
        case MYX_IMG_PNG:
          format= "PNG";
          break;
        case MYX_IMG_BMP:
          format= "BMP";
          break;
        case MYX_IMG_GIF:
          format= "GIF";
          break;
        default:
          format= "Unknown";
        }
        _image_label->set_label(ufmt(_("Image size: %i x %i\nFormat: %s"), pixbuf->get_width(), pixbuf->get_height(),
                                     format));
      }
    }
    catch (Glib::Error &exc)
    {
      _image_label->set_label(_("Cannot display as image data."));
    }
  }

  if (_vtypes & VText)
  {
    // must convert 1st to a normal string because _length is in bytes, not chars
    _text->get_buffer()->set_text(Glib::ustring(std::string((char*)_data?:"", 0, _size)));
  }

  if (_vtypes & VBinary)
  {
    if (can_show_as_text((const char*)_data, _size))
      _text->get_buffer()->set_text(Glib::ustring(std::string((char*)_data?:"", 0, _size)));
    else
    {
      _text->get_buffer()->set_text(_("Cannot display binary data as text."));
      _text->set_sensitive(false);
    }
    _tree_store->set_data((char*)_data, size);
    _tree->set_model(_tree_store);
  }
}
