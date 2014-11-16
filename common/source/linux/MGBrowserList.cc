/* Copyright (C) 2003, 2004 MySQL AB

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



#include "MGBrowserList.h"
#include "myg_gtkutils.h"
#include "myg_gtk_utils.h"

Gtk::Widget *MGBrowserList::create_search_entry(bool with_popup)
{
  Gtk::Frame *frame;
  Gtk::HBox *box;
  Gtk::Image *image;


  frame= new Gtk::Frame();
  frame->set_shadow_type(Gtk::SHADOW_IN);

  box= new Gtk::HBox(false, 0);
  frame->add(*Gtk::manage(box));


  Gtk::EventBox *evbox;

  evbox= new Gtk::EventBox();
  Glib::RefPtr<Gtk::Style> style= evbox->get_style()->copy();
  style->set_bg(Gtk::STATE_NORMAL, style->get_white());
  style->set_bg(Gtk::STATE_PRELIGHT, style->get_white());
  style->set_bg(Gtk::STATE_ACTIVE, style->get_white());
  style->set_bg(Gtk::STATE_INSENSITIVE, style->get_white());
  evbox->set_style(style);
  box->pack_start(*Gtk::manage(evbox), false, false, 0);
  evbox->show();

  if (with_popup)
  {
    image= new Gtk::Image(PIXCACHE->load("magnify_glass_with_popup.png"));

    evbox->signal_button_press_event().connect(sigc::mem_fun(*this,
                                                          &MGBrowserList::popup_handler));
  }
  else
  {
    image= new Gtk::Image(PIXCACHE->load("magnify_glass.png"));
  }
  evbox->add(*Gtk::manage(image));


  image->show();
  
  _entry= new Gtk::Entry();
  _entry->set_size_request(100, -1);
  box->pack_start(*Gtk::manage(_entry), true, true, 0);
  _entry->set_has_frame(false);
  _entry->show();

  box->show();

  return frame;
}

MGBrowserList::SelectionSignal MGBrowserList::signal_selected() 
{
  return _selection_signal;
}

void MGBrowserList::set_menu_items(std::vector<Glib::ustring> &items)
{
  Gtk::RadioMenuItem *item= 0;
  Gtk::RadioMenuItem::Group group;
  int index= 0;

  delete _menu;
  _menu= new Gtk::Menu();

  for (std::vector<Glib::ustring>::const_iterator i= items.begin();
       i != items.end(); ++i)
  {
    item= new Gtk::RadioMenuItem(group, *i, false);

    group= item->get_group();

    _menu->append(*Gtk::manage(item));
    item->show();

    item->signal_activate().connect(sigc::bind<int>(sigc::mem_fun(*this, &MGBrowserList::menu_activate_handler),
                                              index));
    index++;
  }
}


MGBrowserList::MGBrowserList(bool with_popup, const Glib::ustring &caption)
  : VBox(), _menu(0), _refreshing(false), _selkluge(false), _selected_menu_item(-1)
{
  Glib::RefPtr<Gtk::TreeView::Selection> select;
  Gtk::Widget *box;
  Gtk::ScrolledWindow *sview;
  Gtk::Label *label= 0;
  
  _queued_idle_selection= false;

  if (!caption.empty())
  {
    label= new Gtk::Label(caption);
    pack_start(*Gtk::manage(label), false, true, 0);
    label->set_alignment(0.0, 0.5);
    label->show();
  }

  box= create_search_entry(with_popup);
  pack_start(*Gtk::manage(box), false, false, 0);
  box->show();

  _entry->signal_changed().connect(sigc::mem_fun(*this, &MGBrowserList::entry_changed_handler));

  sview= new Gtk::ScrolledWindow();
  sview->set_shadow_type(Gtk::SHADOW_IN);
  pack_start(*Gtk::manage(sview), true, true, 0);
  sview->show();

  _tree= new CustomTreeView();
  sview->add(*Gtk::manage(_tree));
  sview->set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _tree->show();

  _tree->set_headers_visible(false);

  select= _tree->get_selection();
  select->set_mode(Gtk::SELECTION_SINGLE);

  select->signal_changed().connect(sigc::mem_fun(*this, &MGBrowserList::list_selection_changed));

  //_tree->signal_row_expanded().connect(sigc::mem_fun(*this, &MGBrowserList::list_row_expanded_handler));

  select->set_select_function(sigc::mem_fun(*this, &MGBrowserList::list_selection_function));
}


MGBrowserList::~MGBrowserList()
{
  delete _menu;
}

/*
void MGBrowserList::set_allow_multiple_selection(bool flag)
{
  _tree->get_selection()->set_mode(flag?Gtk::SELECTION_MULTIPLE : Gtk::SELECTION_SINGLE);
}
 */


Gtk::TreeIter MGBrowserList::get_iter(const Gtk::TreePath &path)
{
  return _store->get_iter(path);
}


void MGBrowserList::set_store(const Glib::RefPtr<Gtk::TreeStore> &store)
{
  _tree->set_model(store);
  _store= store;
}


void MGBrowserList::set_popup_menu(Gtk::Menu *menu)
{
  _tree->popup= menu;
}


void MGBrowserList::unselect()
{
  _tree->get_selection()->unselect_all();
}


Gtk::TreeIter MGBrowserList::get_selected()
{
  return _tree->get_selection()->get_selected();
}


void MGBrowserList::set_selection(const Gtk::TreeIter &iter)
{
  Glib::RefPtr<Gtk::TreeView::Selection> sel= _tree->get_selection();

  sel->select(iter);
  _tree->scroll_to_row(Gtk::TreePath(iter), 0.0);
}


bool MGBrowserList::find_node(const Gtk::TreeIter &parent,
                              const Glib::ustring &text,
                              const Gtk::TreeModelColumn<Glib::ustring>& column,
                              Gtk::TreeIter &node_ret)
{
  Gtk::TreeModel::Row row= *parent;
  Gtk::TreeModel::Children children= row.children();

  for (Gtk::TreeIter iter= children.begin(); iter != children.end(); ++iter)
  {
    Gtk::TreeModel::Row row= *iter;

    if (row[column] == text)
    {
      node_ret= iter;
      return true;
    }
  }

  return false;
}


bool MGBrowserList::find_node(const Glib::ustring &text,
                              const Gtk::TreeModelColumn<Glib::ustring>& column,
                              Gtk::TreeIter &node_ret)
{
  Gtk::TreeModel::Children children= _store->children();

  for (Gtk::TreeIter iter= children.begin(); iter != children.end(); ++iter)
  {
    Gtk::TreeModel::Row row= *iter;

    if (row[column] == text)
    {
      node_ret= iter;
      return true;
    }
  }

  return false;
}


void MGBrowserList::refresh()
{
  refresh_list(_entry->get_text());
}


void MGBrowserList::clear()
{
  _store->clear();
}


void MGBrowserList::mark_node(const Gtk::TreeIter &node, 
                              const Gtk::TreeModelColumn<Gdk::Color> &column,
                              bool flag,
                              bool follow_parents)
{
  Gtk::TreeIter parent, iter= node;
  Gdk::Color marked("blue"), black("black");

  do {
    Gtk::TreeModel::Row row= *iter;

    row[column]= flag?marked:black;

    parent= row->parent();
    iter= parent;
  } while (iter && follow_parents);
}


void MGBrowserList::expand_node(const Gtk::TreeIter &node)
{
  _tree->expand_to_path(Gtk::TreePath(node));
}


/**********************************************************************/


static void calc_menu_position(int &x, int &y, bool& push_in,
                               Gtk::Widget *w)
{
  w->get_window()->get_origin(x,y);
  x-= 19;
  y+= w->get_height();
}


bool MGBrowserList::popup_handler(GdkEventButton *event)
{
  _menu->popup(sigc::bind<Gtk::Widget*>(sigc::ptr_fun(calc_menu_position),_entry),
               event->button, event->time);
  return true;
}


void MGBrowserList::menu_activate_handler(int index)
{
  _selected_menu_item= index;
}


void MGBrowserList::entry_changed_handler()
{
  refresh_list(_entry->get_text());
}


bool MGBrowserList::emit_when_idle()
{
  _queued_idle_selection= false;
  _selection_signal.emit(this, get_selected());
  return false;
}


void MGBrowserList::list_selection_changed()
{
  _selkluge= false;

  if (!_refreshing)
  {
    if (!_queued_idle_selection)
    {
      _queued_idle_selection= true;
      // This is a mega-kluge to workaround a shitty bug on Gtk, where it will
      // select the first row in the TreeView whenever you select a row 
      // in a unselected tree.
      Glib::signal_idle().connect(sigc::mem_fun(*this,&MGBrowserList::emit_when_idle));
    }
  }
}


bool MGBrowserList::list_selection_function(const Glib::RefPtr<Gtk::TreeModel> &model,
                                            const Gtk::TreePath &path,
                                            bool path_currently_selected)
{
  bool allow_change= true;

  if (_selkluge)
    return true;
  
  if (!_selection_change_delegate.empty() && !path_currently_selected)
  {
    Gtk::TreeIter iter= _store->get_iter(path);
    
    allow_change= _selection_change_delegate.emit(this, iter);
  }
  
  // kluge! for some reason we're getting list_selection_function twice 
  // whenever the selection changes
  if (allow_change)
    _selkluge= true;

  return allow_change;
}




bool MGBrowserList::CustomTreeView::on_button_press_event(GdkEventButton *ev)
{
  bool return_value= true;
  
  // make the tree ignore right mouse buttons
  // to avoid annoying behaviour when rows are editable or draggable
  // and we want a popup menu
  if ((ev->type != GDK_BUTTON_PRESS) || (ev->button != 3))
  {
    return_value = TreeView::on_button_press_event(ev);
  }
  else if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
  {
    Gtk::TreePath path;
    Gtk::TreeViewColumn *column;
    int cell_x, cell_y;
    
    if (get_path_at_pos((int)ev->x, (int)ev->y, path, column, cell_x, cell_y))
    {
      get_selection()->select(get_model()->get_iter(path));
    }
  }

  if (popup)
  {
    if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
    {
      popup->popup(ev->button, ev->time);
    }
  }

  return return_value;
}


