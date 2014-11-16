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


#include "mygpriv.h"

#include <gtkmm/alignment.h>
#include <gtkmm/image.h>
#include <gtkmm/messagedialog.h>
#include <gtkmm/imagemenuitem.h>
#include <gtkmm/stock.h>
#include <gtkmm/entry.h>

#include "myg_gtkutils.h"
#include "myg_utils.h"


MGTextViewUndoManager::Command::Command(const Glib::ustring &text, unsigned int offset, bool del)
  : _text(text), _offset(offset), _delete(del)
{
}


void MGTextViewUndoManager::Command::undo(Gtk::TextView *view)
{
  if (_delete)
  {
    view->get_buffer()->insert(view->get_buffer()->get_iter_at_offset(_offset),
                               _text);
  }
  else
  {
    view->get_buffer()->erase(view->get_buffer()->get_iter_at_offset(_offset),
                              view->get_buffer()->get_iter_at_offset(_offset+_text.length()));
  }
}


void MGTextViewUndoManager::Command::redo(Gtk::TextView *view)
{
  if (!_delete)
  {
    view->get_buffer()->insert(view->get_buffer()->get_iter_at_offset(_offset),
                               _text);
  }
  else
  {
    view->get_buffer()->erase(view->get_buffer()->get_iter_at_offset(_offset),
                              view->get_buffer()->get_iter_at_offset(_offset+_text.length()));
  }
}





void MGTextViewUndoManager::handleErase(const Gtk::TextIter &from, const Gtk::TextIter &to)
{
  if (_undoing)
    return;

  while (_stackP > 0)
  {
    _stack.pop_back();
    --_stackP;
  }
  Glib::ustring text= _textView->get_buffer()->get_text(from, to);
  unsigned int offs= from.get_offset();

  _stack.push_back(Command(text, offs, true));
}


void MGTextViewUndoManager::handleInsert(const Gtk::TextIter &pos, const Glib::ustring &text, int count)
{
  if (_undoing)
    return;
  
  while (_stackP > 0)
  {
    _stack.pop_back();
    --_stackP;
  }
  unsigned int offs= pos.get_offset() - text.length();

  _stack.push_back(Command(text, offs, false));
}


bool MGTextViewUndoManager::handleKey(GdkEventKey *event)
{
  if (gdk_keyval_to_lower(event->keyval) == 'z')
  {
    if ((event->state & (Gdk::CONTROL_MASK|Gdk::SHIFT_MASK))==(Gdk::CONTROL_MASK|Gdk::SHIFT_MASK))
      redo();
    else if ((event->state & Gdk::CONTROL_MASK)==Gdk::CONTROL_MASK)
      undo();
  }
  return false;
}


void MGTextViewUndoManager::reset()
{
  _stack.clear();
  _stackP= 0;
}


MGTextViewUndoManager::MGTextViewUndoManager(Gtk::TextView *textView)
  : _textView(textView)
{
  _stackP= 0;
  _undoing= false;

  textView->get_buffer()->signal_erase().connect(sigc::mem_fun(*this, &MGTextViewUndoManager::handleErase), false);
  textView->get_buffer()->signal_insert().connect(sigc::mem_fun(*this, &MGTextViewUndoManager::handleInsert));

  textView->signal_key_press_event().connect(sigc::mem_fun(*this, &MGTextViewUndoManager::handleKey));
}


bool MGTextViewUndoManager::canRedo()
{
  return _stackP > 0;
}


bool MGTextViewUndoManager::canUndo()
{
  return !_stack.empty() && _stackP < _stack.size();
}


void MGTextViewUndoManager::undo()
{
  _undoing= true;
  if (canUndo())
  {
    _stack[_stack.size()-1-_stackP].undo(_textView);
    ++_stackP;
  }
  _undoing= false;
}


void MGTextViewUndoManager::redo()
{
  _undoing= true;
  if (canRedo())
  {
    --_stackP;
    _stack[_stack.size()-1-_stackP].redo(_textView);
  }
  _undoing= false;
}


/* ---------------------------------------------------------------------- */


MGPixbufCache *MGPixbufCache::_instance= 0;


MGPixbufCache *MGPixbufCache::instance()
{
  if (!_instance)
    _instance= new MGPixbufCache;

  return _instance;
}


void MGPixbufCache::add_search_path(const std::string &path)
{
  _path.push_back(path);
}


Glib::RefPtr<Gdk::Pixbuf> MGPixbufCache::load(const std::string &file, bool cache)
{
  if (_cache.find(file)!=_cache.end())
    return _cache[file];

  for (std::list<std::string>::const_iterator iter= _path.begin();
       iter != _path.end(); ++iter)
  {
    if (Glib::file_test(*iter+"/"+file, Glib::FILE_TEST_EXISTS))
    {
      if (cache)
      {
        _cache[file]= Gdk::Pixbuf::create_from_file(*iter+"/"+file);
        return _cache[file];
      }
      else
        return Gdk::Pixbuf::create_from_file(*iter+"/"+file);
    }
  }
//  g_message("couldn't locate image %s", file.c_str());
  return Glib::RefPtr<Gdk::Pixbuf>();
}


Glib::RefPtr<Gdk::PixbufAnimation> MGPixbufCache::load_anim(const std::string &file, bool cache)
{
  if (_acache.find(file)!=_acache.end())
    return _acache[file];

  for (std::list<std::string>::const_iterator iter= _path.begin();
       iter != _path.end(); ++iter)
  {
    if (Glib::file_test(*iter+"/"+file, Glib::FILE_TEST_EXISTS))
    {
      if (cache)
      {
        _acache[file]= Gdk::PixbufAnimation::create_from_file(*iter+"/"+file);
        return _acache[file];
      }
      else
        return Gdk::PixbufAnimation::create_from_file(*iter+"/"+file);
    }
  }
//  g_message("couldn't locate image %s", file.c_str());
  return Glib::RefPtr<Gdk::PixbufAnimation>();
}





void myg_make_image_button(Gtk::Button &button,
                           const Glib::RefPtr<Gdk::Pixbuf> &image,
                           const Glib::ustring &label)
{
  Gtk::Alignment *alig= Gtk::manage(new Gtk::Alignment(0.5,0.5,0.0,0.0));
  Gtk::HBox *box= Gtk::manage(new Gtk::HBox(false, 2));
  
  alig->add(*box);

  box->pack_start(*Gtk::manage(new Gtk::Image(image)), false, false);
  box->pack_start(*Gtk::manage(new Gtk::Label(label)), false, false);

  button.add(*alig);
  alig->show_all();
}


void myg_image_button_set(Gtk::Button &button,
                          const Glib::RefPtr<Gdk::Pixbuf> &image,
                          const Glib::ustring &label)
{
  Gtk::Box *box= static_cast<Gtk::Box*>(*static_cast<Gtk::Container*>(button.get_child())->get_children().begin());
  Gtk::Label *label_w;
  Gtk::Image *image_w;

  image_w= static_cast<Gtk::Image*>(*box->get_children().begin());
  label_w= static_cast<Gtk::Label*>(*++box->get_children().begin());
  
  image_w->set(image);
  label_w->set_text(label);
}


void myg_image_button_set(Gtk::Button &button,
                          const Gtk::StockID& lhs,
                          const Glib::ustring &label)
{
  Gtk::Box *box= static_cast<Gtk::Box*>(*static_cast<Gtk::Container*>(button.get_child())->get_children().begin());
  Gtk::Label *label_w;
  Gtk::Image *image_w;

  image_w= static_cast<Gtk::Image*>(*box->get_children().begin());
  label_w= static_cast<Gtk::Label*>(*++box->get_children().begin());
  
  image_w->set(lhs, Gtk::ICON_SIZE_BUTTON);
  label_w->set_text(label);
}


Gtk::MenuItem *myg_make_separator_item()
{
  Gtk::MenuItem *item= Gtk::manage(new Gtk::SeparatorMenuItem());
  item->show();
  return item;
}


Gtk::MenuItem *myg_make_stock_image_item(const Gtk::StockID& id,
                                         const Glib::ustring &label)
{
  Gtk::Image *image= Gtk::manage(new Gtk::Image(id, Gtk::ICON_SIZE_MENU));
  Gtk::ImageMenuItem *item= Gtk::manage(new Gtk::ImageMenuItem(*image, label, true));
  image->show();

  return item;
}


Gtk::MenuItem *myg_make_image_item(const std::string &file,
                                   const Glib::ustring &label)
{
  Gtk::Image *image= Gtk::manage(new Gtk::Image(PIXCACHE->load(file)));
  Gtk::ImageMenuItem *item= Gtk::manage(new Gtk::ImageMenuItem(*image, label, true));
  image->show();

  return item;
}


//---------------------------------------------------------------------------

Gtk::MenuItem* myg_menu_add(Gtk::Menu &menu, const Glib::ustring &label,
                  const sigc::slot<void> &slot, const std::string &id)
{
  Gtk::MenuItem *item= Gtk::manage(new Gtk::MenuItem(label, true));
  menu.append(*item);
  item->signal_activate().connect(slot);
  item->set_data("id", g_strdup(id.c_str()), g_free);
  item->show();
  return item;
}


void myg_menu_add(Gtk::Menu &menu, const Gtk::StockID &icon,
                  const Glib::ustring &label, const sigc::slot<void> &slot,
                  const std::string &id)
{
  Gtk::MenuItem *item= myg_make_stock_image_item(icon, label);
  menu.append(*item);
  item->signal_activate().connect(slot);
  item->set_data("id", g_strdup(id.c_str()), g_free);
  item->show();
}


void myg_menu_add(Gtk::Menu &menu, const std::string &icon, 
                  const Glib::ustring &label, const sigc::slot<void> &slot,
                  const std::string &id)
{
  Gtk::MenuItem *item= myg_make_image_item(icon, label);
  menu.append(*item);
  item->signal_activate().connect(slot);
  item->set_data("id", g_strdup(id.c_str()), g_free);
  item->show();
}

void myg_menu_add(Gtk::Menu &menu)
{
  menu.append(*myg_make_separator_item());
}


void myg_menu_set_sensitive(Gtk::Menu &menu, const std::string &id, bool flag)
{
  bool found= false;

  for (unsigned int i= 0; i < menu.items().size(); i++)
  {
    const char *d= (const char*)menu.items()[i].get_data("id");
    if (d && strcmp(d, id.c_str())==0)
    {
      menu.items()[i].set_sensitive(flag);
      found= true;
      break;
    }
  }
  if (!found)
    g_warning("menu item with identifier %s not found", id.c_str());
}


std::string myg_menu_get_id_at_index(Gtk::Menu &menu, int index)
{
  for (unsigned int i= 0; i < menu.items().size(); i++)
  {
    if (index-- == 0)
    {
      const char *d= (const char*)menu.items()[i].get_data("id");
      return d;
    }
  }
  return "";
}


int myg_menu_get_index_with_id(Gtk::Menu &menu, const std::string &id)
{
  int idx= -1;

  for (unsigned int i= 0; i < menu.items().size(); i++)
  {
    idx++;
    const char *d= (const char*)menu.items()[i].get_data("id");
    if (d && strcmp(d, id.c_str())==0)
      break;
  }
  return idx;
}


//---------------------------------------------------------------------------

bool myg_ask_string(Gtk::Window &parent, const Glib::ustring &title,
                    const Glib::ustring &msg, Glib::ustring &input,
                    bool password)
{
  Gtk::Dialog dlg(title, parent, true, true);
  Gtk::Button *okbtn;

  dlg.add_button(Gtk::Stock::CANCEL, Gtk::RESPONSE_CANCEL);
  okbtn= dlg.add_button(Gtk::Stock::OK, Gtk::RESPONSE_OK);

  Gtk::VBox vbox(false, 20);
  Gtk::Entry entry;
  Gtk::Label label("", 0.0, 0.5);

  label.set_line_wrap(true);
  label.set_use_markup(true);
  label.set_markup(msg);

  dlg.get_vbox()->pack_start(vbox);
  
  if (password)
    entry.set_visibility(false);
  
  entry.signal_activate().connect(sigc::mem_fun(*okbtn,&Gtk::Button::clicked));
  
  vbox.set_border_width(20);
  vbox.pack_start(label);
  vbox.pack_start(entry);
  vbox.show();
  label.show();
  entry.show();

  dlg.set_size_request(350, -1);

  if (dlg.run() == Gtk::RESPONSE_OK)
  {
    input= entry.get_text();
    return true;
  }
  return false;
}


bool myg_ask_string(const Glib::ustring &title,
                    const Glib::ustring &msg, Glib::ustring &input,
                    bool password)
{
  Gtk::Dialog dlg(title, true, true);

  dlg.add_button(Gtk::Stock::CANCEL, Gtk::RESPONSE_CANCEL);
  dlg.add_button(Gtk::Stock::OK, Gtk::RESPONSE_OK);

  Gtk::VBox vbox(false, 20);
  Gtk::Entry entry;
  Gtk::Label label("", 0.0, 0.5);

  label.set_line_wrap(true);
  label.set_use_markup(true);
  label.set_markup(msg);

  if (password)
    entry.set_visibility(false);

  dlg.get_vbox()->pack_start(vbox);

  vbox.set_border_width(20);
  vbox.pack_start(label);
  vbox.pack_start(entry);
  vbox.show();
  label.show();
  entry.show();

  dlg.set_size_request(350, -1);

  if (dlg.run() == Gtk::RESPONSE_OK)
  {
    input= entry.get_text();
    return true;
  }
  return false;
}

//----------------------------------------------------------------------

void myg_show_mysql_error(Gtk::Window &parent, const Glib::ustring &msg, MYSQL *mysql)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG2(dlg, parent, 
                                          msg+ufmt("\nMySQL Error Nr. %i\n%s",
                                                   mysql_errno(mysql),
                                                   mysql_error(mysql)),
                                          Gtk::MESSAGE_ERROR,
                                          Gtk::BUTTONS_OK,
                                          true);
  dlg.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dlg.run();
}


void myg_show_mysql_error(const Glib::ustring &msg, MYSQL *mysql)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG(dlg, 
                                          msg+ufmt("\nMySQL Error Nr. %i\n%s",
                                                   mysql_errno(mysql),
                                                   mysql_error(mysql)),
                                          Gtk::MESSAGE_ERROR,
                                          Gtk::BUTTONS_OK,
                                          true);
  dlg.set_position(Gtk::WIN_POS_CENTER);
  dlg.run();
}


void myg_show_error(Gtk::Window &parent, const Glib::ustring &msg)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG2(dlg,parent, msg,
                                          Gtk::MESSAGE_ERROR,
                                          Gtk::BUTTONS_OK,
                                          true);
  dlg.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dlg.run();
}


void myg_show_error(const Glib::ustring &msg)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG(dlg,msg,
                         Gtk::MESSAGE_ERROR,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER);
  dlg.run();
}


void myg_show_warning(Gtk::Window &parent, const Glib::ustring &msg)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG2(dlg,parent, msg,
                         Gtk::MESSAGE_WARNING,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dlg.run();
}

void myg_show_warning(const Glib::ustring &msg)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG(dlg,msg,
                         Gtk::MESSAGE_WARNING,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER);
  dlg.run();
}

void myg_show_info(Gtk::Window &parent, const Glib::ustring &msg)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG2(dlg,parent, msg,
                         Gtk::MESSAGE_INFO,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dlg.run();
}


void myg_show_info(const Glib::ustring &msg)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG(dlg,msg,
                         Gtk::MESSAGE_INFO,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER);
  dlg.run();
}


void myg_show_xlib_error(Gtk::Window &parent, const Glib::ustring &msg, MYX_LIB_ERROR err)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG2(dlg,parent, 
                                          msg+"\n"+myg_message_for_xlib_error(err),
                         Gtk::MESSAGE_ERROR,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dlg.run();
}


void myg_show_xlib_error(const Glib::ustring &msg, MYX_LIB_ERROR err)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG(dlg,
                                          msg+"\n"+myg_message_for_xlib_error(err),
                         Gtk::MESSAGE_ERROR,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER);
  dlg.run();
}

void myg_show_sys_error(Gtk::Window &parent, const Glib::ustring &msg, int error)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG2(dlg,parent, 
                                          ufmt("%s\n%s",msg.c_str(), strerror(error)),
                         Gtk::MESSAGE_ERROR,
                         Gtk::BUTTONS_OK,
                         true);
  dlg.set_position(Gtk::WIN_POS_CENTER_ON_PARENT);
  dlg.run();
}


void myg_show_sys_error(const Glib::ustring &msg, int error)
{
  Gtk::MessageDialog GTKMM_MESSAGE_DIALOG(dlg,
                                          ufmt("%s\n%s",msg.c_str(), strerror(error)),
                                          Gtk::MESSAGE_ERROR,
                                          Gtk::BUTTONS_OK,
                                          true);
  dlg.set_position(Gtk::WIN_POS_CENTER);
  dlg.run();
}

//----------------------------------------------------------------------
