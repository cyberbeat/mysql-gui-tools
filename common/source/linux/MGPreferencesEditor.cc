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


#include "MGPreferencesEditor.h"
#include "mygpriv.h"

#include "myg_gtkutils.h"

#define DEFAULT_MYSQL_PORT 3306


class MGConnectionGroup : public MGPreferenceGroup {
    class ConnectionColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ConnectionColumns() { add(_icon); add(_text); add(_is_sep); add(_data); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<bool> _is_sep;
        Gtk::TreeModelColumn<UserConnection> _data;
    };
    ConnectionColumns _connection_columns;

    MGGladeXML *_xml;

    Glib::RefPtr<Gtk::TreeStore> _connection_store;
    bool _connection_ignore_changes;

    Glib::RefPtr<Gdk::Pixbuf> _conn_group_icon;
    Glib::RefPtr<Gdk::Pixbuf> _conn_icon;

    void add();
    void remove();
    void show();
    void data_changed(const char *field);

  public:
    MGConnectionGroup(MGGladeXML *xml) : _xml(xml) {};
    virtual void init();
    virtual void update();
    virtual void commit();

    Glib::ustring get_title() { return _("Connection"); }

    virtual Gtk::Widget *widget();
};

static MGAdvancedOption option_list[]=
{
  {"COMPRESS", "option1_check", 'B'},
  {"USE_SSL", "option2_check", 'B'},
  {"SOCKET_PATH", "option1_entry", 'S'},
  {NULL, NULL, 0}
};


void MGConnectionGroup::init()
{
#define CONNECT_CHANGED(type,name) ((type*)_xml->get_widget(name))->signal_changed().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGConnectionGroup::data_changed),name))
  CONNECT_CHANGED(Gtk::Entry, "connection_entry");
  CONNECT_CHANGED(Gtk::Entry, "username_entry");
  CONNECT_CHANGED(Gtk::Entry, "password_entry");
  CONNECT_CHANGED(Gtk::Entry, "hostname_entry");
  CONNECT_CHANGED(Gtk::SpinButton, "port_spin");
  CONNECT_CHANGED(Gtk::Entry, "schema_entry");
  ((Gtk::TextView*)_xml->get_widget("connection_note_text"))->get_buffer()->signal_changed().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGConnectionGroup::data_changed),"connection_note_text"));

  ((Gtk::Button*)_xml->get_widget("connection_add_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MGConnectionGroup::add));
  ((Gtk::Button*)_xml->get_widget("connection_remove_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MGConnectionGroup::remove));
  
  for (unsigned int i= 0; option_list[i].name; i++)
  {
    switch (option_list[i].type)
    {
    case 'B':
      ((Gtk::ToggleButton*)_xml->get_widget(option_list[i].widget))->signal_toggled().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGConnectionGroup::data_changed),option_list[i].widget));
      break;
    case 'S':
      ((Gtk::Entry*)_xml->get_widget(option_list[i].widget))->signal_changed().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGConnectionGroup::data_changed),option_list[i].widget));
      break;
    }
  }
  
#undef CONNECT_CHANGED

  Gtk::TreeView *tree= (Gtk::TreeView*)_xml->get_widget("connection_tree");

  tree->get_selection()->set_mode(Gtk::SELECTION_BROWSE);
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGConnectionGroup::show));

  tree->append_column("", _connection_columns._icon);
  tree->append_column("", _connection_columns._text);

  _connection_store= Gtk::TreeStore::create(_connection_columns);

  tree->set_model(_connection_store);

  _conn_group_icon= PIXCACHE->load("folder_16x16.png");
  _conn_icon= PIXCACHE->load("networkhost_16x16.png");
  
  _connection_ignore_changes= false;
}


void MGConnectionGroup::update()
{
  Gtk::TreeIter conn_iter, hist_iter;
  Gtk::TreeModel::Row row;

  _connection_store->clear();

  conn_iter= _connection_store->append();
  row= *conn_iter;
  row[_connection_columns._icon]= _conn_group_icon;
  row[_connection_columns._text]= _("Connections");
  row[_connection_columns._is_sep]= true;

  hist_iter= _connection_store->append();
  row= *hist_iter;
  row[_connection_columns._icon]= _conn_group_icon;
  row[_connection_columns._text]= _("History");
  row[_connection_columns._is_sep]= true;

  int idx= 0;
  Gtk::TreeIter sel;
  for (std::vector<UserConnection>::const_iterator conn= _owner->_connections->connections.begin();
       conn != _owner->_connections->connections.end(); ++conn, ++idx)
  {
    Gtk::TreeIter iter;

    if (conn->storage_type==MYX_FAVORITE_USER_CONNECTION)
      iter= _connection_store->append((*conn_iter).children());
    else
      iter= _connection_store->append((*hist_iter).children());

    row= *iter;
    row[_connection_columns._icon]= _conn_icon;
    row[_connection_columns._text]= conn->connection_name.empty() ?"history":conn->connection_name;
    row[_connection_columns._is_sep]= false;
    row[_connection_columns._data]= *conn;
    
    if (_owner->_connections->last_connection == idx)
      sel= iter;
  }

  ((Gtk::TreeView*)_xml->get_widget("connection_tree"))->expand_all();

  if (sel)
    ((Gtk::TreeView*)_xml->get_widget("connection_tree"))->get_selection()->select(sel);

  _xml->get_widget("connection_notebook")->set_sensitive(false);
}


void MGConnectionGroup::commit()
{
  _owner->_connections->connections.clear();
  _owner->_connections->last_connection= -1;

  // go through everything adding the connections in the list

  Gtk::TreeIter sel= ((Gtk::TreeView*)_xml->get_widget("connection_tree"))->get_selection()->get_selected();
  int idx= 0;
  Gtk::TreeIter iter, piter;
  Gtk::TreeModel::Row row, prow;
  
  for (piter= _connection_store->children().begin(); 
       piter != _connection_store->children().end(); ++piter, ++idx)
  {
    prow= *piter;
    for (iter= prow.children().begin(); iter != prow.children().end(); ++iter)
    {
      row= *iter;
      UserConnection user(row[_connection_columns._data]);
      
      _owner->_connections->connections.push_back(user);
      if (sel && sel == iter)
        _owner->_connections->last_connection= idx;
    }
  }
}


void MGConnectionGroup::add()
{
  UserConnection con;

  con.connection_name= "new connection";
  con.username= "";
  con.password= "";
  con.hostname= "";
  con.port= DEFAULT_MYSQL_PORT;
  con.schema= "";
  con.connection_type= MYX_MYSQL_CONN;
  con.storage_type= MYX_FAVORITE_USER_CONNECTION;

  Gtk::TreeModel::Row prow= *_connection_store->children().begin();
  
  Gtk::TreeView *tree= (Gtk::TreeView*)_xml->get_widget("connection_tree");
  Gtk::TreeIter iter= _connection_store->append(prow.children());
  Gtk::TreeModel::Row row= *iter;

  tree->expand_to_path(Gtk::TreePath(_connection_store->children().begin()));

  row[_connection_columns._icon]= _conn_icon;
  row[_connection_columns._text]= con.connection_name;
  row[_connection_columns._is_sep]= false;
  row[_connection_columns._data]= con;

  tree->get_selection()->select(iter);
  
  set_dirty();
  
  _xml->get_widget("connection_entry")->grab_focus();
}


void MGConnectionGroup::remove()
{
  Gtk::TreeView *tree= (Gtk::TreeView*)_xml->get_widget("connection_tree");
  Gtk::TreeIter iter= tree->get_selection()->get_selected();

  _connection_store->erase(iter);
  
  set_dirty();
}


void MGConnectionGroup::show()
{
  Gtk::TreeView *tree= (Gtk::TreeView*)_xml->get_widget("connection_tree");
  Gtk::TreeIter iter= tree->get_selection()->get_selected();
  UserConnection user;
  bool sens= false;

  _connection_ignore_changes= true;
  
  if (iter)
  {
    Gtk::TreeModel::Row row= *iter;

    if (!row[_connection_columns._is_sep])
    {
      user= row[_connection_columns._data];
      sens= true;
      
      _xml->get_widget("connection_remove_button")->set_sensitive(true);
    }
    else
    {
      _xml->get_widget("connection_remove_button")->set_sensitive(false);
    }
  }

  ((Gtk::Entry*)_xml->get_widget("connection_entry"))->set_text(user.connection_name);
  ((Gtk::Entry*)_xml->get_widget("username_entry"))->set_text(user.username);
  ((Gtk::Entry*)_xml->get_widget("password_entry"))->set_text(user.password);
  ((Gtk::Entry*)_xml->get_widget("hostname_entry"))->set_text(user.hostname);
  ((Gtk::SpinButton*)_xml->get_widget("port_spin"))->set_value(user.port);
  ((Gtk::Entry*)_xml->get_widget("schema_entry"))->set_text(user.schema);
  
  ((Gtk::TextView*)_xml->get_widget("connection_note_text"))->get_buffer()->set_text(user.notes);

  myg_show_advanced_options(_xml, option_list, user.advanced_options);

  _xml->get_widget("connection_notebook")->set_sensitive(sens);

  _connection_ignore_changes= false;
}


void MGConnectionGroup::data_changed(const char *field)
{
  Gtk::TreeIter iter= ((Gtk::TreeView*)_xml->get_widget("connection_tree"))->get_selection()->get_selected();

  if (_connection_ignore_changes || !iter)
    return;

  Gtk::TreeModel::Row row= *iter;

  if (strcmp(field, "connection_entry")==0)
  {    
    row[_connection_columns._text]= ((Gtk::Entry*)_xml->get_widget(field))->get_text();
  }

  UserConnection user= row[_connection_columns._data];

  user.connection_name= ((Gtk::Entry*)_xml->get_widget("connection_entry"))->get_text();
  user.username= ((Gtk::Entry*)_xml->get_widget("username_entry"))->get_text();
  user.password= ((Gtk::Entry*)_xml->get_widget("password_entry"))->get_text();
  user.hostname= ((Gtk::Entry*)_xml->get_widget("hostname_entry"))->get_text();
  if (!((Gtk::SpinButton*)_xml->get_widget("port_spin"))->get_text().empty())
    ((Gtk::SpinButton*)_xml->get_widget("port_spin"))->update();
  user.port= ((Gtk::SpinButton*)_xml->get_widget("port_spin"))->get_value_as_int();
  user.schema= ((Gtk::Entry*)_xml->get_widget("schema_entry"))->get_text();
  user.notes= ((Gtk::TextView*)_xml->get_widget("connection_note_text"))->get_buffer()->get_text();

  myg_fetch_advanced_options(_xml, option_list, user.advanced_options);

  row[_connection_columns._data]= user;

  set_dirty();
}


Gtk::Widget *MGConnectionGroup::widget()
{
  return _xml->get_widget("connection_prefs");
}



//-----------------------------------------------------------------------------

class MGGeneralGroup : public MGPreferenceGroup {
    MGGladeXML *_xml;
    
    void pw_store_clicked();
  public:
    MGGeneralGroup(MGGladeXML *xml) : _xml(xml) {};
    virtual void init();
    virtual void update();
    virtual void commit();

    Glib::ustring get_title() { return _("General Options"); }

    virtual Gtk::Widget *widget();
};


void MGGeneralGroup::pw_store_clicked()
{
  Gtk::CheckButton *ck= (Gtk::CheckButton*)_xml->get_widget("general_store_pw_check");
  
  _xml->get_widget("general_store_pw_box")->set_sensitive(ck->get_active());
}

void MGGeneralGroup::init()
{
  Gtk::ToggleButton *ck= _xml->get_toggle("general_store_pw_check");

  ck->signal_clicked().connect(sigc::mem_fun(*this,&MGGeneralGroup::pw_store_clicked));
  ck->signal_toggled().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));

  _xml->get_combo("general_store_pw_menu")->signal_changed().connect(sigc::mem_fun(*this,&MGPreferenceGroup::set_dirty));
}


void MGGeneralGroup::update()
{
  if (_prefs->password_storage_type == MYX_PASSWORD_NOT_STORED)
  {
    _xml->get_toggle("general_store_pw_check")->set_active(false);
  }
  else
  {
    _xml->get_toggle("general_store_pw_check")->set_active(true);
    _xml->get_combo("general_store_pw_menu")->set_active((int)_prefs->password_storage_type-2);
  }
  pw_store_clicked();
}


void MGGeneralGroup::commit()
{
  if (_xml->get_toggle("general_store_pw_check")->get_active())
    _prefs->password_storage_type= (MYX_PASSWORD_STORAGE_TYPE)(_xml->get_combo("general_store_pw_menu")->get_active_row_number()+(int)MYX_PASSWORD_PLAINTEXT);
  else
    _prefs->password_storage_type= MYX_PASSWORD_NOT_STORED;
}

Gtk::Widget *MGGeneralGroup::widget()
{
  return _xml->get_widget("general_prefs");
}

//-----------------------------------------------------------------------------

void MGPreferenceGroup::set_dirty()
{
  _owner->set_dirty();
}
  

//-----------------------------------------------------------------------------

static MGPreferencesEditor *Instance= 0;
static MGPreferences *InstancePrefs= 0;


MGPreferencesEditor::MGPreferencesEditor(GtkWindow *widget)
  : Gtk::Window(widget), _dirty(false), _connections(0)
{
}


MGPreferencesEditor::~MGPreferencesEditor()
{
  for (std::list<MGPreferenceGroup*>::iterator iter= _groups.begin();
       iter != _groups.end(); ++iter)
  {
    delete (*iter);
  }

  delete _xml;
  delete _connections;
}


void MGPreferencesEditor::setup(MGPreferences *prefs)
{
  InstancePrefs= prefs;
}


MGPreferencesEditor *MGPreferencesEditor::instance()
{
  if (Instance)
    return Instance;
  
  MGGladeXML *xml= new MGGladeXML(myg_datadir+"/preferences.glade");

  MGPreferencesEditor *editor= 0;
  xml->get_widget_derived("preferences_window", editor);
  editor->_xml= xml;
  editor->_prefs= InstancePrefs;
  
  Instance= editor;
  
  return editor;
}


void MGPreferencesEditor::init()
{
  Gtk::Widget *heading= get_widget("heading");
  heading->signal_expose_event().connect(sigc::mem_fun(*this,&MGPreferencesEditor::expose_heading));
  MGPreferenceGroup *group;

  group= new MGGeneralGroup(_xml);
  _buttons.push_back((Gtk::ToggleButton*)_xml->get_widget("general_radio"));
  _groups.push_back(group);

  _connGroup= group= new MGConnectionGroup(_xml);
  _buttons.push_back((Gtk::ToggleButton*)_xml->get_widget("connections_radio"));
  _groups.push_back(group);

  ((Gtk::Button*)_xml->get_widget("close_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MGPreferencesEditor::close_clicked));
  ((Gtk::Button*)_xml->get_widget("discard_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MGPreferencesEditor::discard_clicked));
  ((Gtk::Button*)_xml->get_widget("apply_button"))->signal_clicked().connect(sigc::mem_fun(*this,&MGPreferencesEditor::apply_clicked));
  
  signal_delete_event().connect(sigc::mem_fun(*this,&MGPreferencesEditor::close_window));
  
  // init subpanels
  std::list<Gtk::ToggleButton*>::iterator titer= _buttons.begin();
  for (std::list<MGPreferenceGroup*>::iterator iter= _groups.begin();
       iter != _groups.end(); ++iter, ++titer)
  {
    (*iter)->_prefs= _prefs;
    (*iter)->_owner= this;
    (*titer)->signal_toggled().connect(sigc::bind<MGPreferenceGroup*>(sigc::mem_fun(*this,&MGPreferencesEditor::menu_changed),*iter));
    (*iter)->init();
  }

  (*_buttons.begin())->set_active();
//  menu_changed(*_groups.begin());

  update();
  
  _dirty= false;
  update_sensitivity();
}


void MGPreferencesEditor::addGroup(MGPreferenceGroup *group, const Glib::ustring &caption,
                                 Glib::RefPtr<Gdk::Pixbuf> icon)
{
  Gtk::VBox *vb= Gtk::manage(new Gtk::VBox(false, 0));
  Gtk::Image *img= Gtk::manage(new Gtk::Image(icon));
  Gtk::Label *txt= Gtk::manage(new Gtk::Label());
  Gtk::RadioButton *radio;
  
  txt->set_markup("<small>"+caption+"</small>");
  
  vb->pack_start(*img);
  vb->pack_start(*txt, false, false);

  Gtk::RadioButton::Group g= ((Gtk::RadioButton*)_xml->get_widget("connections_radio"))->get_group();
  radio= Gtk::manage(new Gtk::RadioButton(g));
  radio->property_draw_indicator()=false;
  radio->add(*vb);
  radio->show_all();
  
  _buttons.push_back(radio);
  _groups.push_back(group);

  ((Gtk::Box*)_xml->get_widget("vbox2"))->pack_start(*radio, false, false);
  ((Gtk::Box*)_xml->get_widget("vbox2"))->reorder_child(*radio, _groups.size());

  ((Gtk::Notebook*)_xml->get_widget("notebook"))->insert_page(*group->widget(), "", _groups.size()-1);
}


void MGPreferencesEditor::show(bool connection_only)
{
  if (connection_only)
  {
    get_widget("scrolledwindow1")->hide();
    menu_changed(_connGroup);
  }
  else
  {
    get_widget("scrolledwindow1")->show();
    (*_buttons.begin())->clicked();
    menu_changed(*_groups.begin());
  }
  
  Gtk::Window::show();
}



void MGPreferencesEditor::update()
{
  MYX_LIB_ERROR merror;
  MYX_USER_CONNECTIONS *conns;

  conns= myx_load_user_connections(_prefs->build_path_to(_prefs->connections_filename).c_str(),
                                   &merror);
  if (conns)
  {
    if (_connections)
      delete _connections;
    _connections= new UserConnectionList(conns);
    myx_free_user_connections(conns);
  }
  else
  {
    _connections= new UserConnectionList;
  }

  for (std::list<MGPreferenceGroup*>::iterator iter= _groups.begin();
       iter != _groups.end(); ++iter)
    (*iter)->update();
}


bool MGPreferencesEditor::close_window(GdkEventAny *ev)
{
  close_clicked();

  return true;
}


void MGPreferencesEditor::discard_clicked()
{
  for (std::list<MGPreferenceGroup*>::iterator iter= _groups.begin();
       iter != _groups.end(); ++iter)
    (*iter)->update();
  unset_dirty();
}


void MGPreferencesEditor::apply_clicked()
{
  for (std::list<MGPreferenceGroup*>::iterator iter= _groups.begin();
       iter != _groups.end(); ++iter)
    (*iter)->commit();

  // save
  myx_store_user_connections(_connections->get_obj(),
                             _prefs->password_storage_type,
                             _prefs->build_path_to(_prefs->connections_filename).c_str());

  _prefs->save();
  
  unset_dirty();
  
  _changed_signal.emit();
}


void MGPreferencesEditor::close_clicked()
{
  hide();
  _closed_signal.emit();
}


bool MGPreferencesEditor::expose_heading(GdkEventExpose *expose)
{
  if (expose->count == 0)
  {
    Gtk::Widget *heading= get_widget("heading");
    Glib::RefPtr<Gdk::Window> win= heading->get_window();
    Glib::RefPtr<Gtk::Style> style= heading->get_style();
    int w= heading->get_width();
    int h= heading->get_height();

    win->clear();
    win->draw_rectangle(style->get_dark_gc(Gtk::STATE_NORMAL), true,
                        0, 0, w, h);
    win->draw_line(style->get_black_gc(), 0, 0, w, 0);
    win->draw_line(style->get_black_gc(), 0, 0, 0, h);

    win->draw_line(style->get_white_gc(), 0, h-1, w, h-1);
    win->draw_line(style->get_white_gc(), w-1, 0, w-1, h);
  
    Glib::RefPtr<Pango::Layout> l= create_pango_layout("");
    int tw, th;
    l->set_markup("<b>"+_heading+"</b>");
    l->get_pixel_size(tw, th);
    win->draw_layout(style->get_white_gc(), 5, (h - th) / 2, l);
  }
  return true;
}


void MGPreferencesEditor::menu_changed(MGPreferenceGroup *group)
{
  _heading= group->get_title();

  get_widget("heading")->queue_draw();

  for (unsigned int i= 0; i < _groups.size(); i++)
  {
    if (((Gtk::Notebook*)get_widget("notebook"))->get_nth_page(i) == group->widget())
    {
      ((Gtk::Notebook*)get_widget("notebook"))->set_current_page(i);
      break;
    }
  }
}


void MGPreferencesEditor::update_sensitivity()
{
  get_widget("apply_button")->set_sensitive(_dirty);
  get_widget("discard_button")->set_sensitive(_dirty);
}


void MGPreferencesEditor::set_dirty()
{
  if (_dirty != true)
  {
    _dirty= true;
    update_sensitivity();
  }
}

void MGPreferencesEditor::unset_dirty()
{
  if (_dirty != false)
  {
    _dirty= false;
    update_sensitivity();
  }
}
