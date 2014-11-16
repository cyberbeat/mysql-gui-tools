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

   
#include "myadmin.h"
#include "MAdministrator.h"
#include "MDataInterface.h"
#include "MAUserAdministrationPanel.h"

#include "MGUserBrowserList.h"
#include "MGPreferencesEditor.h"

#include "myg_utils.h"
#include "myg_gtk_utils.h"
#include "myg_gtkutils.h"

#include "myx_util_functions.h"

// first call super::refresh_list
// then add schemas from MYX_USER that do not exist in the tree
void MGUserSchemaBrowserList::refresh_list(const Glib::ustring &filter)
{
  MYX_USER * user = _panel.get_current_user();
  //const char * uname = user ? (char *)user->user_name : "";

  MGSchemaBrowserList::refresh_list(filter);  
  
  if(user)
  {
    for(unsigned int i = 0; i < user->user_object_privileges_num; i++)
    {
      const char *oname= (const char *)user->user_object_privileges[i].object_name;
      if((strlen(oname) == 0) || (strchr(oname, '.') != NULL))
        continue;
      MYX_SCHEMA * sch= new MYX_SCHEMA;
      sch->schema_name= g_strdup(oname);
      sch->catalog_name= NULL;
      if(!contains_schema(sch))
      {
        append_schema(sch);
      }
      else
      {
        delete sch;
      }
    }
  }  
}


MAUserAdministrationPanel::MAUserAdministrationPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _user_names(0),
      _current_user(0), _current_user_icon(0), _current_icon_data(0),
      _global_privileges_template(0),
      _schema_privileges_template(0), _table_privileges_template(0),
      _column_privileges_template(0), _proc_privileges_template(0),
      _user_browser(0), _schema_browser(0), _table_browser(0),
      _first_show(true), _dirty(false), 
      _ignore_entry_changes(false), _ignore_limit_changes(false)
{
  _instance= data->get_instance();

  _apply_changes_on_user_switch= false;
  _discard_changes_on_user_switch= false;

  _instance->signal_disconnect().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::disconnected_cb));
}


MAUserAdministrationPanel::~MAUserAdministrationPanel()
{
  delete _dialog_xml;
  delete _host_dialog_xml;
  // XXX free stuff!
}



void MAUserAdministrationPanel::set_dirty()
{
  if (!_dirty)
  {
    _dirty= true;
    update_sensitivity();
  }
}


void MAUserAdministrationPanel::setup_list_views()
{
  Glib::RefPtr<Gtk::TreeSelection> sel;
  Gtk::TreeView *tree;

  // ** setup global priv tables

  //--
  _global_assigned_privs= Gtk::ListStore::create(_assigned_columns);
  _global_assigned_privs->set_sort_func(_assigned_columns._text,
                                        sigc::mem_fun(*this, &MAUserAdministrationPanel::assigned_list_comparer));

  tree= (Gtk::TreeView*)get_widget("global_assigned_list");
  tree->set_model(_global_assigned_privs);

  tree->append_column("", _assigned_columns._icon);
  tree->append_column("", _assigned_columns._text);

  sel= tree->get_selection();
  sel->set_mode(Gtk::SELECTION_MULTIPLE);
  sel->signal_changed().connect(sigc::bind<Glib::RefPtr<Gtk::TreeSelection>,bool>
                                (sigc::mem_fun(*this, &MAUserAdministrationPanel::global_privilege_list_selected),
                                 sel, true));

  //--
  _global_available_privs= Gtk::ListStore::create(_available_columns);
  _global_available_privs->set_sort_func(_available_columns._text,
                                         sigc::mem_fun(*this, &MAUserAdministrationPanel::available_list_comparer));

  tree= (Gtk::TreeView*)get_widget("global_available_list");
  tree->set_model(_global_available_privs);

  tree->append_column("", _available_columns._icon);
  tree->append_column("", _available_columns._text);
  tree->append_column("", _available_columns._help);

  sel= tree->get_selection();
  sel->set_mode(Gtk::SELECTION_MULTIPLE);
  sel->signal_changed().connect(sigc::bind<Glib::RefPtr<Gtk::TreeSelection>,bool>
                                (sigc::mem_fun(*this, &MAUserAdministrationPanel::global_privilege_list_selected),
                                 sel, false));

  // ** setup schema priv tables

  //--
  _schema_assigned_privs= Gtk::ListStore::create(_assigned_columns);
  _schema_assigned_privs->set_sort_func(_assigned_columns._text,
                                        sigc::mem_fun(*this, &MAUserAdministrationPanel::assigned_list_comparer));

  tree= (Gtk::TreeView*)get_widget("schema_assigned_list");
  tree->set_model(_schema_assigned_privs);

  tree->append_column("", _assigned_columns._icon);
  tree->append_column("", _assigned_columns._text);
  {
    Gtk::TreeViewColumn *column= tree->get_column(1);
    column->add_attribute(((Gtk::CellRendererText*)column->get_first_cell_renderer())->property_foreground(),
                          _assigned_columns._color);
  }

  sel= tree->get_selection();
  sel->set_mode(Gtk::SELECTION_MULTIPLE);
  sel->signal_changed().connect(sigc::bind<Glib::RefPtr<Gtk::TreeSelection>,bool>
                                (sigc::mem_fun(*this, &MAUserAdministrationPanel::schema_privilege_list_selected),
                                 sel, true));

  //--
  _schema_available_privs= Gtk::ListStore::create(_available_columns);
  _schema_available_privs->set_sort_func(_available_columns._text,
                                         sigc::mem_fun(*this, &MAUserAdministrationPanel::available_list_comparer));

  tree= (Gtk::TreeView*)get_widget("schema_available_list");
  tree->set_model(_schema_available_privs);
  
  tree->append_column("", _available_columns._icon);
  tree->append_column("", _available_columns._text);
  tree->append_column("", _available_columns._help);

  sel= tree->get_selection();
  sel->set_mode(Gtk::SELECTION_MULTIPLE);
  sel->signal_changed().connect(sigc::bind<Glib::RefPtr<Gtk::TreeSelection>,bool>
                                (sigc::mem_fun(*this, &MAUserAdministrationPanel::schema_privilege_list_selected),
                                 sel, false));

  // ** setup table/column priv tables

  //--
  _table_assigned_privs= Gtk::ListStore::create(_assigned_columns);
  _table_assigned_privs->set_sort_func(_assigned_columns._text,
                                        sigc::mem_fun(*this, &MAUserAdministrationPanel::assigned_list_comparer));

  tree= (Gtk::TreeView*)get_widget("table_assigned_list");
  tree->set_model(_table_assigned_privs);

  tree->append_column("", _assigned_columns._icon);
  tree->append_column("", _assigned_columns._text);

  sel= tree->get_selection();
  sel->set_mode(Gtk::SELECTION_MULTIPLE);
  sel->signal_changed().connect(sigc::bind<Glib::RefPtr<Gtk::TreeSelection>,bool>
                                (sigc::mem_fun(*this, &MAUserAdministrationPanel::table_privilege_list_selected),
                                 sel, true));

  //--
  _table_available_privs= Gtk::ListStore::create(_available_columns);
  _table_available_privs->set_sort_func(_available_columns._text,
                                        sigc::mem_fun(*this, &MAUserAdministrationPanel::available_list_comparer));

  tree= (Gtk::TreeView*)get_widget("table_available_list");
  tree->set_model(_table_available_privs);

  tree->append_column("", _available_columns._icon);
  tree->append_column("", _available_columns._text);
  tree->append_column("", _available_columns._help);

  sel= tree->get_selection();
  sel->set_mode(Gtk::SELECTION_MULTIPLE);
  sel->signal_changed().connect(sigc::bind<Glib::RefPtr<Gtk::TreeSelection>,bool>
                                (sigc::mem_fun(*this, &MAUserAdministrationPanel::table_privilege_list_selected),
                                 sel, false));
}

static Gtk::MenuItem *make_item(Gtk::Menu *menu, 
                                const char *text,
                                const Gtk::StockID &icon)
{
  Gtk::Image *image= Gtk::manage(new Gtk::Image(icon, Gtk::ICON_SIZE_MENU));
  Gtk::MenuItem *item= new Gtk::ImageMenuItem(*image, text, true);
  image->show();
  item->show();
  menu->append(*Gtk::manage(item));

  return item;
}


void MAUserAdministrationPanel::connect_signals()
{
#define CONNECT_CLICKED(name, cb) \
  ((Gtk::Button*)_xml->get_widget(name))->signal_clicked().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::cb))

#define CONNECT_CLICKEDB(name, cb, val) \
  ((Gtk::Button*)_xml->get_widget(name))->signal_clicked().connect(sigc::bind<bool>(sigc::mem_fun(*this, &MAUserAdministrationPanel::cb),val))

#define CONNECT_CHANGED(name, cb) \
  ((Gtk::Entry*)_xml->get_widget(name))->signal_changed().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::cb))

  CONNECT_CLICKEDB("global_grant_button", global_revoke_grant_button_clicked,
                   false);
  CONNECT_CLICKEDB("global_revoke_button", global_revoke_grant_button_clicked,
                   true);

  CONNECT_CLICKEDB("schema_grant_button", schema_revoke_grant_button_clicked,
                   false);
  CONNECT_CLICKEDB("schema_revoke_button", schema_revoke_grant_button_clicked,
                   true);

  CONNECT_CLICKEDB("table_grant_button", table_revoke_grant_button_clicked,
                   false);
  CONNECT_CLICKEDB("table_revoke_button", table_revoke_grant_button_clicked,
                   true);

  CONNECT_CLICKED("icon_change_button", icon_change_button_clicked);

  CONNECT_CLICKED("apply_button", apply_button_clicked);
  CONNECT_CLICKED("discard_button", discard_button_clicked);
  CONNECT_CLICKED("new_user_button", new_user_button_clicked);
 
  ((Gtk::TextView*)get_widget("contact_text"))->get_buffer()->signal_changed().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::entry_changed));
  CONNECT_CHANGED("user_entry", entry_changed);
  CONNECT_CHANGED("password_entry", entry_changed);
  CONNECT_CHANGED("confirm_entry", entry_changed);
  CONNECT_CHANGED("fullname_entry", entry_changed);
  CONNECT_CHANGED("description_entry", entry_changed);
  CONNECT_CHANGED("email_entry", entry_changed);

  get_spin("maxq_spin")->signal_changed().connect(sigc::bind<const char*>
                                                  (sigc::mem_fun(*this,&MAUserAdministrationPanel::limit_changed),"maxq_spin"));
  get_spin("maxc_spin")->signal_changed().connect(sigc::bind<const char*>
                                                  (sigc::mem_fun(*this,&MAUserAdministrationPanel::limit_changed),"maxc_spin"));
  get_spin("maxu_spin")->signal_changed().connect(sigc::bind<const char*>
                                                  (sigc::mem_fun(*this,&MAUserAdministrationPanel::limit_changed),"maxu_spin"));
  
  
  _user_browser->signal_selected().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::user_selected));
  _user_browser->signal_selection_will_change().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::user_will_change_delegate));
  _schema_browser->signal_selected().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::schema_selected));
  _table_browser->signal_selected().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::table_selected));
}


bool MAUserAdministrationPanel::init()
{
  Glib::RefPtr<Gdk::Pixbuf> schema_icon;
  Glib::RefPtr<Gdk::Pixbuf> sys_schema_icon;
  Glib::RefPtr<Gdk::Pixbuf> table_icon;
  Glib::RefPtr<Gdk::Pixbuf> column_icon;
  Glib::RefPtr<Gdk::Pixbuf> sp_icon;

  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_USERADMINISTRATION_FILE), 
                                "panel_frame"))
    return false;

  _dialog_xml= new MGGladeXML(get_glade_file(GLADE_USERADMINISTRATION_FILE),
                              "icon_selection", "mysql-administrator");

  _host_dialog_xml= new MGGladeXML(get_glade_file(GLADE_USERADMINISTRATION_FILE),
                                   "add_host_dialog", "mysql-administrator");

  
  _size_group= Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);
  _size_group->add_widget(*get_widget("left_label1"));
  _size_group->add_widget(*get_widget("left_label2"));
  _size_group->add_widget(*get_widget("left_label3"));
  _size_group->add_widget(*get_widget("left_label4"));
  _size_group->add_widget(*get_widget("left_label5"));
  _size_group->add_widget(*get_widget("left_label6"));
  _size_group->add_widget(*get_widget("left_label7"));
  _size_group->add_widget(*get_widget("left_label8"));

  {
    Gtk::MenuItem *item;
    int i= 0;
    
    _menu= new Gtk::Menu();
  
    item= make_item(_menu, _("_New User"), Gtk::Stock::NEW);
    _user_menu_items[i++]= item;
    item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::add_user_activate));

/*    item= make_item(_menu, _("_Clone User"), Gtk::Stock::COPY);
    _user_menu_items[i++]= item;
    item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::clone_user_activate));
*/
    item= new Gtk::SeparatorMenuItem();
    item->show();
    _menu->append(*Gtk::manage(item));

    item= make_item(_menu, _("Add _Host"), Gtk::Stock::ADD);
    _user_menu_items[i++]= item;
    item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::add_host_activate));

    item= make_item(_menu, _("_Remove Host"), Gtk::Stock::REMOVE);
    _user_menu_items[i++]= item;
    item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::remove_host_activate));

    item= new Gtk::SeparatorMenuItem();
    item->show();
    _menu->append(*Gtk::manage(item));

    item= make_item(_menu, _("R_emove User"), Gtk::Stock::DELETE);
    _user_menu_items[i++]= item;
    item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::remove_user_activate));

    
    item= new Gtk::SeparatorMenuItem();
    item->show();
    _menu->append(*Gtk::manage(item));

    item= make_item(_menu, _("Refresh list"), Gtk::Stock::REFRESH);
    _user_menu_items[i++]= item;
    item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::refresh_user_list));
  }

  _default_user_icon= PIXCACHE->load("24x24_User.png");
  if (!_default_user_icon)
  {
    g_warning("couldn't load default user icon");
  }
  else
  {
    _default_user_mini_icon= _default_user_icon->scale_simple(16, 16, Gdk::INTERP_BILINEAR);
  }
  _right_icon= PIXCACHE->load("16x16_Right.png");
  if (!_right_icon)
  {
    g_warning("couldn't load privilege right icon");
  }
  
  schema_icon= PIXCACHE->load("16x16_Database.png");
  sys_schema_icon= schema_icon;

  table_icon= PIXCACHE->load("16x16_Table.png");
  column_icon= PIXCACHE->load("16x16_Field.png");
  sp_icon= PIXCACHE->load("myx_schema_sp_16x16.png");

  _user_browser= new MGUserBrowserList(_("User Accounts"));
  _user_browser->set_default_icon(_default_user_mini_icon);
  _user_browser->set_populate_func(sigc::mem_fun(*this,&MAUserAdministrationPanel::populate_user_host));
  _user_browser->set_popup_menu(_menu);

  _schema_browser= new MGUserSchemaBrowserList(*this, "", MGSchemaBrowserList::SchemaOnly);
  _schema_browser->set_mark_delegate(schema_mark_delegate, this);
  _schema_browser->set_icons(schema_icon, sys_schema_icon, table_icon, column_icon, sp_icon);
  ((Gtk::Container*)get_widget("schema_frame"))->add(*Gtk::manage(_schema_browser));
  _schema_browser->show();
  
  _table_browser= new MGSchemaBrowserList("", MGSchemaBrowserList::Normal);
  _table_browser->set_populate_func(sigc::mem_fun(*this,&MAUserAdministrationPanel::populate_schema_tables));
  _table_browser->set_mark_delegate(table_mark_delegate, this);
  _table_browser->set_icons(schema_icon, sys_schema_icon, table_icon, column_icon, sp_icon);
  ((Gtk::Container*)get_widget("table_frame"))->add(*Gtk::manage(_table_browser));
  _table_browser->show();

  setup_list_views();

  {
    Gtk::MenuItem *item;
    
    _schemas_popup_menu= new Gtk::Menu();
    item= make_item(_schemas_popup_menu, _("_Remove Schema from Priv List"), Gtk::Stock::NEW);
    //_user_menu_items[i++]= item;
    //item->signal_activate().connect(sigc::mem_fun(*this, &MAUserAdministrationPanel::add_user_activate));
    _schema_browser->set_popup_menu(_schemas_popup_menu);    
  }

  // monitor changes in preferences
  _app->signal_prefs_changed().connect(sigc::mem_fun(*this,&MAUserAdministrationPanel::prefs_changed));

  return true;
}

void MAUserAdministrationPanel::reload_catalogs()
{
  Glib::RefPtr<MGPtrWrap<MYX_CATALOGS*> > catalogs= _data->get_catalogs();
  if (catalogs && catalogs->ptr())
  {
    _schema_browser->set_catalogs(catalogs->ptr());
    _table_browser->set_catalogs(catalogs->ptr());
  }
}

void MAUserAdministrationPanel::show()
{
  MAPanel::show();

  if (_first_show)
  {
    if (!fetch_privilege_templates())
    {
      myg_show_error(_("Could not retrieve user privilege information."));
      return;
    }
    connect_signals();
  }
  _user_names= _data->get_user_names();

  if (_user_names)
    _user_browser->set_user_list(_user_names);

  reload_catalogs();

  _first_show= false;
}


bool MAUserAdministrationPanel::before_show()
{
  update_sensitivity();

  _app->add_side_panel(_user_browser);

  if (prefs.show_user_table_column_privileges)
    get_widget("table_page")->show();
  else
    get_widget("table_page")->hide();

  if (prefs.show_user_global_privileges)
    get_widget("global_page")->show();
  else
    get_widget("global_page")->hide();

  return true;
}


bool MAUserAdministrationPanel::before_hide()
{
//  _menu_item->hide();
  _app->remove_side_panel(_user_browser);
  return true;
}


void MAUserAdministrationPanel::disconnected_cb()
{
  if (_user_browser)
    _user_browser->set_user_list(0);
  discard_changes();
}


void MAUserAdministrationPanel::prefs_changed()
{
  if (prefs.show_user_table_column_privileges)
    get_widget("table_page")->show();
  else
    get_widget("table_page")->hide();

  if (prefs.show_user_global_privileges)
    get_widget("global_page")->show();
  else
    get_widget("global_page")->hide();
}


/**********************************************************************/

bool MAUserAdministrationPanel::add_host_to_user(MYX_USER *user, const Glib::ustring &host)
{
  bool dupe= false;
  for (unsigned int i= 0; i < user->hosts_num; i++)
  {
    if (host.compare((char*)user->hosts[i])==0)
    {
      dupe= true;
      break;
    }
  }

  if (!host.empty() && !dupe)
  {
    user->hosts_num++;
    user->hosts= (char**)myx_resize_vector_block((char*)user->hosts,
                                                          sizeof(char*), user->hosts_num);
    user->hosts[user->hosts_num-1]= (char*)g_strdup(host.c_str());
    
    return true;
  }
  
  return false;
}


MYX_USER_OBJECT_PRIVILEGES *MAUserAdministrationPanel::clone_privilege_struct(MYX_USER_OBJECT_PRIVILEGES*priv)
{
  MYX_USER_OBJECT_PRIVILEGES *new_priv;

  new_priv= (MYX_USER_OBJECT_PRIVILEGES*)g_malloc0(sizeof(MYX_USER_OBJECT_PRIVILEGES));
  new_priv->host= NULL;
  new_priv->object_name= NULL;
  new_priv->user_privileges_num= priv->user_privileges_num;
  new_priv->user_privileges= (char**)myx_resize_vector_block(NULL, sizeof(char*), priv->user_privileges_num);
  for (unsigned int i= 0; i < priv->user_privileges_num; i++)
  {
    new_priv->user_privileges[i]= (char*)g_strdup((gchar*)priv->user_privileges[i]);
  }

  return new_priv;
}


typedef struct {
  MYX_USER_OBJECT_PRIVILEGES *global;
  MYX_USER_OBJECT_PRIVILEGES *schema;
  MYX_USER_OBJECT_PRIVILEGES *table;
  MYX_USER_OBJECT_PRIVILEGES *proc;
  MYX_USER_OBJECT_PRIVILEGES *column;
} WantedPrivileges;


static void* fetch_privilege_templates_worker(MYSQL *mysql)
{
  WantedPrivileges *privs= g_new0(WantedPrivileges, 1);
  
  if (!(privs->global= myx_get_privilege_struct(mysql, "", MYX_UOP_GLOBAL)))
    goto end;
  if (!(privs->schema= myx_get_privilege_struct(mysql, "x", MYX_UOP_SCHEMA)))
    goto end;
  if (mysql_version_is_later_or_equal_than(mysql,5,0))
  {
    if (!(privs->proc= myx_get_privilege_struct(mysql, "x.x", MYX_UOP_ROUTINE)))
      goto end;
  }
  else
  {
    privs->proc= NULL;
  }    
  if (!(privs->table= myx_get_privilege_struct(mysql, "mysql.user", MYX_UOP_TABLE)))
    goto end;
  if (!(privs->column= myx_get_privilege_struct(mysql, "x.x.x", MYX_UOP_COLUMN)))
    goto end;
  return privs;
end:
  g_free(privs->global);
  g_free(privs->schema);
  g_free(privs->table);
  g_free(privs->column);
  g_free(privs);
  return NULL;
}


bool MAUserAdministrationPanel::fetch_privilege_templates()
{
  WantedPrivileges *privs;
  
  privs= (WantedPrivileges*)_instance->perform_data_fetch((MInstanceInfo::DataFetcher)fetch_privilege_templates_worker);
  if (privs)
  {
    _global_privileges_template= privs->global;
    
    _schema_privileges_template= privs->schema;
    
    _table_privileges_template= privs->table;
    
    _proc_privileges_template= privs->proc;
    
    _column_privileges_template= privs->column;
    
    g_free(privs);
    return true;
  }
  return false;
}


MYX_USER_OBJECT_PRIVILEGES *
  MAUserAdministrationPanel::fetch_privileges_for_object(MYX_USER *user,
                                                         const Glib::ustring &host,
                                                         const Glib::ustring &object,
                                                         bool cache_only,
                                                         bool is_stored_proc)
{
  MYX_USER_OBJECT_PRIVILEGES *priv= 0;
  unsigned int i;
  bool found= false;

  g_assert(!host.empty());

  // search for currently assigned privileges
  for (i= 0; i< user->user_object_privileges_num; i++)
  {
    priv= user->user_object_privileges+i;
    
    if (priv->host && host.compare((char*)priv->host)==0 &&
        ((object.empty() && priv->object_name==NULL) ||
         priv->object_name && object.compare((char*)priv->object_name)==0))
    {
      found= true;
      break;
    }
  }
  if (!found)
  {
    Glib::ustring key(object+"@"+host);

    // search for previously created privileges
    if (_object_privilege.find(key)!=_object_privilege.end())
    {
      priv= _object_privilege[key];
    }
    else if (!cache_only)
    {
//      g_message("cloning privileges for %s",key.c_str());
      
      switch (!object.empty() ? sub_str_count(".", object.c_str()) : -1)
      {
      case -1:
        priv= clone_privilege_struct(_global_privileges_template);
        break;
      case 0:
        priv= clone_privilege_struct(_schema_privileges_template);
        break;
      case 1:
        if(is_stored_proc) 
        {
          priv= clone_privilege_struct(_proc_privileges_template);
        }
        else
        {
          priv= clone_privilege_struct(_table_privileges_template);
        }
        break;
      case 2:
        priv= clone_privilege_struct(_column_privileges_template);
        break;
      default:
        g_assert_not_reached();
      }

      priv->host= g_strdup(host.c_str());
      priv->object_name= g_strdup(object.c_str());

      _object_privilege[key] = priv;
    }
    else
    {
      priv= NULL;
    }
  }
  return priv;
}


/**********************************************************************/


void MAUserAdministrationPanel::update_sensitivity()
{
  get_widget("discard_button")->set_sensitive(_dirty);

  get_widget("apply_button")->set_sensitive(_dirty);


  if (_current_user)
  {
    get_widget("top_note")->set_sensitive(true);
  }
  else
  {
    get_widget("top_note")->set_sensitive(false);
  }
  
  bool fl= _current_host.empty()?false:true;

  get_widget("global_tab")->set_sensitive(fl);
  get_widget("global_page")->set_sensitive(fl);
  get_widget("schema_tab")->set_sensitive(fl);
  get_widget("schema_page")->set_sensitive(fl);
  get_widget("table_tab")->set_sensitive(fl);
  get_widget("table_page")->set_sensitive(fl);
  get_widget("resource_tab")->set_sensitive(fl);
  get_widget("resource_page")->set_sensitive(fl);
}


#define LIST_HAS_SELECTION(l) ((Gtk::TreeView*)get_widget(l))->get_selection()->count_selected_rows()>0


void MAUserAdministrationPanel::update_global_privilege_sensitivity()
{
  if (LIST_HAS_SELECTION("global_assigned_list"))
  {
    get_widget("global_revoke_button")->set_sensitive(true);
  }
  else
  {
    get_widget("global_revoke_button")->set_sensitive(false);
  }

  if (LIST_HAS_SELECTION("global_available_list"))
  {
    get_widget("global_grant_button")->set_sensitive(true);
  }
  else
  {
    get_widget("global_grant_button")->set_sensitive(false);
  }
}



void MAUserAdministrationPanel::update_schema_privilege_sensitivity()
{
  if (LIST_HAS_SELECTION("schema_assigned_list"))
  {
    get_widget("schema_revoke_button")->set_sensitive(true);
  }
  else
  {
    get_widget("schema_revoke_button")->set_sensitive(false);
  }

  if (LIST_HAS_SELECTION("schema_available_list"))
  {
    get_widget("schema_grant_button")->set_sensitive(true);
  }
  else
  {
    get_widget("schema_grant_button")->set_sensitive(false);
  }
}


void MAUserAdministrationPanel::update_table_privilege_sensitivity()
{
  if (LIST_HAS_SELECTION("table_assigned_list"))
  {
    get_widget("table_revoke_button")->set_sensitive(true);
  }
  else
  {
    get_widget("table_revoke_button")->set_sensitive(false);
  }

  if (LIST_HAS_SELECTION("table_available_list"))
  {
    get_widget("table_grant_button")->set_sensitive(true);
  }
  else
  {
    get_widget("table_grant_button")->set_sensitive(false);
  }
}


bool MAUserAdministrationPanel::populate_user_host(MGUserBrowserList *sender,
                                                   const Gtk::TreeIter &node,
                                                   std::list<Glib::ustring> &hosts)
{
  Glib::ustring username, host;

  _user_browser->get_row_user(node, username, host);

  MYX_USER *user= _data->get_user_if_cached(username);

  if (user)
  {
    for (unsigned int i= 0; i < user->hosts_num; i++)
    {
      hosts.push_back("@ "+Glib::ustring((char*)user->hosts[i]));
    }
    return true;
  }

  return false;
}

  

bool MAUserAdministrationPanel::populate_schema_tables(MGSchemaBrowserList *sender,
                                                       const Gtk::TreeIter &node)
{
  bool b1= false, b2= false;
  
  Glib::ustring catalog, schema, table, column, child;

  sender->get_row_object(node, catalog, schema, table, column, child);

  b1= _data->get_schema_tables(catalog, schema);
  b2= _data->get_schema_sps(catalog, schema);
  return b1 && b2;
}


 
void MAUserAdministrationPanel::unshow_user()
{  
  set_shown_user(NULL, "");

  _current_user= NULL;
//  _current_user_name= username;
  _current_host= "";
  _dirty= false;

  _schema_browser->refresh_markings();
  _table_browser->refresh_markings();

  update_sensitivity();
}


void MAUserAdministrationPanel::show_user(const Glib::ustring &username, 
                                          const Glib::ustring &host)
{
  MYX_USER *user= _data->get_user(username);

  set_shown_user(user, host);

  _current_user= user;
//  _current_user_name= username;
  _current_host= host;

  _schema_browser->refresh_markings();
  _table_browser->refresh_markings();

  update_sensitivity();
}


void MAUserAdministrationPanel::apply_shown_user()
{
  Gtk::Entry *entry;

#define REPLACE(s, us) if (s) g_free(s); s= g_strdup(us.c_str())

  entry= (Gtk::Entry*)get_widget("user_entry");
  REPLACE(_current_user->user_name, entry->get_text());
    
  entry= (Gtk::Entry*)get_widget("password_entry");
  REPLACE(_current_user->password, entry->get_text());

  entry= (Gtk::Entry*)get_widget("fullname_entry");
  REPLACE(_current_user->full_name, entry->get_text());

  entry= (Gtk::Entry*)get_widget("description_entry");
  REPLACE(_current_user->description, entry->get_text());

  entry= (Gtk::Entry*)get_widget("email_entry");
  REPLACE(_current_user->email, entry->get_text());

  Glib::RefPtr<Gtk::TextBuffer> buffer = ((Gtk::TextView*)get_widget("contact_text"))->get_buffer();
  REPLACE(_current_user->contact_information,   
          buffer->get_text(buffer->begin(), buffer->end(), false));

#undef REPLACE

  if (_current_icon_data)
  {
    if (_current_user->icon)
      free(_current_user->icon);
    _current_user->icon= (char*)_current_icon_data;
    _current_user->icon_length= _current_icon_data_length;
  }
}


void MAUserAdministrationPanel::set_shown_user(MYX_USER *user,
                                               const Glib::ustring &host)
{
  Glib::RefPtr<Gdk::Pixbuf> new_icon;
  Glib::ustring ustr;
  Glib::ustring uhstr;
  Glib::ustring str;

  if (_current_icon_data)
      free(_current_icon_data);
    
  _current_icon_data= NULL;
  _current_icon_data_length= 0;

  if (user) 
  {
    Glib::ustring name, fname;
    if (!*user->user_name)
      name= _("<i>anonymous</i>");
    else
      name= Glib::ustring((char*)user->user_name);
    
    ustr= "<b>"+name;

    if (user->full_name && *user->full_name)
    {
      fname= "("+Glib::ustring((char*)user->full_name)+")";
      ustr= ustr+" "+fname;
    }
    ustr= ustr+"</b>\n";

    if (!host.empty())
    {
      if (host == "%")
        uhstr= "<b>"+name+"@ <i>anywhere</i> "+fname+"</b>\n";
      else
        uhstr= "<b>"+name+"@"+host+" "+fname+"</b>\n";
    }
    else
      uhstr= ustr;

    if (user->icon!=NULL)
    {
      new_icon= make_pixbuf_from_data(user->icon, user->icon_length);
      
      new_icon= new_icon->scale_simple(32, 32, Gdk::INTERP_BILINEAR);
    }
    else
    {
      new_icon.clear();
    }
  }
  else
  {
    ustr= " <i>"+Glib::ustring(_("no user selected"))+"</i>\n";
    new_icon.clear();
  }

  // user info
  str= ustr + Glib::ustring(_("Login and additional information on the user"));
  ((Gtk::Label*)get_widget("user_label"))->set_markup(str);
    

  ((Gtk::Image*)get_widget("user_image"))->set(new_icon?new_icon:_default_user_icon);

  if (_current_user != user)
  {
    _ignore_entry_changes= true;

#define NVALUE(f) ((user&&user->f)?(char*)user->f:"")

    ((Gtk::Entry*)get_widget("user_entry"))->set_text(NVALUE(user_name));

    // the _______ is a hack from the core lib to not change the current password
    ((Gtk::Entry*)get_widget("password_entry"))->set_text("________");
    ((Gtk::Entry*)get_widget("confirm_entry"))->set_text("________");

    ((Gtk::Entry*)get_widget("fullname_entry"))->set_text(NVALUE(full_name));

    ((Gtk::Entry*)get_widget("description_entry"))->set_text(NVALUE(description));

    ((Gtk::Entry*)get_widget("email_entry"))->set_text(NVALUE(email));

    Glib::RefPtr<Gtk::TextBuffer> buffer = ((Gtk::TextView*)get_widget("contact_text"))->get_buffer();
    buffer->set_text(NVALUE(contact_information));

    ((Gtk::Image*)get_widget("icon_image"))->set(new_icon);
#undef NVALUE
    _ignore_entry_changes= false;
  }

  // global privileges

  str = uhstr + _("Global privileges assigned to the user");
  ((Gtk::Label*)get_widget("global_label"))->set_markup(str);

  ((Gtk::Image*)get_widget("global_image"))->set(new_icon?new_icon:_default_user_icon);

  if (host.empty() || _current_host != host || _current_user != user)
  {
    show_global_privileges(user, host);

    update_global_privilege_sensitivity();
  }

  // schema privileges

  str= uhstr + std::string(_("Schema (database) privileges assigned to the user"));
  ((Gtk::Label*)get_widget("schema_label"))->set_markup(str);

  ((Gtk::Image*)get_widget("schema_image"))->set(new_icon?new_icon:_default_user_icon);

  if (host.empty() || _current_host != host || _current_user != user)
  {
    Glib::ustring catalog, schema, child;
    Gtk::TreeIter node;

    if (_schema_browser->get_selected_schema(node, catalog, schema, child)
        && child.empty())
      show_schema_privileges(user, catalog, schema, host);

    update_schema_privilege_sensitivity();
  }

  // table privileges
  
  str= uhstr + std::string(_("Table and column privileges assigned to the user"));
  ((Gtk::Label*)get_widget("table_label"))->set_markup(str);

  ((Gtk::Image*)get_widget("table_image"))->set(new_icon?new_icon:_default_user_icon);

  if (host.empty() || _current_host != host || _current_user != user)
  {
    Glib::ustring catalog, schema, table, column, child;
    Gtk::TreeIter node;
    
    if ((node= _table_browser->get_selected()))
    {
      _table_browser->get_row_object(node, catalog, schema, table, 
                                      column, child);
      if (!table.empty() && child.empty())
        show_table_privileges(user, catalog, schema, table, column, host);
    }
    update_table_privilege_sensitivity();
  }

  // resource limits
  str= uhstr + std::string(_("Various resource limits for the user"));
  ((Gtk::Label*)get_widget("resource_label"))->set_markup(str);

  ((Gtk::Image*)get_widget("resource_image"))->set(new_icon?new_icon:_default_user_icon);
  if (host.empty() || _current_host != host || _current_user != user)
  {
    show_limits(user, host);
  }

  _current_user_icon= new_icon;
}


void MAUserAdministrationPanel::refresh_user_list()
{
  _user_names= _data->get_user_names(true);
  //refresh user browser
  _user_browser->set_user_list(_user_names);
}
  


void MAUserAdministrationPanel::fill_privilege_list(Glib::RefPtr<Gtk::ListStore> &lstore,
                                                    Glib::RefPtr<Gtk::ListStore> &rstore,
                                                    char **privs,
                                                    unsigned int nprivs,
                                                    char **global_privs,
                                                    unsigned int global_nprivs,
                                                    const char *priv_prefix,
                                                    const char *priv_suffix)
{
  Gtk::TreeIter liter, riter;

  lstore->clear();
  rstore->clear();
  
  for (unsigned int i= 0; i < nprivs; i++)
  {
    char opname[strlen(privs[i])+1];
    char buffer[strlen(privs[i])+1];
    bool flag;
    bool global= false;
    
    name_of_str(opname, privs[i]);

    if (priv_prefix && !str_beginswith(opname, priv_prefix))
      continue;

    if (priv_suffix && !str_endswith(opname, priv_suffix))
      continue;

    value_of_str(buffer, privs[i]);
    flag= strcasecmp(buffer, "y")==0;

    if (!flag)
    {
      for (unsigned int j= 0; j < global_nprivs; j++)
      {
        char gbuffer[strlen(global_privs[j])+1];

        name_of_str(gbuffer, global_privs[j]);
        
        if (strcmp(opname, gbuffer)==0)
        {
          value_of_str(gbuffer, global_privs[j]);
          flag= strcasecmp(gbuffer, "y")==0;
          if (flag)
            global= true;
          break;
        }
      }
    }

    if (priv_prefix)
    {
      strcpy(buffer, priv_prefix);
      strcpy(strchr(buffer, '_'), strrchr(opname, '_')+1);
    }
    else
      strcpy(buffer, opname/*+(priv_prefix?strlen(priv_prefix):0)*/);
    buffer[strlen(buffer)-(priv_suffix?strlen(priv_suffix):0)]=0;
    str_toupper(buffer);
    
    if (flag)
    {
      liter= lstore->append();
      Gtk::TreeModel::Row row= *liter;
      
      row[_assigned_columns._icon]= _right_icon;
      row[_assigned_columns._text]= buffer;
      row[_assigned_columns._name]= opname;
      if (global)
        row[_assigned_columns._color]= "gray";
      else
        row[_assigned_columns._color]= "black";
    }
    
    if (!flag || global)
    {
      //XXX put decent help text
      riter= rstore->append();
      Gtk::TreeModel::Row row= *riter;
      
      row[_available_columns._icon]= _right_icon;
      row[_available_columns._text]= buffer;
      row[_available_columns._name]= opname;
      row[_available_columns._help]= ufmt(_("Grants the %s privilege to the user"),
                                              buffer);
    }
  }
}


void MAUserAdministrationPanel::set_privilege_values(const Gtk::TreeIter &iter,
                                                     MYX_USER_OBJECT_PRIVILEGES *privs,
                                                     bool revoke)
{
  Gtk::TreeModel::Row row= *iter;
  Glib::ustring name;
  Gtk::TreeModelColumn<Glib::ustring> column;
  char *new_value;

  if (revoke)
  {
    new_value= "N";
    column= _assigned_columns._name;
  }
  else
  {
    new_value= "Y";
    column= _available_columns._name;
  }
  
  name= row[column];
  
  ::set_value((char**)privs->user_privileges,
              privs->user_privileges_num,
              (char*)name.c_str(), new_value);
}


bool MAUserAdministrationPanel::grant_revoke_privilege(MYX_USER_OBJECT_PRIVILEGES *privs,
                                                       Gtk::TreeView *llist,
                                                       Gtk::TreeView *rlist,
                                                       bool revoke)
{
  Glib::RefPtr<Gtk::TreeSelection> sel;
  Gtk::TreeIter siter;
  int old_selected= 0, new_selected= 0;
  
  if (revoke)
  {
    sel= llist->get_selection();
  }
  else
  {
    sel= rlist->get_selection();
  }

  for (unsigned int i= 0; i < privs->user_privileges_num; i++)
  {
    char *ptr= strchr((char*)privs->user_privileges[i],'=');
    if (ptr && toupper(*(ptr+1))=='Y')
      old_selected++;
  }
  
  sel->selected_foreach_iter(sigc::bind<MYX_USER_OBJECT_PRIVILEGES*,bool>
                             (sigc::mem_fun(*this, &MAUserAdministrationPanel::set_privilege_values),
                              privs, revoke));

  for (unsigned int i= 0; i < privs->user_privileges_num; i++)
  {
    char *ptr= strchr((char*)privs->user_privileges[i],'=');
    if (ptr && toupper(*(ptr+1))=='Y')
      new_selected++;
  }

  if (old_selected != new_selected)
  {
    set_dirty();
    return true;
  }
  return false;
}


void MAUserAdministrationPanel::limit_changed(const char *w)
{
  if (_ignore_limit_changes)
    return;
  
  MYX_USER_OBJECT_PRIVILEGES *privs= get_current_global_privileges();
  Glib::ustring value = get_spin(w)->get_text();
  const char *name= NULL;

  if (strcmp(w,"maxc_spin")==0)
    name= "max_connections";
  else if (strcmp(w,"maxq_spin")==0)
    name= "max_queries";
  else if (strcmp(w,"maxu_spin")==0)
    name= "max_updates";

  if (::set_value((char**)privs->user_privileges,
                  privs->user_privileges_num,
                  (char*)name, (char*)value.c_str()) < 0)
  {
    privs->user_privileges_num++;
    privs->user_privileges= (char**)myx_resize_vector_block((char*)privs->user_privileges,
                                                                     sizeof(char*), privs->user_privileges_num);
    privs->user_privileges[privs->user_privileges_num-1]=
      (char*)g_strdup_printf("%s=%s", name, value.c_str());
  }

  set_dirty();
}


void MAUserAdministrationPanel::set_user_icon_file(const Glib::ustring &filename)
{
  Glib::RefPtr<Gdk::Pixbuf> pixbuf;

  if (!str_endswith(filename.c_str(), ".png") && !str_endswith(filename.c_str(), ".PNG"))
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("The icon must be in PNG format."),
                           false,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
    return;
  }

  try 
  {
    pixbuf= Gdk::Pixbuf::create_from_file(filename);
  } 
  catch (Gdk::PixbufError &exc)
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("Error loading icon"),
                           false,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK, 
                           true);
    dlg.run();
    return;
  }

  _current_user_icon= pixbuf;

  if (_current_icon_data)
  {
    g_free(_current_icon_data);
    _current_icon_data= NULL;
  }
  g_file_get_contents(filename.c_str(),
                      (gchar**)&_current_icon_data, (gsize*)&_current_icon_data_length,
                      NULL);

  ((Gtk::Image*)get_widget("icon_image"))->set(pixbuf);

  set_dirty();
}


MYX_USER_OBJECT_PRIVILEGES *MAUserAdministrationPanel::get_current_global_privileges()
{
  return fetch_privileges_for_object(_current_user, _current_host, "");
}


MYX_USER_OBJECT_PRIVILEGES *MAUserAdministrationPanel::get_current_schema_privileges(Gtk::TreeIter &node_ret)
{
  Glib::ustring catalog, schema, child;

  if (_schema_browser->get_selected_schema(node_ret, catalog, schema, child) 
      && child.empty())
  {
    return fetch_privileges_for_object(_current_user, _current_host, schema);
  }
  return NULL;
}


MYX_USER_OBJECT_PRIVILEGES *MAUserAdministrationPanel::get_current_table_privileges(Gtk::TreeIter &node_ret)
{
  Glib::ustring catalog, schema, table, column, child;

  node_ret= _table_browser->get_selected();

  if (node_ret)
  {
    _table_browser->get_row_object(node_ret, catalog, schema, table, 
                                   column, child);
    if (child.empty())
    {
      Glib::ustring name(schema);
      name += "."+table;
      if (!column.empty())
      {
        name += "."+column;
      }
      return fetch_privileges_for_object(_current_user, _current_host, name);
    }
  }
  return NULL;
}


void MAUserAdministrationPanel::show_global_privileges(MYX_USER *user, 
                                                       const Glib::ustring &host)
{
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;

  if (!host.empty())
  {
    if (user)
    {
        privs= fetch_privileges_for_object(user, host, "");
    }
    if (privs)
    {
      fill_privilege_list(_global_assigned_privs, _global_available_privs,
                          (char**)privs->user_privileges,
                          privs->user_privileges_num, NULL, 0,
                          NULL, "_priv");
    }
  }
  
  if (!privs)
  {
      fill_privilege_list(_global_assigned_privs, _global_available_privs,
                          NULL, 0, NULL, 0, NULL, NULL);
  }
}

  
void MAUserAdministrationPanel::show_schema_privileges(MYX_USER *user,
                                                       const Glib::ustring &catalog,
                                                       const Glib::ustring &schema,
                                                       const Glib::ustring &host)
{
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
  MYX_USER_OBJECT_PRIVILEGES *global_privs= NULL;

  if (!host.empty() && !schema.empty())
  {
    if (user)
    {
      privs= fetch_privileges_for_object(user, host, schema);

      global_privs= fetch_privileges_for_object(user, host, "");
    }
    if (privs)
    {
      fill_privilege_list(_schema_assigned_privs, _schema_available_privs,
                          (char**)privs->user_privileges,
                          privs->user_privileges_num,
                          (char**)global_privs->user_privileges,
                          global_privs->user_privileges_num,
                          NULL, "_priv");
    }
    
  }

  if (!privs && !schema.empty())
  {
    if (global_privs)
      fill_privilege_list(_schema_assigned_privs, _schema_available_privs,
                          NULL, 0, (char**)global_privs->user_privileges,
                          global_privs->user_privileges_num, NULL, "_priv");
    else
      fill_privilege_list(_schema_assigned_privs, _schema_available_privs,
                          NULL, 0, NULL, 0, NULL, NULL);
  }
}


void MAUserAdministrationPanel::show_table_privileges(MYX_USER *user,
                                                      const Glib::ustring &catalog,
                                                      const Glib::ustring &schema,
                                                      const Glib::ustring &table, 
                                                      const Glib::ustring &column,
                                                      const Glib::ustring &host)
{
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
  std::string name(schema);
  name += "."+table;
  if (!column.empty())
  {
    name += "."+column;
  }

  if (!host.empty())
  {
    if (user)
    {
      privs= fetch_privileges_for_object(user, host, name);
    }
    if (privs)
    {
      fill_privilege_list(_table_assigned_privs, _table_available_privs,
                          (char**)privs->user_privileges,
                          privs->user_privileges_num, 
                          NULL, 0,
                          column.empty()?NULL:"Column_priv_", NULL);
    }
  }

  if (!privs)
  {
    fill_privilege_list(_table_assigned_privs, _table_available_privs,
                        NULL, 0, NULL, 0, NULL, NULL);
  }
}

void MAUserAdministrationPanel::show_proc_privileges(MYX_USER *user,
                                                      const Glib::ustring &catalog,
                                                      const Glib::ustring &schema,
                                                      const Glib::ustring &table, 
                                                      const Glib::ustring &host)
{
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
  std::string name(schema);
  name += "."+table;

  if (!host.empty())
  {
    if (user)
    {
      privs= fetch_privileges_for_object(user, host, name, false, true);
    }
    if (privs)
    {
      fill_privilege_list(_table_assigned_privs, _table_available_privs,
                          (char**)privs->user_privileges,
                          privs->user_privileges_num, 
                          NULL, 0,
                          "Proc_priv_", NULL);
    }
  }

  if (!privs)
  {
    fill_privilege_list(_table_assigned_privs, _table_available_privs,
                        NULL, 0, NULL, 0, NULL, NULL);
  }
}

void MAUserAdministrationPanel::show_limits(MYX_USER *user, const Glib::ustring &host)
{
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
  int maxq=0, maxu=0, maxc=0;
  
  if (!host.empty())
  {
    if (user)
      privs= fetch_privileges_for_object(user, host, "");
  }

  if (privs)
  {
    for (unsigned int i= 0; i < privs->user_privileges_num; i++)
    {
      char *priv= (char*)privs->user_privileges[i];
      char opname[strlen(priv)+1];
      char value[strlen(priv)+1];

      value_of_str(value, priv);
      name_of_str(opname, priv);

      if (strcmp(opname, "max_connections")==0)
      {
        maxc= strtoul(value, NULL, 10);
      }
      else if (strcmp(opname, "max_questions")==0)
      {
        maxq= strtoul(value, NULL, 10);
      }
      else if (strcmp(opname, "max_updates")==0)
      {
        maxu= strtoul(value, NULL, 10);
      }
    }
  }
  _ignore_limit_changes= true;
  get_spin("maxc_spin")->set_value(maxc);
  get_spin("maxq_spin")->set_value(maxq);
  get_spin("maxu_spin")->set_value(maxu);
  _ignore_limit_changes= false;
}


void MAUserAdministrationPanel::create_new_user()
{
  if (confirm_user_change())
  {
    MYX_USER *user= (MYX_USER*)g_malloc0(sizeof(MYX_USER));
    const char *new_name= "new_user";
    char buffer[strlen(new_name)+10];
    int i= 1;
    bool dupe;

    do
    {
      dupe= false;
      for (unsigned int j= 0; j < _user_names->user_names_num; j++)
      {
        if (strcmp(_user_names->user_names[j],new_name)==0)
        {
          sprintf(buffer, "new_user%i", i++);
          new_name= (char*)buffer;
          dupe= true;
          break;
        }
      }
    } while (dupe);

    user->user_name= g_strdup(new_name);
    user->full_name= g_strdup("");
    user->password= g_strdup("");
    user->full_name= g_strdup("");
    user->description= g_strdup("");
    user->email= g_strdup("");
    user->contact_information= g_strdup("");

    add_host_to_user(user, "%");
    
    _data->add_new_user(user, new_name);

    _user_browser->clear_filter();
    _user_browser->refresh();
    _user_browser->set_user_list(_user_names);

    _user_browser->select_user(new_name);
    
    _user_is_new= true;
    set_dirty();
  }
}


void MAUserAdministrationPanel::clone_user()
{
  /*
  if (confirm_user_change())
  {
    MYX_USER *user= (MYX_USER*)g_malloc0(sizeof(MYX_USER));
    const char *new_name= _current_user->username;
    char buffer[strlen(new_name)+10];
    int i= 1;
    bool dupe;

    do
    {
      dupe= false;
      for (unsigned int j= 0; j < _user_names->user_names_num; j++)
      {
        if (strcmp(_user_names->user_names[j],new_name)==0)
        {
          sprintf(buffer, "%s_copy%i", _current_user->username, i++);
          new_name= (char*)buffer;
          dupe= true;
          break;
        }
      }
    } while (dupe);

    user->user_name= (unsigned char*)g_strdup(new_name);
    user->password= (unsigned char*)g_strdup(_current_user->password);
    user->full_name= (unsigned char*)g_strdup(_current_user->full_name);
    user->description= (unsigned char*)g_strdup(_current_user->description);
    user->email= (unsigned char*)g_strdup(_current_user->email);
    user->contact_information= (unsigned char*)g_strdup(_current_user->contact_information);

    add_host_to_user(user, "%");

    _data->add_new_user(user, new_name);

    _user_browser->set_user_list(_user_names);

    _user_browser->select_user(new_name);

    _user_is_new= true;
    set_dirty();
  }
  */
}


void MAUserAdministrationPanel::remove_user()
{
  if (strcmp(_current_user->user_name, "root")==0)
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("You cannot delete the root user."),
                           false,
                           Gtk::MESSAGE_WARNING,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();
    return;
  }

  Gtk::MessageDialog dlg(*_app->window(),
                         ufmt(_("Permanently remove account '%s' from MySQL server at %s:%i?"),
                              _current_user->user_name,//_current_user_name.c_str(),
                              _instance->get_connection_data().hostname.c_str(),
                              _instance->get_connection_data().port),
                         false,
                         Gtk::MESSAGE_QUESTION,
                         Gtk::BUTTONS_OK_CANCEL,
                         true);

  if (dlg.run() == Gtk::RESPONSE_OK)
  {
    Glib::ustring old_name((char*)_current_user->user_name);//_current_user_name);

    discard_changes();

    _data->remove_user(old_name);

    refresh_user_list();

    set_shown_user(NULL, "");
  }
}


bool MAUserAdministrationPanel::validate()
{
  Glib::ustring uname;
  Glib::ustring pw1, pw2;
  Glib::ustring msg;

  do
  {
    if (_current_user->hosts_num == 0)
    {
      msg= _("You need to add at least one host from where the user can connect to.");
      break;
    }

    // check password
    pw1= ((Gtk::Entry*)get_widget("password_entry"))->get_text();
    pw2= ((Gtk::Entry*)get_widget("confirm_entry"))->get_text();

    if (pw1 != pw2)
    {
      msg= _("Entered passwords do not match, please type again.");

      ((Gtk::Entry*)get_widget("password_entry"))->set_text((char*)_current_user->password);
      ((Gtk::Entry*)get_widget("confirm_entry"))->set_text((char*)_current_user->password);
      break;
    }

    // check duplicate name
    uname= ((Gtk::Entry*)get_widget("user_entry"))->get_text();
    if(strcmp((const char *)_current_user->user_name, uname.c_str()) && 
      _user_browser->exists_user(uname))
    {
      msg= _("Duplicate user name, please enter another name.");
      break;
    }
  } 
  while(0);

  if (!msg.empty())
  {
    Gtk::MessageDialog dlg(*_app->window(), msg, false,
                           Gtk::MESSAGE_ERROR,
                           Gtk::BUTTONS_OK,
                           true);
    dlg.run();

    return false;
  }
  
  return true;
}


bool MAUserAdministrationPanel::apply_changes()
{
  if (!_current_user)
    return false;
  
  // commit edited permission stuff from temporary area (_object_privilege)
  // to user privilege list
  for (std::map<Glib::ustring,MYX_USER_OBJECT_PRIVILEGES*>::iterator it= _object_privilege.begin();
       it != _object_privilege.end(); ++it)
  {
    MYX_USER_OBJECT_PRIVILEGES *templ;
    bool needs_save= false;

    if (it->second->object_name == NULL)
    {
      needs_save= true;
    }
    else 
    {
      switch (*it->second->object_name=='\0' ? -1 : sub_str_count(".", (char*)it->second->object_name))
      {
      case -1:
        templ= _global_privileges_template;
        break;
      case 0:
        templ= _schema_privileges_template;
        break;
      case 1:
        templ= _table_privileges_template;
        break;
      case 2:
        templ= _column_privileges_template;
        break;
      default:
        g_assert_not_reached();
      }
      for (unsigned int i= 0; i < it->second->user_privileges_num; i++)
      {
        char *end, *end_def;

        end= strchr((char*)it->second->user_privileges[i], '=');
        end_def= strchr((char*)templ->user_privileges[i], '=');

        if (strstr((char*)it->second->user_privileges[i],"_priv"))
        {
          if (strcmp(end, end_def)!=0)
          {
            needs_save= true;
            break;
          }
        }
        else if (strstr((char*)it->second->user_privileges[i],"max_"))
        {
          if (!end_def || strcmp(end, end_def)!=0)
          {
            needs_save= true;
            break;
          }
        }
      }
    }

    if (needs_save)
    {
      // add to user object priv list
      _current_user->user_object_privileges_num++;
      _current_user->user_object_privileges= 
        (MYX_USER_OBJECT_PRIVILEGES*)myx_resize_vector_block((char*)_current_user->user_object_privileges,
                                             sizeof(MYX_USER_OBJECT_PRIVILEGES), _current_user->user_object_privileges_num);

      memcpy(_current_user->user_object_privileges+(_current_user->user_object_privileges_num-1),
             it->second, sizeof(MYX_USER_OBJECT_PRIVILEGES));
      g_free(it->second);
      it->second= NULL;
    }
  }

  // validate
  if (validate())
  {
    std::string old_name((char*)_current_user->user_name);

    apply_shown_user();

    std::string new_name((char*)_current_user->user_name);
    
    if (!_data->save_user(_current_user, old_name, new_name))
    {
      _data->show_last_error(_("Error applying user changes."));
    }
    else
    {
      discard_state_data();
      _dirty= false;
      update_sensitivity();

      _user_names= _data->get_user_names();
      _user_browser->set_user_list(_user_names);
      _user_is_new= false;
      return true;
    }
  }
  return false;
}


void MAUserAdministrationPanel::discard_changes()
{
  Glib::ustring user, host;

  if (!_current_user)
    return;

  if (!_first_show)
  {
    discard_state_data();

    if (_current_user)
      user= (char*)_current_user->user_name;//_current_user_name;
    host= _current_host;

    if (_user_is_new)
    {
      set_shown_user(NULL, "");
    }

    _current_user= NULL;
    //  _current_user_name.clear();
    _current_host.clear();
    
    // force cached user data reload
    _data->forget_user(user);
    
    if (!_user_is_new)
    {
      // refetch user
      _data->get_user(user);
    }
    _dirty= false;
    
    if (_user_browser)
      _user_browser->refresh();
  }
  
  _user_is_new= false;
}


void MAUserAdministrationPanel::discard_state_data()
{
  if (_current_user_icon)
    _current_user_icon.clear();
  if (_current_icon_data)
    _current_icon_data= NULL;

  for (std::map<Glib::ustring,MYX_USER_OBJECT_PRIVILEGES*>::iterator it= _object_privilege.begin();
       it != _object_privilege.end(); ++it)
  {
    if (it->second)
    {
      myx_free_user_priv(it->second);
      g_free(it->second);
    }
  }

  _object_privilege.clear();
}



bool MAUserAdministrationPanel::confirm_user_change()
{
  bool ret= true;

  if (_dirty)
  {
    switch (confirm_change(*_app->window(),
                           _("There are unsaved changes to the currently selected user.\n"
                             "Do you wish to save them?")))
    {
    case Gtk::RESPONSE_CANCEL:
      ret= false;
      break;
    case Gtk::RESPONSE_APPLY:
      ret= apply_changes();
      break;
    case Gtk::RESPONSE_NO:
      discard_changes();
      ret= true;
      break;
    }
  }

  return ret;
}


void MAUserAdministrationPanel::add_host()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_host_dialog_xml->get_widget("add_host_dialog");
  Glib::ustring new_host;

  // autoselect the 3rd radio when user types in the host entry
  _host_dialog_xml->get_entry("new_host_entry")->signal_changed().connect(sigc::bind<bool>(sigc::mem_fun(*_host_dialog_xml->get_toggle("new_host_radio3"),
                                                                                                      &Gtk::ToggleButton::set_active),true));
  
  if (dlg->run()!=Gtk::RESPONSE_OK)
  {
    dlg->hide();
    return;
  }
  dlg->hide();
  
  if (((Gtk::ToggleButton*)_host_dialog_xml->get_widget("new_host_radio1"))->get_active())
  {
    new_host= "%";
  }
  else if (((Gtk::ToggleButton*)_host_dialog_xml->get_widget("new_host_radio2"))->get_active())
  {
    new_host= "localhost";
  }
  else
  {
    new_host= ((Gtk::Entry*)_host_dialog_xml->get_widget("new_host_entry"))->get_text();
    char tmp[new_host.size()+1];

    strcpy(tmp, new_host.c_str());
    str_trim(tmp);
    new_host= Glib::ustring(tmp);
  }

  Glib::ustring username= (char*)_current_user->user_name;
  
  if (add_host_to_user(_current_user, new_host))
  {
    Gtk::TreeIter node;

    if (_user_browser->find_node(username,//_current_user_name,
                                 _user_browser->_columns._text, node))
    {
      std::string host_str;

      if (new_host == "%")
        host_str= "";
      else
        host_str= "@ "+new_host;

      _user_browser->populate_node(node);
      _user_browser->expand_node(node);
      _user_browser->select_user(username, //_current_user_name,
                                 host_str,
                                 true);
      //apply_changes();
    }
    else
      g_warning("?");
    
    set_dirty();
  }
}


void MAUserAdministrationPanel::remove_host()
{
  if (_current_host.empty())
    return;

  if (strcmp(_current_user->user_name, "root")==0)
  {
    if (_current_user->hosts_num == 1)
    {
      Gtk::MessageDialog dlg(*_app->window(),
                             ufmt(_("Cannot totally remove access from root user.\nPlease remove manually if you are sure about that."),
                                  _current_host.c_str()),
                             false,
                             Gtk::MESSAGE_WARNING,
                             Gtk::BUTTONS_OK,
                             true);
      dlg.run();
      return;
    }
  }

  if (_current_user->hosts_num == 1)
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           ufmt(_("Removing the last host from the users host list will also remove user itself. Please confirm."),
                                _current_host.c_str()),
                           false,
                           Gtk::MESSAGE_QUESTION,
                           Gtk::BUTTONS_OK_CANCEL,
                           true);

    if (dlg.run()==Gtk::RESPONSE_OK)
    {
      remove_user();
    }
    return;
  }
  else
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           ufmt(_("Remove '%s' from the list of hosts the user can connect from?"),
                                _current_host.c_str()),
                           false,
                           Gtk::MESSAGE_QUESTION,
                           Gtk::BUTTONS_OK_CANCEL,
                           true);

    if (dlg.run()!=Gtk::RESPONSE_OK)
      return;
  }

  Gtk::TreeIter node;

  // remove host
  for (unsigned int i= 0; i < _current_user->hosts_num; i++)
  {
    if (_current_host.compare((char*)_current_user->hosts[i])==0)
    {
      g_free(_current_user->hosts[i]);

      memmove(_current_user->hosts+i,
              _current_user->hosts+i+1,
              sizeof(char*)*(_current_user->hosts_num-i-1));
      _current_user->hosts_num--;
      break;
    }
  }

  // remove host data
  std::map<Glib::ustring,MYX_USER_OBJECT_PRIVILEGES*>::iterator tmp,it=_object_privilege.begin();
  while (it != _object_privilege.end())
  {
    if (g_utf8_collate((char*)_current_user->user_name,
                       (char*)it->second->host)==0)
    {
      tmp= it;
      ++tmp;
      myx_free_user_priv(it->second);
      g_free(it->second);
      _object_privilege.erase(it);
      it= tmp;
    }
    else
    {
      ++it;
    }
  }
  
  // remove more host data
  unsigned int i= 0;
  while (i < _current_user->user_object_privileges_num)
  {
    if (_current_host.compare((char*)_current_user->user_object_privileges[i].host)==0)
    {
      myx_free_user_priv(_current_user->user_object_privileges+i);
      memmove(&_current_user->user_object_privileges[i],
              &_current_user->user_object_privileges[i+1],
              sizeof(MYX_USER_OBJECT_PRIVILEGES)*(_current_user->user_object_privileges_num-i-1));
      _current_user->user_object_privileges_num--;
    }
    else
    {
      ++i;
    }
  }

  // refresh browser
  if (_user_browser->find_node((char*)_current_user->user_name,//_current_user_name,
                               _user_browser->_columns._text, node))
  {
    _user_browser->set_selection(node);
    _user_browser->populate_node(node);
  }
  
  set_dirty();
}

MYX_USER * MAUserAdministrationPanel::get_current_user()
{
  return _current_user;
}

Glib::ustring MAUserAdministrationPanel::get_current_host()
{
  return _current_host;
}

/***********************************************************************/

void MAUserAdministrationPanel::user_selected(MGBrowserList *sender,
                                              const Gtk::TreeIter &cnode)
{
  Glib::ustring username, host, host_str;
  Gtk::TreeIter node= cnode;
  bool is_host= false;

  if (node)
  {
    _user_browser->get_row_user(node, username, host);
    host_str= host;

    if (host.empty())
      host= "%";
    else
    {
      host= host.substr(2);
      is_host= true;
    }

    if (_current_user && 
        Glib::ustring((char*)_current_user->user_name) == username && 
        _current_host == host)
    {
      return;
    }
  }

  bool reget_node= false;

  // if the user requested a save before switching, then save
  // WARNING: this can change the user list
  if (_apply_changes_on_user_switch)
  {
    apply_changes();
    reget_node= true;
  }
  // if there were unsaved changes so far, its because they should be discarded
  // WARNING: this can change the user list
  if (_discard_changes_on_user_switch)
  {
    discard_changes();
    reget_node= true;
  }
  
  // get equivalent node after refresh
  if (reget_node)
  {
    Gtk::TreeIter tmp;
    if (_user_browser->find_node(username, _user_browser->_columns._text, tmp))
    {
      node= tmp;
      if (_user_browser->find_node(node, host_str, _user_browser->_columns._text, tmp))
        node= tmp;
    }
    else
    {
      node= tmp;
    }
  }

  if (node)
  {
    bool found= false;
    MYX_USER *user= _data->get_user(username);
    
    if (user)
    {
      for (unsigned int i= 0; i < user->hosts_num; i++)
      {
        if (host.compare((char*)user->hosts[i])==0)
        {
          found= true;
          break;
        }
      }
      if (!found)
      {
        // selected host doesn't exist (there's no % host for this user)
        host= "";
      }
      show_user(username, host);
      
      // fill list of hosts for this user
      if (!is_host) 
      {
        Gtk::TreeModel::Row row= *node;
        // if didnt refresh yet
        if (!row[_user_browser->_columns._populated])
        {
          _user_browser->populate_node(node);
        }
      }
      
      _user_browser->expand_node(node);
      
      reload_catalogs();
    }
    else
      unshow_user();
  }
  else
    unshow_user();
}


void MAUserAdministrationPanel::schema_selected(MGBrowserList *sender,
                                                const Gtk::TreeIter &node)
{
  Glib::ustring catalog;
  Glib::ustring schema;
  Glib::ustring table;
  Glib::ustring column;
  Glib::ustring child;

  if (node)
    _schema_browser->get_row_object(node, catalog, schema, table, column, child);

  show_schema_privileges(_current_user, catalog, schema, _current_host);
}


void MAUserAdministrationPanel::table_selected(MGBrowserList *sender,
                                               const Gtk::TreeIter &node)
{
  char t;
  Glib::ustring catalog;
  Glib::ustring schema;
  Glib::ustring table;
  Glib::ustring column;
  Glib::ustring child;
  Gtk::TreeModel::Row row= *node;

  if (node)
    t= _table_browser->get_row_object(node, catalog, schema, table, column, child);

  // now this is done in MGSchemaBrowserList
/*
  if (!schema.empty() && table.empty() && column.empty() && child.empty() &&
      row[_table_browser->_columns._populated])
  {
    _table_browser->populate_node(node);
  }
*/

  if (!table.empty() && child.empty())
  {
    if(t == 'P')
    {
      show_proc_privileges(_current_user,
                            catalog, schema, 
                            table,
                            _current_host);
    }
    else
    {
      // display table (or column) privs
      show_table_privileges(_current_user,
                            catalog, schema, table, column,
                            _current_host);
    }
  }
}


bool MAUserAdministrationPanel::user_will_change_delegate(MGBrowserList *sender,
                                                          const Gtk::TreeIter &node)
{
  Glib::ustring user, host;

  _user_browser->get_row_user(node, user, host);

  _apply_changes_on_user_switch= false;
  _discard_changes_on_user_switch= false;

  // if changing between nodes of different users, confirm
  if (_dirty && *_current_user->user_name && user.compare((char*)_current_user->user_name) != 0)
  {
    switch (confirm_change(*_app->window(),
                           ufmt(_("There are unsaved changes to the currently selected user (%s).\n"
                                  "Do you wish to save them?"),
                                _current_user->user_name)))
                                //_current_user_name.c_str())))
    {
    case Gtk::RESPONSE_APPLY:
      if(!validate())
      {
        return false;
      }
      _apply_changes_on_user_switch= true;
      break;

    case Gtk::RESPONSE_NO:
      _discard_changes_on_user_switch= true;
      break;

    case Gtk::RESPONSE_CANCEL:
      return false;
    }
  }
  return true;
}

void MAUserAdministrationPanel::global_privilege_list_selected(Glib::RefPtr<Gtk::TreeSelection> sel,
                                                               bool assigned)
{
  if (sel->count_selected_rows())
  {
    Gtk::TreeView *list;
    if (assigned)
    {
      list= (Gtk::TreeView*)get_widget("global_available_list");
    }
    else
    {
      list= (Gtk::TreeView*)get_widget("global_assigned_list");
    }
    list->get_selection()->unselect_all();
  }
  update_global_privilege_sensitivity();
}


void MAUserAdministrationPanel::schema_privilege_list_selected(Glib::RefPtr<Gtk::TreeSelection> sel,
                                                               bool assigned)
{
  if (sel->count_selected_rows())
  {
    Gtk::TreeView *list;
    if (assigned)
    {
      list= (Gtk::TreeView*)get_widget("schema_available_list");
    }
    else
    {
      list= (Gtk::TreeView*)get_widget("schema_assigned_list");
    }
    list->get_selection()->unselect_all();
  }
  update_schema_privilege_sensitivity();
}


void MAUserAdministrationPanel::table_privilege_list_selected(Glib::RefPtr<Gtk::TreeSelection> sel,
                                                              bool assigned)
{
  if (sel->count_selected_rows())
  {
    Gtk::TreeView *list;
    if (assigned)
    {
      list= (Gtk::TreeView*)get_widget("table_available_list");
    }
    else
    {
      list= (Gtk::TreeView*)get_widget("table_assigned_list");
    }
    list->get_selection()->unselect_all();
  }
  update_table_privilege_sensitivity();
}



bool MAUserAdministrationPanel::schema_mark_delegate(void *data, 
                                                     const Glib::ustring &schema,
                                                     const Glib::ustring &table, 
                                                     const Glib::ustring &column)
{
  MAUserAdministrationPanel *me= (MAUserAdministrationPanel*)data;
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
  
  if (me->_current_host.empty())
    return false;
  
  if (me->_current_user)
  {
    privs= me->fetch_privileges_for_object(me->_current_user,
                                           me->_current_host.c_str(), 
                                           schema, true);
  }
  if (privs)
  {
    for (unsigned int i= 0; i< privs->user_privileges_num; i++)
    {
      if (str_endswith((char*)privs->user_privileges[i], "=Y"))
        return true;
    }
  }
  
  return false;
}



bool MAUserAdministrationPanel::table_mark_delegate(void *data,
                                                    const Glib::ustring &schema,
                                                    const Glib::ustring &table, 
                                                    const Glib::ustring &column)
{
  MAUserAdministrationPanel *me= (MAUserAdministrationPanel*)data;
  MYX_USER_OBJECT_PRIVILEGES *privs= NULL;
  Glib::ustring name(schema);
  
  if (me->_current_host.empty())
    return false;
  
  if (!table.empty())
  {
    name+= "."+table;
    if (!column.empty())
      name+= "."+column;

    if (me->_current_user)
    {
      privs= me->fetch_privileges_for_object(me->_current_user,
                                             me->_current_host,
                                             name.c_str(), true);
    }
    if (privs)
    {
      for (unsigned int i= 0; i< privs->user_privileges_num; i++)
      {
        if (str_endswith((char*)privs->user_privileges[i], "=Y"))
          return true;
      }
    }
  }

  return false;
}


void MAUserAdministrationPanel::entry_changed()
{
  if (!_ignore_entry_changes)
    set_dirty();
}


void MAUserAdministrationPanel::global_revoke_grant_button_clicked(bool revoke)
{
  MYX_USER_OBJECT_PRIVILEGES *privs= get_current_global_privileges();
  Gtk::TreeView *llist= (Gtk::TreeView*)get_widget("global_assigned_list");
  Gtk::TreeView *rlist= (Gtk::TreeView*)get_widget("global_available_list");
  
  grant_revoke_privilege(privs, llist, rlist, revoke);
  
  // refill list
  fill_privilege_list(_global_assigned_privs,
                      _global_available_privs,
                      (char**)privs->user_privileges,
                      privs->user_privileges_num,
                      NULL, 0,
                      NULL, "_priv");
}


void MAUserAdministrationPanel::schema_revoke_grant_button_clicked(bool revoke)
{
  Gtk::TreeIter node;
  MYX_USER_OBJECT_PRIVILEGES *privs= get_current_schema_privileges(node);
  MYX_USER_OBJECT_PRIVILEGES *global_privs= get_current_global_privileges();
  Gtk::TreeView *llist= (Gtk::TreeView*)get_widget("schema_assigned_list");
  Gtk::TreeView *rlist= (Gtk::TreeView*)get_widget("schema_available_list");

  if (privs && grant_revoke_privilege(privs, llist, rlist, revoke))
  {
    // refill list
    fill_privilege_list(_schema_assigned_privs,
                        _schema_available_privs,
                        (char**)privs->user_privileges,
                        privs->user_privileges_num,
                        (char**)global_privs->user_privileges,
                        global_privs->user_privileges_num,
                        NULL, "_priv");
    
    _schema_browser->mark(node);
  }
}


void MAUserAdministrationPanel::table_revoke_grant_button_clicked(bool revoke)
{
  Gtk::TreeIter node;
  MYX_USER_OBJECT_PRIVILEGES *privs= get_current_table_privileges(node);
  Gtk::TreeView *llist= (Gtk::TreeView*)get_widget("table_assigned_list");
  Gtk::TreeView *rlist= (Gtk::TreeView*)get_widget("table_available_list");

  if (privs && grant_revoke_privilege(privs, llist, rlist, revoke))
  {
    bool is_column= sub_str_count(".", (const char*)privs->object_name)>1;

    // refill list
    fill_privilege_list(_table_assigned_privs,
                        _table_available_privs,
                        (char**)privs->user_privileges,
                        privs->user_privileges_num,
                        NULL,0,
                        is_column?"Column_priv_":NULL, NULL);

    Gtk::TreeRow row= *node;
    Glib::ustring str=row[_table_browser->_columns._text];
  
    if (node)
      _table_browser->mark(node);
  }
}


void MAUserAdministrationPanel::icon_change_button_clicked()
{
  Gtk::FileSelection *dlg;
  bool cancelled= false;
  
  dlg= (Gtk::FileSelection*)_dialog_xml->get_widget("icon_selection");
  
  dlg->show();
  if (dlg->run() != Gtk::RESPONSE_OK)
    cancelled= true;
  dlg->hide();
  
  if (!cancelled)
    set_user_icon_file(dlg->get_filename());
}


void MAUserAdministrationPanel::apply_button_clicked()
{  
  apply_changes();
}


void MAUserAdministrationPanel::discard_button_clicked()
{
  Glib::ustring user((char*)_current_user->user_name);//_current_user_name);
  Glib::ustring host(_current_host);

  discard_changes();

  _user_browser->select_user(user, host, true);
}


void MAUserAdministrationPanel::new_user_button_clicked()
{  
  create_new_user();
}


void MAUserAdministrationPanel::add_user_activate()
{  
  create_new_user();
}


void MAUserAdministrationPanel::clone_user_activate()
{  
  clone_user();
}


void MAUserAdministrationPanel::remove_user_activate()
{
  remove_user();
}


void MAUserAdministrationPanel::add_host_activate()
{
  add_host();
}


void MAUserAdministrationPanel::remove_host_activate()
{
  remove_host();
}


int MAUserAdministrationPanel::assigned_list_comparer(const Gtk::TreeIter &a, const Gtk::TreeIter &b)
{
  Gtk::TreeModel::Row ra(*a), rb(*b);
  Glib::ustring sa, sb;
  
  sa= ra[_assigned_columns._text];
  sb= rb[_assigned_columns._text];
  
  return sa.compare(sb);
}


int MAUserAdministrationPanel::available_list_comparer(const Gtk::TreeIter &a, const Gtk::TreeIter &b)
{
  Gtk::TreeModel::Row ra(*a), rb(*b);
  Glib::ustring sa, sb;
  
  sa= ra[_available_columns._text];
  sb= rb[_available_columns._text];
  
  return sa.compare(sb);
}

/***********************************************************************/


MAPanel *create_user_administration_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAUserAdministrationPanel(app, data);
}
