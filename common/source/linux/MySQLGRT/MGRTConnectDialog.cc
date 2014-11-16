/* Copyright (C) 2005 MySQL AB

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

#include "MGRTConnectDialog.h"
#include <gtkmm/entry.h>
#include <gtkmm/checkbutton.h>
#include <gtkmm/table.h>
#include <gtkmm/messagedialog.h>
#include <gtkmm/box.h>
#include <gtkmm/treestore.h>
#include <gtkmm/image.h>
#include <gtkmm/stock.h>
#include "myg_gtkutils.h"
#include "mygpriv.h"
/**
 * @file  MGRTConnectDialog.cc
 * @brief 
 */



  
// ----------------------------------------------------------------------

class MGRTConnectDialog::ParameterMapping {
  public:
    ParameterMapping();
    ~ParameterMapping();

    void update(MGRTValue &paramValues);
    
    Gtk::Label *label;
    char type;
    Gtk::Widget *widget;
    
    Gtk::Entry *as_entry() { return (Gtk::Entry*)widget; };
    Gtk::CheckButton *as_check() { return (Gtk::CheckButton*)widget; };
    void set(const char *value);
    void set(bool value);

    MGRTValue *param;
};


MGRTConnectDialog::ParameterMapping::ParameterMapping()
  : label(0), param(0)
{
  widget= 0;
}


MGRTConnectDialog::ParameterMapping::~ParameterMapping()
{
  delete param;
}


void MGRTConnectDialog::ParameterMapping::set(const char *value)
{
  if (type == 'T')
    as_entry()->set_text(value);
  else
    as_check()->set_active(atoi(value));
}


void MGRTConnectDialog::ParameterMapping::set(bool value)
{
  as_check()->set_active(value);
}


void MGRTConnectDialog::ParameterMapping::update(MGRTValue &paramValues)
{
  switch (type)
  {
  case 'T':
    paramValues.set((*param)["name"].asString(),
                    as_entry()->get_text().c_str());
    break;
  case 'C':
    paramValues.set((*param)["name"].asString(),
                    as_check()->get_active() ? "1" : "0");
    break;
  }
}

//----------------------------------------------------------------------

MGRTConnectDialog::MGRTConnectDialog(MGRT *grt, const Glib::ustring &path, const Glib::ustring &target, bool windowed)
  : _grt(grt), _window(0), _rdbms_frame(0),
_option_frame(0), _advanced_frame(0)
{
  _conn_info_path= path;
  _conn_target_path= target;
  _jdbc_only= false;
  _connection_combo= 0;
  
  _details_button= 0;
  _cancel_button= 0;
  _clear_button= 0;
  _connect_button= 0;
}


MGRTConnectDialog::~MGRTConnectDialog()
{
  for (std::map<Glib::ustring,ParameterMapping*>::iterator iter= _parameters.begin();
       iter != _parameters.end(); ++iter)
  {
    delete iter->second;
  }

  delete _window;
  
  delete _rdbms_frame;
  delete _option_frame;
  delete _advanced_frame;
}


void MGRTConnectDialog::setup()
{
  if (_pick_rdbms)
  {
    Gtk::Table *table;
    
    _rdbms_frame= new Gtk::Frame(_("Driver Selection"));
    
    table= new Gtk::Table(2, 3);
    _rdbms_frame->add(*table);
    table->set_border_width(12);
    table->set_row_spacings(6);
    table->set_col_spacings(8);

    _rdbms_combo= new Gtk::ComboBox();
    _rdbms_combo->pack_start(_combo_columns.name);
    _rdbms_combo->signal_changed().connect(sigc::mem_fun(*this,&MGRTConnectDialog::rdbms_selected));
    _driver_combo= new Gtk::ComboBox();
    _driver_combo->pack_start(_combo_columns.name);
    _driver_combo->signal_changed().connect(sigc::mem_fun(*this,&MGRTConnectDialog::driver_selected));
    
    table->attach(*new Gtk::Label(_("Database System:"), 1.0, 0.5), 0, 1, 0, 1,
                  Gtk::FILL, Gtk::AttachOptions(0),
                  0, 0);
    table->attach(*_rdbms_combo, 1, 2, 0, 1, 
                  Gtk::FILL, Gtk::AttachOptions(0),
                  0, 0);

    table->attach(*new Gtk::Label(_("Driver:"), 1.0, 0.5), 0, 1, 1, 2,
                  Gtk::FILL, Gtk::AttachOptions(0),
                  0, 0);
    table->attach(*_driver_combo, 1, 2, 1, 2,
                  Gtk::FILL, Gtk::AttachOptions(0),
                  0, 0);
  }

  _connection_store= Gtk::ListStore::create(_combo_columns);

  _connection_box= Gtk::manage(new Gtk::HBox(false, 8));

  _connection_label= Gtk::manage(new Gtk::Label(_("Stored Connections:"), 1.0, 0.5));
  _connection_box->pack_start(*_connection_label, false, false);

  _connection_combo= Gtk::manage(new Gtk::ComboBox());
  _connection_combo->set_model(_connection_store);
  _connection_combo->pack_start(_combo_columns.name);
  _connection_combo->signal_changed().connect(sigc::mem_fun(*this,&MGRTConnectDialog::connection_changed));
  _connection_combo->set_size_request(220, -1);
  _connection_box->pack_start(*_connection_combo, false, true);

  _add_button= Gtk::manage(new Gtk::Button(""));
  Gtk::Image *image= Gtk::manage(new Gtk::Image(Gtk::Stock::ADD, Gtk::ICON_SIZE_BUTTON));
  _add_button->remove();
  _add_button->add(*image);
  _add_button->signal_clicked().connect(sigc::mem_fun(*this,&MGRTConnectDialog::add_connection));
  image->show();
  _connection_box->pack_start(*_add_button, false, false);
  _remove_button= Gtk::manage(new Gtk::Button(""));
  image= Gtk::manage(new Gtk::Image(Gtk::Stock::REMOVE, Gtk::ICON_SIZE_BUTTON));
  _remove_button->remove();
  _remove_button->add(*image);
  _remove_button->signal_clicked().connect(sigc::mem_fun(*this,&MGRTConnectDialog::remove_connection));
  image->show();
  _connection_box->pack_start(*_remove_button, false, false);
  _connection_box->show_all();

  _option_frame= new Gtk::Frame(_("Connection Options"));


  _advanced_frame= new Gtk::Frame(_("Advanced Options"));
  
  if (_pick_rdbms)
    refresh_rdbms_info();
  else
    driver_selected();
}
  
    
void MGRTConnectDialog::set_selects_rdbms(bool flag)
{
  _pick_rdbms= flag;
}


void MGRTConnectDialog::set_edits_schema(bool flag)
{
  _pick_schema= flag;
}


int MGRTConnectDialog::add_drivers_to_combo(MGRTValue *rdbms)
{
  MGRTValue drivers((*rdbms)["drivers"]);
  unsigned int i;
  Glib::RefPtr<Gtk::ListStore> model= _driver_list= Gtk::ListStore::create(_combo_columns);

  for (i= 0; i < drivers.count(); i++)
  {
    MGRTValue driver(drivers[i]);

    if (_jdbc_only && strcmp(driver.contentStruct(), "db.mgmt.JdbcDriver")!=0)
      continue;

    // we dont support driver fetching yet
    if (!driver["isInstalled"].asInt())
      continue;

    // driverFilter

    Gtk::TreeIter iter;
    
    // place default driver on top of dropdown
    if ((*rdbms)["defaultDriver"].compareObject(driver))
      iter= model->prepend();
    else
      iter= model->append();
    Gtk::TreeRow row= *iter;
    row[_combo_columns.name]= (const char*)driver["caption"];
    row[_combo_columns.data]= driver;
  }

  _driver_combo->set_model(model);
  
  int count= model->children().size();
  
  _driver_combo->set_sensitive(count > 1);

  return count;
}
 

void MGRTConnectDialog::rdbms_selected()
{
  MGRTValue rdbms;
  Gtk::TreeIter iter= _rdbms_combo->get_active();
  Gtk::TreeRow row= *iter;
  
  rdbms= (MGRTValue)row[_combo_columns.data];
  
  add_drivers_to_combo(&rdbms);
  _driver_combo->set_active(0);
  driver_selected();
}


void MGRTConnectDialog::driver_param_value_changed()
{
  if (!_setting_conn_values)
  {
    bool ok= true;
    for (std::map<Glib::ustring,ParameterMapping*>::const_iterator iter= _parameters.begin();
         iter != _parameters.end(); ++iter)
    {
      ParameterMapping *mapping= iter->second;

      if ((*mapping->param)["required"].asInt() == 1 ||
          _pick_schema && strcmp((*mapping->param)["name"].asString(), "schema")==0)
      {
        if (mapping->type == 'T')
        {
          if (mapping->as_entry()->get_text() == "")
          {
            ok= false;
            break;
          }
        }
      }
    }
    _signal_ready_changed.emit(ok);
    //_connect_button->set_sensitive(true);
  }
}


void MGRTConnectDialog::build_driver_controls(Gtk::Frame *frame,
                                              MGRTValue *driver,
                                              bool advanced)
{
  unsigned int i;
  MGRTValue params((*driver)["parameters"]);
  int maxRow= -1;
  Gtk::VBox *vbox;
  Gtk::HBox *hbox;

  Glib::RefPtr<Gtk::SizeGroup> sg;

  sg= Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);

  if (advanced)
    _advanced_sg= sg;
  else
    _param_sg= sg;

  vbox= Gtk::manage(new Gtk::VBox(false, 8));
  vbox->set_border_width(12);
  frame->add(*vbox);

  if (!advanced)
  {
    if (_connection_box && _connection_box->get_parent())
    {
      _connection_box->reference();
      _connection_box->get_parent()->remove(*_connection_box);
    }

    sg->add_widget(*_connection_label);
    vbox->pack_start(*_connection_box, false, false);
  }
  _first_control= 0;

  for (i= 0; i < params.count(); i++)
  {
    MGRTValue param(params[i]);
    int currentRow;
    ParameterMapping *mapping;

    if ((param["layoutAdvanced"].asInt()!=0) != advanced)
      continue;

    if (!_pick_schema && strcmp(param["name"].asString(), "schema") == 0)
      continue;

    currentRow= param["layoutRow"].asInt();
    if (currentRow == -1)
      currentRow = maxRow + 1;

    if (currentRow > maxRow)
    {
      hbox= Gtk::manage(new Gtk::HBox(false, 8));
      vbox->pack_start(*hbox, false, true);
    }
    maxRow = currentRow;

    const char *caption= param["caption"];
    const char *descr= param["description"];
    const char *defVal= param["defaultValue"];
    const char *paramType= param["paramType"];

    mapping= new ParameterMapping();

    _parameters[(const char*)param["name"]]= mapping;

    mapping->param= new MGRTValue(param);

    if (strcmp(paramType, "boolean")!=0 && strcmp(paramType, "tristate")!=0)
    {
      if (caption && *caption)
      {
        mapping->label= Gtk::manage(new Gtk::Label(caption, 1.0, 0.5));

        if (hbox->children().empty())
          sg->add_widget(*mapping->label);

        hbox->pack_start(*mapping->label, false, true);
      }

      mapping->type= 'T';

      // create param edit
      mapping->widget= Gtk::manage(new Gtk::Entry());
      if (strcmp(paramType, "password")==0)
        mapping->as_entry()->set_visibility(false);

      mapping->as_entry()->signal_changed().connect(sigc::mem_fun(*this,&MGRTConnectDialog::driver_param_value_changed));

      mapping->as_entry()->set_text(defVal);
    }
    else
    {
      mapping->type= 'C';

      // create checkbox
      mapping->widget = Gtk::manage(new Gtk::CheckButton());
      
      mapping->as_check()->signal_toggled().connect(sigc::mem_fun(*this,&MGRTConnectDialog::driver_param_value_changed));

      mapping->as_check()->set_active(strcmp(defVal, "1")==0);

      mapping->as_check()->set_label(caption);
    }

    if (!_first_control)
      _first_control= mapping->widget;

    int width= param.get("layoutWidth", -1);
    if (width >= 0)
      mapping->widget->set_size_request(width);

    if (descr && *descr)
      _tooltip.set_tip(*mapping->widget, descr);

    hbox->pack_start(*mapping->widget, width < 0 ? true : false, true);

    // XXX add lookup button

    // build param description
    /*
    if (_showDescriptions)
    {
      NSTextField *text;

      text= [[[NSTextField alloc] init] autorelease];
      [text setStringValue:NSStr(descr)];
      [text setEditable:NO];
      [text sizeToFit];
      [text setFrameOrigin:NSMakePoint(offsetLeft + currentLeft[currentRow], currentTop + 4)];
      [container addSubview:text];
    }*/
  }
  vbox->show_all();
}


void MGRTConnectDialog::fill_stored_connections_for_driver(MGRTValue *driver)
{
  MGRTValue connections(MGRTValue::fromGlobal(_grt->grt(), std::string(_conn_info_path+"/storedConns").c_str()));
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  _connection_store->clear();

  for (unsigned int i= 0; i < connections.count(); i++)
  {
    MGRTValue connection(connections[i]);
  
    if (strcmp(driver->dictId(), connection["driver"].asString())==0)
    {
      iter= _connection_store->append();
      row= *iter;
      row[_combo_columns.name]= connection["name"].asString();
      row[_combo_columns.data]= connection;
    }
  }
  iter= _connection_store->append();
  row= *iter;
  row[_combo_columns.name]= _("(New Connection)");
  
  _connection_combo->set_active(0);
}


void MGRTConnectDialog::driver_selected()
{
  MGRTValue driver;

  if (_pick_rdbms)
  {
    if (_rdbms_list->children().empty())
      return;
  }
  if (_option_frame->get_child())
    _option_frame->remove();
  if (_advanced_frame->get_child())
    _advanced_frame->remove();

  for (std::map<Glib::ustring,ParameterMapping*>::iterator iter= _parameters.begin();
       iter != _parameters.end(); ++iter)
  {
    delete iter->second;
  }
  _parameters.clear();
  
  _first_control= 0;

  if (_pick_rdbms)
  {
    Gtk::TreeIter iter= _driver_combo->get_active();
    if (iter)
    {
      Gtk::TreeRow row= *iter;
      
      driver= (MGRTValue)row[_combo_columns.data];
    }
  }
  else
  {
    driver= get_default_driver();
  }

  if (driver.isValid())
  {
    if (driver["isInstalled"].asInt())
    {
      if (_connection_store->children().size() > 0)
        _connection_combo->set_active(0);
      
      build_driver_controls(_option_frame, &driver, false);

      build_driver_controls(_advanced_frame, &driver, true);
      
      fill_stored_connections_for_driver(&driver);
    }
    
    if (_first_control)
      _first_control->grab_focus();
  }
}

void MGRTConnectDialog::add_connection()
{
  Glib::ustring name;
  
  if (_connection_combo->get_active_row_number() >= 0 && _connection_combo->get_active_row_number() < (int)_connection_store->children().size()-1)
  {
    Gtk::TreeRow row= *_connection_combo->get_active();
    name= row[_combo_columns.name];
  }

  if (myg_ask_string(*(Gtk::Window*)_connection_combo->get_toplevel(), _("Store Connection"), _("Connection Name:"), name))
  {   
    MGRTValue newConn(write_connection_to_target());

    newConn.set("name", name.c_str());

    // check if there already is a connection with this name stored
    MGRTValue connList(_grt->global_value(_conn_info_path+"/storedConns"));

    for (unsigned int i= 0; i < connList.count(); i++)
    {
      MGRTValue conn(connList[i]);
    
      if (strcmp(conn["driver"].asString(), newConn["driver"].asString())==0
          && strcmp(conn["name"].asString(), name.c_str())==0)
      {
        connList.remove(i);
        break;
      }
    }
    
    // Store connection
    connList.append(newConn.copy());

    Gtk::TreeIter iter= _driver_combo->get_active();
    if (iter)
    {
      Gtk::TreeRow row= *iter;
      MGRTValue driver(row[_combo_columns.data]);
      fill_stored_connections_for_driver(&driver);

      _connection_combo->set_active(_connection_store->children().size()-2);
      
      connection_changed();
    }
  }
}


void MGRTConnectDialog::remove_connection()
{
  Gtk::TreeIter iter= _connection_combo->get_active();
  if (!iter) return ;

  Gtk::TreeRow row= *iter;

  Gtk::MessageDialog dlg(*(Gtk::Window*)_connection_combo->get_toplevel(), ufmt(_("Remove Connection '%s'?"),
                                     ((Glib::ustring)row[_combo_columns.name]).c_str()),
                         false, Gtk::MESSAGE_QUESTION, Gtk::BUTTONS_YES_NO);
  if (dlg.run() == Gtk::RESPONSE_YES)
  {
    MGRTValue delConn= row[_combo_columns.data];
    
    MGRTValue connList(_grt->global_value(_conn_info_path+"/storedConns"));
    
    for (unsigned int i= 0; i < connList.count(); i++)
    {
      MGRTValue conn(connList[i]);
      
      if (strcmp(conn["driver"].asString(), delConn["driver"].asString())==0
          && strcmp(conn["name"].asString(), delConn["name"].asString())==0)
      {
        connList.remove(i);
        
        row= *_driver_combo->get_active();
        MGRTValue driver(row[_combo_columns.data]);
        fill_stored_connections_for_driver(&driver);
        connection_changed();
        break;
      }
    }
  }
}


int MGRTConnectDialog::index_of_list_item(Glib::RefPtr<Gtk::ListStore> list, const MGRTValue &value)
{
  Gtk::TreeIter iter= list->children().begin();
  Gtk::TreeRow row;
  int i= 0;

  while (iter != list->children().end())
  {
    row= *iter;
    if (((MGRTValue)row[_combo_columns.data]).grtValue() == value.grtValue())
      return i;
    ++i;
    ++iter;
  }
  return -1;
}


void MGRTConnectDialog::set_connection(MGRTValue conn)
{
  _add_button->set_sensitive(!conn.isValid());
  _remove_button->set_sensitive(conn.isValid());
  
  // if the last entry is selected <New Connection>, clear edits
  if (!conn.isValid())
  {
    for (std::map<Glib::ustring,ParameterMapping*>::const_iterator i= _parameters.begin();
         i != _parameters.end(); ++i)
    {
      ParameterMapping *mapping= i->second;

      mapping->set((*mapping->param)["defaultValue"].asString());
    }

    if (_connect_button)
      _connect_button->set_sensitive(false);
  }
  else
  {
    MGRTValue rdbmsList(MGRTValue::fromGlobal(_grt->grt(), "/rdbmsMgmt/rdbms"));
    MGRTValue driver(MGRTValue::refObject(_grt->grt(), conn["driver"].asString()));
    MGRTValue rdbms;
    const char *driverId= driver.dictId();
    
    rdbms.invalidate();
    driver.invalidate();
    
    for (unsigned int i= 0; i < rdbmsList.count(); i++)
    {
      rdbms= rdbmsList[i];
      MGRTValue drivers(rdbms["drivers"]);

      for (unsigned int j= 0; j < drivers.count(); j++)
      {
        driver= drivers[j];
  
        if (strcmp(driver.dictId(), driverId)==0)
          break;
        
        driver.invalidate();
      }
      
      if (driver.isValid())
        break;

      rdbms.invalidate();
    }
    
    if (!driver.isValid())
    {
      g_warning("The driver used by the given connection is not available.");
      return;
    }
    if (_pick_rdbms)
    {
      // Select correct RDBMS
      int idx= index_of_list_item(_rdbms_list, rdbms);
      if (idx > -1)
      {
        if (_rdbms_combo->get_active_row_number() != idx)
        {
          _rdbms_combo->set_active(idx);
          rdbms_selected();
        }
        // Select correct Driver
        idx = index_of_list_item(_driver_list, driver);
        if (idx > -1) 
        {
          if (_driver_combo->get_active_row_number() != idx)
          {
            _driver_combo->set_active(idx);
            driver_selected();
          }
        }
        else
          g_warning("The driver %s is not available for selection", driver["caption"].asString());
      }
      else
        g_warning("The RDBMS '%s' is not available for selection", rdbms["caption"].asString());
    }
    MGRTValue parameterValues(conn["parameterValues"]);

    for (std::map<Glib::ustring,ParameterMapping*>::const_iterator i= _parameters.begin();
         i != _parameters.end(); ++i)
    {
      ParameterMapping *mapping= i->second;
      MGRTValue value(parameterValues[(*mapping->param)["name"].asString()]);

      if (value.isValid())
        mapping->set(value.asString());
    }
  }

  driver_param_value_changed();
}


void MGRTConnectDialog::connection_changed()
{
  if (_connection_combo)
  {
    Gtk::TreeIter iter =_connection_combo->get_active();
    if (iter)
    {
      Gtk::TreeRow row= *iter;
      MGRTValue item(row[_combo_columns.data]);
      
      set_connection(item);
      
      if (_first_control)
        _first_control->grab_focus();
    }
    else
      set_connection();
  }
  else
    set_connection();
}


void MGRTConnectDialog::refresh_rdbms_info()
{
  MGRTValue rdbmsList(_grt->global_value(_conn_info_path+"/rdbms"));
  Glib::RefPtr<Gtk::ListStore> rdbmsModel;
  Gtk::TreeIter iter;
  int mysqlIndex= -1;
  unsigned int i;

  rdbmsModel= Gtk::ListStore::create(_combo_columns);
  _rdbms_combo->set_model(rdbmsModel);
  _rdbms_list= rdbmsModel;
  
  for (i= 0; i < rdbmsList.count(); i++)
  {
    MGRTValue rdbms(rdbmsList[i]);
    
    if (strcasecmp(rdbms["name"].asString(), "MySQL")==0)
      mysqlIndex= _rdbms_combo->get_model()->children().size();
    
    if (add_drivers_to_combo(&rdbms) > 0)
    {
      iter= rdbmsModel->append();
      Gtk::TreeRow row= *iter;
      
      row[_combo_columns.name]= (const char*)rdbms["caption"];
      row[_combo_columns.data]= rdbms;
    }
  }
  _rdbms_combo->set_active(mysqlIndex);

  rdbms_selected();
  connection_changed();
}


MGRTValue MGRTConnectDialog::get_default_driver()
{ 
  MGRTValue rdbmsList(_grt->global_value(_conn_info_path+"/rdbms"));
  for (unsigned int i= 0; i < rdbmsList.count(); i++)
  {
    MGRTValue rdbms(rdbmsList[i]);
    
    if (strcasecmp(rdbms["name"].asString(), "MySQL")==0)
    {
      MGRTValue driver(rdbms["defaultDriver"]);
      
      if (driver.type() == MYX_STRING_VALUE)
        driver= MGRTValue::refObject(_grt->grt(), driver);
      return driver;
    }
  }
  return MGRTValue();
}


MGRTValue MGRTConnectDialog::write_connection_to_target()
{
  MGRTValue driver;
  
  if (_pick_rdbms)
  {
    Gtk::TreeIter iter= _driver_combo->get_active();
    Gtk::TreeRow row= *iter;
    driver= row[_combo_columns.data];
  }
  else
  {
    driver= get_default_driver();
  }

  MGRTValue conn(MGRTValue::createObject(_grt->grt(), "db.mgmt.Connection"));

  conn.set("driver", driver.dictId());

  MGRTValue paramValues(MGRTValue::createTypedDict(MYX_STRING_VALUE));

  conn.set("parameterValues", paramValues);
  for (std::map<Glib::ustring,ParameterMapping*>::iterator iter= _parameters.begin();
       iter != _parameters.end(); ++iter)
  {
    ParameterMapping *mapping= iter->second;

    mapping->update(paramValues);
  }

  conn.set("modules", driver["defaultModules"].copy().grtValue());

  _grt->set_global_value(_conn_target_path, conn);

  return conn;
}
 
