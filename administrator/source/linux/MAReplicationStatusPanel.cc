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
#include "MAReplicationStatusPanel.h"
#include "myg_gtkutils.h"



MAReplicationStatusPanel::MAReplicationStatusPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _replist(0)
{
}


bool MAReplicationStatusPanel::init()
{
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_REPLICATIONSTATUS_FILE), "panel_frame"))
    return false;

  _running_icon= PIXCACHE->load("16x16_start.png");
  _stopped_icon= PIXCACHE->load("16x16_error.png");
  _new_host_icon= PIXCACHE->load("16x16_warning.png");

  _info_store= Gtk::ListStore::create(_info_columns);
  Gtk::TreeView *tree= (Gtk::TreeView*)get_widget("info_tree");
  tree->set_model(_info_store);

  Gtk::TreeView::Column *column= new Gtk::TreeView::Column(_("Hostname"));
  column->pack_start(_info_columns._icon);
  column->pack_start(_info_columns._server);
  tree->append_column(*Gtk::manage(column));
  tree->append_column(_("ID"), _info_columns._id);
  tree->append_column(_("Port"), _info_columns._port);
  tree->append_column(_("Kind"), _info_columns._kind);
  tree->append_column(_("Status"), _info_columns._status);
  tree->append_column(_("Log File"), _info_columns._logfile);
  tree->append_column(_("Log Pos."), _info_columns._logpos);
  for (int i= 0; i < 7; i++)
  {
    tree->get_column(i)->set_resizable(true);
  }
  
  get_button("add_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAReplicationStatusPanel::add_host));
  get_button("remove_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAReplicationStatusPanel::remove_host));
  get_button("refresh_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAReplicationStatusPanel::update_list));
  
  get_tree("info_tree")->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MAReplicationStatusPanel::update_sensitivity));
  
  std::string path= prefs.build_path_to("administrator/mysqladmin_replication_hosts_"+_inst->get_connection_data().hostname+".xml");

  MYX_ADMIN_LIB_ERROR err;
  _replist= myx_read_repl_user_hosts(path.c_str(), &err);
  if (!_replist)
  {
    _replist= (MYX_USER_REPL_HOSTS*)g_malloc0(sizeof(MYX_USER_REPL_HOSTS));
  }
  
  update_sensitivity();

  return true;
}


void MAReplicationStatusPanel::update_sensitivity()
{
  Gtk::TreeIter iter= get_tree("info_tree")->get_selection()->get_selected();

  if (iter)
  {
    Gtk::TreeRow row= *iter;
    get_button("add_button")->set_sensitive(!row[_info_columns._is_known]);
    get_button("remove_button")->set_sensitive(row[_info_columns._is_known]);
  }
  else
  {
    get_button("add_button")->set_sensitive(false);
    get_button("remove_button")->set_sensitive(false);
  }
}


void MAReplicationStatusPanel::add_host()
{
  Gtk::TreeIter iter= get_tree("info_tree")->get_selection()->get_selected();
  Gtk::TreeRow row= *iter;
  Glib::ustring name= row[_info_columns._server];

  _replist->hosts_num++;
  _replist->hosts= (MYX_USER_REPL_HOST*)g_realloc(_replist->hosts, 
                             sizeof(MYX_USER_REPL_HOST)*_replist->hosts_num);
  _replist->hosts[_replist->hosts_num-1].name= g_strdup(name.c_str());

  update_list();
}


void MAReplicationStatusPanel::remove_host()
{
  Gtk::TreeIter iter= get_tree("info_tree")->get_selection()->get_selected();
  Gtk::TreeRow row= *iter;
  Glib::ustring name= row[_info_columns._server];

  row[_info_columns._is_known]= false;

  for (unsigned int i= 0; i < _replist->hosts_num; i++)
  {
    if (name.compare(_replist->hosts[i].name)==0)
    {
      g_free(_replist->hosts[i].name);
      if (_replist->hosts_num > 1)
      {
        memmove(_replist->hosts+i, _replist->hosts+i+1, 
                sizeof(MYX_USER_REPL_HOST)*(_replist->hosts_num-i-1));
      }
      _replist->hosts_num--;
      break;
    }
  }
  update_list();
}



void MAReplicationStatusPanel::update_list()
{
  MYX_ADMIN_LIB_ERROR err= MYX_ADMIN_NO_ERROR;
  MYX_REPL_HOSTS *slaves;

  _info_store->clear();

  slaves= (MYX_REPL_HOSTS*)_inst->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_show_repl_hosts_status,
                                                      _replist, &err,
                                                      _("Retrieving slave status information..."));
  if (slaves)
  {
    for (unsigned int i= 0; i < slaves->hosts_num; i++)
    {
      MYX_REPL_HOST *slave= slaves->hosts+i;
      Gtk::TreeIter iter= _info_store->append();
      Gtk::TreeRow row= *iter;
      
      row[_info_columns._server]= slave->host;
      row[_info_columns._id]= slave->server_id;
      switch (slave->status)
      {
      case MYX_RHS_NEW_HOST:
        row[_info_columns._icon]= _new_host_icon;
        row[_info_columns._is_known]= false;
        row[_info_columns._status]= _("New Instance");
        break;
      case MYX_RHS_AVAILABLE:
        row[_info_columns._icon]= _running_icon;
        row[_info_columns._is_known]= true;
        row[_info_columns._status]= _("Available");
        break;
      case MYX_RHS_NOT_AVAILABLE:
        row[_info_columns._icon]= _stopped_icon;
        row[_info_columns._is_known]= true;
        row[_info_columns._status]= _("Not Available");
        break;
      }
      row[_info_columns._kind]= slave->is_master ? _("MASTER") : _("SLAVE");
      row[_info_columns._logfile]= (slave->binlog_file)?:"";
      row[_info_columns._logpos]= (slave->binlog_pos)?:"";
    }

    myx_free_repl_hosts_status(slaves);
  }
  else
  {
    if (err == MYX_ADMIN_NO_ERROR)
      _app->set_status(_("Replication not enabled."));
    else
      show_adminlib_error(*_app->window(),
                          _("Could not retrieve replication status information."), err);
  }
}


bool MAReplicationStatusPanel::before_show()
{
  update_list();
  
  return true;
}


bool MAReplicationStatusPanel::before_quit()
{
  if (_inst->is_connected())
  {
    std::string path= prefs.build_path_to("administrator/mysqladmin_replication_hosts_"+_inst->get_connection_data().hostname+".xml");

    if (_replist && myx_save_repl_user_hosts(_replist, path.c_str()) < 0)
    {
      g_warning(_("Error saving replication monitoring list."));
    }
  }

  return true;
}


MAPanel *create_replication_status_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAReplicationStatusPanel(app, data);
}

