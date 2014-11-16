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


#include "MGSchemaBrowserHelper.h"
#include "MGTableEditor.h"
#include "myg_utils.h"
#include "myg_gtkutils.h"
#include "mygpriv.h"

#include "MGTableEditor.h"

#include <gtkmm/messagedialog.h>


MGSchemaBrowserHelper::MGSchemaBrowserHelper(MGTableBrowserList *browser)
  : _editor(0), _browser(browser), _mysql(0)
{
  
}


void MGSchemaBrowserHelper::set_mysql(MYSQL *mysql)
{
  _mysql= mysql;
}


bool MGSchemaBrowserHelper::confirm(const Glib::ustring &message)
{
  Gtk::MessageDialog dlg(message, false, Gtk::MESSAGE_WARNING, Gtk::BUTTONS_OK_CANCEL,
                         true);

  return dlg.run() == Gtk::RESPONSE_OK;
}


void MGSchemaBrowserHelper::create_schema()
{
  Glib::ustring query, name;

  if (myg_ask_string(_("Create Schema"),
                     _("<b>Create Schema</b>\nEnter a name for the new schema."), name))
  {
    MYX_LIB_ERROR error;
    long long int affected_rows= 0L;

    myx_query_execute_direct(_mysql, ufmt("CREATE DATABASE `%s`", name.c_str()).c_str(), &error, &affected_rows);

    if (error != MYX_NO_ERROR)
      myg_show_xlib_error(_("Error creating schema."), error);
    else
      _signal_refresh.emit();
  }
}


void MGSchemaBrowserHelper::create_table()
{
  Glib::ustring catalog, schema;
  Gtk::TreeIter it= _browser->get_selected();
  if (it)
  {
    catalog= _browser->get_catalog(it);
    schema= _browser->get_schema(it);

    if (!_editor)
    {
      _editor= new MGTableEditor(true);
      _editor->signal_close().connect(sigc::mem_fun(*this,&MGSchemaBrowserHelper::table_editor_closed));
    }
    _editor->new_table(_mysql, catalog, schema, _browser->get_catalogs());
  }
}


void MGSchemaBrowserHelper::edit_table()
{
  Gtk::TreeIter iter= _browser->get_selected();
  if (iter)
  {    
    Gtk::TreeRow row= *iter;
    Glib::ustring catalog, schema;
    Glib::ustring table= _browser->get_table(iter);

    catalog= _browser->get_catalog(iter);
    schema= _browser->get_schema(iter);

    if (!_editor)
    {
      _editor= new MGTableEditor(true);
      _editor->signal_close().connect(sigc::mem_fun(*this,&MGSchemaBrowserHelper::table_editor_closed));
    }
    _editor->show_table(_mysql, catalog, schema, table,
                        _browser->get_catalogs());
  }
}


void MGSchemaBrowserHelper::table_editor_closed()
{
  delete _editor;
  _editor= 0;
  
  _signal_refresh.emit();
}


void MGSchemaBrowserHelper::edit_object()
{
  Gtk::TreeIter iter= _browser->get_selected();
  if (iter) 
  {
    switch (_browser->get_type(iter))
    {
    case MGTableBrowserList::Table:
      edit_table();
      break;
    default:
      break;
    }
  }
}


void MGSchemaBrowserHelper::drop_object()
{
  bool is_func;
  Gtk::TreeIter iter= _browser->get_selected();
  if (iter) 
  {
    Glib::ustring query, what, schema;
    schema= _browser->get_schema(iter);
    
    switch (_browser->get_type(iter))
    {
    case MGTableBrowserList::Schema:
      what= schema;
      if (confirm(ufmt(_("This will permanently drop (delete) the Schema '%s' and all its tables.\nPlease confirm."),
                       what.c_str())))
      {
        query= ufmt("DROP DATABASE `%s`", what.c_str());
      }
      break;
    case MGTableBrowserList::Table:
      what= _browser->get_table(iter);
      if (_browser->is_view(iter))
      {
        if (confirm(ufmt(_("This will permanently drop (delete) the View '%s'.\nPlease confirm."),
                         what.c_str())))
        {
          query= ufmt("DROP VIEW `%s`.`%s`", schema.c_str(), what.c_str());
        }
      }
      else
      {
        if (confirm(ufmt(_("This will permanently drop (delete) the Table '%s'.\nPlease confirm."),
                         what.c_str())))
        {
          query= ufmt("DROP TABLE `%s`.`%s`", schema.c_str(), what.c_str());
        }
      }
      break;
    case MGTableBrowserList::SP:
      //what= _browser->get_table(iter);
      what= _browser->get_procedure(iter, is_func);
      if (confirm(ufmt(_("This will permanently drop (delete) the Procedure/Function '%s'.\nPlease confirm."),
                       what.c_str())))
      {
        if(is_func)
        {
          query= ufmt("DROP FUNCTION `%s`.`%s`", schema.c_str(), what.c_str());
        }
        else
        {
          query= ufmt("DROP PROCEDURE `%s`.`%s`", schema.c_str(), what.c_str());
        }
      }
      break;
    default:
      break;
    }

    if (!query.empty())
    {
      MYX_LIB_ERROR error;
      long long int affected_rows= 0L;

      myx_query_execute_direct(_mysql, 
        "/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;", 
          &error, &affected_rows);
      myx_query_execute_direct(_mysql, query.c_str(), &error, &affected_rows);
      myx_query_execute_direct(_mysql, 
        "/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;", 
        &error, &affected_rows);

      if (error != MYX_NO_ERROR)
        myg_show_xlib_error(ufmt(_("Error executing drop of %s."), what.c_str()), error);
      else
        _signal_refresh.emit();
    }
  }
}
