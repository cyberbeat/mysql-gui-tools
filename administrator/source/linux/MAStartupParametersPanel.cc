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
#include "MAStartupParametersPanel.h"
#include "MDataInterface.h"
#include "myg_utils.h"
#include "myg_gtkutils.h"

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

MAStartupParametersPanel::MAStartupParametersPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _file_dlg(0), _dint(0), _my_cnf_missing_error_displayed(false)
{
}


MAStartupParametersPanel::~MAStartupParametersPanel()
{
  delete _dint;
}


bool MAStartupParametersPanel::init()
{
  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_STARTUPPARAMETERS_FILE), "panel_frame"))
    return false;
  
  _showcase= new MGGladeXML(get_glade_file(GLADE_STARTUPPARAMETERS_FILE), "ad_box");

  _showcase->get_button("info_button")->signal_clicked().connect(sigc::mem_fun(*_app,&MAdministrator::open_merlin_page));
  
  get_button("select_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAStartupParametersPanel::select_file));
  get_button("discard_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAStartupParametersPanel::revert_changes));
  get_button("apply_button")->signal_clicked().connect(sigc::mem_fun(*this,&MAStartupParametersPanel::apply_changes));
  
  return true;
}


bool MAStartupParametersPanel::show_configuration(const std::string &file, const std::string &section)
{
  char *version;

  _dint= new MDynamicInterface((Gtk::Notebook*)get_widget("notebook"),
                               PIXCACHE->load("24x24_ServerStartupParams.png"),
                               _data->get_instance());

  if (_inst->is_connected())
  {
    version= (char*)_inst->perform_data_fetch2((MInstanceInfo::DataFetcher2)myx_get_server_variable,
                                               (char*)"version");
    if (!version)
      return false;
  }
  else
  {
    version= NULL;
    g_spawn_command_line_sync("mysql_config --version", &version, NULL, NULL, NULL);
    if (!version)
      version= g_strdup("5.0");
  }

  if (_dint->open(get_app_file("mysqladmin_startup_variables_description.xml"), version,
                  file, section) == MYX_ADMIN_NO_ERROR)
  {
    Glib::ustring editable;
    
    if (access(file.c_str(), W_OK)!=0)
      editable= _("Note: changes can't be saved because the file is not writable.");

    g_free(version);
    get_label("path_label")->set_markup(ufmt(_("Editing file <b>%s</b>         Section <b>%s</b>"),
                                             file.c_str(), section.c_str())+"\n<small>"+editable+"</small>");

    ((Gtk::Notebook*)get_widget("notebook"))->append_page(*_showcase->get_widget("ad_box"),
                                                          _("Administration & Security Advisors"));

    return true;
  }
  g_free(version);
  return false;
}


void MAStartupParametersPanel::select_file()
{
  Gtk::Dialog *dlg;
  
  if (!_file_dlg)
  {
    _file_dlg= new MGGladeXML(get_glade_file(GLADE_STARTUPPARAMETERS_FILE), "select_dialog");
  }

  dlg= (Gtk::Dialog*)_file_dlg->get_widget("select_dialog");
  
  dlg->run();
}


void MAStartupParametersPanel::revert_changes()
{
  if (_dint)
  {
    _app->push_status(_("Reverting changes..."));
    _dint->revert_values();
    _app->pop_status();
  }
}


void MAStartupParametersPanel::apply_changes()
{
  int res;

  if (_dint)
  {
    _app->push_status(_("Saving configurations..."));
    res= _dint->save();
    _app->pop_status();

    if (res==0)
    {
      _app->set_status(_("Configuration saved."));
    }
    else if (res == -1)
    {
      myg_show_error(*_app->window(), _("Could not save configurations."));
    }
    else
    {
      myg_show_error(*_app->window(), _("Error updating configuration."));
    }
  }
}



bool MAStartupParametersPanel::create_file(const std::string &path)
{
  int fd= creat(path.c_str(), O_CREAT|O_EXCL);

  if (fd < 0)
  {
    myg_show_sys_error(*_app->window(), 
                       ufmt(_("Could not create %s"), path.c_str()),
                       errno);
    return false;
  }
  else
  {
    close(fd);
    myg_show_info(*_app->window(), ufmt(_("%s created."), path.c_str()));
    return true;
  }
}



void MAStartupParametersPanel::request_file_path()
{
  Gtk::FileSelection fsel(_("Locate my.cnf file"));

retry:

  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    if (!Glib::file_test(fsel.get_filename(), Glib::FILE_TEST_EXISTS))
    {
      Gtk::MessageDialog dlg(*_app->window(),
                             ufmt(_("File %s does not exist. Create it?"),
                                  fsel.get_filename().c_str()),
                             false,
                             Gtk::MESSAGE_QUESTION,
                             Gtk::BUTTONS_YES_NO, true);
      
      if (dlg.run() == Gtk::RESPONSE_YES)
      {
        if (!create_file(fsel.get_filename()))
          goto retry;
      }
      else
        goto retry;
    }
    else if (!Glib::file_test(fsel.get_filename(), Glib::FILE_TEST_IS_REGULAR))
    {
      myg_show_error(*_app->window(), ufmt(_("%s is not a file."),
                                           fsel.get_filename().c_str()));
      goto retry;
    }
    myx_set_my_cnf_path(fsel.get_filename().c_str());
  }
}



void MAStartupParametersPanel::show()
{
  MAPanel::show();

  if (!_dint)
  {
    bool picked_file= false;
    
    if (!myx_get_my_cnf_path() && _inst->get_saved_info())
    {
      std::string path= _inst->get_saved_info()->mycnf_path;
      
      if (!path.empty())
        myx_set_my_cnf_path(path.c_str());
    }
    
    if (myx_get_my_cnf_path() && 
        !Glib::file_test(myx_get_my_cnf_path(), Glib::FILE_TEST_EXISTS))
      myx_set_my_cnf_path(NULL);
    
    if (!myx_get_my_cnf_path())
    {
      if (!_my_cnf_missing_error_displayed)
      {
        Gtk::MessageDialog dlg(*_app->window(),
                               _("The my.cnf configuration file could not be found.\n"
                                 "Please select an action.\n"),
                               false,
                               Gtk::MESSAGE_WARNING,
                               Gtk::BUTTONS_NONE,
                               true);
        
        dlg.add_button(_("Specify Location"), 1);
        dlg.add_button(_("Create a New One"), 2);
        dlg.add_button(_("Ignore"), 3);

        switch (dlg.run())
        {
        case 1:
          request_file_path();
          picked_file= true;
          break;
        case 2:
          create_file("/etc/my.cnf");
          break;
        case 3:
          _my_cnf_missing_error_displayed= true;
          break;
        }        
      }
    }

    if (myx_get_my_cnf_path())
    {
      bool ok;
      bool cancelled= false;
      MYX_STRINGLIST *sections;
      MYX_ADMIN_LIB_ERROR error_code;
      std::string section= "mysqld";

      sections= myx_get_all_cnf_sections(myx_get_my_cnf_path(),
                                         &error_code);
      
      if (sections && sections->strings_num > 0)
      {
        MGGladeXML *xml= new MGGladeXML(get_glade_file(GLADE_STARTUPPARAMETERS_FILE), "section_dialog");
        Gtk::VBox *vbox= (Gtk::VBox*)xml->get_widget("vbox1");
        Gtk::RadioButton::Group group;
        std::vector<Gtk::RadioButton*> buttons;

        for (unsigned int i= 0; i < sections->strings_num; i++)
        {
          if (g_str_has_prefix(sections->strings[i], "mysqld") && strcmp(sections->strings[i], "mysqldump")!=0)
          {
            Gtk::RadioButton *btn;
            if (i > 0)
              btn= Gtk::manage(new Gtk::RadioButton(group, sections->strings[i], false));
            else
              btn= Gtk::manage(new Gtk::RadioButton(sections->strings[i]));
            section= sections->strings[i]; // in case there's only one section
            vbox->pack_start(*btn, false, false);
            
            buttons.push_back(btn);
            
            if (i==0)
              group= btn->get_group();
          }
        }
        vbox->show_all();

        if (buttons.size()>1)
        {
          if (((Gtk::Dialog*)xml->get_widget("section_dialog"))->run() == Gtk::RESPONSE_OK)
          {
            int b= 0;
            for (unsigned int i= 0; i < sections->strings_num; i++)
            {
              if (g_str_has_prefix(sections->strings[i], "mysqld") && strcmp(sections->strings[i], "mysqldump")!=0)
              {
                if (buttons[b]->get_active())
                {
                  section= sections->strings[i];
                  break;
                }
                b++;
              }
            }
          }
          else
            cancelled= true;

          ((Gtk::Dialog*)xml->get_widget("section_dialog"))->hide();
        }
        else
          cancelled= false;

        delete xml;
      }
      if (sections)
        myx_free_stringlist(sections);

      _app->push_status(_("Processing configuration file..."));
      ok= show_configuration(myx_get_my_cnf_path(), section.c_str());

      _app->pop_status();

      if (picked_file && ok && _inst->get_saved_info())
      {
        // save the file path
        _inst->get_saved_info()->mycnf_path= myx_get_my_cnf_path();
        _inst->get_saved_info()->save();
      }
      
      if (cancelled)
      {
	get_button("apply_button")->set_sensitive(false);
	get_button("discard_button")->set_sensitive(false);
      }
    }
  }
}


bool MAStartupParametersPanel::before_show()
{
  return true;
}


bool MAStartupParametersPanel::before_hide()
{
  return true;
}



MAPanel *create_startup_parameters_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAStartupParametersPanel(app, data);
}

