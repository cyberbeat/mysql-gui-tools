/* Copyright (C) 2006 MySQL AB

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

#include "MGRTWizardBase.h"
#include "myg_gtkutils.h"
#include "MGGladeXML.h"
#include "mygpriv.h"
/**
 * @file  MGRTWizardBase.cc
 * @brief 
 */


MGRTWizardBase::MGRTWizardBase(GtkWindow *win)
  : Gtk::Window(win)
{
  _grt= 0;
  _xml= 0;

  _section= 0;
  _last_section= 0;
    
  _advanced_shown= false;
  _back_was_enabled= true;
  _next_was_enabled= true;
  
  _task_unchecked= PIXCACHE->load("task_unchecked.png");
  _task_checked= PIXCACHE->load("task_checked.png");
  _task_error= PIXCACHE->load("task_error.png");
  _task_disabled= PIXCACHE->load("task_disabled.png");
}


void MGRTWizardBase::setup()
{
  _xml->get_button("next_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTWizardBase::go_next));
  _xml->get_button("fin_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTWizardBase::go_finish));
  _xml->get_button("back_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTWizardBase::go_back));
  _xml->get_button("cancel_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTWizardBase::cancel));
  _xml->get_button("show_more_button")->signal_clicked().connect(sigc::mem_fun(*this,&MGRTWizardBase::show_details));

  _xml->get_image("sakila_image")->set(PIXCACHE->load("sakila.png"));
}


void MGRTWizardBase::log_text(const Glib::ustring &text)
{
  _xml->get_text("log_text")->get_buffer()->insert_at_cursor(text+"\n");
}


void MGRTWizardBase::log_msg(const Glib::ustring &msg, const Glib::ustring &text)
{
  _xml->get_text("log_text")->get_buffer()->insert_at_cursor(msg+": "+text+"\n");
}


void MGRTWizardBase::begin_work()
{
  _xml->get_image("sakila_image")->set(PIXCACHE->load_anim("dolphin_anim.gif"));

  _back_was_enabled= _xml->get_button("back_button")->is_sensitive();
  _next_was_enabled= _xml->get_button("next_button")->is_sensitive();
  
  _xml->get_button("back_button")->set_sensitive(false);
  _xml->get_button("next_button")->set_sensitive(false);
  _xml->get_button("cancel_button")->set_sensitive(false);

  _grt->set_output_handler(sigc::mem_fun(*this,&MGRTWizardBase::log_text));
  _grt->set_message_handler(sigc::mem_fun(*this,&MGRTWizardBase::log_msg));
}


void MGRTWizardBase::end_work()
{
  _grt->reset_output_handler();
  _grt->reset_message_handler();
  
  _xml->get_image("sakila_image")->set(PIXCACHE->load("sakila.png"));
  
  _xml->get_button("back_button")->set_sensitive(_back_was_enabled);
  _xml->get_button("next_button")->set_sensitive(_next_was_enabled);
  _xml->get_button("cancel_button")->set_sensitive(true);
}



void MGRTWizardBase::set_grt(MGRT *grt)
{
  _grt= grt;
}



void MGRTWizardBase::go_back()
{
  if (_section > 0)
  {
    _section--;
    update_section(true);
  }
}


void MGRTWizardBase::go_next()
{
  if (_section < _last_section)
  {
    _section++;
    update_section(false);
  }
  else
    Gtk::Main::instance()->quit();
}


void MGRTWizardBase::go_finish()
{
  Gtk::Main::instance()->quit();
}


void MGRTWizardBase::update_advanced()
{
}


bool MGRTWizardBase::validate_section()
{
  return true;
}


void MGRTWizardBase::update_section(bool back)
{
  if (_section == 0)
    _xml->get_button("back_button")->set_sensitive(false);
  else
    _xml->get_button("back_button")->set_sensitive(true);
  
  if (validate_section())
    _xml->get_button("next_button")->set_sensitive(true);
  else
    _xml->get_button("next_button")->set_sensitive(false);
    

  if (_section == _last_section)
  {
    _xml->get_button("next_button")->hide();
    _xml->get_button("fin_button")->show();
  }
  else
  {
    _xml->get_button("next_button")->show();
    _xml->get_button("fin_button")->hide();
  }

  update_advanced();
}


int MGRTWizardBase::status_query()
{
  return _cancelled ? 1 : 0;
}


void MGRTWizardBase::cancel()
{
  _cancelled= true;
  
  Gtk::Main::instance()->quit();
}


void MGRTWizardBase::show_details()
{
  _advanced_shown= !_advanced_shown;
  if (_advanced_shown)
  {
    _xml->get_button("show_more_button")->set_label(_("Show Less <<"));
    update_advanced();
  }
  else
  {
    _xml->get_button("show_more_button")->set_label(_("Show More >>"));
    update_advanced();
  }
}


bool MGRTWizardBase::run()
{
  show();
  update_section(false);
  
  Gtk::Main::instance()->run();
  
  hide();

  return true;
}
