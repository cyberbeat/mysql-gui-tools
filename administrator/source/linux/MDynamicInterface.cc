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

#include "myadmin.h"
#include "MDynamicInterface.h"
#include "MInstanceInfo.h"

#include "MGImageCheckButton.h"

#include "myg_gtkutils.h"
#include "myg_utils.h"
#include <math.h>
#include <ctype.h>
#include <errno.h>

#include <list>


MDynamicInterface::MDynamicInterface(Gtk::Notebook *top_note,
                                     const Glib::RefPtr<Gdk::Pixbuf> &icon,
                                     MInstanceInfo *instance)
    : _top_notebook(top_note), _instance(instance), _descr(0), _page_icon(icon)
{
  _edit_on= PIXCACHE->load("variable_editable.png");
  _edit_off= PIXCACHE->load("variable_disabled.png");
}


MDynamicInterface::~MDynamicInterface()
{
#if 0
  for (std::list<WSensControl*>::iterator it= _sens_controls.begin();
       it != _sens_controls.end(); ++it)
    delete *it;
#endif
  myx_free_gui_description(_descr);
}


MYX_ADMIN_LIB_ERROR MDynamicInterface::open(const std::string &file, const std::string &version,
                                            const std::string &cnf_file,
                                            const std::string &section)
{
  MYX_ADMIN_LIB_ERROR err;

  if (_descr)
    myx_free_gui_description(_descr);

  _xml_file= file;
  _version= version;
  _cnf_file= cnf_file;
  _cnf_section= section;

  _descr= myx_get_gui_description(file.c_str(), version.c_str(), MYX_LINUX, &err,
                                  cnf_file.c_str(),
                                  section.empty() ? NULL : section.c_str());
  if (!_descr)
      return err;

  for (unsigned int i= 0; i < _descr->pages_num; ++i)
  {
    if (!process_page(_descr->pages+i))
    {
      g_error(_("error processing page %i of interface description file '%s'"),
              i, file.c_str());
    }
  }

  return MYX_ADMIN_NO_ERROR;
}


int MDynamicInterface::save()
{ 
  MYX_ADMIN_LIB_ERROR err;
  std::string backup_name;

  backup_name= _cnf_file+".maold";
  int num=1;
  while (Glib::file_test(backup_name, Glib::FILE_TEST_EXISTS))
  {
    backup_name= _cnf_file+".maold"+tostr(num++);
  }
  if (copy_file(_cnf_file.c_str(), backup_name.c_str()) < 0)
  {
    myg_show_sys_error(*(Gtk::Window*)_top_notebook->get_toplevel(),
                       ufmt(_("There was an error backing up old configuration data to '%s'"),
                            backup_name.c_str()),
                       errno);
    return -1;
  }

  // fetch values from GUI and update MYX_GUI_DESCRIPTION
  fetch_values();

  err= myx_update_mysql_cnf_file(_descr, _cnf_file.c_str(), _cnf_section.c_str());

  if (err != MYX_ADMIN_NO_ERROR)
  {
    MYX_ADMIN_LIB_ERROR err2;
    MYX_GUI_DESCRIPTION *tmp;

    // something went wrong while saving
    // rollback the backup
    if (rename(backup_name.c_str(), _cnf_file.c_str()) < 0)
    {
      myg_show_sys_error(*(Gtk::Window*)_top_notebook->get_toplevel(),
                         ufmt(_("There was an error restoring the backup '%s' for the configuration file."),
                              backup_name.c_str()),
                         errno);
      return -2;
    }

    // retrieve old values from config file
    tmp= myx_get_gui_description(_xml_file.c_str(), _version.c_str(), MYX_LINUX, &err2,
                                 _cnf_file.c_str(),
                                 _cnf_section.empty() ? NULL : _cnf_section.c_str());
    if (tmp)
    {
      myx_free_gui_description(_descr);
      _descr= tmp;
    }
    else
    {
      return -2;
    }

    return -1;
  }

  return 0;
}


Gtk::Widget *MDynamicInterface::create_widget(MYX_GUI_WIDGET *widget)
{
  Gtk::Widget *w;
  WObject obj;
  char *value;

  memset(&obj, 0, sizeof(obj));
  obj.type= widget->widget_type;
  obj.ptr= widget;

  value= (char*)widget->value;
  if (!value)
    value= (char*)widget->default_value;

  switch (widget->widget_type)
  {
   case MYX_CHECKBOX:
    {
      Gtk::CheckButton *cb= Gtk::manage(new Gtk::CheckButton((char*)widget->caption));
      bool flag;
      w= cb;
      if (!value || strcasecmp(value,"Unchecked")==0)
	flag= false;
      else
	flag= true;
      if (widget->checkbox->invert)
	flag= !flag;

      cb->set_active(flag);
      
      obj.w= cb;
    }
    break;

   case MYX_SPINEDIT:
    {
      Gtk::SpinButton *spin= Gtk::manage(new Gtk::SpinButton(1.0, 0));
      int unit= 0;

      if (value && !isdigit(value[strlen(value)-1]))
      {
        switch (value[strlen(value)-1])
        {
        case 'k': unit= 1; break;
        case 'M': unit= 2; break;
        case 'G': unit= 3; break;
        }
      }

      w= spin;
      spin->set_range(0.0,4294967295.0);
      spin->set_increments(1.0, 100.0);

      spin->set_value(strtoul(value?:"0", NULL, 10));
      
      if (widget->spinedit->unitcontrolbox)
      {
        Gtk::HBox *hb= Gtk::manage(new Gtk::HBox(false, 3));
        w= hb;
        hb->pack_start(*spin, false, true);
        spin->show();
        Gtk::OptionMenu *om= Gtk::manage(new Gtk::OptionMenu);
        Gtk::Menu *menu= Gtk::manage(new Gtk::Menu);
        Gtk::MenuItem *item;
        hb->pack_start(*om, false, true);
        om->show();
        om->set_menu(*menu);
        
        item= Gtk::manage(new Gtk::MenuItem(" "));
        item->set_data("value", (void*)"");
        menu->append(*item);
        item->show();

        const char *beg=(char*)widget->spinedit->unitcontrolbox, *end;
        do
        {
          end= strchr(beg, ';');
          Glib::ustring str;
          if (end)
          {
            str= Glib::ustring(beg,end);
            item= Gtk::manage(new Gtk::MenuItem(str));
          }
          else
          {
            str= Glib::ustring(beg);
            item= Gtk::manage(new Gtk::MenuItem(str));
          }
          item->set_data("value", g_strdup(str.c_str()), g_free);
          menu->append(*item);
          item->show();
          beg= end+1;
        }
        while (end);

        om->set_history(unit);
        
        obj.aux= om;
      }
      obj.w= spin;
    }
    break;

   case MYX_TEXTEDIT:
    {      
      if (strcmp2((char*)widget->textedit->edit_type,"innodbfilepath")==0)
      {
        MInnoDBFilePathEditor *editor= Gtk::manage(new MInnoDBFilePathEditor(this, _instance));
        w= editor;
        
        editor->set_text(value?:"");

        obj.w= editor;
      } 
      else if (strcmp2((char*)widget->textedit->edit_type,"directory")==0)
      {
        MFilePathEditor *editor= Gtk::manage(new MFilePathEditor(this,true));
        w= editor;
        editor->set_text(value?:"");
        obj.w= editor;
      } 
      else if (strcmp2((char*)widget->textedit->edit_type,"file")==0)
      {
        MFilePathEditor *editor= Gtk::manage(new MFilePathEditor(this,false));
        w= editor;
        editor->set_text(value?:"");
        obj.w= editor;
      }
      else
      {
        Gtk::Entry *entry= Gtk::manage(new Gtk::Entry);
        w= entry;
        entry->set_text(value?:"");
        
        obj.w= entry;
      }
    }
    break;

   case MYX_DROPDOWNBOX:
    if (widget->dropdownbox->editable)
    {
      Gtk::Combo *combo= Gtk::manage(new Gtk::Combo);
      w= combo;
      std::list<Glib::ustring> l;
      for (unsigned int i= 0; i < widget->dropdownbox->items_num; ++i)
      {
        char *s= (char*)widget->dropdownbox->items[i];
        l.push_back(Glib::ustring(s,strchr(s,'=')));
      }
      combo->set_popdown_strings(l);

      g_message("!");
      //XXX
      obj.w= combo;
    }
    else
    {
      Gtk::OptionMenu *omenu= Gtk::manage(new Gtk::OptionMenu);
      Gtk::Menu *menu= Gtk::manage(new Gtk::Menu);
      w= omenu;
      int index= 0;
      
      for (unsigned int i= 0; i < widget->dropdownbox->items_num; ++i)
      {
        Glib::ustring s((char*)widget->dropdownbox->items[i]);
        s= s.substr(0, s.find('='));
        Gtk::MenuItem *item= Gtk::manage(new Gtk::MenuItem(s));
        menu->append(*item);
        item->show();

        if (value && strcmp((char*)value, 
                            strchr((char*)widget->dropdownbox->items[i], '=')+1)==0)
        {
          index= i;
        }
      }
      omenu->set_menu(*menu);

      omenu->set_history(index);

      obj.w= omenu;
    }
    break;
  }

  _widgets[(char*)widget->id]= obj;
 
  _tips.set_tip(*obj.w, ufmt(_("Option: %s"),(char*)widget->id));

  return w;
}


Gtk::Widget *MDynamicInterface::process_group(MYX_GUI_GROUP *group)
{
  Gtk::Frame *frame= Gtk::manage(new Gtk::Frame((char*)group->caption));
  Gtk::Table *table= Gtk::manage(new Gtk::Table(group->widgets_num, 4));

  frame->add(*table);
  table->show();
  table->set_border_width(8);
  table->set_row_spacings(8);
  table->set_col_spacings(6);
  
  int y=0;
  for (unsigned int i= 0; i < group->widgets_num; ++i, ++y)
  {
    Gtk::Widget *w= create_widget(group->widgets+i);
    Gtk::Label *l;

    if (group->widgets[i].widget_type != MYX_CHECKBOX)
    {
      MGImageCheckButton *check;

      // create the button to tell whether the variable goes to the 
      // config file or not
      check= Gtk::manage(new MGImageCheckButton(_edit_on, _edit_off));
      _tips.set_tip(*check, _("Click to add or remove this option from the configuration file. If this option is not set, the default or a value in a more general section in the configuration file will be used."));
      
      check->set_active(group->widgets[i].active!=0);
      
      check->signal_toggled().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MDynamicInterface::option_enable_toggled),(char*)group->widgets[i].id));
      
      table->attach(*check, 0, 1, y, y+1, Gtk::FILL, Gtk::FILL);

      _widgets[(char*)group->widgets[i].id].enable= check;
    }

    if (group->widgets[i].widget_type == MYX_TEXTEDIT)
    {
      Gtk::Alignment *al= Gtk::manage(new Gtk::Alignment(0.0, 0.0, 1.0, 0.0));
      al->add(*w);

      l= Gtk::manage(new Gtk::Label((char*)group->widgets[i].caption?:""));
      table->attach(*l, 1, 2, y, y+1, Gtk::FILL, Gtk::FILL);
      l->set_alignment(0.0, 0.0);
      l->set_padding(0, 3);

      table->attach(*al, 2, 3, y, y+1, Gtk::FILL|Gtk::EXPAND, Gtk::FILL);
      
      _widgets[(char*)group->widgets[i].id].label= l;
    }
    else if (group->widgets[i].widget_type == MYX_CHECKBOX)
    {
      Gtk::Alignment *al= Gtk::manage(new Gtk::Alignment(0.0, 0.0, 0.0, 0.0));
      al->add(*w);

      table->attach(*al, 1, 3, y, y+1, Gtk::FILL, Gtk::FILL);
    }
    else
    {
      Gtk::Alignment *al= Gtk::manage(new Gtk::Alignment(0.0, 0.0, 0.0, 0.0));
      al->add(*w);

      l= Gtk::manage(new Gtk::Label((char*)group->widgets[i].caption?:""));
      l->set_padding(0, 3);
      table->attach(*l, 1, 2, y, y+1, Gtk::FILL, Gtk::FILL);
      l->set_alignment(0.0, 0.0);
      
      table->attach(*al, 2, 3, y, y+1, Gtk::FILL, Gtk::FILL);
      
      _widgets[(char*)group->widgets[i].id].label= l;
    }

    if (group->widgets[i].description)
    {
      l= Gtk::manage(new Gtk::Label((char*)group->widgets[i].description));
      l->set_line_wrap(true);
      l->set_alignment(0.0, 0.0);
      l->set_padding(0, 3);
      // XXX kluge to workaround gtk label wrapping thing
      if (group->position==4)
        l->set_size_request(210, -1);
      table->attach(*l, 3, 4, y, y+1, Gtk::FILL|Gtk::EXPAND, Gtk::FILL);

      _widgets[(char*)group->widgets[i].id].dlabel= l;
    }

    if (group->widgets[i].widget_type != MYX_CHECKBOX)
      option_enable_toggled((char*)group->widgets[i].id);
  }
  
  table->show_all();

  return frame;
}

#if 0
void MDynamicInterface::add_enabled_by_handler(MYX_GUI_ENABLED_BY_CONTROL *info,
                                               Gtk::Widget *target)
{
  WObject obj= _widgets[(char*)info->id];
  WSensControl *data= new WSensControl;

  data->type= obj.type;
  data->source= obj.w;
  data->target= target;
  data->value= (char*)info->value;

  _sens_controls.push_back(data);
      
  switch (obj.type)
  {
  case MYX_CHECKBOX:
    ((Gtk::CheckButton*)obj.w)->signal_toggled().connect(sigc::bind<WSensControl*>
                                        (sigc::mem_fun(*this,&MDynamicInterface::update_sensitivity),
                                         data));
    break;
  default:
    g_assert_not_reached();
    break;
  }
  
  update_sensitivity(data);
}
#endif

bool MDynamicInterface::process_page(MYX_GUI_PAGE *page)
{
  Gtk::VBox *vbox= Gtk::manage(new Gtk::VBox(false, 6));
  vbox->set_border_width(12);

  {
    Gtk::HBox *hbox= Gtk::manage(new Gtk::HBox(false, 6));
    Gtk::Label *label= Gtk::manage(new Gtk::Label);
    Gtk::Image *image= Gtk::manage(new Gtk::Image(_page_icon));
    
    label->set_markup(ufmt("<b>%s</b>\n%s", page->caption, 
                           (char*)page->description));
    
    hbox->pack_start(*image, false, true);
    image->show();
    label->set_alignment(0.0, 0.5);
    hbox->pack_start(*label, true, true);
    label->show();
    vbox->pack_start(*hbox, false, true);
    hbox->show();
  }

  {
    Gtk::HSeparator *sep= Gtk::manage(new Gtk::HSeparator);
    vbox->pack_start(*sep, false, true);
    sep->show();
  }
  
  Gtk::ScrolledWindow *swin= Gtk::manage(new Gtk::ScrolledWindow());
  vbox->pack_start(*swin, true, true);
  swin->set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  swin->show();
  
  Gtk::VBox *box= Gtk::manage(new Gtk::VBox(false, 6));
  box->set_border_width(12);
  swin->add(*box);
  box->show();

  std::vector<Gtk::Widget*> groups;

  // process groups
  for (unsigned int i= 0; i < page->groups_num; ++i)
  {
    Gtk::Widget *w= process_group(page->groups+i);
    box->pack_start(*w, false, true);
    w->show();
    groups.push_back(w);
  }
#if 0
  // setup signals for sensitiveness control
  for (unsigned int m= 0; m < page->enabled_by_num; m++)
  {
    add_enabled_by_handler(page->enabled_by+m, vbox);
  }

  for (unsigned int i= 0; i < page->groups_num; ++i)
  {
    MYX_GUI_GROUP *group= page->groups+i;

    for (unsigned int g= 0; g < group->enabled_by_num; g++)
    {
      add_enabled_by_handler(group->enabled_by+g, groups[i]);
    }

    for (unsigned int g= 0; g < group->widgets_num; g++)
    {
      for (unsigned int m= 0; m < group->widgets[g].enabled_by_num; m++)
      {
        add_enabled_by_handler(group->widgets[g].enabled_by+m,
                               _widgets[(char*)group->widgets[g].id].w);
        
        if (_widgets[(char*)group->widgets[g].id].label)
          add_enabled_by_handler(group->widgets[g].enabled_by+m,
                                 _widgets[(char*)group->widgets[g].id].label);

        if (_widgets[(char*)group->widgets[g].id].dlabel)
          add_enabled_by_handler(group->widgets[g].enabled_by+m,
                                 _widgets[(char*)group->widgets[g].id].dlabel);
      }
    }
  }
#endif
  vbox->show();

  _top_notebook->append_page(*vbox, (char*)page->caption?:"", false);

  return true;
}



bool MDynamicInterface::fetch_page_values(MYX_GUI_PAGE *page)
{
  for (unsigned int i= 0; i < page->groups_num; i++)
  {
    MYX_GUI_GROUP *group= page->groups + i;

    for (unsigned int j= 0; j < group->widgets_num; j++)
    {
      MYX_GUI_WIDGET *widget= group->widgets + j;
      Glib::ustring value= get_option_value((char*)widget->id);

      switch (widget->widget_type)
      {
      case MYX_CHECKBOX:
        widget->active= 1;//XXX???
        break;
      default:
        if (_widgets[(char*)widget->id].enable->get_active())
          widget->active= 1;
        else
          widget->active= 0;
        break;
      }
      
      widget->value= (unsigned char*)g_strdup(value.c_str());
    }
  }

  return true;
}


bool MDynamicInterface::fetch_values()
{
  bool ok= false;
  
  for (unsigned int i= 0; i < _descr->pages_num; ++i)
  {
    if (!fetch_page_values(_descr->pages+i))
    {
      g_error(_("error processing data from page '%s' of interface description file"),
              _descr->pages[i].caption);
      ok= false;
    }
  }
  
  return ok;
}

#if 0
void MDynamicInterface::update_sensitivity(WSensControl *data)
{
  bool ok= false;
  
  switch ((MYX_GUI_WIDGET_TYPE)data->type)
  {
  case MYX_CHECKBOX:
    if ((((Gtk::CheckButton*)data->source)->get_active() && data->value!="Unchecked")
        || (!((Gtk::CheckButton*)data->source)->get_active() && data->value=="Unchecked"))
      ok= true;
    break;
  default:
    g_assert_not_reached();
    break;
  }

  data->target->set_sensitive(ok);
}
#endif

Glib::ustring MDynamicInterface::get_text_value(const std::string &id)
{
  WObject obj= _widgets[id];
  
  if (strcmp((char*)obj.ptr->textedit->edit_type,"innodbfilepath")==0)
    return ((MInnoDBFilePathEditor*)obj.w)->get_text();
  else if (strcmp((char*)obj.ptr->textedit->edit_type,"file")==0
           || strcmp((char*)obj.ptr->textedit->edit_type,"directory")==0)
    return ((MFilePathEditor*)obj.w)->get_text();
  else
    return ((Gtk::Entry*)obj.w)->get_text();
}

Glib::ustring MDynamicInterface::get_spin_value(const std::string &id)
{
  WObject obj= _widgets[id];
  Glib::ustring value;

  value= ((Gtk::SpinButton*)obj.w)->get_text();

  if (obj.aux)
  {
    Glib::ustring str;
    int sel= ((Gtk::OptionMenu*)obj.aux)->get_history();

    str= (char*)((Gtk::OptionMenu*)obj.aux)->get_menu()->items()[sel].get_data("value");

    if ( !strcmp(str.c_str(), "k"))
      value += "k";
    else if ( !strcmp(str.c_str(), "M") )
      value += "M";
    else if ( !strcmp(str.c_str(), "G") )
      value += "G";
  }
  return value;
}

Glib::ustring MDynamicInterface::get_check_value(const std::string &id)
{
  bool flag= ((Gtk::CheckButton*)_widgets[id].w)->get_active();
  if (_widgets[id].ptr->checkbox->invert)
    flag=!flag;
  return flag ? "Checked":"Unchecked";
}

Glib::ustring MDynamicInterface::get_dropdown_value(const std::string &id)
{
  WObject obj= _widgets[id];
  int i= ((Gtk::OptionMenu*)obj.w)->get_history();

  return Glib::ustring(strchr((char*)obj.ptr->dropdownbox->items[i],'=')+1);
}

Glib::ustring MDynamicInterface::get_option_value(const std::string &id)
{
  switch (_widgets[id].type)
  {
  case MYX_CHECKBOX: return get_check_value(id);
  case MYX_SPINEDIT: return get_spin_value(id);
  case MYX_TEXTEDIT: return get_text_value(id);
  case MYX_DROPDOWNBOX: return get_dropdown_value(id);    
  default:
    g_warning("request for unknown option '%s' in MYX_GUI", id.c_str());
    return "";
  }
}



void MDynamicInterface::set_text_value(const std::string &id, 
                                       const std::string &value)
{
  WObject obj= _widgets[id];
  
  if (strcmp((char*)obj.ptr->textedit->edit_type,"innodbfilepath")==0)
    return ((MInnoDBFilePathEditor*)obj.w)->set_text(value);
  else if (strcmp((char*)obj.ptr->textedit->edit_type,"file")==0
           || strcmp((char*)obj.ptr->textedit->edit_type,"directory")==0)
    return ((MFilePathEditor*)obj.w)->set_text(value);
  else
    return ((Gtk::Entry*)obj.w)->set_text(value);
}


void MDynamicInterface::set_spin_value(const std::string &id,
                                       const std::string &value)
{
  WObject obj= _widgets[id];

  if (!isdigit(value[value.size()-1]))
  {
    switch (value[value.size()-1])
    {
    case 'k': ((Gtk::OptionMenu*)obj.aux)->set_history(1); break;
    case 'M': ((Gtk::OptionMenu*)obj.aux)->set_history(2); break;
    case 'G': ((Gtk::OptionMenu*)obj.aux)->set_history(3); break;
    }
    ((Gtk::SpinButton*)obj.w)->set_text(value.substr(0, value.size()-1));
  }
  else
  {
    if (obj.aux)
      ((Gtk::OptionMenu*)obj.aux)->set_history(0);
    ((Gtk::SpinButton*)obj.w)->set_text(value);
  }

}

void MDynamicInterface::set_check_value(const std::string &id,
                                        const std::string &value)
{
  bool flag= strcasecmp(value.c_str(), "Checked")==0;
  if (_widgets[id].ptr->checkbox->invert)
    flag=!flag;
  return ((Gtk::CheckButton*)_widgets[id].w)->set_active(flag);
}


void MDynamicInterface::set_dropdown_value(const std::string &id,
                                           const std::string &value)
{
  WObject obj= _widgets[id];
  
  for (unsigned int i= 0; i < obj.ptr->dropdownbox->items_num; i++)
  {
    if (value.compare(strchr((char*)obj.ptr->dropdownbox->items[i],'=')+1)==0)
    {
      ((Gtk::OptionMenu*)obj.w)->set_history(i);
      break;
    }
  }
}


void MDynamicInterface::set_option_value(const std::string &id, const std::string &value)
{
  WObject obj= _widgets[id];

  switch (_widgets[id].type)
  {
  case MYX_CHECKBOX: set_check_value(id, value); break;
  case MYX_SPINEDIT: set_spin_value(id, value); break;
  case MYX_TEXTEDIT: set_text_value(id, value); break;
  case MYX_DROPDOWNBOX: set_dropdown_value(id, value); break;
  }
}


void MDynamicInterface::revert_values()
{
  if (!_descr) return;
  for (unsigned int i= 0; i < _descr->pages_num; i++)
  {    
    revert_page_values(_descr->pages+i);
  }
}


void MDynamicInterface::revert_page_values(MYX_GUI_PAGE *page)
{
  for (unsigned int i= 0; i < page->groups_num; i++)
  {
    MYX_GUI_GROUP *group= page->groups + i;

    for (unsigned int j= 0; j < group->widgets_num; j++)
    {
      MYX_GUI_WIDGET *widget= group->widgets + j;
      std::string value;

      if (_widgets[(char*)widget->id].enable)
        _widgets[(char*)widget->id].enable->set_active(widget->active);
      
      if (widget->value)
        value= (char*)widget->value;
      else if (widget->default_value)
        value= (char*)widget->default_value;
      else
        value= "";
      set_option_value((char*)widget->id, value);      
    }
  }
}


void MDynamicInterface::option_enable_toggled(const char *id)
{
  WObject &obj= _widgets[id];
  bool flag= obj.enable->get_active();
  
  
  if (obj.w)
    obj.w->set_sensitive(flag);

  if (obj.aux)
    obj.aux->set_sensitive(flag);

  if (obj.label)
    obj.label->set_sensitive(flag);

  if (obj.dlabel)
    obj.dlabel->set_sensitive(flag);
}


//**********************************************************************

void MInnoDBFilePathEditor::update_dlg_sensitivity()
{
  bool ok= true;
  
  if (((Gtk::Entry*)_dlg_xml->get_widget("name_entry"))->get_text().empty())
    ok= false;

  _dlg_xml->get_widget("ok_button")->set_sensitive(ok);
}


int MInnoDBFilePathEditor::fscompare(const Gtk::TreeIter &a, const Gtk::TreeIter &b)
{
  Gtk::TreeModel::Row row;
  Glib::ustring sa, sb;
  row= *a; sa= row[_fs_columns._path];
  row= *b; sb= row[_fs_columns._path];
  return sa.compare(sb);
}


void MInnoDBFilePathEditor::update_fs_list()
{
  std::list<MInstanceInfo::FilesystemInfo> fsinfo;

  _fs_list->clear();
  if (_instance->get_fs_info(fsinfo))
  {  
    _fs_list->set_sort_func(0, sigc::mem_fun(*this, &MInnoDBFilePathEditor::fscompare));
    _fs_list->set_sort_column_id(0, Gtk::SORT_DESCENDING);

    for (std::list<MInstanceInfo::FilesystemInfo>::iterator it= fsinfo.begin();
         it != fsinfo.end(); ++it)
    {
      Gtk::TreeIter iter= _fs_list->append();
      Gtk::TreeModel::Row row= *iter;
      
      row[_fs_columns._fs]= it->fs;
      row[_fs_columns._path]= it->path;
      row[_fs_columns._type]= it->type;
      row[_fs_columns._size]= format_value(it->size);
      row[_fs_columns._space]= format_value(it->free);
      row[_fs_columns._info]= *it;
    }
  }
}


void MInnoDBFilePathEditor::draw_fs_graph(long long total, 
                                          long long used,
                                          long long used_data,
                                          long long new_data)
{
  Gtk::DrawingArea *da= (Gtk::DrawingArea*)_dlg_xml->get_widget("graph");
  Glib::RefPtr<Gdk::Window> win= da->get_window();
  double pct_used, pct_inno, pct_inno_new;
  Glib::RefPtr<Gtk::Style> style= da->get_style();
  int w, h;
  
  if (!_gc)
  {
    _gc= Gdk::GC::create(get_window());
    _gc->set_foreground(style->get_black());
  }

  w= da->get_width();
  h= da->get_height();
  
  pct_used= (double)used/total;
  pct_inno= (double)used_data/total;
  pct_inno_new= (double)new_data/total;


  win->clear();

    
  // paint caption
  win->draw_rectangle(style->get_dark_gc(Gtk::STATE_NORMAL), true,
                      w-130, 2,
                      130, 65);
  win->draw_rectangle(style->get_white_gc(), true,
                      w-132, 0,
                      130, 65);
  win->draw_rectangle(style->get_black_gc(), false,
                      w-132, 0,
                      130, 65);

  {
    Glib::RefPtr<Pango::Layout> l;
    
    l= create_pango_layout(_("Free"));
    win->draw_layout(style->get_black_gc(), w-105, 5, l);

    l= create_pango_layout(_("Used"));
    win->draw_layout(style->get_black_gc(), w-105, 25, l);

    l= create_pango_layout(_("Tablespace"));
    win->draw_layout(style->get_black_gc(), w-105, 45, l);
  }
  
  int d0, d1;

  // paint caption lines and graphs
  _gc->set_foreground(_free_color);
  win->draw_rectangle(_gc, true, w-125, 5, 15, 15);

  win->draw_arc(_gc, true, 0, 0, 150, 150, 0, 64*360);


  d0= 0;
  d1= (int)(64*360*pct_used);
  
  _gc->set_foreground(_used_color);
  win->draw_rectangle(_gc, true, w-125, 25, 15, 15);

  win->draw_arc(_gc, true, 0, 0, 150, 150, d0, d1);

  d0= d0+d1;
  d1= (int)(64*360*pct_inno);

  _gc->set_foreground(_data_color);
  win->draw_rectangle(_gc, true, w-125, 45,15, 15);

  win->draw_arc(_gc, true, 0, 0, 150, 150, d0, d1);


  d0= d0+d1;
  d1= (int)(64*360*pct_inno_new);

  _gc->set_foreground(_data_new_color);
  win->draw_arc(_gc, true, 0, 0, 150, 150, d0, d1);

  win->draw_arc(style->get_black_gc(), false, 0, 0, 150, 150, 0, 64*360);
}


long long MInnoDBFilePathEditor::parse_size(const Glib::ustring &size)
{
  long long value= 1;

  sscanf(size.substr(0, size.size()-1).c_str(), "%lli", &value);

  switch (size.uppercase()[size.size()-1])
  {
  case 'G': value*= 1024;
  case 'M': value*= 1024;
  case 'K': value*= 1024;
  }
  
  return value;
}
    


long long MInnoDBFilePathEditor::get_total_table_space(const std::string &fspath)
{
  long long total= 0;
  Gtk::TreeIter it;
  std::string data_path= get_data_directory();
  
  
  // if no data directory defined, then each file has its own path
  if (data_path.empty())
  {
    // calculate total size occupied by all files in fspath
    // go through all files defined and compute each file that is in fspath 
    // filesystem
    for (it= _list->children().begin(); it != _list->children().end(); ++it)
    {
      Gtk::TreeModel::Row row= *it;
      Glib::ustring file= row[_columns._file];

      if (check_if_path_in_fs(fspath, file))
      {
        total += parse_size(row[_columns._size]);
      }
    }
  }
  else
  {    
    // if data directory defined, check if its in fspath
    if (check_if_path_in_fs(fspath, data_path))
    {
      // if it is in fspath, calculate total size of files
      for (it= _list->children().begin(); it != _list->children().end(); ++it)
      {
        Gtk::TreeModel::Row row= *it;
        
        total += parse_size(row[_columns._size]);
      }
    }
    // otherwise do nothing
  }

  return total;
}


bool MInnoDBFilePathEditor::check_if_path_in_fs(const std::string &fspath, const std::string &file)
{
  Gtk::TreeIter it;
  bool result= false;

  for (it= _fs_list->children().begin(); it != _fs_list->children().end(); ++it)
  {
    Gtk::TreeModel::Row row= *it;
    Glib::ustring fs= row[_fs_columns._path];

    if (Glib::str_has_prefix(file, fs))
    {
      if (Glib::str_has_prefix(fspath, fs))
        result= true;
      break;
    }
    else 
    {
      if (fspath == fs)
      {
        return false;
      }
    }
  }
  return result;
}


void MInnoDBFilePathEditor::selected_fs()
{
  Gtk::TreeIter it= ((Gtk::TreeView*)_dlg_xml->get_widget("fs_tree"))->get_selection()->get_selected();

  if (it)
  {
    Gtk::TreeModel::Row row= *it;
    MInstanceInfo::FilesystemInfo info= row[_fs_columns._info];
    long long size= info.size;
    long long avail= info.free;
    long long used= 0;
    std::string file= ((Gtk::Entry*)_dlg_xml->get_widget("name_entry"))->get_text();
    Glib::ustring tmp= row[_fs_columns._path];
    std::string path= tmp;
    std::string datadir= get_data_directory();
    std::string p;

    if (datadir.empty())
    {
      if (Glib::str_has_prefix(file,"/"))
        p= file.rfind('/')==file.find('/') ? "/" : file.substr(0, file.rfind('/')); // extract dir only
      else
        p= DEFAULT_INNODB_DATADIR;
    }
    else
    {
      p= datadir;
    }

    if (check_if_path_in_fs(path, p))
    {
      double tmp;
      tmp= ((Gtk::SpinButton*)_dlg_xml->get_widget("size_spin"))->get_value();

      switch (((Gtk::OptionMenu*)_dlg_xml->get_widget("unit_menu"))->get_history())
      {
      case 2: tmp *= 1024;
      case 1: tmp *= 1024;
      case 0: tmp *= 1024;
      }
      used= (long long)tmp;
    }
    draw_fs_graph(size, (size-avail), get_total_table_space(path), used);
  }
}


bool MInnoDBFilePathEditor::expose_graph(GdkEventExpose *ev)
{
  selected_fs();
  
  return true;
}

//---------------------------------------------------------------------

MFilePathEditor::MFilePathEditor(MDynamicInterface *owner, bool dir_only)
  : _button(_("Change...")), _dir_only(dir_only)
{
  pack_start(_entry);
  pack_start(_button, false, false);
  
  _button.signal_clicked().connect(sigc::mem_fun(*this,&MFilePathEditor::clicked));
  show_all();
}


Glib::ustring MFilePathEditor::get_text()
{
  return _entry.get_text();
}


void MFilePathEditor::set_text(const Glib::ustring &value)
{
  _entry.set_text(value);
}


void MFilePathEditor::clicked()
{
  Gtk::FileSelection fsel(_dir_only?_("Select Directory"):_("Select File"));
  
  fsel.set_filename(_entry.get_text());
  
  fsel.get_file_list()->set_sensitive(!_dir_only);
  
  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    _entry.set_text(fsel.get_filename());
  }
}


//---------------------------------------------------------------------

void MInnoDBFilePathEditor::add_file()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_dlg_xml->get_widget("innodb_dialog");
  bool cancelled= false;

  dlg->show();

  update_fs_list();
  update_dlg_sensitivity();

  ((Gtk::Entry*)_dlg_xml->get_widget("name_entry"))->set_text("ibdata");
  ((Gtk::SpinButton*)_dlg_xml->get_widget("size_spin"))->set_value(1);

  if (dlg->run() != Gtk::RESPONSE_OK)
    cancelled= true;

  dlg->hide();

  if (!cancelled)
  {
    Glib::ustring name= ((Gtk::Entry*)_dlg_xml->get_widget("name_entry"))->get_text();
    Glib::ustring size= ((Gtk::SpinButton*)_dlg_xml->get_widget("size_spin"))->get_text();

    switch (((Gtk::OptionMenu*)_dlg_xml->get_widget("unit_menu"))->get_history())
    {
    case 0: size += "k"; break;
    case 1: size += "M"; break;
    case 2: size += "G"; break;
    }

    Gtk::TreeIter iter= _list->append();
    Gtk::TreeModel::Row row= *iter;

    row[_columns._file]= name;
    row[_columns._size]= size;
  }
}


void MInnoDBFilePathEditor::update_sensitivity()
{
  if (_tree.get_selection()->get_selected())
  {
    _remove_button.set_sensitive(true);
  }
  else
  {
    _remove_button.set_sensitive(false);
  }
}

void MInnoDBFilePathEditor::remove_file()
{
  Gtk::TreeIter iter= _tree.get_selection()->get_selected();

  if (iter)
    _list->erase(iter);
  
  update_sensitivity();
}

    
MInnoDBFilePathEditor::MInnoDBFilePathEditor(MDynamicInterface *owner, MInstanceInfo *instance)
    : Gtk::Table(2, 3), _owner(owner), _instance(instance), _add_button(Gtk::Stock::ADD), _remove_button(Gtk::Stock::REMOVE)
{
  _list= Gtk::ListStore::create(_columns);
  
  _tree.append_column(_("File name"), _columns._file);
  _tree.append_column(_("Size"), _columns._size);
//XXX  _tree.get_column(1)->set_alignment(1.0);
  _tree.set_model(_list);
  _swin.add(_tree);
  _swin.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _tree.show();

  set_row_spacings(6);
  set_col_spacings(6);
  
  attach(_swin, 0, 1, 0, 2);
  _swin.show();
  _swin.set_shadow_type(Gtk::SHADOW_IN);

  attach(_add_button, 1, 2, 0, 1, Gtk::FILL, Gtk::SHRINK);
  _add_button.show();
  _add_button.signal_clicked().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::add_file));

  attach(_remove_button, 1, 2, 1, 2, Gtk::FILL, Gtk::SHRINK);
  _remove_button.show();
  _remove_button.signal_clicked().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::remove_file));

  _tree.get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::update_sensitivity));

  _tree.set_size_request(180, 100);

  _extend_check.set_label(_("Extend size of last file automatically"));
  attach(_extend_check, 0, 2, 2, 3);
  _extend_check.show();

  update_sensitivity();

  _dlg_xml= new MGGladeXML(get_glade_file("innodb_data_file.glade"));
  {
    Gtk::TreeView *tree= (Gtk::TreeView*)_dlg_xml->get_widget("fs_tree");

    tree->get_selection()->set_mode(Gtk::SELECTION_BROWSE);
    tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::selected_fs));

    tree->append_column(_("Path"), _fs_columns._path);
    tree->append_column(_("Type"), _fs_columns._type);
    tree->append_column(_("Size"), _fs_columns._size);
    tree->append_column(_("Free"), _fs_columns._space);
    tree->append_column(_("Filesystem"), _fs_columns._fs);

    _fs_list= Gtk::ListStore::create(_fs_columns);
    tree->set_model(_fs_list);

    _dlg_xml->get_widget("graph")->signal_expose_event().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::expose_graph));;

    ((Gtk::Entry*)_dlg_xml->get_widget("name_entry"))->signal_changed().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::update_dlg_sensitivity));
    ((Gtk::SpinButton*)_dlg_xml->get_widget("size_spin"))->signal_changed().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::selected_fs));
    ((Gtk::OptionMenu*)_dlg_xml->get_widget("unit_menu"))->signal_changed().connect(sigc::mem_fun(*this,&MInnoDBFilePathEditor::selected_fs));
  }

  {
    Glib::RefPtr<Gdk::Colormap> cmap= get_default_colormap();

    _free_color= Gdk::Color("#0044ff");
    cmap->alloc_color(_free_color);

    _used_color= Gdk::Color("#d00050");
    cmap->alloc_color(_used_color);

    _data_color= Gdk::Color("#0da000");
    cmap->alloc_color(_data_color);

    _data_new_color= Gdk::Color("#7ce952");
    cmap->alloc_color(_data_new_color);
  }
}


MInnoDBFilePathEditor::~MInnoDBFilePathEditor()
{
  delete _dlg_xml;
}


void MInnoDBFilePathEditor::add_value(const Glib::ustring &file,
                                      const Glib::ustring &size)
{
  Gtk::TreeIter iter= _list->append();
  Gtk::TreeModel::Row row= *iter;

  row[_columns._file]= file;
  row[_columns._size]= size;

  update_sensitivity();
}


void MInnoDBFilePathEditor::set_auto_extends(bool flag)
{
  _extend_check.set_active(flag);
}


std::string MInnoDBFilePathEditor::get_data_directory()
{
  return _owner->get_text_value("innodb_data_home_dir");
}


Glib::ustring MInnoDBFilePathEditor::get_text()
{
  Gtk::TreeIter iter;
  Glib::ustring value;

  for (iter= _list->children().begin(); 
       iter != _list->children().end(); ++iter)
  {
    Gtk::TreeModel::Row row= *iter;

    if (!value.empty())
      value += ';';
    
    value+= row[_columns._file] + ':' + row[_columns._size];
  }
  
  if (_extend_check.get_active())
    value+= ":autoextend";

  return value;
}



void MInnoDBFilePathEditor::set_text(const Glib::ustring &value)
{
  Glib::ustring cur= value, next;

  set_auto_extends(false);
  _list->clear();
  
  while (!cur.empty())
  {
    Glib::ustring::size_type p= cur.find(';');

    if (p == Glib::ustring::npos)
      next= "";
    else
    {
      next= cur.substr(p+1);
      cur= cur.substr(0, p);
    }

    p= cur.find(':');
    Glib::ustring name, size;
    name= cur.substr(0, p);
    size= cur.substr(p+1);
    
    p= size.find(':');
    if (p != Glib::ustring::npos)
    {
      if (size.substr(p+1) == "autoextend")
        set_auto_extends(true);
      else
        g_warning("unknown innodb data file value '%s'",
                  size.substr(p+1).c_str());
      size= size.substr(0, p);
    }

    add_value(name, size);

    cur= next;
  }
}
