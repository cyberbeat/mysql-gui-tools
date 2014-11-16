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

#include "MGRTShell.h"
#include "myg_gtkutils.h"
#include "mygpriv.h"
#include <gtkmm/fileselection.h>
#include "myx_grt_private.h"
/**
 * @file  MGRTShell.cc
 * @brief 
 */


MGRTShell::MGRTShell(MGRT *grt)
  : _grt(grt)
{
  Gtk::TreeViewColumn *column;
 
  set_name("grt_shell");
  set_title("GRT Shell");
  set_size_request(800, 600);

  setup_menu();
  
  add(_top_box);

  _top_box.pack_start(_menu, false, true);

  _top_box.pack_start(_paned, true, true);

  _text.set_name("shell_text");
  _text_scroll.add(_text);
  _text_scroll.set_policy(Gtk::POLICY_NEVER, Gtk::POLICY_AUTOMATIC);
  _text_scroll.set_shadow_type(Gtk::SHADOW_IN);
  _paned.add1(_text_scroll);
  _paned.add2(_sidenote);

  _paned.set_position(600);
  
  _value_box.set_spacing(8);

  _value_box.pack_start(_value_combo, false, true);

  _value_tree.set_name("value_tree");
  _value_store= Gtk::TreeStore::create(_columns);
  _value_tree.set_model(_value_store);
  _value_tree.get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGRTShell::value_selected));
  _value_tree.signal_row_activated().connect(sigc::mem_fun(*this,&MGRTShell::value_activated));

  column= new Gtk::TreeViewColumn(_("Object Tree"));
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  column->set_resizable(true);
  _value_tree.append_column(*Gtk::manage(column));
//  _value_tree.append_column_numeric("Ref#", _columns.detail, "%i");
//  _value_tree.get_column(1)->set_max_width(30);
  _value_scroll.add(_value_tree);
  _value_scroll.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _value_scroll.set_shadow_type(Gtk::SHADOW_IN);
  _value_box.pack_start(_value_scroll, true, true);
  
  _value_entry.set_editable(false);
  _value_box.pack_start(_value_entry, false, true);
  
  _detail_tree.set_name("detail_tree");
  _detail_scroll.add(_detail_tree);
  _detail_scroll.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _detail_tree.set_headers_visible(false);
  
  _detail_scroll.set_shadow_type(Gtk::SHADOW_IN);
  _value_paned.pack1(_value_box);
  _value_paned.pack2(_detail_scroll);
  _sidenote.append_page(_value_paned, _("Values"));
  
  _value_paned.set_position(350);

  _detail_store= Gtk::ListStore::create(_dcolumns);
  _detail_tree.set_model(_detail_store);

  _struct_store= Gtk::TreeStore::create(_columns);
  _struct_tree.set_model(_struct_store);

  // -----------------
  
  _struct_tree.set_name("struct_tree");
  column= new Gtk::TreeViewColumn("");
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  _struct_tree.append_column(*Gtk::manage(column));
  _struct_tree.set_headers_visible(false);

  _struct_scroll.set_shadow_type(Gtk::SHADOW_IN);

  _struct_scroll.add(_struct_tree);
  _struct_scroll.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  _sidenote.append_page(_struct_scroll, _("Structs"));

  // -----------------

  _module_label.set_name("module_label");
  _module_label.set_padding(8, 8);
  _module_label.set_alignment(0.0, 0.0);
  
  _module_store= Gtk::TreeStore::create(_columns);
  _module_tree.set_model(_module_store);

  _module_tree.set_name("module_tree");
  _module_tree.get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGRTShell::module_selected));

  _module_scroll.add(_module_tree);
  _module_scroll.set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  
  _module_box.pack_start(_module_scroll, true, true);
  _module_box.pack_start(_module_label, false, true);
  _sidenote.append_page(_module_box, _("Modules"));

  column= new Gtk::TreeViewColumn("");
  column->pack_start(_columns.icon, false);
  column->pack_start(_columns.text);
  _module_tree.append_column(*Gtk::manage(column));
  _module_scroll.set_shadow_type(Gtk::SHADOW_IN);
  _module_tree.set_headers_visible(false);
  
  _simple_icon= PIXCACHE->load("grt_value_simple.png");
  _dict_icon= PIXCACHE->load("grt_value_dict.png");
  _list_icon= PIXCACHE->load("grt_value_list.png");
  _struct_icon= PIXCACHE->load("grt_value_struct.png");
  _folder_icon= PIXCACHE->load("folder_16x16.png");
  _module_icon= PIXCACHE->load("grt_module.png");
  _function_icon= PIXCACHE->load("grt_function.png");

  
  _grt->set_console(&_text);
  _text.set_shell(this);
  
  show_all();
  hide();

  refresh();
}


void MGRTShell::save_tree()
{
  Gtk::FileSelection fsel(_("Save GRT Tree"));
  
  if (fsel.run() == Gtk::RESPONSE_OK)
  {
    MGRTValue root(_grt->global_value(_root_path.empty() ? "/" : _root_path));
    
    myx_grt_store_to_file(_grt->grt(),
                          root.grtValue(),
                          fsel.get_filename().c_str());
  }
}


void MGRTShell::toggle_view_option()
{
  
}


void MGRTShell::setup_menu()
{
  Gtk::Menu *menu;
  Gtk::MenuItem *item;

  menu= Gtk::manage(new Gtk::Menu());

  myg_menu_add(*menu, _("_Refresh"), sigc::mem_fun(*this,&MGRTShell::refresh), "ref");
  myg_menu_add(*menu);
  myg_menu_add(*menu, _("_Save Tree"), sigc::mem_fun(*this,&MGRTShell::save_tree), "sav");
  myg_menu_add(*menu);
  myg_menu_add(*menu, _("_Close"), sigc::mem_fun(*this,&Gtk::Widget::hide), "cl");

  item= Gtk::manage(new Gtk::MenuItem("_Shell", true));
  _menu.append(*item);
  item->set_submenu(*menu);

  
  menu= Gtk::manage(new Gtk::Menu());

  item= _container_only_item= Gtk::manage(new Gtk::CheckMenuItem(_("_Structure Only"), true));
  menu->append(*item);
  item->signal_activate().connect(sigc::mem_fun(*this,&MGRTShell::toggle_view_option));
  
  _container_only_item->set_active(true);

  item= Gtk::manage(new Gtk::MenuItem("_View", true));
  _menu.append(*item);
  item->set_submenu(*menu);

}


void MGRTShell::set_icon(Gtk::TreeRow row, MGRTValue value)
{
  std::string path;

  switch (value.type())
  {
  case MYX_DICT_VALUE:
    {
      MYX_GRT_STRUCT *vstr= myx_grt_dict_struct_get(_grt->grt(), value.grtValue());

      if (vstr)
        path= myx_grt_struct_get_icon_path(_grt->grt(), vstr, MYX_IT_SMALL);
      else
        path= "";
      if (!path.empty())
      {
        Glib::RefPtr<Gdk::Pixbuf> icon;
        try {
          icon= PIXCACHE->load(path);
        } catch (Glib::Error &exc) {
          icon.clear();
        };
        if (!icon)
        {
          try {
            icon= PIXCACHE->load("GrtObject.16x16.png");
          } catch (Glib::Error &exc) {
            icon.clear();
          }
        }
        if (!icon)
          icon= _dict_icon;

        row[_columns.icon]= icon;
      }
      else
        row[_columns.icon]= _dict_icon;
      break;
    }

  case MYX_LIST_VALUE:
    row[_columns.icon]= _list_icon;
    break;

  default:
    row[_columns.icon]= _simple_icon;
    break;
  }
}


Glib::ustring MGRTShell::get_caption(MGRTValue value)
{
  const char *sname;
  
  switch (value.type())
  {
  case MYX_STRING_VALUE:
    return value.asString();
    break;
    
  case MYX_LIST_VALUE:
    sname= value.listContentStruct();
    if (sname)
      return ufmt("list [dict: %s]", sname);
    else
      return ufmt("list [%s]", myx_get_value_type_as_string(value.listContentType()));
    break;
    
  case MYX_DICT_VALUE:
    sname= value.contentStruct();
    if (sname)
      return ufmt("dict: %s", sname);
    else
      return ufmt("dict");
    break;
    
  case MYX_INT_VALUE:
    return "int";
    
  case MYX_REAL_VALUE:
    return "double";

  default:
  case MYX_ANY_VALUE:
    return "???";
  }
}


void MGRTShell::add_list_to_store(MGRTValue list, Gtk::TreeRow &parent, Glib::RefPtr<Gtk::TreeStore> store)
{
  unsigned int c= list.count();
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  for (unsigned int i= 0; i < c; i++)
  {
    MGRTValue value(list[i]);
    
    iter= store->append(parent.children());
    row= *iter;

    row[_columns.path]= tostr(i);
    
    switch (value.type())
    {
    case MYX_STRING_VALUE:
      if (list.listContentStruct())
      {
        MGRTValue rvalue= MGRTValue::refObject(_grt->grt(), value.asString());

        set_icon(row, rvalue);
        row[_columns.text]= ufmt("%s    %s", rvalue["name"].asString(), get_caption(rvalue).c_str());
        row[_columns.data]= rvalue;
        add_dict_to_store(rvalue, row, store);
      }
      else
      {
        set_icon(row, value);
        row[_columns.text]= get_caption(value);
        row[_columns.data]= value;
      }
      break;
      
    case MYX_LIST_VALUE:
      set_icon(row, value);
      row[_columns.text]= get_caption(value);
      row[_columns.data]= value;
      add_list_to_store(value, row, store);
      break;

    case MYX_DICT_VALUE:
      set_icon(row, value);
      row[_columns.text]= ufmt("%s    %s %i", value["name"].isValid() ? value["name"].asString() : "", get_caption(value).c_str(), _myx_grt_get_refcount(value.grtValue()));
      row[_columns.data]= value;
      add_dict_to_store(value, row, store);
      break;

    case MYX_INT_VALUE:
    case MYX_REAL_VALUE:
      set_icon(row, value);
      row[_columns.text]= get_caption(value);
      row[_columns.data]= value;
      break;
      
    case MYX_ANY_VALUE:
      row[_columns.text]= get_caption(value);
      row[_columns.data]= value;
      break;
    }
  }
}


void MGRTShell::add_dict_to_store(MGRTValue dict, Gtk::TreeRow &parent, Glib::RefPtr<Gtk::TreeStore> store)
{
  unsigned int c= _container_only_item->get_active() ? dict.countComplex() : dict.count();
  
  Gtk::TreeIter iter;
  
  for (unsigned int i= 0; i < c; i++)
  {
    MGRTValue value;
    const char *key;
    
    if (_container_only_item->get_active())
      dict.complexDictItemByIndex(i, key, value);
    else
      dict.dictItemByIndex(i, key, value);

    iter= store->append(parent.children());
    Gtk::TreeRow row= *iter;

    set_icon(row, value);
    row[_columns.text]= key;
    row[_columns.data]= value;
    row[_columns.path]= key;
    
    switch (value.type())
    {
    case MYX_DICT_VALUE:
      add_dict_to_store(value, row, store);
      break;
    case MYX_LIST_VALUE:
      add_list_to_store(value, row, store);
      break;
      /* not working
    case MYX_STRING_VALUE:
      {
        MYX_GRT_STRUCT *gstruct= myx_grt_struct_get(_grt->grt(), dict.contentStructOfMember(_grt->grt(), key));
        if (gstruct)
        {
          MYX_GRT_STRUCT_MEMBER *smember= myx_grt_struct_get_member_by_name(_grt->grt(), gstruct, key, 1);

          if (myx_grt_struct_member_get_is_ref(smember))
          {
            value= MGRTValue::refObject(_grt->grt(), value.asString());
            add_dict_to_store(value, row, store);
          }
        }
      }
       */
    default:
      break;
    }
  }
}


void MGRTShell::refresh()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  _value_store->clear();
  
  MGRTValue root(_grt->global_value(_root_path.empty() ? "/" : _root_path));
  
  iter= _value_store->append();
  row= *iter;
  row[_columns.icon]= _dict_icon;
  if (_root_path.empty())
    row[_columns.text]= "root";
  else
    row[_columns.text]= myx_grt_dict_name_item_as_string(root.grtValue());
  row[_columns.data]= root;
  add_dict_to_store(root, row, _value_store);
  
  
  // structs
  
  _struct_store->clear();
  
  unsigned int pc= myx_grt_package_count(_grt->grt());
  for (unsigned int i= 0; i < pc; i++)
  {
    char *pkg= myx_grt_package_by_index(_grt->grt(), i);
    
    iter= _struct_store->append();
    row= *iter;
    
    row[_columns.icon]= _folder_icon;
    row[_columns.text]= (pkg && *pkg) ? pkg : "<base>";
    row[_columns.data]= pkg;
    
    unsigned int sc= myx_grt_package_struct_count(_grt->grt(), pkg);
    for (unsigned int j= 0; j < sc; j++)
    {
      Gtk::TreeIter iter2= _struct_store->append(row.children());
      Gtk::TreeRow row2= *iter2;
      MYX_GRT_STRUCT *gstruct= myx_grt_package_struct_by_index(_grt->grt(), pkg, j);
      int inherited;
      const char *caption;

      caption= myx_grt_struct_get_caption(_grt->grt(), gstruct, &inherited);
      
      row2[_columns.icon]= _struct_icon;
      if (inherited && caption && *caption)
      {
        row2[_columns.text]= ufmt("%s   <<%s>>", myx_grt_struct_get_name(gstruct), caption);
        row2[_columns.data]= gstruct;
      }
      else
      {
        row2[_columns.text]= ufmt("%s    (%s)", myx_grt_struct_get_name(gstruct),caption);
        row2[_columns.data]= gstruct;
      }

      unsigned int mc= myx_grt_struct_get_member_count(gstruct);
      for (unsigned int k= 0; k < mc; k++)
      {
        Gtk::TreeIter iter3= _struct_store->append(row2.children());
        Gtk::TreeRow row3= *iter3;
        MYX_GRT_STRUCT_MEMBER *member= myx_grt_struct_get_member_by_index(gstruct, k);

        row3[_columns.text]= ufmt("%s: %s;", member->name,
                                  myx_get_value_type_as_string(myx_grt_struct_member_get_type(member)));
        row3[_columns.data]= member;
      }
    }
  }

  // modules

  for (int i= 0; i < myx_grt_module_get_count(_grt->grt()); i++)
  {
    MYX_GRT_MODULE *module= myx_grt_module_get_by_index(_grt->grt(), i);
    iter= _module_store->append();
    row= *iter;
    
    row[_columns.icon]= _module_icon;
    row[_columns.text]= module->name;
    row[_columns.data]= module;
    
    for (int j= 0; j < myx_grt_module_function_get_count(module); j++)
    {
      MYX_GRT_FUNCTION *func= myx_grt_module_function_get_by_index(module, j);
      Gtk::TreeIter iter2= _module_store->append(row.children());
      Gtk::TreeRow row2= *iter2;

      row2[_columns.icon]= _function_icon;
      row2[_columns.text]= func->name;
      row2[_columns.data]= func;
    }
  }
}



MGShellView *MGRTShell::shell_view()
{
  return &_text;
}


Glib::ustring MGRTShell::get_prompt()
{
  char *tmp= myx_grt_shell_get_prompt(_grt->grt());
  Glib::ustring prompt= tmp;
  g_free(tmp);
  return prompt;
}


void MGRTShell::perform_command(const Glib::ustring &command)
{
  _grt->perform_shell_command(command);
}


void MGRTShell::value_activated(const Gtk::TreeModel::Path &path,Gtk::TreeViewColumn *column)
{
  Gtk::TreeIter iter= _value_store->get_iter(path);
  Gtk::TreeRow row= *iter;
  Glib::ustring grt_path;
  
  while ((iter= row.parent()))
  {
    grt_path= "/"+row[_columns.path]+grt_path;
    row= *iter;
  }
  if (grt_path.empty())
    grt_path= "/";

  _text.get_buffer()->erase_selection();
  
  Glib::RefPtr<Gtk::TextMark> start= _text.get_buffer()->create_mark(_text.get_buffer()->get_insert()->get_iter());
  _text.get_buffer()->insert(start->get_iter(), grt_path);
  Gtk::TextIter end= _text.get_buffer()->get_insert()->get_iter();
  
  _text.get_buffer()->select_range(start->get_iter(), end);
  _text.get_buffer()->delete_mark(start);
}


void MGRTShell::value_selected()
{
  Gtk::TreeIter sel= _value_tree.get_selection()->get_selected();
  _detail_store->clear();
  if (sel)
  {
    Gtk::TreeIter iter;
    Gtk::TreeRow row= *sel;
    MGRTValue value((MYX_GRT_VALUE*)(void*)row[_columns.data]);

    _detail_tree.remove_all_columns();
    _detail_tree.append_column("", _dcolumns.text);
    _detail_tree.append_column("", _dcolumns.value);

    switch (value.type())
    {
    case MYX_DICT_VALUE:
      for (unsigned int i= 0; i < value.count(); i++)
      {
        Gtk::TreeRow nrow;
        const char *k;
        MGRTValue v;
        char *vf;
        
        value.dictItemByIndex(i, k, v);
        
        vf= myx_grt_value_formated_as_string(v.grtValue());
        
        iter= _detail_store->append();
        nrow= *iter;
        nrow[_dcolumns.text]= k;
        nrow[_dcolumns.value]= vf;
        g_free(vf);
      }
      break;
    case MYX_LIST_VALUE:
      for (unsigned int i= 0; i < value.count(); i++)
      {
        Gtk::TreeRow nrow;
        char k[32];
        MGRTValue v;
        char *vf;

        g_snprintf(k, sizeof(k), "%i", i);
        v= value[i];
        
        vf= myx_grt_value_formated_as_string(v.grtValue());
        
        iter= _detail_store->append();
        nrow= *iter;
        nrow[_dcolumns.text]= k;
        nrow[_dcolumns.value]= vf;
        g_free(vf);
      }
      break;
      
    default:
      break;
    }
  }
}


static const char *type2str(MYX_GRT_MODULE_TYPE type)
{
  switch (type)
  {
  case MYX_BUILTIN_MODULE_TYPE: return "Builtin (C)"; 
  case MYX_JAVA_MODULE_TYPE: return "Java";
  case MYX_LUA_MODULE_TYPE: return "Lua";
  case MYX_PYTHON_MODULE_TYPE: return "Python";
  case MYX_PHP_MODULE_TYPE: return "PHP";
  case MYX_DELPHI_MODULE_TYPE: return "Delphi";
  case MYX_OBJC_MODULE_TYPE: return "Objective-C";
  case MYX_CPP_MODULE_TYPE: return "C++";
  default: return "?";
  }
}


void MGRTShell::module_selected()
{
  Gtk::TreeIter sel= _module_tree.get_selection()->get_selected();
  
  _module_label.set_text("\n\n\n");
  
  if (sel)
  {
    Gtk::TreeIter iter;
    Gtk::TreeRow row= *sel;
    
    iter= row.parent();
    if (!iter)
    {
      MYX_GRT_MODULE *module= (MYX_GRT_MODULE*)(void*)row[_columns.data];

      _module_label.set_markup(ufmt("Module: %s\n"
                                    "Language: %s\n"
                                    "Extends: %s",
                                    module->name,
                                    type2str(module->loader->loader_type),
                                    module->extends?:""));
    }
    else
    {
      Gtk::TreeRow prow= *iter;
      MYX_GRT_MODULE *module= (MYX_GRT_MODULE*)(void*)prow[_columns.data];
      MYX_GRT_FUNCTION *function= (MYX_GRT_FUNCTION*)(void*)row[_columns.data];

      _module_label.set_markup(ufmt("Module: %s\n"
                                    "Function: %s\n",
                                    module->name,
                                    function->name));
    }
  }
}
