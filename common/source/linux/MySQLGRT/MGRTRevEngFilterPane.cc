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

#include "MGRTRevEngFilterPane.h"
#include "mygpriv.h"
#include "myg_gtkutils.h"
/**
 * @file  MGRTRevEngFilterPane.cc
 * @brief 
 */



MGRTRevEngFilterPane::MGRTRevEngFilterPane(MGRT *grt, const Glib::ustring &struct_name)
  : _grt(grt), _table(4,3), _label1("", 0.0, 0.5), _label2("", 0.0, 0.5), 
    _label3(_("<small>Click [Show Detailed Selection] to put objects in the ignore list.</small>"), 1.0, 0.5, false),
    _button_box(true, 4), _detail_table(1, 3), _struct_name(struct_name)
{
  MYX_GRT_STRUCT *gstruct= myx_grt_struct_get(_grt->grt(), _struct_name.c_str());
  Glib::ustring caption;
  int inherited;
  const char *imagePath;

  _label3.set_use_markup(true);
  
  _table.set_border_width(12);
  _table.set_row_spacings(8);
  _table.set_col_spacings(8);
  
  if (gstruct)
    caption=  myx_grt_struct_get_caption(_grt->grt(), gstruct, &inherited);
  else
    caption= struct_name;

  _label1.set_markup("<b>"+caption+"</b>");
  _label1.show();

  _check.set_label(ufmt(_("Migrate Objects of Type %s"), caption.c_str()));
  _check.signal_toggled().connect(sigc::mem_fun(*this,&MGRTRevEngFilterPane::toggle_migrate));
  _check.show();
  set_label_widget(_check);

  while (gstruct)
  {
    imagePath= myx_grt_struct_get_icon_path(_grt->grt(),
                                            gstruct, MYX_IT_MANY_STANDARD);
    if (imagePath)
    {
      Gtk::Image *img= new Gtk::Image(PIXCACHE->load(imagePath));
      
      _table.attach(*Gtk::manage(img), 0,1, 0, 1, Gtk::FILL, Gtk::FILL);
      img->show();
      break;
    }
    
    gstruct= myx_grt_struct_get(_grt->grt(), myx_grt_struct_get_parent_name(gstruct));
  }

  _source_store= Gtk::ListStore::create(_columns);
  _source_tree.set_model(_source_store);
  _ignore_store= Gtk::ListStore::create(_columns);
  _ignore_tree.set_model(_ignore_store);
  
  Gtk::ScrolledWindow *scroll1, *scroll2;
  
  scroll1= Gtk::manage(new Gtk::ScrolledWindow());
  scroll2= Gtk::manage(new Gtk::ScrolledWindow());
  
  scroll1->set_policy(Gtk::POLICY_ALWAYS, Gtk::POLICY_AUTOMATIC);
  scroll1->set_shadow_type(Gtk::SHADOW_IN);
  scroll1->add(_source_tree);

  scroll2->set_policy(Gtk::POLICY_AUTOMATIC, Gtk::POLICY_AUTOMATIC);
  scroll2->set_shadow_type(Gtk::SHADOW_IN);
  scroll2->add(_ignore_tree);

  _source_tree.append_column(_("Objects to Migrate"), _columns.text);
  _ignore_tree.append_column(_("Ignored Objects"), _columns.text);

  _table.attach(*Gtk::manage(new Gtk::Label(_("Objects of Type:"), 1.0, 0.5)),
                       0, 1, 0, 1, Gtk::FILL, Gtk::FILL);
  _table.attach(*Gtk::manage(new Gtk::Label(_("Number to Migrate:"), 1.0, 0.5)),
                       0, 1, 1, 2, Gtk::FILL, Gtk::FILL);

  _table.attach(_label1, 1, 2, 0, 1, Gtk::FILL, Gtk::FILL);
  _table.attach(_label2, 1, 2, 1, 2, Gtk::FILL, Gtk::FILL);

  _table.attach(_label3, 3, 4, 0, 1, Gtk::FILL, Gtk::FILL);

  Gtk::Button *btn;
  
  btn= Gtk::manage(new Gtk::Button(">"));
  _button_box.pack_start(*btn, false, false);
  btn->signal_clicked().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGRTRevEngFilterPane::move_object),"add"));
  btn= Gtk::manage(new Gtk::Button("<"));
  _button_box.pack_start(*btn, false, false);
  btn->signal_clicked().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGRTRevEngFilterPane::move_object),"del"));
  btn= Gtk::manage(new Gtk::Button(">>"));
  _button_box.pack_start(*btn, false, false);
  btn->signal_clicked().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGRTRevEngFilterPane::move_object),"addall"));
  btn= Gtk::manage(new Gtk::Button("<<"));
  _button_box.pack_start(*btn, false, false);
  btn->signal_clicked().connect(sigc::bind<const char*>(sigc::mem_fun(*this,&MGRTRevEngFilterPane::move_object),"delall"));
  
  _detail_table.set_col_spacings(8);
  _detail_table.set_row_spacings(8);
  _detail_table.attach(*scroll1, 0, 1, 1, 2, Gtk::EXPAND|Gtk::FILL, Gtk::EXPAND|Gtk::FILL);
  _detail_table.attach(_button_box, 1, 2, 1, 2, Gtk::FILL, Gtk::EXPAND|Gtk::FILL);
  _detail_table.attach(*scroll2, 2, 3, 1, 2, Gtk::EXPAND|Gtk::FILL, Gtk::EXPAND|Gtk::FILL);

  _detail_table.show_all();
  _table.attach(_detail_table, 1, 4, 2, 3, Gtk::FILL, Gtk::EXPAND|Gtk::FILL);

  _detail_button.set_label(_("Show Detailed Selection"));
  _detail_button.signal_clicked().connect(sigc::mem_fun(*this,&MGRTRevEngFilterPane::show_details));

  _detail_button.show();
  _table.attach(_detail_button, 3, 4, 3, 4, Gtk::FILL, Gtk::FILL);

  _table.show_all();
  _detail_table.hide();
  add(_table);
}



void MGRTRevEngFilterPane::move_object(const char *dir)
{
  MGRTValue ignoreL(MGRTValue::fromGlobal(_grt->grt(), "/migration/ignoreList"));
  
  if (strcmp(dir, "add")==0)
  {
    Gtk::TreeIter iter;
    std::list<Gtk::TreePath> sel= _source_tree.get_selection()->get_selected_rows();

    for (std::list<Gtk::TreePath>::const_iterator i= sel.begin(); i != sel.end(); ++i)
    {
      iter= _source_store->get_iter(*i);
      Gtk::TreeRow row= *iter;

      ignoreL.append(MGRTValue(ufmt("%s:%s", _struct_name.c_str(), ((Glib::ustring)row[_columns.text]).c_str()).c_str()));
    }
  }
  else if (strcmp(dir, "del")==0)
  {
    Gtk::TreeIter iter;
    std::list<Gtk::TreePath> sel= _ignore_tree.get_selection()->get_selected_rows();

    for (std::list<Gtk::TreePath>::reverse_iterator i= sel.rbegin(); i != sel.rend(); ++i)
    {
      ignoreL.remove((*i)[0]);
    }
  }
  else if (strcmp(dir, "addall")==0)
  {
    Gtk::TreeNodeChildren sel= _source_store->children();

    for (Gtk::TreeNodeChildren::const_iterator i= sel.begin(); i != sel.end(); ++i)
    {
      Gtk::TreeRow row= **i;

      ignoreL.append(MGRTValue(ufmt("%s:%s", _struct_name.c_str(), ((Glib::ustring)row[_columns.text]).c_str()).c_str()));
    }
  }
  else if (strcmp(dir, "delall")==0)
  {
    ignoreL.clear();
  }
  refresh();
}


void MGRTRevEngFilterPane::set_selected(bool flag)
{
  _check.set_active(flag);
  toggle_migrate();
}


void MGRTRevEngFilterPane::show_details()
{
  if (_detail_table.is_visible())
  {
    _detail_table.hide();
    _detail_button.set_label(_("Show Detailed Selection"));
  }
  else
  {
    _detail_table.show();
    _detail_button.set_label(_("Hide Details"));
  }
}


void MGRTRevEngFilterPane::toggle_migrate()
{
  MGRTValue ignoreL(MGRTValue::fromGlobal(_grt->grt(), "/migration/ignoreList"));
  if (_check.get_active())
  {
    _table.set_sensitive(true);
    myx_grt_list_item_del_as_string(ignoreL.grtValue(),
                                    ufmt("%s:*", _struct_name.c_str()).c_str());
  }
  else
  {
    _table.set_sensitive(false);
    myx_grt_list_item_add_as_string(ignoreL.grtValue(),
                                    ufmt("%s:*", _struct_name.c_str()).c_str());
  }
}



void MGRTRevEngFilterPane::refresh()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  MGRTValue ignoreL(MGRTValue::fromGlobal(_grt->grt(), "/migration/ignoreList"));
  MGRTValue sourceL(MGRTValue::fromGlobal(_grt->grt(), "/migration/sourceObjects"));
  int totalObjectCount= 0;
  
  _source_store->clear();
  _ignore_store->clear();

  for (unsigned int i= 0; i < sourceL.count(); i++)
  {
    MGRTValue obj(myx_grt_list_item_get_reference_value(_grt->grt(), sourceL.grtValue(), i));
    const char *objStructName;

    if (obj.isValid())
      objStructName= obj.contentStruct();

    //If the object is of the same struct
    if (obj.isValid()
        && myx_grt_struct_is_or_inherits_from(_grt->grt(),
                                              objStructName,
                                              _struct_name.c_str()) == 1)
    {
      MYX_GRT_VALUE *ownerObj;
      Glib::ustring name;

      totalObjectCount++;

      //Find owner schema
      ownerObj = myx_grt_dict_item_get_reference_value(_grt->grt(), obj.grtValue(), "owner");

      //Build name
      if (ownerObj)
        name= ufmt("%s.%s",
                   myx_grt_dict_item_get_as_string(ownerObj, "name"),
                   obj["name"].asString());
      else
      {
        g_message("Owner object of %s not found", myx_grt_dict_item_get_as_string(obj.grtValue(), "name"));
        continue;
      }

      //Check object name against filter list
      bool filterHit= false;
      for (unsigned int i= 0; i < ignoreL.count(); i++)
      {
        if (strncmp(ignoreL[i].asString(), _struct_name.c_str(), _struct_name.size()) == 0 &&
            ignoreL[i].asString()[_struct_name.size()] == ':'
            && strcmp(ignoreL[i].asString() + _struct_name.size() + 1, name.c_str())==0)
          filterHit= true;
      }

      /*
      for (int j= 0; j < [_ignoreFilters count]; j++)
      {
        const char *filter= [[_ignoreFilters objectAtIndex:j] UTF8String];

        if (myx_match_pattern([[NSString stringWithFormat:@"%@:%@", _struct_name, name] UTF8String],
                              filter, 1, 1)==1)
        {
          filterHit= YES;
          break;
        }
      }*/
      //Check if the object was searched
      if (!filterHit)
      {
        //if (myx_match_pattern([name UTF8String],
        //                      [
        //                     FilterMigrateListEd.SearchEd.Text + '*', 0, 1)==1)

        iter= _source_store->append();
        row= *iter;
        //else
        //Inc(SearchIgnores);
      }
      else
      {
        iter= _ignore_store->append();
        row= *iter;
      }
      row[_columns.text]= name;
      row[_columns.grt]= obj;
      row[_columns.owner]= MGRTValue(ownerObj);
    }
  }

  _label2.set_markup(ufmt("<b>%i / %i</b>", _source_store->children().size(), totalObjectCount));

  if (totalObjectCount == 0)
  {
    _check.set_active(false);
    toggle_migrate();
  }
}

