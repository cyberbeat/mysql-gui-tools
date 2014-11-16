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

#include "MGRTTableEditor.h"
#include "mygpriv.h"
#include "myg_gtkutils.h"
#include "MGGladeXML.h"
#include "MGCellRendererCombo.h"


/**
 * @file  MGRTTableEditor.cc
 * @brief 
 */



class CellRendererPixToggle : public Gtk::CellRendererPixbuf {
    sigc::signal<void,const Glib::ustring&> _toggle_signal;
  public:
    sigc::signal<void,const Glib::ustring&> signal_toggled() { return _toggle_signal; };

    virtual bool activate_vfunc(GdkEvent* event, Gtk::Widget& widget, const Glib::ustring& path, const Gdk::Rectangle& background_area, const Gdk::Rectangle& cell_area, Gtk::CellRendererState flags)
    {
      _toggle_signal.emit(path);
      return true;
    }
};


static char *pack_key_values[]= {
  "", "", "0", "1"
};
static char *row_format_values[]= {
  "", "", "dynamic", "fixed", "compressed", "redundant", "compact"
};
static char *merge_insert_values[]= {
  "", "no", "last", "first"
};
static char *raid_type_values[]= {
  "", "", "striped"
};
static char *engine_values[]= {
  "", "InnoDB", "MyISAM", "Memory", "Merge", "NDB", "BDB", "JStar", "BLACKHOLE", "CSV"
};


MGRTValue MGRTTableEditor::edited_object()
{
  return _table_data->value();
}


static bool streq(const char *a, const char *b)
{
  if ((!a || !*a) && (!b || !*b))
    return true;
  
  return strcmp(a, b)==0;
}


static void update_value(MGRTValue &table, const char *key, const char *value)
{
  if (!streq(table.get(key, ""), value))
    table.set(key, value);
}

static void update_value(MGRTValue &table, const char *key, int value)
{
  if (table.get(key, 0) != value)
    table.set(key, value);
}

bool MGRTTableEditor::commit_changes()
{
  MGRTValue table(_table_data->value());

#define FROMENTRY(v,s) update_value(table, v, _xml->get_entry(s)->get_text().c_str())
#define COMBOVALUE(v,s,options) update_value(table, v, options[_xml->get_combo(s)->get_active_row_number()+1])

  _table_data->setName(_xml->get_entry("name_entry")->get_text().c_str());
  
  COMBOVALUE("tableEngine", "engine_combo", engine_values);

  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  
  iter= _xml->get_combo("collation_combo")->get_active();
  row= *iter;
  
  _table_data->setDefaultCollation(((Glib::ustring)row[_charset_columns.collation]).c_str());
  
  _table_data->setComment(_xml->get_text("description_text")->get_buffer()->get_text().c_str());

  // Misc. Options
  COMBOVALUE("packKeys", "pack_keys_option", pack_key_values);
  FROMENTRY("password", "password_entry");
  FROMENTRY("nextAutoInc", "next_autoinc_entry");
  update_value(table, "delayKeyWrite", _xml->get_toggle("delay_update_check")->get_active());
  
  // Row Options
  COMBOVALUE("rowFormat", "format_option", row_format_values);
  update_value(table, "checksum", _xml->get_toggle("checksum_check")->get_active());
  FROMENTRY("avgRowLength", "avg_length_entry");
  FROMENTRY("minRows", "min_rows_entry");
  FROMENTRY("maxRows", "max_rows_entry");

  // Storage Options
  FROMENTRY("tableDataDir", "datadir_entry");
  FROMENTRY("tableIndexDir", "indexdir_entry");

  // Merge Options
  FROMENTRY("mergeUnion", "merge_tables_entry");
  COMBOVALUE("mergeInsert", "insert_method_option", merge_insert_values);
  
  // Table RAID stuff
  COMBOVALUE("raidType", "raid_type_option", raid_type_values);
  FROMENTRY("raidChunks", "chunk_num_entry");
  FROMENTRY("raidChunkSize", "chunk_size_entry");

#undef FROMENTRY
#undef COMBOVALUE

  return true;
}


bool MGRTTableEditor::commit()
{
  if (commit_changes())
  {
    _table_data->commit();
    return true;
  }
  return false;
}


void MGRTTableEditor::revert()
{
  _table_data->revert();
}


void MGRTTableEditor::show_object()
{
  MGRTValue table(_table_data->value());

  _displaying= true;

#define TOENTRY(v,s) _xml->get_entry(s)->set_text(table.get(v, ""))
#define SETCOMBOVALUE(v,s,options) { for (unsigned int i= 0; i < sizeof(options)/sizeof(char*); i++) if (strcasecmp(options[i], table.get(v, ""))==0) { _xml->get_combo(s)->set_active(i-1); break; } } while (0)
  
  _xml->get_entry("name_entry")->set_text(_table_data->name());

  _xml->get_text("description_text")->get_buffer()->set_text(_table_data->comment());

  if (strcmp(table.get("tableEngine", ""), "")==0)
    _xml->get_combo("engine_combo")->set_active(0);
  else
    SETCOMBOVALUE("tableEngine", "engine_combo", engine_values);

  Gtk::TreeIter iter= _collation->children().begin();
  while (iter != _collation->children().end())
  {
    Gtk::TreeRow row= *iter;
    if (strcmp(((Glib::ustring)row[_charset_columns.collation]).c_str(), table.get("defaultCollationName", ""))==0)
    {
      _xml->get_combo("collation_combo")->set_active(iter);
      break;
    }
    ++iter;
  }

  // Misc. Options
  SETCOMBOVALUE("packKeys", "pack_keys_option", pack_key_values);
  TOENTRY("password", "password_entry");
  TOENTRY("nextAutoInc", "next_autoinc_entry");
  _xml->get_toggle("delay_update_check")->set_active(table.get("delayKeyWrite", 0));
  
  // Row Options
  SETCOMBOVALUE("rowFormat", "format_option", row_format_values);
  _xml->get_toggle("checksum_check")->set_active(table.get("checksum", 0));
  TOENTRY("avgRowLength", "avg_length_entry");
  TOENTRY("minRows", "min_rows_entry");
  TOENTRY("maxRows", "max_rows_entry");

  // Storage Options
  TOENTRY("tableDataDir", "datadir_entry");
  TOENTRY("tableIndexDir", "indexdir_entry");

  // Merge Options
  TOENTRY("mergeUnion", "merge_tables_entry");
  SETCOMBOVALUE("mergeInsert", "insert_method_option", merge_insert_values);
  
  // Table RAID stuff
  SETCOMBOVALUE("raidType", "raid_type_option", raid_type_values);
  TOENTRY("raidChunks", "chunk_num_entry");
  TOENTRY("raidChunkSize", "chunk_size_entry");

#undef TOENTRY
#undef SETCOMBOVALUE

  refresh_column_list();
  refresh_index_list();
  refresh_fk_list();

  column_selected();
  refresh_index_column_list();
  refresh_fk_column_list();
  
  refresh_reftable_list();
  
  _displaying= false;
}


void MGRTTableEditor::toggle_advanced()
{
  if (!_xml->get_widget("column_page")->is_visible())
  {
    _xml->get_button("advanced_button")->set_label(_("Hide Advanced <<"));
    _xml->get_widget("column_page")->show();
  }
  else
  {
    _xml->get_button("advanced_button")->set_label(_("Show Advanced >>"));
    _xml->get_widget("column_page")->hide();
  }
}


void MGRTTableEditor::setup()
{
  Gtk::TreeView *tree;
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  Gtk::TreeViewColumn *tree_column;

  MGRTObjectEditor::setup();

  _placeholder_color= Gdk::Color("#ffffb0");
  get_colormap()->alloc_color(_placeholder_color);
  
  _xml->get_button("apply_button")->signal_clicked().connect(sigc::mem_fun(*this,
                                                                          &MGRTTableEditor::apply_changes));
  _xml->get_button("close_button")->signal_clicked().connect(sigc::mem_fun(*this,
                                                                          &MGRTTableEditor::close));


  _xml->get_button("advanced_button")->signal_clicked().connect(sigc::mem_fun(*this,
                                                                              &MGRTTableEditor::toggle_advanced));
  _xml->get_widget("column_page")->hide();
  
  _collation= make_collation_list();
  _xml->get_combo("collation_combo")->set_model(_collation);
  _xml->get_combo("collation_combo")->pack_start(_charset_columns.text);

  _column_collation= make_collation_list();
  _xml->get_combo("column_collation_combo")->set_model(_column_collation);
  _xml->get_combo("column_collation_combo")->pack_start(_charset_columns.text);
  

  _xml->get_toggle("pk_flag")->signal_toggled().connect(sigc::bind<Gtk::CheckButton*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_flag_changed),
                                                                                                  (Gtk::CheckButton*)_xml->get_toggle("pk_flag"), "pk"));
  _xml->get_toggle("null_flag")->signal_toggled().connect(sigc::bind<Gtk::CheckButton*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_flag_changed),
                                                                                                    (Gtk::CheckButton*)_xml->get_toggle("null_flag"), "isNullable"));
  _xml->get_toggle("autoinc_flag")->signal_toggled().connect(sigc::bind<Gtk::CheckButton*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_flag_changed),
                                                                                                       (Gtk::CheckButton*)_xml->get_toggle("autoinc_flag"), "autoIncrement"));

  _xml->get_entry("column_name_entry")->signal_changed().connect(sigc::bind<Gtk::Widget*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_entry_changed),
                                                                                                      _xml->get_entry("column_name_entry"), "name"));
  _xml->get_combo_entry("column_type_combo")->signal_changed().connect(sigc::bind<Gtk::Widget*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_entry_changed),
                                                                                                            _xml->get_combo_entry("column_type_combo"), "type"));
  _xml->get_combo("column_collation_combo")->signal_changed().connect(sigc::bind<Gtk::Widget*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_entry_changed),
                                                                                                           _xml->get_combo("column_collation_combo"), "collation"));
  _xml->get_entry("default_entry")->signal_changed().connect(sigc::bind<Gtk::Widget*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_entry_changed),
                                                                                                  _xml->get_entry("default_entry"), "default"));
  _xml->get_toggle("null_toggle")->signal_toggled().connect(sigc::bind<Gtk::Widget*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_entry_changed),
                                                                                                  _xml->get_toggle("null_toggle"), "defaultNull"));
  _xml->get_text("column_comment_text")->get_buffer()->signal_changed().connect(sigc::bind<Gtk::Widget*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_entry_changed),
                                                                                                                     _xml->get_text("column_comment_text"), "comment"));
  
  
  
  _column_list= Gtk::ListStore::create(_c_columns);
  _index_list= Gtk::ListStore::create(_i_columns);
  _index_column_list= Gtk::ListStore::create(_ic_columns);
  _fk_list= Gtk::ListStore::create(_fk_columns);
  _fk_column_list= Gtk::ListStore::create(_fkc_columns);

  _aux_column_names_list= Gtk::ListStore::create(_columns);

  _pk_icon= PIXCACHE->load("column_pk.png");
  _column_icon= PIXCACHE->load("column.png");
  _blob_icon= PIXCACHE->load("datatype_blob.png");
  _date_icon= PIXCACHE->load("datatype_datetime.png");
  _numeric_icon= PIXCACHE->load("datatype_numeric.png");
  _spatial_icon= PIXCACHE->load("datatype_spatial.png");
  _string_icon= PIXCACHE->load("datatype_string.png");
  _userdef_icon= PIXCACHE->load("datatype_userdefined.png");
  _null_icon= PIXCACHE->load("field_overlay_null.png");

  _fk_reftable_list= Gtk::ListStore::create(_columns);
 
  _fk_refcolumn_list= Gtk::ListStore::create(_columns);

  _engine_list= Gtk::ListStore::create(_columns);
  iter= _engine_list->append();
  row= *iter;
  row[_columns.name]= "Transactional (InnoDB)";
  row[_columns.grt]= MGRTValue("InnoDB");
  iter= _engine_list->append();
  row= *iter;
  row[_columns.name]= "Non transactional (MyISAM)";
  row[_columns.grt]= MGRTValue("MyISAM");
  iter= _engine_list->append();
  row= *iter;
  row[_columns.name]= "Memory only (Memory)";
  row[_columns.grt]= MGRTValue("Memory");
  iter= _engine_list->append();
  row= *iter;
  row[_columns.name]= "Merge (Merge)";
  row[_columns.grt]= MGRTValue("Merge");
  iter= _engine_list->append();
  row= *iter;
  row[_columns.name]= "Cluster (NDB)";
  row[_columns.grt]= MGRTValue("NDB");
  iter= _engine_list->append();
  row= *iter;
  row[_columns.name]= "Berkeley DB (BDB)";
  row[_columns.grt]= MGRTValue("BDB");

  _index_type_list= Gtk::ListStore::create(_columns);
  iter= _index_type_list->append();
  row= *iter;
  row[_columns.name]= "INDEX";
  row[_columns.grt]= MGRTValue("INDEX");
  iter= _index_type_list->append();
  row= *iter;
  row[_columns.name]= "PRIMARY";
  row[_columns.grt]= MGRTValue("PRIMARY");
  iter= _index_type_list->append();
  row= *iter;
  row[_columns.name]= "UNIQUE";
  row[_columns.grt]= MGRTValue("UNIQUE");
  iter= _index_type_list->append();
  row= *iter;
  row[_columns.name]= "FULLTEXT";
  row[_columns.grt]= MGRTValue("FULLTEXT");
  iter= _index_type_list->append();
  row= *iter;
  row[_columns.name]= "SPATIAL";
  row[_columns.grt]= MGRTValue("SPATIAL");
  
  _index_order_list= Gtk::ListStore::create(_columns);
  iter= _index_order_list->append();
  row= *iter;
  row[_columns.name]= "ASC";
  row[_columns.grt]= MGRTValue("ASC");
  iter= _index_order_list->append();
  row= *iter;
  row[_columns.name]= "DESC";
  row[_columns.grt]= MGRTValue("DESC");
  
  _fk_ondelete_list= Gtk::ListStore::create(_columns);
  iter= _fk_ondelete_list->append();
  row= *iter;
  row[_columns.name]= "RESTRICT";
  row[_columns.grt]= MGRTValue("RESTRICT");
  iter= _fk_ondelete_list->append();
  row= *iter;
  row[_columns.name]= "CASCADE";
  row[_columns.grt]= MGRTValue("CASCADE");
  iter= _fk_ondelete_list->append();
  row= *iter;
  row[_columns.name]= "SET NULL";
  row[_columns.grt]= MGRTValue("SET NULL");
  iter= _fk_ondelete_list->append();
  row= *iter;
  row[_columns.name]= "NO ACTION";
  row[_columns.grt]= MGRTValue("NO ACTION");
                                      
  _fk_onupdate_list= Gtk::ListStore::create(_columns);
  iter= _fk_onupdate_list->append();
  row= *iter;
  row[_columns.name]= "RESTRICT";
  row[_columns.grt]= MGRTValue("RESTRICT");
  iter= _fk_onupdate_list->append();
  row= *iter;
  row[_columns.name]= "CASCADE";
  row[_columns.grt]= MGRTValue("CASCADE");
  iter= _fk_onupdate_list->append();
  row= *iter;
  row[_columns.name]= "SET NULL";
  row[_columns.grt]= MGRTValue("SET NULL");
  iter= _fk_onupdate_list->append();
  row= *iter;
  row[_columns.name]= "NO ACTION";
  row[_columns.grt]= MGRTValue("NO ACTION");

  
  _xml->get_combo("engine_combo")->set_model(_engine_list);
  _xml->get_combo("engine_combo")->pack_start(_columns.name);
  
  // column tree  
  tree= _xml->get_tree("column_tree");
  tree->set_model(_column_list);
  CellRendererPixToggle *pixt= Gtk::manage(new CellRendererPixToggle());
  pixt->property_mode()= Gtk::CELL_RENDERER_MODE_ACTIVATABLE;
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(""));
  tree_column->pack_start(*pixt);
  tree_column->add_attribute(pixt->property_pixbuf(), _c_columns.icon);
  pixt->signal_toggled().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_toggled),0));
  tree->append_column(*tree_column);
  tree->append_column_editable(_("Column Name"), _c_columns.name);
  tree->append_column_editable(_("Data Type"), _c_columns.type);
  tree->append_column_editable("", _c_columns.nnull);
  tree->append_column_editable("", _c_columns.autoinc);
  tree->append_column_editable(_("Flags"), _c_columns.flags);
  tree->append_column_editable(_("Default Value"), _c_columns.defval);
  tree->append_column_editable(_("Comment"), _c_columns.comment);
  tree->get_column(6)->add_attribute(static_cast<Gtk::CellRendererText*>(tree->get_column(6)->get_first_cell_renderer())->property_style_set(),
                                     _c_columns.defnull);

  Gtk::Widget *w;
  tree->get_column(3)->set_widget(*(w=Gtk::manage(new Gtk::Image(PIXCACHE->load("editor_table_not_null.png")))));
  w->show();
  tree->get_column(4)->set_widget(*(w=Gtk::manage(new Gtk::Image(PIXCACHE->load("editor_table_auto_inc.png")))));
  w->show();

  ((Gtk::CellRendererText*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_edited),1));
  ((Gtk::CellRendererText*)tree->get_column(2)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_edited),2));
  ((Gtk::CellRendererToggle*)tree->get_column(3)->get_first_cell_renderer())->signal_toggled().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_toggled),3));
  ((Gtk::CellRendererToggle*)tree->get_column(4)->get_first_cell_renderer())->signal_toggled().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_toggled),4));
  ((Gtk::CellRendererText*)tree->get_column(5)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_edited),5));
  ((Gtk::CellRendererText*)tree->get_column(6)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_edited),6));
  ((Gtk::CellRendererText*)tree->get_column(7)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::column_value_edited),7));
  ((Gtk::CellRendererText*)tree->get_column(7)->get_first_cell_renderer())->set_fixed_height_from_font(1);

  for (int i= 0; i < 8; i++)
  {
    tree->get_column(i)->get_first_cell_renderer()->property_cell_background_gdk()= _placeholder_color;
    tree->get_column(i)->add_attribute(tree->get_column(i)->get_first_cell_renderer()->property_cell_background_set(),
                                       _c_columns.placeholder);
    tree->get_column(i)->set_resizable(true);
  }
  tree->signal_key_release_event().connect(sigc::bind<Gtk::TreeView*>(sigc::mem_fun(*this,&MGRTTableEditor::tree_key_up), tree));
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGRTTableEditor::column_selected));

  // index tree
  tree= _xml->get_tree("index_tree");
  tree->set_model(_index_list);
  tree->append_column_editable(_("Index Name"), _i_columns.name);
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("Type")));

  MGCellRendererCombo *crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(100);
  tree_column->add_attribute(crend->property_text(), _i_columns.type);
  crend->set_model(_index_type_list);
  crend->property_editable()= true;

  tree->append_column_editable(_("Comment"), _i_columns.comment);

  ((Gtk::CellRendererText*)tree->get_column(0)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_value_edited),0));
  ((MGCellRendererCombo*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_value_edited),1));
  ((Gtk::CellRendererText*)tree->get_column(2)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_value_edited),2));

  for (int i= 0; i < 3; i++)
  {
    tree->get_column(i)->get_first_cell_renderer()->property_cell_background_gdk()= _placeholder_color;
    tree->get_column(i)->add_attribute(tree->get_column(i)->get_first_cell_renderer()->property_cell_background_set(),                                       
                                       _i_columns.placeholder);
    tree->get_column(i)->set_resizable(true);
  }
  tree->signal_key_release_event().connect(sigc::bind<Gtk::TreeView*>(sigc::mem_fun(*this,&MGRTTableEditor::tree_key_up),tree));
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGRTTableEditor::refresh_index_column_list));

  tree->show_all();

  // index column tree
  tree= _xml->get_tree("index_columns_tree");
  tree->set_model(_index_column_list);
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("Column")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(120);
  tree_column->add_attribute(crend->property_text(), _ic_columns.column);
  crend->set_model(_aux_column_names_list);
  crend->property_editable()= true;
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("Order")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(80);
  tree_column->add_attribute(crend->property_text(), _ic_columns.order);
  crend->set_model(_index_order_list);
  crend->property_editable()= true;
  tree->append_column_editable(_("Length"), _ic_columns.length);
  tree->append_column_editable(_("Stored Function"), _ic_columns.function);
  tree->append_column_editable(_("Comment"), _ic_columns.comment);

  ((Gtk::CellRendererText*)tree->get_column(0)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_column_value_edited),0));
  ((MGCellRendererCombo*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_column_value_edited),1));
  ((Gtk::CellRendererText*)tree->get_column(2)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_column_value_edited),2));
  ((Gtk::CellRendererText*)tree->get_column(3)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_column_value_edited),3));
  ((Gtk::CellRendererText*)tree->get_column(4)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::index_column_value_edited),4));

  for (int i= 0; i < 5; i++)
  {
    tree->get_column(i)->get_first_cell_renderer()->property_cell_background_gdk()= _placeholder_color;
    tree->get_column(i)->add_attribute(tree->get_column(i)->get_first_cell_renderer()->property_cell_background_set(),                                       
                                       _ic_columns.placeholder);    
    tree->get_column(i)->set_resizable(true);
  }
  tree->signal_key_release_event().connect(sigc::bind<Gtk::TreeView*>(sigc::mem_fun(*this,&MGRTTableEditor::tree_key_up),tree));

  tree->show_all();

  // fk tree
  tree= _xml->get_tree("fk_tree");
  tree->set_model(_fk_list);
  tree->append_column_editable(_("Foreign Key Name"), _fk_columns.name);
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("On Delete")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(100);
  tree_column->add_attribute(crend->property_text(), _fk_columns.ondelete);
  crend->set_model(_fk_ondelete_list);
  crend->property_editable()= true;
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("On Update")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(100);
  tree_column->add_attribute(crend->property_text(), _fk_columns.onupdate);
  crend->set_model(_fk_onupdate_list);
  crend->property_editable()= true;
  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("Ref. Table")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(120);
  tree_column->add_attribute(crend->property_text(), _fk_columns.reftable);
  crend->set_model(_fk_reftable_list);
  crend->property_editable()= true;
  tree->append_column_editable(_("Comment"), _fk_columns.comment);

  ((Gtk::CellRendererText*)tree->get_column(0)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_value_edited),0));
  ((MGCellRendererCombo*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_value_edited),1));
  ((MGCellRendererCombo*)tree->get_column(2)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_value_edited),2));
  ((MGCellRendererCombo*)tree->get_column(3)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_value_edited),3));
  ((Gtk::CellRendererText*)tree->get_column(4)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_value_edited),4));

  for (int i= 0; i < 5; i++)
  {
    tree->get_column(i)->get_first_cell_renderer()->property_cell_background_gdk()= _placeholder_color;
    tree->get_column(i)->add_attribute(tree->get_column(i)->get_first_cell_renderer()->property_cell_background_set(),                                       
                                       _fk_columns.placeholder);
    tree->get_column(i)->set_resizable(true);
  }
  tree->signal_key_release_event().connect(sigc::bind<Gtk::TreeView*>(sigc::mem_fun(*this,&MGRTTableEditor::tree_key_up),tree));
  tree->get_selection()->signal_changed().connect(sigc::mem_fun(*this,&MGRTTableEditor::refresh_fk_column_list));

  tree->show_all();

  // fk column tree
  tree= _xml->get_tree("fk_columns_tree");
  tree->set_model(_fk_column_list);

  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("Column")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(80);
  tree_column->add_attribute(crend->property_text(), _fkc_columns.column);
  crend->set_model(_aux_column_names_list);
  crend->property_editable()= true;

  tree_column= Gtk::manage(new Gtk::TreeViewColumn(_("Ref. Column")));
  crend= Gtk::manage(new MGCellRendererCombo());
  tree_column->pack_start(*crend);
  tree->append_column(*tree_column);
  tree_column->set_min_width(80);
  tree_column->add_attribute(crend->property_text(), _fkc_columns.refcolumn);
  crend->set_model(_fk_refcolumn_list);
  crend->property_editable()= true;

  ((MGCellRendererCombo*)tree->get_column(0)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_column_value_edited),0));
  ((MGCellRendererCombo*)tree->get_column(1)->get_first_cell_renderer())->signal_edited().connect(sigc::bind<int>(sigc::mem_fun(*this,&MGRTTableEditor::fk_column_value_edited),1));

  for (int i= 0; i < 2; i++)
  {
    tree->get_column(i)->get_first_cell_renderer()->property_cell_background_gdk()= _placeholder_color;
    tree->get_column(i)->add_attribute(tree->get_column(i)->get_first_cell_renderer()->property_cell_background_set(),
                                       _fkc_columns.placeholder);
    tree->get_column(i)->set_resizable(true);
  }
  tree->signal_key_release_event().connect(sigc::bind<Gtk::TreeView*>(sigc::mem_fun(*this,&MGRTTableEditor::tree_key_up),tree));

  tree->show_all();
}


MGRTTableEditor::MGRTTableEditor(GtkWindow *window)
  : MGRTObjectEditor(window)
{
  _displaying= false;
}


MGRTTableEditor *MGRTTableEditor::create(MGRT *grt, MGRTValue catalog)
{
  MGRTTableEditor *editor= 0;
  MGGladeXML *xml;
  
  xml= new MGGladeXML(myg_datadir+"/grt_table_editor.glade", "editor_window");
  
  xml->get_widget_derived("editor_window", editor);
  
  editor->_xml= xml;
  editor->set_grt(grt);
  editor->set_catalog(catalog);

  editor->setup();

  return editor;
}


void MGRTTableEditor::edit_object(MGRTValue object)
{
  _table_data= new MGRTTable(_grt->grt(), object.grtValue());
  show_object();
}


void MGRTTableEditor::create_new()
{
}


void MGRTTableEditor::refresh_reftable_list()
{
  unsigned int i;
  MGRTValue schema= _table_data->ownerSchema();
  if (schema.isValid())
  {
    MGRTValue tables(schema["tables"]);
    if (tables.isValid())
    {
      _fk_reftable_list->clear();
      
      for (i= 0; i < tables.count(); i++)
      {
        Gtk::TreeIter iter;
        Gtk::TreeRow row;
        iter= _fk_reftable_list->append();
        row= *iter;
        row[_columns.name]= tables[i]["name"].asString();
        row[_columns.grt]= tables[i];
      }
    }
  }
}


void MGRTTableEditor::refresh_refcolumn_list()
{
  unsigned int i;
  Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    MGRTValue fk(row[_fk_columns.grt]);
    MGRTValue table(MGRTValue::refObject(_grt->grt(), fk["referedTable"].asString()));
    
    if (table.isValid())
    {
      MGRTValue columns(table["columns"]);
      if (columns.isValid())
      {
        _fk_refcolumn_list->clear();
        
        for (i= 0; i < columns.count(); i++)
        {
          iter= _fk_refcolumn_list->append();
          row= *iter;
          row[_columns.name]= columns[i]["name"].asString();
          row[_columns.grt]= columns[i];
        }
      }
    }
  }
}



void MGRTTableEditor::refresh_column_list()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  MGRTValue oldsel;
    
  iter= _xml->get_tree("column_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    oldsel= row[_c_columns.grt];
  }
  
  _column_list->clear();
  _aux_column_names_list->clear();
  for (int i= 0; i < _table_data->columnCount(); i++)
  {
    MGRTValue column= _table_data->getColumn(i);
    
    iter= _column_list->append();
    row= *iter;
    
    row[_c_columns.name]= column["name"].asString();
    row[_c_columns.type]= _table_data->formatColumnType(column);
    row[_c_columns.pk]= _table_data->columnIsPK(column);
    row[_c_columns.flags]= _table_data->getEnabledColumnFlags(column).c_str();
    row[_c_columns.nnull]= column.get("isNullable",0) ? false : true;
    row[_c_columns.autoinc]= column.get("autoIncrement",0) ? true : false;
    row[_c_columns.defval]= !column.get("defaultValueIsNull",0) ? column["defaultValue"].asString() : "";
    row[_c_columns.defnull]= column.get("defaultValueIsNull",0) ? true : false;
    row[_c_columns.comment]= column.get("comment","");
    row[_c_columns.grt]= column;
    set_column_icon(iter);
    if (column.grtValue() == oldsel.grtValue())
      _xml->get_tree("column_tree")->get_selection()->select(iter);

    iter= _aux_column_names_list->append();
    row= *iter;
    
    row[_columns.name]= column["name"].asString();
    row[_columns.grt]= column;
  }
  iter= _column_list->append();
  row= *iter;
  row[_c_columns.placeholder]= true;
}



void MGRTTableEditor::delete_column()
{
  Gtk::TreeIter iter= _xml->get_tree("column_tree")->get_selection()->get_selected();
  
  if (iter)
  {
    Gtk::TreePath path(iter);
    _table_data->removeColumn(path[0]);
    refresh_column_list();
  }
}



void MGRTTableEditor::column_selected()
{
  Gtk::TreeIter iter= _xml->get_tree("column_tree")->get_selection()->get_selected();
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    show_column(row[_c_columns.grt]);
    _xml->get_widget("column_page")->set_sensitive(true);
  }
  else
    _xml->get_widget("column_page")->set_sensitive(false);
}


void MGRTTableEditor::select_last_item(Gtk::TreeView *tree)
{
  Gtk::TreeIter iter= tree->get_model()->children().end();
  --iter;
  if (iter)
  {
    --iter;
    if (iter)
      tree->get_selection()->select(iter);
  }
}


void MGRTTableEditor::column_value_edited(const Glib::ustring &path, const Glib::ustring &nvalue, int colnum)
{  
  if (Gtk::TreePath(path)[0] == (int)_column_list->children().size()-1)
  {
    if (nvalue != "")
    {
      _table_data->addColumn(colnum == 1 ? nvalue.c_str() : "new_column");
      refresh_column_list();
      select_last_item(_xml->get_tree("column_tree"));
    }
    else
      return;
  }

  Gtk::TreeIter iter= _column_list->get_iter(path);
  Gtk::TreeRow row= *iter;
  MGRTValue column(row[_c_columns.grt]);
  
  switch (colnum)
  {
  case 1:
    _table_data->setColumnName(column, nvalue.c_str());
    row[_c_columns.name]= nvalue;
    refresh_column_list();
    refresh_refcolumn_list();
    refresh_index_column_list();
    break;
  case 2:
    _table_data->setColumnType(column, nvalue.c_str());
    row[_c_columns.type]= _table_data->formatColumnType(column).c_str();
    break;
  case 5:
    _table_data->setEnabledColumnFlags(column, nvalue.c_str());
    row[_c_columns.flags]= _table_data->getEnabledColumnFlags(column).c_str();
    break;
  case 6:
    column.set("defaultValue", nvalue.c_str());
    if (!nvalue.empty())
      column.set("defaultValueIsNull", 0);
    row[_c_columns.defval]= nvalue;
    break;
  case 7:
    column.set("comment", nvalue.c_str());
    row[_c_columns.comment]= nvalue;
    break;
  }
  show_column(column);
}


void MGRTTableEditor::column_value_toggled(const Glib::ustring &path, int colnum)
{
  if (Gtk::TreePath(path)[0] == (int)_column_list->children().size()-1)
  {
    _table_data->addColumn("new_column");
    refresh_column_list();
    select_last_item(_xml->get_tree("column_tree"));
  }

  Gtk::TreeIter iter= _column_list->get_iter(path);
  Gtk::TreeRow row= *iter;
  MGRTValue column(row[_c_columns.grt]);
  
  switch (colnum)
  {
  case 0:
    _table_data->setColumnPK(column, !_table_data->columnIsPK(column));
    row[_c_columns.pk]= _table_data->columnIsPK(column);
    set_column_icon(iter);
    break;
  case 3:
    column.set("isNullable", (bool)row[_c_columns.nnull] ? 0 : 1);
    break;
  case 4:
    column.set("autoIncrement", (bool)row[_c_columns.autoinc] ? 1 : 0);
    break;
  }
  if (_xml->get_tree("column_tree")->get_selection()->get_selected())
  {
    if (path == Gtk::TreePath(_xml->get_tree("column_tree")->get_selection()->get_selected()).to_string())
      show_column(column);
  }
}


void MGRTTableEditor::set_column_icon(Gtk::TreeIter iter)
{
  Gtk::TreeRow row= *iter;
  MGRTValue column(row[_c_columns.grt]);
  Glib::RefPtr<Gdk::Pixbuf> img;

  if (_table_data->columnIsPK(column))
    img= _pk_icon;
  else
  {
    switch (_table_data->columnTypeGroup(column))
    {
    case RDG_NUMERIC:
      img= _numeric_icon;
      break;
    case RDG_DATETIME:
      img= _date_icon;
      break;
    case RDG_STRING:
      img= _string_icon;
      break;
    case RDG_TEXT:
      img= _string_icon;
      break;
    case RDG_BLOB:
      img= _blob_icon;
      break;
    case RDG_GEO:
      img= _spatial_icon;
      break;
    case RDG_USER:
      img= _userdef_icon;
      break;
    case RDG_STRUCTURED:
      img= _userdef_icon;
      break;
    case RDG_VARIOUS:
      img= _userdef_icon;
      break;
    }
  }
  row[_c_columns.icon]= img;
}


void MGRTTableEditor::column_flag_changed(Gtk::CheckButton *check, const char *flag)
{
  Gtk::TreeIter iter= _xml->get_tree("column_tree")->get_selection()->get_selected();
  Gtk::TreeRow row;
  
  if (_displaying)
    return;
  if (iter)
  {
    row= *iter;
    MGRTValue column= row[_c_columns.grt];

    if (column.isValid())
    {
      if (strcmp(flag, "pk")==0)
      {
        _table_data->setColumnPK(column, check->get_active());
        set_column_icon(iter);
      }
      else if (strcmp(flag, "isNullable")==0)
      {
        column.set("isNullable", check->get_active()?1:0);
        row[_c_columns.nnull]= !column.get("isNullable",0);
      }
      else if (strcmp(flag, "autoIncrement")==0)
      {
        column.set("autoIncrement", check->get_active()?1:0);
        row[_c_columns.autoinc]= column.get("autoIncrement", 0);
      }
      else
      {
        _table_data->setColumnFlagState(column, flag, check->get_active());
        row[_c_columns.flags]= _table_data->getEnabledColumnFlags(column).c_str();
      }
    }
  }
}


void MGRTTableEditor::column_entry_changed(Gtk::Widget *widget, const char *name)
{
  Gtk::TreeIter iter= _xml->get_tree("column_tree")->get_selection()->get_selected();
  Gtk::TreeRow row;

  if (_displaying)
    return;
  
  if (iter)
  {
    row= *iter;
    MGRTValue column= row[_c_columns.grt];

    if (column.isValid())
    {
      if (strcmp(name, "name")==0)
      {
        column.set("name", ((Gtk::Entry*)widget)->get_text().c_str());
        row[_c_columns.name]= column.get("name", "");
      }
      else if (strcmp(name, "type")==0)
      {
        _table_data->setColumnType(column, (((Gtk::Entry*)((Gtk::ComboBoxEntry*)widget)->get_child()))->get_text().c_str());
        row[_c_columns.type]= _table_data->formatColumnType(column).c_str();
      }
      else if (strcmp(name, "default")==0)
      {
        column.set("defaultValue", ((Gtk::Entry*)widget)->get_text().c_str());
        row[_c_columns.defval]= column.get("defaultValue", "");
      }
      else if (strcmp(name, "defaultNull")==0)
      {
        if (((Gtk::ToggleButton*)widget)->get_active())
        {
          _xml->get_entry("default_entry")->set_sensitive(false);
          column.set("defaultValueIsNull", 1);
          row[_c_columns.defval]= "";
        }
        else
        {
          _xml->get_entry("default_entry")->set_sensitive(true);
          column.set("defaultValueIsNull", 0);
          row[_c_columns.defval]= column.get("defaultValue", "");
        }
      }
      else if (strcmp(name, "comment")==0)
      {
        column.set("comment", ((Gtk::TextView*)widget)->get_buffer()->get_text().c_str());
        row[_c_columns.comment]= column.get("comment", "");
      }
      else if (strcmp(name, "collation")==0)
      {
        Gtk::TreeIter iter= ((Gtk::ComboBox*)widget)->get_active();
        Gtk::TreeRow row= *iter;
        Glib::ustring coll= row[_charset_columns.collation];
        column.set("collationName", coll.c_str());
      }
    }
  }
}


void MGRTTableEditor::show_column(MGRTValue column)
{  
  if (column.isValid())
  {
    _xml->get_entry("column_name_entry")->set_text((const char*)column["name"]);
    
    _xml->get_combo_entry_entry("column_type_combo")->set_text(_table_data->formatColumnType(column));

    Gtk::Box *box= _xml->get_box("flag_box");
    std::vector<Gtk::Widget*> children= box->get_children();
    while (children.size() > 3)
    {
      box->remove(**children.rbegin());
      children= box->get_children();
    }

    MGRTValue flags= _table_data->getColumnFlags(column);
    for (unsigned int i= 0; i < flags.count(); i++)
    {
      Gtk::CheckButton *check= Gtk::manage(new Gtk::CheckButton(flags[i].asString()));

      _xml->get_box("flag_box")->pack_start(*check, false, false);

      check->signal_toggled().connect(sigc::bind<Gtk::CheckButton*,const char*>(sigc::mem_fun(*this,&MGRTTableEditor::column_flag_changed),
                                                                                check, flags[i].asString()));
    }

    _xml->get_toggle("null_toggle")->set_active((int)column["defaultValueIsNull"] != 0);

    if ((int)column["defaultValueIsNull"] != 0)
    {
      _xml->get_entry("default_entry")->set_text("");
      _xml->get_entry("default_entry")->set_sensitive(false);
    }
    else
    {
      _xml->get_entry("default_entry")->set_text((const char*)column["defaultValue"]);
      _xml->get_entry("default_entry")->set_sensitive(true);
    }

    _xml->get_combo("column_collation_combo")->set_active(collation_iter(_column_collation,
                                                                         _table_data->getColumnCollation(column)?:""));

    _xml->get_text("column_comment_text")->get_buffer()->set_text(column["comment"].asString());

    
    _xml->get_toggle("pk_flag")->set_active(_table_data->columnIsPK(column));
    _xml->get_toggle("null_flag")->set_active(column.get("isNullable", 0));
    _xml->get_toggle("autoinc_flag")->set_active(column.get("autoIncrement", 0));

    _xml->get_toggle("pk_flag")->set_sensitive(true);
    _xml->get_toggle("null_flag")->set_sensitive(true);
    _xml->get_toggle("autoinc_flag")->set_sensitive(_table_data->columnIsNumeric(column));
  }
  else
  {
    _xml->get_entry("column_name_entry")->set_text("");
    _xml->get_combo_entry_entry("column_type_combo")->set_text("");    
    _xml->get_entry("default_entry")->set_text("");
    _xml->get_toggle("null_toggle")->set_active(false);

    Gtk::Box *box= _xml->get_box("flag_box");
    std::vector<Gtk::Widget*> children= box->get_children();
    while (children.size() > 3)
    {
      box->remove(**children.rbegin());
      children= box->get_children();
    }

    _xml->get_combo("column_collation_combo")->set_active(-1);

    _xml->get_text("column_comment_text")->get_buffer()->set_text("");

    _xml->get_toggle("pk_flag")->set_sensitive(false);
    _xml->get_toggle("autoinc_flag")->set_sensitive(false);
    _xml->get_toggle("null_flag")->set_sensitive(false);
  }
}


void MGRTTableEditor::index_value_edited(const Glib::ustring &path, const Glib::ustring &nvalue, int colnum)
{
  if (Gtk::TreePath(path)[0] == (int)_index_list->children().size()-1)
  {
    if (nvalue != "")
    {
      _table_data->addIndex(colnum == 0 ? nvalue.c_str() : "new_index");
      refresh_index_list();
      refresh_index_column_list();
      select_last_item(_xml->get_tree("index_tree"));
    }
    else
      return;
  }

  Gtk::TreeIter iter= _index_list->get_iter(path);
  Gtk::TreeRow row= *iter;
  MGRTValue index(row[_i_columns.grt]);
  
  switch (colnum)
  {
  case 0:
    index.set("name", nvalue.c_str());
    row[_i_columns.name]= nvalue;
    break;
  case 1:
    index.set("indexType", nvalue.c_str());
    row[_i_columns.type]= nvalue;
    break;
  case 2:
    index.set("comment", nvalue.c_str());
    row[_i_columns.comment]= nvalue;
    break;
  }
  refresh_index_list();
}


void MGRTTableEditor::index_column_value_edited(const Glib::ustring &path, const Glib::ustring &nvalue, int colnum)
{
  Gtk::TreeIter iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  Gtk::TreeRow row= *iter;
  MGRTValue index(row[_i_columns.grt]);

  if (Gtk::TreePath(path)[0] == (int)_index_column_list->children().size()-1)
  {
    if (nvalue != "")
    {
      _table_data->addColumnToIndex(index);
      refresh_index_column_list();
      select_last_item(_xml->get_tree("index_columns_tree"));
    }
    else
      return;
  }

  iter= _index_column_list->get_iter(path);
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    MGRTValue column(row[_ic_columns.grt]);

    switch (colnum)
    {
    case 0:
      _table_data->setIndexColumn(index, Gtk::TreePath(path)[0], nvalue.c_str());
      row[_ic_columns.column]= nvalue;
      break;
    case 1:
      column.set("descend", strcmp(nvalue.c_str(), "DESC")==0?1:0);
      row[_ic_columns.order]= nvalue;
      break;
    case 2:
      if (nvalue == "")
        column.set("columnLength", 0);
      else
        column.set("columnLength", atoi(nvalue.c_str()));
      row[_ic_columns.length]= atoi(nvalue.c_str());
      break;
    case 3:
      column.set("storedFunction", nvalue.c_str());
      row[_ic_columns.function]= nvalue;
      break;
    case 4:
      column.set("comment", nvalue.c_str());
      row[_ic_columns.comment]= nvalue;
      break;
    }
    refresh_index_column_list();
  }
}


void MGRTTableEditor::delete_index(bool column)
{
  if (column)
  {
    Gtk::TreeIter index_iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
    if (index_iter)
    {
      Gtk::TreeRow row= *index_iter;

      Gtk::TreeIter iter= _xml->get_tree("index_columns_tree")->get_selection()->get_selected();

      if (iter)
      {
        Gtk::TreePath path(iter);
        _table_data->removeColumnFromIndex(row[_i_columns.grt], path[0]);
        refresh_index_column_list();
      }
    }
  }
  else
  {
    Gtk::TreeIter iter= _xml->get_tree("index_tree")->get_selection()->get_selected();

    if (iter)
    {
      Gtk::TreePath path(iter);
      _table_data->removeIndex(path[0]);
      refresh_index_list();
    }
  }
}


void MGRTTableEditor::refresh_index_list()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  MGRTValue oldsel;
  
  iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    oldsel= row[_i_columns.grt];
  }

  _index_list->clear();
  for (int i= 0; i < _table_data->indexCount(); i++)
  {
    MGRTValue index(_table_data->getIndex(i));
    
    iter= _index_list->append();
    row= *iter;
    row[_i_columns.name]= index.get("name", "");
    row[_i_columns.type]= index.get("indexType", "");
    row[_i_columns.comment]= index.get("comment", "");
    row[_i_columns.grt]= index;
    if (oldsel.grtValue() == index.grtValue())
      _xml->get_tree("index_tree")->get_selection()->select(iter);
  }
  iter= _index_list->append();
  row= *iter;
  row[_i_columns.placeholder]= true;
}


void MGRTTableEditor::refresh_index_column_list()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  MGRTValue oldsel;

  iter= _xml->get_tree("index_columns_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    oldsel= row[_ic_columns.grt];
  }
  _index_column_list->clear();
  iter= _xml->get_tree("index_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    MGRTValue index(row[_i_columns.grt]);
    if (index.isValid())
    {
      for (int i= 0; i < _table_data->indexColumnCount(index); i++)
      {
        MGRTValue column(_table_data->getIndexColumn(index, i));
        iter= _index_column_list->append();
        row= *iter;
        row[_ic_columns.column]= column["name"].asString();
        row[_ic_columns.order]= column.get("descend", 0) == 0 ? "ASC" : "DESC";
        row[_ic_columns.length]= column.get("columnLength", 0);
        row[_ic_columns.function]= column.get("storedFunction", "");
        row[_ic_columns.comment]= column.get("comment", "");
        row[_ic_columns.grt]= column;
        if ((oldsel.grtValue() && column.grtValue() == oldsel.grtValue()) || (!oldsel.grtValue() && i == _table_data->indexColumnCount(index)-1))
          _xml->get_tree("index_columns_tree")->get_selection()->select(iter);
      }
      iter= _index_column_list->append();
      row= *iter;
      row[_ic_columns.placeholder]= true;
    }
  }
}



void MGRTTableEditor::fk_value_edited(const Glib::ustring &path, const Glib::ustring &nvalue, int colnum)
{
  if (Gtk::TreePath(path)[0] == (int)_fk_list->children().size()-1)
  {
    if (nvalue != "")
    {
      _table_data->addFK(colnum == 0 ? nvalue.c_str() : "new_fk");
      refresh_fk_list();
      refresh_fk_column_list();
      refresh_reftable_list();
      select_last_item(_xml->get_tree("fk_tree"));
    }
    else
      return;
  }

  Gtk::TreeIter iter= _fk_list->get_iter(path);
  Gtk::TreeRow row= *iter;
  MGRTValue fk(row[_fk_columns.grt]);
  
  switch (colnum)
  {
  case 0:
    fk.set("name", nvalue.c_str());
    row[_fk_columns.name]= nvalue;
    break;
  case 1:
    fk.set("deleteRule", nvalue.c_str());
    row[_fk_columns.ondelete]= nvalue;
    break;
  case 2:
    fk.set("updateRule", nvalue.c_str());
    row[_fk_columns.onupdate]= nvalue;
    break;
  case 3:
    {
      Gtk::TreeIter titer= _fk_reftable_list->children().begin();
      while (titer != _fk_reftable_list->children().end())
      {
        Gtk::TreeRow trow= *titer;
        if (trow[_columns.name] == nvalue)
        {
          _table_data->setFKRefTable(fk, trow[_columns.grt]);
          refresh_refcolumn_list();
          break;
        }
      }
    }
    row[_fk_columns.reftable]= nvalue;
    break;
  case 4:
    fk.set("comment", nvalue.c_str());
    row[_fk_columns.comment]= nvalue;
    break;
  }
  refresh_fk_column_list();  
}


void MGRTTableEditor::fk_column_value_edited(const Glib::ustring &path, const Glib::ustring &nvalue, int colnum)
{
  Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  Gtk::TreeRow row= *iter;
  MGRTValue fk(row[_fk_columns.grt]);

  if (Gtk::TreePath(path)[0] == (int)_fk_column_list->children().size()-1)
  {
    if (nvalue != "")
    {
      _table_data->addColumnToFK(fk);
      refresh_fk_column_list();
      select_last_item(_xml->get_tree("fk_columns_tree"));
    }
    else
      return;
  }
  iter= _fk_column_list->get_iter(path);
  if (iter)
  {
    Gtk::TreeRow row= *iter;
    int index= row[_fkc_columns.index];

    switch (colnum)
    {
    case 0:
      {
        Gtk::TreeIter citer= _aux_column_names_list->children().begin();
        while (citer != _aux_column_names_list->children().end())
        {
          Gtk::TreeRow crow= *citer;
          // find the column from this table that matches
          if (crow[_columns.name] == nvalue)
          {
            MGRTValue column(crow[_columns.grt]);
            _table_data->setFKColumn(fk, index, column.dictId());
            break;
          }
          ++iter;
        }
        row[_fkc_columns.column]= nvalue;
      }
      break;
    case 1:
      _table_data->setFKRefColumn(fk, index, nvalue.c_str());
      row[_fkc_columns.refcolumn]= nvalue;
      break;
    }
    refresh_fk_column_list();
  }
}


void MGRTTableEditor::delete_fk(bool column)
{
  if (column)
  {
    Gtk::TreeIter fk_iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
    if (fk_iter)
    {
      Gtk::TreeRow row= *fk_iter;
      Gtk::TreeIter iter= _xml->get_tree("fk_columns_tree")->get_selection()->get_selected();
    
      if (iter)
      {
        Gtk::TreePath path(iter);
        _table_data->removeColumnFromFK(row[_fk_columns.grt], path[0]);
        refresh_fk_column_list();
      }
    }
  }
  else
  {
    Gtk::TreeIter iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
    
    if (iter)
    {
      Gtk::TreePath path(iter);
      _table_data->removeFK(path[0]);
      refresh_fk_list();
    }
  }
}


void MGRTTableEditor::refresh_fk_list()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  MGRTValue oldsel;
  
  iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    oldsel= row[_fk_columns.grt];
  }
  _fk_list->clear();
  for (int i= 0; i < _table_data->fkCount(); i++)
  {
    MGRTValue fk(_table_data->getFK(i));
    
    iter= _fk_list->append();
    row= *iter;
    row[_fk_columns.name]= fk.get("name", "");
    row[_fk_columns.onupdate]= fk.get("updateRule", "");
    row[_fk_columns.ondelete]= fk.get("deleteRule", "");
    row[_fk_columns.reftable]= fk.get("referedTableName", "");
    row[_fk_columns.comment]= fk.get("comment", "");
    row[_fk_columns.grt]= fk;
    if (fk.grtValue() == oldsel.grtValue())
      _xml->get_tree("fk_tree")->get_selection()->select(iter);
  }
  iter= _fk_list->append();
  row= *iter;
  row[_fk_columns.placeholder]= true;
}


void MGRTTableEditor::refresh_fk_column_list()
{
  Gtk::TreeIter iter;
  Gtk::TreeRow row;
  int oldsel;
  
  iter= _xml->get_tree("fk_columns_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    oldsel= row[_fkc_columns.index];
  }
  _fk_column_list->clear();
  iter= _xml->get_tree("fk_tree")->get_selection()->get_selected();
  if (iter)
  {
    row= *iter;
    MGRTValue fk(row[_fk_columns.grt]);
    
    if (fk.isValid())
    {
      for (int i= 0; i < _table_data->fkColumnCount(fk); i++)
      {
        iter= _fk_column_list->append();
        row= *iter;
        g_message("%s", _table_data->getFKColumn(fk, i));
        row[_fkc_columns.column]= _table_data->getFKColumn(fk, i);
        row[_fkc_columns.refcolumn]= _table_data->getFKRefColumn(fk, i);
        row[_fkc_columns.index]= i;
        if (i == oldsel)
          _xml->get_tree("fk_columns_tree")->get_selection()->select(iter);
      }
      iter= _fk_column_list->append();
      row= *iter;
      row[_fkc_columns.placeholder]= true;
    }
  }
}


bool MGRTTableEditor::tree_key_up(GdkEventKey *event, Gtk::TreeView *tree)
{
  if (event->keyval == GDK_Tab || event->keyval == GDK_Delete)
  {
    Gtk::TreeViewColumn *column;
    Gtk::TreePath path;
    tree->get_cursor(path, column);

    if (event->keyval == GDK_Tab)
    {
      if (tree == _xml->get_tree("column_tree"))
      {
        if (column == tree->get_column(1))
          tree->set_cursor(path, *tree->get_column(2), true);
        else if (column == tree->get_column(2))
          tree->set_cursor(path, *tree->get_column(5), true);
        else if (column == tree->get_column(5))
          tree->set_cursor(path, *tree->get_column(6), true);
        else if (column == tree->get_column(6))
          tree->set_cursor(path, *tree->get_column(7), true);
        else
        {
          path.next();      
          if (_column_list->get_iter(path))
            tree->set_cursor(path, *tree->get_column(1), true);
        }
      }
      else if (tree == _xml->get_tree("index_tree"))
      {
        if (column == tree->get_column(0))
          tree->set_cursor(path, *tree->get_column(2), true);
        else
        {
          path.next();      
          if (_column_list->get_iter(path))
            tree->set_cursor(path, *tree->get_column(0), true);
        }
      }
      else if (tree == _xml->get_tree("fk_tree"))
      {
        if (column == tree->get_column(0))
          tree->set_cursor(path, *tree->get_column(4), true);
        else
        {
          path.next();      
          if (_column_list->get_iter(path))
            tree->set_cursor(path, *tree->get_column(0), true);
        }
      }
    }
    else if (event->keyval == GDK_Delete)
    {
      if (tree == _xml->get_tree("column_tree"))
      {
        if (tree == static_cast<Gtk::Window*>(tree->get_toplevel())->get_focus())
          delete_column();
      }
      else if (tree == _xml->get_tree("index_tree"))
      {
        if (tree == static_cast<Gtk::Window*>(tree->get_toplevel())->get_focus())
          delete_index(false);
      }
      else if (tree == _xml->get_tree("index_columns_tree"))
      {
        if (tree == static_cast<Gtk::Window*>(tree->get_toplevel())->get_focus())
          delete_index(true);
      }
      else if (tree == _xml->get_tree("fk_tree"))
      {
        if (tree == static_cast<Gtk::Window*>(tree->get_toplevel())->get_focus())
          delete_fk(false);
      }
      else if (tree == _xml->get_tree("fk_columns_tree"))
      {
        if (tree == static_cast<Gtk::Window*>(tree->get_toplevel())->get_focus())
          delete_fk(true);
      }
    }
    return true;
  }
  return false;
}


