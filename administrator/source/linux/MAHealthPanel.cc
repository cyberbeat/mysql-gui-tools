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
#include "MAHealthPanel.h"
#include "MInstanceInfo.h"
#include "MDataInterface.h"

#include "myg_utils.h"
#include "myg_gtkutils.h"
#include "myx_admin_library.h"

#define BAR_GRAPH_HEIGHT 21
#define GRAPH_HEIGHT  80


class MAHealthPanel::BaseGraph {
  protected:
    MYX_VARIABLES *_old_values;
    MYX_VARIABLES *_values;
    MYX_VARIABLES *_server_values;
    bool _server_values_changed;

    double compute(const Glib::ustring &s)
    {
      MYX_COMPILED_EXPRESSION *expr;
      MYX_EXPRESSION_ERROR err;
      Glib::ustring ss;
      double res;

      ss= replace_variables(s);
      expr= myx_compile_expression(ss.c_str(), &err);
      if (!expr)
        return 0;
      res= myx_eval_expression(expr, _old_values, _values, &err);
      myx_free_expression(expr);
      
      return res;
    }
    
    Glib::ustring replace_variables(const Glib::ustring &expr, bool server_only=true)
    {
      Glib::ustring s= expr;

      if (_values && !server_only)
      {
        // replace status variables
        for (unsigned int i= 0; i < _values->variables_num; i++)
        {
          Glib::ustring::size_type sbeg;
          int len= strlen((char*)_values->variables[i].name);
          
          while ((sbeg= s.find((char*)_values->variables[i].name)) != std::string::npos)
          {
            s= s.replace(sbeg, len, tostr(i));
          }
        }
      }

      // replace server variable static values
      for (unsigned int i= 0; i < _server_values->variables_num; i++)
      {
        Glib::ustring::size_type sbeg;
        Glib::ustring tmp= ufmt("[%s]", (char*)_server_values->variables[i].name);
        
        while ((sbeg= s.find(tmp)) != std::string::npos)
        {
          s= s.replace(sbeg, tmp.length(), (char*)_server_values->variables[i].value?:"0");
        }
      }
      return s;
    }

  public:
    BaseGraph() : _old_values(0), _values(0), _server_values(0), _server_values_changed(0) {};
    virtual ~BaseGraph() {};
    virtual Gtk::Widget *get_widget()= 0;
    virtual Gtk::Widget *get_aligned_widget()= 0;
    virtual void set_style(const Glib::RefPtr<Gtk::Style> &style)= 0;
    virtual void update_server_values(MYX_VARIABLES *vars)
    {
      _server_values_changed= true;
      _server_values= vars;
    }
    virtual void update_values(MYX_VARIABLES *vars)= 0;
    virtual void modify(MYX_HEALTH_GRAPH *graph)= 0;
};


class TimeGraph : public MAHealthPanel::BaseGraph {
    Gtk::Table *_table;
    MGTimeGraphPlotter *_graph;
    MGMeterGraphPlotter *_meter;
    Glib::ustring _expr;
    Glib::ustring _caption;
    MYX_COMPILED_EXPRESSION *_cexpr;
    Gtk::Label *_label[4];
    Glib::RefPtr<Gdk::Pixmap> _back;
    Glib::RefPtr<Gdk::Pixmap> _fore;
    Glib::RefPtr<Gtk::Style> _style;
    Gdk::Color _line_color;
    Gdk::Color _grid_color;
    double _min, _max;
    Glib::ustring _max_formula;
  public:
    TimeGraph(MYX_HEALTH_GRAPH *graph)
      : _meter(0), _cexpr(0)
    {
      _table= Gtk::manage(new Gtk::Table(2,2));
      _table->show();
      
      _table->set_col_spacings(8);

      _graph= Gtk::manage(new MGTimeGraphPlotter);
      _table->attach(*_graph, 1, 2, 0, 1);
      _graph->set_size_request(-1, GRAPH_HEIGHT);

      _graph->show();
      
      Gtk::HBox *hbox= Gtk::manage(new Gtk::HBox(true, 0));
      _table->attach(*hbox, 1, 2, 1, 2);
      hbox->show();

      for (int i= 0; i < 4; i++)
      {
        switch (i)
        {
        case 0: _label[i]= Gtk::manage(new Gtk::Label(_("Current: 0"), 0.0, 0.5)); break;
        case 1: _label[i]= Gtk::manage(new Gtk::Label(_("Maximal: 0"), 0.0, 0.5)); break;
        case 2: _label[i]= Gtk::manage(new Gtk::Label(_("Minimal: 0"), 0.0, 0.5)); break;
        case 3: _label[i]= Gtk::manage(new Gtk::Label(_("Average: 0"), 0.0, 0.5)); break;
        }
        _label[i]->set_selectable(true);
        _label[i]->set_padding(4, 0);
        _label[i]->show();
        Gtk::Frame *frame= Gtk::manage(new Gtk::Frame);
        frame->add(*_label[i]);
        frame->set_shadow_type(Gtk::SHADOW_IN);
        hbox->pack_start(*frame, true, true);
        frame->show();
      }

      modify(graph);
    }
    virtual void modify(MYX_HEALTH_GRAPH *graph)
    {
      if (graph->value_caption)
      {
        if (!_meter)
        {
          _meter= Gtk::manage(new MGMeterGraphPlotter);
          _meter->set_segment_number(10);
          _meter->set_size_request(GRAPH_HEIGHT, GRAPH_HEIGHT);
          _table->attach(*_meter, 0, 1, 0, 1, Gtk::FILL);
          _meter->show();
          _meter->set_style(_style);
          _meter->set_images(_back, _fore);
        }
      }
      else
      {
        if (_meter)
          delete _meter;
        _meter= 0;
      }

      if (_meter)
        _meter->set_text_format(Glib::ustring((char*)graph->value_caption?:"")+" %i%%");

      if (graph->graph_caption && graph->display_graph_caption)
        _graph->set_caption(graph->graph_caption);
      else
        _graph->set_caption("");

      _graph->set_line_color(_line_color);
      _graph->set_grid_color(_grid_color);
      _graph->set_style(_style);
      
      _min= graph->min;
      _max= graph->max;
      _graph->set_range(graph->min, graph->max);
      
      if (graph->max_formula)
        _max_formula= graph->max_formula;

      _caption= graph->graph_caption?:"";
      _expr= graph->value_formula;
      if (_cexpr)
        myx_free_expression(_cexpr);
      _cexpr= NULL;
    }
    virtual ~TimeGraph()
    {
      delete _table;
      if (_cexpr)
        myx_free_expression(_cexpr);
    }
    virtual Gtk::Widget *get_widget() { return _table; };
    virtual Gtk::Widget *get_aligned_widget() { return 0; };
    virtual void set_style(const Glib::RefPtr<Gtk::Style> &style)
    {
      _style= style;
      _graph->set_style(style);
      if (_meter)
        _meter->set_style(style);
    }
    virtual void set_colors(const Gdk::Color &line_color,
                            const Gdk::Color &grid_color)
    {
      _line_color= line_color;
      _grid_color= grid_color;
      _graph->set_line_color(line_color);
      _graph->set_grid_color(grid_color);
    }
    virtual void set_images(const Glib::RefPtr<Gdk::Pixmap> &back_image,
                            const Glib::RefPtr<Gdk::Pixmap> &fore_image)
    {
      _back= back_image;
      _fore= fore_image;
      if (_meter)
        _meter->set_images(back_image, fore_image);
    }

    virtual void update_values(MYX_VARIABLES *vars)
    {
      MYX_EXPRESSION_ERROR err;

      _old_values= _values;
      _values= vars;

      if (_server_values_changed)
      {
        Glib::ustring expr= replace_variables(_expr, false);
        if (_cexpr)
          myx_free_expression(_cexpr);
        _cexpr= myx_compile_expression(expr.c_str(), &err);
        if (!_cexpr)
        {
          g_warning("could not compile expression %s", expr.c_str());
        }
        _server_values_changed= false;

        if (!_max_formula.empty())
        {
          _max= compute(_max_formula);
          _graph->set_range(_min, _max);
        }
      }

      if (_cexpr && _old_values)
      {
        double value;
        value= myx_eval_expression(_cexpr, _old_values, _values, &err);
        
        if (value > _max)
          _max= value;
        
        _graph->add_value(value, time(NULL));
        if (_meter)
          _meter->set_value((unsigned int)((value-_min)*100/_max));
        
        double max, min, avg;
        _graph->get_stats(min, max, avg);
        _label[0]->set_text(ufmt(_("Current: %.0f"), value));
        _label[1]->set_text(ufmt(_("Maximal: %.0f"), max));
        _label[2]->set_text(ufmt(_("Minimal: %.0f"), min));
        _label[3]->set_text(ufmt(_("Average: %.0f"), avg));
      }
    }
};


class BarGraph : public MAHealthPanel::BaseGraph {
    Gtk::HBox *_hbox;
    Gtk::Label *_label;
    MGHMeterGraphPlotter *_graph;
    Glib::ustring _expr, _max_expr;
    MYX_COMPILED_EXPRESSION *_cexpr, *_max_cexpr;
  public:
    BarGraph(MYX_HEALTH_GRAPH *graph)
      : _cexpr(0), _max_cexpr(0)
    {
      _hbox= Gtk::manage(new Gtk::HBox(false, 8));
      _hbox->show();

      _label= Gtk::manage(new Gtk::Label((char*)graph->graph_caption?:"", 0.0, 0.5));
      _label->show();
      _hbox->pack_start(*_label, false, true);

      _graph= Gtk::manage(new MGHMeterGraphPlotter());
      _graph->show();
      _hbox->pack_start(*_graph, true, true);
      _graph->set_size_request(-1, BAR_GRAPH_HEIGHT);

      modify(graph);

      _hbox->show();
    }
    virtual void modify(MYX_HEALTH_GRAPH *graph)
    {
      if (graph->value_caption)
        _graph->set_current_format(Glib::ustring((char*)graph->value_caption?:""));
      else
        _graph->set_current_format("");

      _graph->set_value_unit((MGHMeterGraphPlotter::Unit)graph->value_unit);
      // this caused behaviour described in bug #11631 (maybe not a bug?)
      /*
      if (graph->max_caption)
        _graph->set_total_format(Glib::ustring((char*)graph->max_caption));
      else
        _graph->set_total_format("");
      */
      _expr= graph->value_formula?:"";
      _max_expr= graph->max_formula?:"";

      if (_cexpr)
        myx_free_expression(_cexpr);
      _cexpr= 0;
    }
    virtual ~BarGraph()
    {
      delete _hbox;
      if (_cexpr)
        myx_free_expression(_cexpr);
    }
    virtual Gtk::Widget *get_widget() { return _hbox; };
    virtual Gtk::Widget *get_aligned_widget() { return _label; };
    virtual void set_style(const Glib::RefPtr<Gtk::Style> &style)
    {
      _graph->set_style(style);
    }
    virtual void set_images(const Glib::RefPtr<Gdk::Pixbuf> &back_image,
                            const Glib::RefPtr<Gdk::Pixbuf> &fore_image)
    {
      _graph->set_images(back_image, fore_image);
    }
    virtual void update_values(MYX_VARIABLES *vars)
    {
      MYX_EXPRESSION_ERROR err;

      _old_values= _values;
      _values= vars;

      if (_server_values_changed)
      {
        Glib::ustring expr= replace_variables(_expr, false);
        if (_cexpr)
          myx_free_expression(_cexpr);
        _cexpr= myx_compile_expression(expr.c_str(), &err);
        if (!_cexpr)
          g_warning("could not compile expression %s", expr.c_str());

        expr= replace_variables(_max_expr, false);
        if (_max_cexpr)
          myx_free_expression(_max_cexpr);
        _max_cexpr= myx_compile_expression(expr.c_str(), &err);
        if (!_max_cexpr)
          g_warning("could not compile expression %s", expr.c_str());

        _server_values_changed= false;
      }

      if (_cexpr && _old_values)
      {
        double value;
        value= myx_eval_expression(_cexpr, _old_values, _values, &err);
        _graph->set_current_value(value);

        value= myx_eval_expression(_max_cexpr, _old_values, _values, &err);
        _graph->set_total_value(value);
      }
    }
};




//--------------------------------------------------

MAHealthPanel::MAHealthPanel(MAdministrator *app, MDataInterface *data)
    : MAPanel(app, data), _fetching(false), _graphs_ready(false),
     _instance(data->get_instance()), _graph_defs(0)
{
  for (int i=0; i < 2; i++)
  {
    _var_values[i]= 0;
    _var_lists[i]= 0;
  }
  
  _status_values= 0;
  _old_status_values= 0;
  
  Gtk::MenuItem *item;

  // setup graph menu
  item= Gtk::manage(new Gtk::MenuItem(_("Add Graph")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::add_graph_mi));
  _graph_menu.append(*item);

  item= Gtk::manage(new Gtk::MenuItem(_("Edit Graph")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::edit_graph_mi));
  _graph_menu.append(*item);

  _graph_menu.append(*Gtk::manage(new Gtk::SeparatorMenuItem()));
  item= Gtk::manage(new Gtk::ImageMenuItem(*Gtk::manage(new Gtk::Image(Gtk::Stock::DELETE,Gtk::ICON_SIZE_MENU)),
                                           _("Delete Graph")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::delete_graph_mi));
  _graph_menu.append(*item);
  _graph_menu.show_all();

  // setup group menu
  item= Gtk::manage(new Gtk::MenuItem(_("Add a Group")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::add_group_mi));
  _group_menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("Rename Group")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::rename_group_mi));
  _group_menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("Add a Graph")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::add_graph_mi));
  _group_menu.append(*item);
  _group_menu.append(*Gtk::manage(new Gtk::SeparatorMenuItem()));
  item= Gtk::manage(new Gtk::ImageMenuItem(*Gtk::manage(new Gtk::Image(Gtk::Stock::DELETE,Gtk::ICON_SIZE_MENU)),
                                           _("Delete Group")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::delete_group_mi));
  _group_menu.append(*item);
  _group_menu.show_all();
  
  // setup page menu
  item= Gtk::manage(new Gtk::MenuItem(_("Add a Page")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::add_page_mi));
  _page_menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("Edit Page")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::rename_page_mi));
  _page_menu.append(*item);
  item= Gtk::manage(new Gtk::MenuItem(_("Add Group")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::add_group_mi));
  _page_menu.append(*item);
  _page_menu.append(*Gtk::manage(new Gtk::SeparatorMenuItem()));
  item= Gtk::manage(new Gtk::ImageMenuItem(*Gtk::manage(new Gtk::Image(Gtk::Stock::DELETE,Gtk::ICON_SIZE_MENU)),
                                           _("Delete Page")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::delete_page_mi));
  _page_menu.append(*item);
  _page_menu.append(*Gtk::manage(new Gtk::SeparatorMenuItem()));
  item= Gtk::manage(new Gtk::ImageMenuItem(*Gtk::manage(new Gtk::Image(Gtk::Stock::UNDO,Gtk::ICON_SIZE_MENU)),
                                           _("Restore Defaults")));
  item->signal_activate().connect(sigc::mem_fun(*this,&MAHealthPanel::restore_defaults_mi));
  _page_menu.append(*item);
  _page_menu.show_all();
  
  _instance->signal_disconnect().connect(sigc::mem_fun(*this,&MAHealthPanel::handle_disconnect));
}


MAHealthPanel::~MAHealthPanel()
{
  for (int i= 0; i < 2; i++)
  {
    if (_var_lists[i])
      myx_free_variables_listing(_var_lists[i]);
    if (_var_values[i])
      myx_free_variables(_var_values[i]);
  }
  
  if (_status_values)
    myx_free_variables(_status_values);

  if (_old_status_values)
    myx_free_variables(_old_status_values);

//  for (unsigned int i= 0; i < _graphs.size(); i++)
//    delete _graphs[i];

  delete _new_page_dlg;
  delete _new_group_dlg;
  delete _graph_dlg;

  //XXX
//  if (_fetching)
//      _instance->cancel_async_data_fetch2("status");
}


Glib::ustring MAHealthPanel::replace_variables(const Glib::ustring &expr)
{ 
  Glib::ustring s= expr;
  MYX_VARIABLES *vars= _status_values;

  if (vars)
  {
    // replace status variables
    for (unsigned int i= 0; i < vars->variables_num; i++)
    {
      Glib::ustring::size_type sbeg;
      int len= strlen((char*)vars->variables[i].name);

      while ((sbeg= s.find((char*)vars->variables[i].name)) != std::string::npos)
      {
        s= s.replace(sbeg, len, tostr(i));
      }
    }
  }

  if (_var_values[VServer])
  {
    // replace server variable static values
    for (unsigned int i= 0; i < _var_values[VServer]->variables_num; i++)
    {
      Glib::ustring::size_type sbeg;
      int len= strlen((char*)_var_values[VServer]->variables[i].name);
      
      while ((sbeg= s.find((char*)_var_values[VServer]->variables[i].name)) != std::string::npos)
      {
        s= s.replace(sbeg, len, (char*)_var_values[VServer]->variables[i].value?:"0");
      }
    }
  }
  
  return s;
}



bool MAHealthPanel::init()
{
  Glib::RefPtr<Gdk::Colormap> cmap;
  MYX_ADMIN_LIB_ERROR merror;

  if (_xml)
    return true;

  if (!MAPanel::init_from_glade(get_glade_file(GLADE_HEALTH_FILE), "panel_frame"))
    return false;

  get_button("info_button")->signal_clicked().connect(sigc::mem_fun(*_app,&MAdministrator::open_merlin_page));
  
  _new_page_dlg= new MGGladeXML(get_glade_file(GLADE_HEALTH_FILE), "new_page_dialog");
  _new_group_dlg= new MGGladeXML(get_glade_file(GLADE_HEALTH_FILE), "new_group_dialog");
  _graph_dlg= new MGGladeXML(get_glade_file(GLADE_HEALTH_FILE), "graph_dialog");

  _var_lists[VStatus]= myx_get_variables_listing(get_app_file("mysqladmin_status_variables.xml").c_str(),
                                           &merror);
  if (!_var_lists[VStatus])
  {
    g_warning("could not load file '%s'", get_app_file("mysqladmin_status_variables.xml").c_str());
  }
  _var_lists[VServer]= myx_get_variables_listing(get_app_file("mysqladmin_system_variables.xml").c_str(),
                                                 &merror);
  if (!_var_lists[VServer])
  {
    g_warning("could not load file '%s'", get_app_file("mysqladmin_system_variables.xml").c_str());
  }


  {
    Glib::RefPtr<Gdk::Bitmap> mask;
    Glib::RefPtr<Gdk::Pixbuf> pbuf;

    pbuf= PIXCACHE->load("health_level_bg.png");
    pbuf->render_pixmap_and_mask(_level_back, mask, 100);

    pbuf= PIXCACHE->load("health_level_fg.png");
    pbuf->render_pixmap_and_mask(_level_fore, mask, 100);
  }
  _usage_fore= PIXCACHE->load("disk_level_fg.png");
  _usage_back= PIXCACHE->load("disk_level_bg.png");

  _type_icon= PIXCACHE->load("16x16_ServerParams.png");
  _var_ro_icon= PIXCACHE->load("variable.png");
  _var_rw_icon= PIXCACHE->load("variable_editable.png");

  cmap= _panel->get_default_colormap();

  _line_color= Gdk::Color("#00aaff");
  cmap->alloc_color(_line_color);

  _grid_color= Gdk::Color("#113344");
  cmap->alloc_color(_grid_color);

  _graph_style= _xml->get_widget("notebook")->get_style()->copy(); //arbitrary object
  _graph_style->set_bg(Gtk::STATE_NORMAL, _graph_style->get_black());
  _graph_style->set_text(Gtk::STATE_NORMAL, _line_color);
  {
    Pango::FontDescription font;
    font= _graph_style->get_font();
    font.set_size(9*PANGO_SCALE);
    _graph_style->set_font(font);
  }

  // setup variable lists
  _type_tree[VStatus]= get_tree("status_type_tree");
  _type_tree[VServer]= get_tree("server_type_tree");
  
  _value_tree[VStatus]= get_tree("status_value_list");
  _value_tree[VServer]= get_tree("server_value_list");

  setup_value_list(get_tree("status_value_list"));
  setup_value_list(get_tree("server_value_list"));

  setup_type_tree(VStatus);
  setup_type_tree(VServer);

  // setup other signals
  get_button("refresh_status_button")->signal_clicked().connect(sigc::bind<int>(sigc::mem_fun(*this,&MAHealthPanel::refresh_list),0));
  get_button("refresh_server_button")->signal_clicked().connect(sigc::bind<int>(sigc::mem_fun(*this,&MAHealthPanel::refresh_list),1));
  
  return true;
}


void MAHealthPanel::handle_disconnect()
{
  if (_graph_defs)
  {
    if (myx_save_health_pages(_graph_defs, (char*)prefs.build_path_to("administrator/mysqladmin_health.xml").c_str()) < 0)
      g_warning("could not write to '%s'",(char*)prefs.build_path_to("administrator/mysqladmin_health.xml").c_str());
  }

  // destroy all graph pages
  if (_graphs_ready)
  {
    while (_pages.begin()!=_pages.end())
    {
      GraphPage *page= *_pages.begin();

      remove_page(page);
    }
  }
  _graphs_ready= false;
  
  if (_graph_defs)
  {
    myx_free_health_pages(_graph_defs);
    _graph_defs= NULL;
  }

  // stop async fetch
  if (_fetching)
  {
    _instance->cancel_async_data_fetch("status");
    _fetching= false;
  }
}


void MAHealthPanel::setup_type_tree(int index)
{
  Gtk::TreeView *tree= _type_tree[index];
  MYX_VARIABLES_LISTING *vars= _var_lists[index];

  Glib::RefPtr<Gtk::TreeStore> store;

  tree->append_column("", _type_columns._icon);
  tree->append_column("", _type_columns._text);
  
  
  store= Gtk::TreeStore::create(_type_columns);
  tree->set_model(store);

  if (vars)
  {
    for (unsigned int i= 0; i < vars->groups_num; i++)
    {
      Gtk::TreeIter iter= store->append();
      Gtk::TreeModel::Row row= *iter;

      row[_type_columns._icon]= _type_icon;
      //row[_type_columns._text]= _transl->get("MySQLVariables",
      //                                       (char*)vars->groups[i].name);
      row[_type_columns._text]= _((char*)vars->groups[i].name);
      row[_type_columns._vars]= vars->groups[i].variables;
      row[_type_columns._var_counts]= vars->groups[i].variables_num;

      for (unsigned int g= 0; g < vars->groups[i].subgroups_num; g++)
      {
        Gtk::TreeIter giter= store->append(row.children());
        Gtk::TreeModel::Row grow= *giter;

        grow[_type_columns._icon]= _type_icon;
        //grow[_type_columns._text]= _transl->get("MySQLVariables",
        //                                   (char*)vars->groups[i].subgroups[g].name);
        grow[_type_columns._text]= _((char*)vars->groups[i].subgroups[g].name);
        grow[_type_columns._vars]= vars->groups[i].subgroups[g].variables;
        grow[_type_columns._var_counts]= vars->groups[i].subgroups[g].variables_num;
      }
    }
  }

  tree->expand_all();

  Glib::RefPtr<Gtk::TreeSelection> sel= tree->get_selection();
  if (store->children().begin())
    sel->select(store->children().begin());
  sel->set_mode(Gtk::SELECTION_BROWSE);
  sel->signal_changed().connect(sigc::bind<int>
                               (sigc::mem_fun(*this, &MAHealthPanel::type_selected),
                                index));
}


void MAHealthPanel::setup_value_list(Gtk::TreeView *tree)
{
  Glib::RefPtr<Gtk::ListStore> store;
  Gtk::CellRendererText *rend;
  
  Gtk::TreeView::Column *column= new Gtk::TreeView::Column(_("Variable"));
  column->pack_start(_value_columns._icon, false);
  column->pack_start(_value_columns._name);
  tree->append_column(*Gtk::manage(column));
  tree->append_column_editable(_("Value"), _value_columns._value);
  column= tree->get_column(1);
  rend= (Gtk::CellRendererText*)*column->get_cell_renderers().begin();
  column->add_attribute(rend->property_editable(),
                        _value_columns._editable);
  rend->signal_edited().connect(sigc::mem_fun(*this,&MAHealthPanel::variable_edited));

  tree->append_column(_("Description"), _value_columns._descr);

  for (int i= 0; i < 3; i++)
  {
    tree->get_column(i)->set_resizable(true);
  }

  store= Gtk::ListStore::create(_value_columns);
  tree->set_model(store);
}


void MAHealthPanel::add_page(MYX_HEALTH_PAGE *page)
{
  GraphPage *page_info= new GraphPage;
  page_info->box= Gtk::manage(new Gtk::VBox(false, 6));
  page_info->box->set_border_width(12);
  
  page_info->page= page;

  Gtk::HBox *hbox= Gtk::manage(new Gtk::HBox(false, 6));
  hbox->pack_start(*Gtk::manage(new Gtk::Image(PIXCACHE->load("health.png"))), false, true);
  Gtk::Label *label= Gtk::manage(new Gtk::Label(page->description?:"", 0.0, 0.5));
  hbox->pack_start(*label, true, true);
  page_info->box->pack_start(*hbox, false, true);
  
  Gtk::HSeparator *sep= Gtk::manage(new Gtk::HSeparator);
  page_info->box->pack_start(*sep, false, true);

  page_info->sizer= Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);

  for (unsigned int g= 0; g < page->groups_num; g++)
  {
    MYX_HEALTH_GROUP *group= page->groups+g;

    add_group(page_info, group);
  }
  
  label= Gtk::manage(new Gtk::Label);
  label->set_alignment(0.0, 0.5);
  label->set_markup(_("<small>Right-click in graphs, frames or elsewhere in the page for graph edition menu.</small>"));
  page_info->box->pack_end(*label, false, false);
  

  Gtk::EventBox *evbox= Gtk::manage(new Gtk::EventBox);
  evbox->add(*page_info->box);
  evbox->signal_button_press_event().connect(sigc::bind<GraphPage*>(sigc::mem_fun(*this,&MAHealthPanel::page_button_press),
                                                                   page_info));

  evbox->show_all();

  page_info->description= label;
  page_info->caption= Gtk::manage(new Gtk::Label(page->caption?:""));
  page_info->evbox= evbox;
  
  _pages.push_back(page_info);
  Gtk::Notebook *note= (Gtk::Notebook*)get_widget("notebook");
  note->insert_page(*evbox, *page_info->caption, note->get_n_pages()-3);
}


void MAHealthPanel::add_group(GraphPage *page, MYX_HEALTH_GROUP *group)
{
  Gtk::Frame *frame= Gtk::manage(new Gtk::Frame(group->caption?:""));
  GraphGroup *group_info= new GraphGroup;

  group_info->frame= frame;
  group_info->page= page;
  group_info->group= group;
  group_info->box= Gtk::manage(new Gtk::VBox(false,8));
  group_info->box->set_border_width(8);
  
  page->groups.push_back(group_info);

  for (unsigned int h= 0; h < group->graphs_num; h++)
  {
    MYX_HEALTH_GRAPH *graph= group->graphs+h;
    
    add_graph(group_info, graph);
  }
  
  frame->add(*group_info->box);

  Gtk::EventBox *evbox= Gtk::manage(new Gtk::EventBox);
  evbox->add(*frame);
  evbox->signal_button_press_event().connect(sigc::bind<GraphGroup*>(sigc::mem_fun(*this,&MAHealthPanel::group_button_press),
                                                                     group_info));
  page->box->pack_start(*evbox, false, true);

  page->box->show_all();
}


MAHealthPanel::GraphItem *MAHealthPanel::add_graph(GraphGroup *group, MYX_HEALTH_GRAPH *graph)
{
  GraphItem *item= new GraphItem;

  item->group= group;
  item->def= graph;
  group->graphs.push_back(item);

  switch (graph->graphtype)
  {
  case MYX_LINE_GRAPH:
    {
      TimeGraph *tg= new TimeGraph(graph);
      
      tg->set_style(_graph_style);
      tg->set_colors(_line_color, _grid_color);
      tg->set_images(_level_back, _level_fore);
      
      {
        Gtk::EventBox *evbox= Gtk::manage(new Gtk::EventBox);
        evbox->add(*tg->get_widget());
        evbox->signal_button_press_event().connect(sigc::bind<GraphItem*>(sigc::mem_fun(*this,&MAHealthPanel::graph_button_press),
                                                                          item));
        group->box->pack_start(*evbox, true, true);
        item->evbox= evbox;
      }
      item->graph= tg;
      
      if (tg->get_aligned_widget())
        group->page->sizer->add_widget(*tg->get_aligned_widget());
    }
    break;
  case MYX_BAR_GRAPH:
    {
      BarGraph *bg= new BarGraph(graph);
      
      bg->set_style(_graph_style);
      bg->set_images(_usage_back, _usage_fore);
      
      {
        Gtk::EventBox *evbox= Gtk::manage(new Gtk::EventBox);
        evbox->add(*bg->get_widget());
        evbox->signal_button_press_event().connect(sigc::bind<GraphItem*>(sigc::mem_fun(*this,&MAHealthPanel::graph_button_press),
                                                                          item));
        group->box->pack_start(*evbox, false, true);
        item->evbox= evbox;
      }
      item->graph= bg;

      if (bg->get_aligned_widget())
        group->page->sizer->add_widget(*bg->get_aligned_widget());
    }
    break;
  }
  
  group->box->show_all();

  return item;
}


void MAHealthPanel::remove_page(GraphPage *rpage)
{
  // remove all groups in this group
  while (!rpage->groups.empty())
  {
    remove_group(*rpage->groups.begin());
  }

  g_assert(rpage->page->groups_num==0);

  MYX_HEALTH_PAGE *page= rpage->page;

  // remove page (ours)
  _pages.remove(rpage);

  // free page
  g_free(page->caption);
  g_free(page->caption_trans_id);
  g_free(page->description);
  g_free(page->description_trans_id);

  g_free(page->groups);

  delete rpage->evbox;

  // remove page (MYX)
  unsigned int i;
  for (i= 0; _graph_defs->pages+i != rpage->page &&
       i < _graph_defs->pages_num; i++);
  for (; i < _graph_defs->pages_num-1; i++)
  {
    _graph_defs->pages[i]= _graph_defs->pages[i+1];
  }
  _graph_defs->pages_num--;

  // update pointers in other pages
  i= 0;
  for (std::list<GraphPage*>::iterator it= _pages.begin();
       it != _pages.end(); ++it)
  {
    (*it)->page= _graph_defs->pages+i;
  }

  delete rpage;
}


void MAHealthPanel::remove_group(GraphGroup *rgroup)
{
  // remove all graphs in this group
  while (!rgroup->graphs.empty())
  {
    remove_graph(*rgroup->graphs.begin());
  }

  g_assert(rgroup->group->graphs_num==0);

  GraphPage *rpage= rgroup->page;
  MYX_HEALTH_GROUP *group= rgroup->group;

  // remove group from page (ours)
  rpage->groups.remove(rgroup);
  
  // free group
  g_free(group->caption);
  g_free(group->caption_trans_id);

  g_free(group->graphs);

  delete rgroup->frame;

  // remove group from page (MYX)
  unsigned int i;
  for (i= 0; rpage->page->groups+i != rgroup->group &&
       i < rpage->page->groups_num; i++);
  for (; i < rpage->page->groups_num-1; i++)
  {
    rpage->page->groups[i]= rpage->page->groups[i+1];
  }
  rpage->page->groups_num--;

  // update pointers in other groups
  i= 0;
  for (std::list<GraphGroup*>::iterator it= rpage->groups.begin();
       it != rpage->groups.end(); ++it)
  {
    (*it)->group= rpage->page->groups+i;
  }

  delete rgroup;
}


void MAHealthPanel::remove_graph(GraphItem *rgraph)
{
  GraphGroup *rgroup= rgraph->group;
  MYX_HEALTH_GRAPH *graph= rgraph->def;
  
  // remove graph from group (ours)
  rgroup->graphs.remove(rgraph);
  
  // free graph
  g_free(graph->graph_caption);
  g_free(graph->value_formula);
  g_free(graph->max_formula);
  g_free(graph->value_caption);
  g_free(graph->value_caption_trans_id);
  g_free(graph->max_caption);
  g_free(graph->max_caption_trans_id);

  delete rgraph->evbox;
  
  // remove graph from groups (MYX)
  unsigned int i;
  for (i= 0; rgroup->group->graphs+i != rgraph->def &&
       i < rgroup->group->graphs_num; i++);
  for (; i < rgroup->group->graphs_num-1; i++)
  {
    rgroup->group->graphs[i]= rgroup->group->graphs[i+1];
  }
  rgroup->group->graphs_num--;
 
  // update pointers in the other graph objects
  i= 0;
  for (std::list<GraphItem*>::iterator it= rgroup->graphs.begin();
       it != rgroup->graphs.end(); ++it)
  {
    (*it)->def= rgroup->group->graphs+i;
  }

  delete rgraph;
}


void MAHealthPanel::setup_graphs()
{
  MYX_ADMIN_LIB_ERROR err;

  if (_graph_defs)
    myx_free_health_pages(_graph_defs);

  // first try to load user defined graphs
  _graph_defs= myx_read_in_health_pages((char*)prefs.build_path_to("administrator/mysqladmin_health.xml").c_str(),
                                        &err);
  
  // if it doesnt exist, try the system one
  if (!_graph_defs)
    _graph_defs= myx_read_in_health_pages((char*)get_app_file("mysqladmin_health.xml").c_str(),
                                          &err);
  
  if (!_graph_defs)
  {
    show_adminlib_error(*_app->window(),
                        ufmt(_("Could not open health graph description '%s'"),
                             get_app_file("mysqladmin_health.xml").c_str()), err);
    return;
  }

  for (unsigned int p= 0; p < _graph_defs->pages_num; p++)
  {
    MYX_HEALTH_PAGE *page= _graph_defs->pages+p;

    add_page(page);
  }
}


void MAHealthPanel::update_graphs(MYX_VARIABLES *server_vars)
{
  if (server_vars)
  {
    for (std::list<GraphPage*>::iterator pg= _pages.begin();
         pg != _pages.end(); ++pg)
    {
      for (std::list<GraphGroup*>::iterator gp= (*pg)->groups.begin();
           gp != (*pg)->groups.end(); ++gp)
      {
        for (std::list<GraphItem*>::iterator gr= (*gp)->graphs.begin();
             gr != (*gp)->graphs.end(); ++gr)
        {
          (*gr)->graph->update_server_values(server_vars);
        }
      }
    }
  }
}


void MAHealthPanel::refresh_graphs(MYX_VARIABLES *status_vars)
{  
  for (std::list<GraphPage*>::iterator pg= _pages.begin();
       pg != _pages.end(); ++pg)
  {
    for (std::list<GraphGroup*>::iterator gp= (*pg)->groups.begin();
         gp != (*pg)->groups.end(); ++gp)
    {
      for (std::list<GraphItem*>::iterator gr= (*gp)->graphs.begin();
           gr != (*gp)->graphs.end(); ++gr)
      {
        (*gr)->graph->update_values(status_vars);
      }
    }
  }
}


void MAHealthPanel::refresh_list(int index)
{
  switch (index)
  {
  case 0:
    if (_var_values[VStatus])
      myx_free_variables(_var_values[VStatus]);
    _var_values[VStatus]= (MYX_VARIABLES*)_instance->perform_data_fetch((MInstanceInfo::DataFetcher)myx_get_status_variables);
    type_selected(VStatus);
    break;
  case 1:
    if (_var_values[VServer])
      myx_free_variables(_var_values[VServer]);
    _var_values[VServer]= (MYX_VARIABLES*)_instance->perform_data_fetch((MInstanceInfo::DataFetcher)myx_get_server_variables);
    type_selected(VServer);
    update_graphs(_var_values[VServer]);
    break;
  }
}


bool MAHealthPanel::page_button_press(GdkEventButton *ev, GraphPage *page)
{  
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
  {
    _cur_graph_page= page;
    _cur_graph_group= 0;
    _cur_graph_item= 0;

    // we assume that if there is a translation, it's a built-in
    if (page->page->caption_trans_id || page->page->description_trans_id)
    {
      _page_menu.items()[1].set_sensitive(false);
      _page_menu.items()[4].set_sensitive(false);
    }
    else
    {
      _page_menu.items()[1].set_sensitive(true);
      _page_menu.items()[4].set_sensitive(true);
    }
    _page_menu.popup(ev->button, ev->time);
    return true;
  }
  else
    return false;
}


bool MAHealthPanel::group_button_press(GdkEventButton *ev, GraphGroup *group)
{
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
  {
    _cur_graph_page= group->page;
    _cur_graph_group= group;
    _cur_graph_item= 0;
    
    // we assume that if there is a translation, it's a built-in
    if (group->group->caption_trans_id)
    {
      _group_menu.items()[2].set_sensitive(false);
    }
    else
    {
      _group_menu.items()[2].set_sensitive(true);
    }
    _group_menu.popup(ev->button, ev->time);
    return true;
  }
  else
    return false;
}


bool MAHealthPanel::graph_button_press(GdkEventButton *ev, GraphItem *graph)
{
  if ((ev->type == GDK_BUTTON_PRESS) && (ev->button == 3))
  {
    _cur_graph_page= graph->group->page;
    _cur_graph_group= graph->group;
    _cur_graph_item= graph;

    _graph_menu.popup(ev->button, ev->time);
    return true;
  }
  else
    return false;
}


bool MAHealthPanel::before_show()
{
  if (!_graphs_ready)
  {
    setup_graphs();
    ((Gtk::Notebook*)get_widget("notebook"))->set_current_page(0);
  }
  
  return true;
}


bool MAHealthPanel::check_formula(const Glib::ustring &text)
{
  MYX_EXPRESSION_ERROR err;

  if (text.empty())
    return false;

  Glib::ustring expr= replace_variables(text);

  MYX_COMPILED_EXPRESSION *ex= myx_compile_expression(expr.c_str(), &err);
  if (ex)
  {
    myx_free_expression(ex);
    return true;
  }

  return false;
}


bool MAHealthPanel::commit_graph_edit(MYX_HEALTH_GRAPH *graph)
{
  Glib::ustring str;
  
  str= _graph_dlg->get_text("formula_text")->get_buffer()->get_text();
  if (str.empty())
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("Please fill the Value Formula field."),
                           false,
                           Gtk::MESSAGE_ERROR, Gtk::BUTTONS_OK);
    dlg.run();
    return false;
  }
  if (!check_formula(str))
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("The Value Formula has a syntax error."),
                           false,
                           Gtk::MESSAGE_ERROR, Gtk::BUTTONS_OK);
    dlg.run();

    return false;
  }
  str= _graph_dlg->get_entry("max_formula_entry")->get_text();
  if (!str.empty() && !check_formula(str))
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("The Max. Formula field has a syntax error."),
                           false,
                           Gtk::MESSAGE_ERROR, Gtk::BUTTONS_OK);
    dlg.run();
    return false;
  }
  if (_graph_dlg->get_spin("min_spin")->get_value() >= _graph_dlg->get_spin("max_spin")->get_value())
  {
    Gtk::MessageDialog dlg(*_app->window(),
                           _("Minimal and Maximal Value fields have inconsistent values (Minimal must be smaller than Maximal)."),
                           false,
                           Gtk::MESSAGE_ERROR, Gtk::BUTTONS_OK);
    dlg.run();
    return false;
  }

  g_free(graph->graph_caption);
  g_free(graph->value_formula);
  g_free(graph->max_formula);
  g_free(graph->value_caption);
  g_free(graph->value_caption_trans_id);
  g_free(graph->max_caption);
  g_free(graph->max_caption_trans_id);

  str= _graph_dlg->get_entry("caption_entry")->get_text();
  graph->graph_caption= str.empty()?0:g_strdup(str.c_str());
  graph->display_graph_caption= _graph_dlg->get_toggle("caption_check")->get_active();
  graph->graphtype= (MYX_HEALTH_GRAPH_TYPE)(_graph_dlg->get_option("type_menu")->get_history()+1);
  graph->value_unit= (MYX_HEALTH_GRAPH_VALUE_UNIT)(_graph_dlg->get_option("unit_menu")->get_history());
  graph->min= _graph_dlg->get_spin("min_spin")->get_value();
  graph->max= _graph_dlg->get_spin("max_spin")->get_value();
  graph->autoextend_max= _graph_dlg->get_toggle("autoextend_check")->get_active();
  graph->value_formula= g_strdup(_graph_dlg->get_text("formula_text")->get_buffer()->get_text().c_str());
  str= _graph_dlg->get_entry("max_formula_entry")->get_text();
  graph->max_formula= str.empty()?0:g_strdup(str.c_str());
  str= _graph_dlg->get_entry("value_caption_entry")->get_text();
  graph->value_caption= str.empty()?0:g_strdup(str.c_str());
  str= _graph_dlg->get_entry("max_caption_entry")->get_text();
  graph->max_caption= str.empty()?0:g_strdup(str.c_str());

  return true;
}


void MAHealthPanel::add_graph_mi()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_graph_dlg->get_widget("graph_dialog");
  Glib::ustring str;

  dlg->set_title(_("Create a New Graph"));

  _graph_dlg->get_option("type_menu")->set_sensitive(true);

  MYX_HEALTH_GRAPH graph;
  memset(&graph, 0, sizeof(MYX_HEALTH_GRAPH));

  do {
    if (dlg->run()!=Gtk::RESPONSE_OK)
    {
      dlg->hide();
      return;
    }
    dlg->hide();
  } while (!commit_graph_edit(&graph));

  graph.value_caption_trans_id= 0;
  graph.max_caption_trans_id= 0;
  graph.refreshtime= 0;
  graph.pos= _cur_graph_group->graphs.size();

  MYX_HEALTH_GROUP *group= _cur_graph_group->group;
  MYX_HEALTH_GRAPH *old_graphs= group->graphs;
  
  group->graphs= (MYX_HEALTH_GRAPH*)g_malloc(sizeof(MYX_HEALTH_GRAPH)*(_cur_graph_group->graphs.size()+1));
  memcpy(group->graphs, old_graphs, 
         sizeof(MYX_HEALTH_GRAPH)*_cur_graph_group->graphs.size());
  g_free(old_graphs);

  // update pointers to the graph array
  unsigned int i= 0;
  for (std::list<GraphItem*>::iterator it= _cur_graph_group->graphs.begin();
       it != _cur_graph_group->graphs.end(); ++it, i++)
  {
    (*it)->def= group->graphs+i;
  }

  group->graphs_num++;
  // add new graph
  memcpy(group->graphs+group->graphs_num-1, &graph, sizeof(MYX_HEALTH_GRAPH));
  GraphItem *item= add_graph(_cur_graph_group, group->graphs+group->graphs_num-1);

  // initial creation of all expressions and stuffs
  if (_var_values[VServer])
    item->graph->update_server_values(_var_values[VServer]);
}


void MAHealthPanel::delete_graph_mi()
{
  Gtk::MessageDialog dlg(*_app->window(), _("Please confirm graph removal."),
                         false,
                         Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK_CANCEL, true);
  if (dlg.run() != Gtk::RESPONSE_OK)
    return;

  remove_graph(_cur_graph_item);
}


void MAHealthPanel::edit_graph_mi()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_graph_dlg->get_widget("graph_dialog");

  _graph_dlg->get_option("type_menu")->set_sensitive(false);
  
  dlg->set_title(_("Edit Graph"));
  
  MYX_HEALTH_GRAPH *graph= _cur_graph_item->def;
  
  _graph_dlg->get_entry("caption_entry")->set_text(graph->graph_caption?:"");
  _graph_dlg->get_toggle("caption_check")->set_active(graph->display_graph_caption);
  _graph_dlg->get_option("type_menu")->set_history((int)graph->graphtype-1);
  _graph_dlg->get_option("unit_menu")->set_history((int)graph->value_unit);
  _graph_dlg->get_spin("min_spin")->set_value(graph->min);
  _graph_dlg->get_spin("max_spin")->set_value(graph->max);
  _graph_dlg->get_toggle("autoextend_check")->set_active(graph->autoextend_max);
  _graph_dlg->get_text("formula_text")->get_buffer()->set_text(graph->value_formula?:"");
  _graph_dlg->get_entry("max_formula_entry")->set_text(graph->max_formula?:"");
  _graph_dlg->get_entry("value_caption_entry")->set_text(graph->value_caption?:"");
  _graph_dlg->get_entry("max_caption_entry")->set_text(graph->max_caption?:"");
  //graph->refreshtime= 0;

  do {
    if (dlg->run()!=Gtk::RESPONSE_OK)
    {
      dlg->hide();
      return;
    }
    dlg->hide();
  } while (!commit_graph_edit(_cur_graph_item->def));

  _cur_graph_item->graph->modify(_cur_graph_item->def);
  if (_var_values[VServer])
    _cur_graph_item->graph->update_server_values(_var_values[VServer]);
}


void MAHealthPanel::add_group_mi()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_new_group_dlg->get_widget("new_group_dialog");

  _new_group_dlg->get_entry("name_entry")->set_text("");  
  
  dlg->set_title(_("Create a New Graph Grouping"));
  if (dlg->run() != Gtk::RESPONSE_OK)
  {
    dlg->hide();
    return;
  }
  dlg->hide();
  
  MYX_HEALTH_GROUP group;
  memset(&group, 0, sizeof(MYX_HEALTH_GROUP));
  group.caption= g_strdup(_new_group_dlg->get_entry("name_entry")->get_text().c_str());
  group.caption_trans_id= 0;
  group.pos= _cur_graph_page->groups.size();
  
  MYX_HEALTH_PAGE *page= _cur_graph_page->page;
  MYX_HEALTH_GROUP *old_groups= page->groups;
  
  page->groups= (MYX_HEALTH_GROUP*)g_malloc(sizeof(MYX_HEALTH_GROUP)*(_cur_graph_page->groups.size()+1));
  memcpy(page->groups, old_groups,
         sizeof(MYX_HEALTH_GROUP)*_cur_graph_page->groups.size());
  g_free(old_groups);

  // update pointers to the group array
  unsigned int i=0;
  for (std::list<GraphGroup*>::iterator it= _cur_graph_page->groups.begin();
       it != _cur_graph_page->groups.end(); ++it, i++)
  {
    (*it)->group= page->groups+i;
  }

  page->groups_num++;
  // add new group
  memcpy(page->groups+page->groups_num-1, &group, sizeof(MYX_HEALTH_GROUP));
  add_group(_cur_graph_page, page->groups+page->groups_num-1);
}


void MAHealthPanel::delete_group_mi()
{
  Gtk::MessageDialog dlg(*_app->window(), _("Please confirm group removal. All graphs inside will be removed."),
                         false,
                         Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK_CANCEL, true);
  if (dlg.run() != Gtk::RESPONSE_OK)
    return;
  
  remove_group(_cur_graph_group);
}


void MAHealthPanel::rename_group_mi()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_new_group_dlg->get_widget("new_group_dialog");
  MYX_HEALTH_GROUP *group= _cur_graph_group->group;

  _new_group_dlg->get_entry("name_entry")->set_text(group->caption?:"");
  
  dlg->set_title(_("Edit Graph Grouping Caption"));
  if (dlg->run() != Gtk::RESPONSE_OK)
  {
    dlg->hide();
    return;
  }
  dlg->hide();
  
  if (group->caption)
    g_free(group->caption);
  group->caption= g_strdup(_new_group_dlg->get_entry("name_entry")->get_text().c_str());

  _cur_graph_group->frame->set_label(group->caption);
}


void MAHealthPanel::rename_page_mi()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_new_page_dlg->get_widget("new_page_dialog");
  MYX_HEALTH_PAGE *page= _cur_graph_page->page;

  _new_page_dlg->get_entry("name_entry")->set_text(page->caption);
  _new_page_dlg->get_entry("description_entry")->set_text(page->description);
  
  dlg->set_title(_("Edit Page Text"));
  if (dlg->run() != Gtk::RESPONSE_OK)
  {
    dlg->hide();
    return;
  }
  dlg->hide();
  

  if (page->caption)
    g_free(page->caption);
  page->caption= g_strdup(_new_page_dlg->get_entry("name_entry")->get_text().c_str());

  _cur_graph_page->caption->set_text(page->caption);

  if (page->description)
    g_free(page->description);
  page->description= g_strdup(_new_page_dlg->get_entry("description_entry")->get_text().c_str());

  _cur_graph_page->description->set_text(page->description);
}


void MAHealthPanel::add_page_mi()
{
  Gtk::Dialog *dlg= (Gtk::Dialog*)_new_page_dlg->get_widget("new_page_dialog");

  _new_page_dlg->get_entry("name_entry")->set_text("");
  _new_page_dlg->get_entry("description_entry")->set_text("");

  dlg->set_title(_("Create a New Graph Page"));
  if (dlg->run() != Gtk::RESPONSE_OK)
  {
    dlg->hide();
    return;
  }
  dlg->hide();
  
  Glib::ustring name= ((Gtk::Entry*)_new_page_dlg->get_widget("name_entry"))->get_text();
  Glib::ustring description= ((Gtk::Entry*)_new_page_dlg->get_widget("description_entry"))->get_text();
  
  MYX_HEALTH_PAGE page;
  memset(&page, 0, sizeof(MYX_HEALTH_PAGE));
  page.caption= g_strdup(name.c_str());
  page.description= g_strdup(description.c_str());
  page.pos= _pages.size();
  
  MYX_HEALTH_PAGE *old_pages= _graph_defs->pages;

  _graph_defs->pages= (MYX_HEALTH_PAGE*)g_malloc(sizeof(MYX_HEALTH_PAGE)*(_pages.size()+1));
  memcpy(_graph_defs->pages, old_pages,
         sizeof(MYX_HEALTH_PAGE)*_pages.size());
  g_free(old_pages);

  // update pointers to the group array
  unsigned int i=0;
  for (std::list<GraphPage*>::iterator it= _pages.begin();
       it != _pages.end(); ++it, i++)
  {
    (*it)->page= _graph_defs->pages+i;
  }

  _graph_defs->pages_num++;
  // add new group
  memcpy(_graph_defs->pages+_graph_defs->pages_num-1, &page, 
         sizeof(MYX_HEALTH_PAGE));
  add_page(_graph_defs->pages+_graph_defs->pages_num-1);
}


void MAHealthPanel::delete_page_mi()
{
  Gtk::MessageDialog dlg(*_app->window(), _("Please confirm page removal. All groups and graphs inside will be removed."),
                         false,
                         Gtk::MESSAGE_INFO, Gtk::BUTTONS_OK_CANCEL, true);
  if (dlg.run()!=Gtk::RESPONSE_OK)
    return;
  
  remove_page(_cur_graph_page);
}


void MAHealthPanel::restore_defaults_mi()
{
  // remove user defined health graphs
  unlink((char*)prefs.build_path_to("administrator/mysqladmin_health.xml").c_str());

  while (!_pages.empty())
  {
    remove_page(*_pages.begin());
  }
  _cur_graph_page= 0;
  
  setup_graphs();

  _xml->get_note("notebook")->set_current_page(0);
}


void MAHealthPanel::show()
{
  MAPanel::show();

  if (!_fetching)
  {
    if (_instance->perform_async_data_fetch("status",
                                            (MInstanceInfo::DataFetcher)myx_get_status_variables,
                                            status_callback, this,
                                            1000))
      _fetching= true;
  }

  if (!_graphs_ready)
  {
    // will retrieve _var_values
    refresh_list(0);
    refresh_list(1);

    _graphs_ready= true;

    update_graphs(_var_values[VServer]);
  }
}


bool MAHealthPanel::before_hide()
{
  return true;
}


bool MAHealthPanel::before_quit()
{
  if (_graph_defs)
  {
    if (myx_save_health_pages(_graph_defs, (char*)prefs.build_path_to("administrator/mysqladmin_health.xml").c_str()) < 0)
      g_warning("could not write to '%s'",(char*)prefs.build_path_to("administrator/mysqladmin_health.xml").c_str());
  }
  return true;
}


char *MAHealthPanel::get_value_for_variable(MYX_VARIABLES *values,
                                            const char *name)
{
  //warning: values is not sorted! (cant use bsearch)
  for (unsigned int i= 0; i < values->variables_num; i++)
  {
    if (strcmp(name, (char*)values->variables[i].name)==0)
      return (char*)values->variables[i].value;
  }

  return NULL;
}


/**********************************************************************/

void MAHealthPanel::variable_edited(const Glib::ustring& path,
                                    const Glib::ustring& new_text)
{
  Glib::RefPtr<Gtk::TreeModel> store= _value_tree[VServer]->get_model();
  Glib::ustring variable;
  Gtk::TreeIter iter= store->get_iter(path);
  Gtk::TreeRow row= *iter;

  variable= row[_value_columns._name];
  
  if ((long)_instance->perform_data_fetch3((MInstanceInfo::DataFetcher3)myx_set_variable,
                                          (void*)variable.c_str(), 
                                          (void*)new_text.c_str())!=0)
  {
    _data->show_last_error(ufmt(_("Could not change server variable '%s' to '%s'"),
                                variable.c_str(),
                                new_text.c_str()));
    refresh_list(1);
  }
  else
  {
    _app->set_status(ufmt(_("Changed server variable '%s' to '%s'"),
                          variable.c_str(), new_text.c_str()));
  }
}


void MAHealthPanel::type_selected(int index)
{
  Glib::RefPtr<Gtk::TreeSelection> sel= _type_tree[index]->get_selection();

  if (sel->get_selected() && _var_values[index])
  {
    Gtk::TreeModel::Row row= *sel->get_selected();
    Glib::RefPtr<Gtk::ListStore> store= Glib::RefPtr<Gtk::ListStore>::cast_static(_value_tree[index]->get_model());
    MYX_VARIABLE_ELEMENT *vars= row[_type_columns._vars];

    store->clear();

    for (unsigned int i= 0; i < row[_type_columns._var_counts]; i++)
    {
      char *value;
      
      value= get_value_for_variable(_var_values[index],
                                    (char*)vars[i].mysql_id?:"");
      if (!value)
      {
        continue;
        //value= "";
      }

      Gtk::TreeModel::Row trow= *store->append();
      trow[_value_columns._icon]= vars[i].editable?_var_rw_icon:_var_ro_icon;
      trow[_value_columns._name]= (char*)vars[i].mysql_id?:"";
      trow[_value_columns._value]= value;
      //trow[_value_columns._descr]= _transl->get("MySQLVariables",
      //                                          (char*)vars[i].desc_id);
      if (vars[i].desc_id)
        trow[_value_columns._descr]= _((char*)vars[i].desc_id);
      trow[_value_columns._editable]= vars[i].editable;
    }
  }
  _value_tree[index]->columns_autosize();
}


bool MAHealthPanel::status_callback(void *data, void *udata)
{
  MAHealthPanel *self= (MAHealthPanel*)udata;
  MYX_VARIABLES *vars= (MYX_VARIABLES*)data;

  // update variable data
  if (self->_old_status_values)
  {
    myx_free_variables(self->_old_status_values);
  }
  self->_old_status_values= self->_status_values;
  self->_status_values= vars;

  if (vars)
  {
    self->refresh_graphs(vars);

    return true;
  }
  return false;
}


/**********************************************************************/

MAPanel *create_health_panel(MAdministrator *app, MDataInterface *data)
{
  return new MAHealthPanel(app, data);
}
