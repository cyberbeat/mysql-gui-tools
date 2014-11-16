/* Copyright (C) 2003 MySQL AB

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


#ifndef _MAHEALTHPANEL_H_
#define _MAHEALTHPANEL_H_

#include "MAPanel.h"

#include "myx_admin_public_interface.h"

#include "MGTimeGraphPlotter.h"
#include "MGMeterGraphPlotter.h"
#include "MGHMeterGraphPlotter.h"

#include <vector>


class MDataInterface;
class MInstanceInfo;

class MAHealthPanel : public MAPanel {
  public:
    class BaseGraph;

  private:
    class TypeColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        TypeColumns() { add(_icon); add(_text); add(_vars); add(_var_counts); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _text;
        Gtk::TreeModelColumn<MYX_VARIABLE_ELEMENT*> _vars;
        Gtk::TreeModelColumn<unsigned int> _var_counts;
    };
    TypeColumns _type_columns;

    class ValueColumns : public Gtk::TreeModel::ColumnRecord {
      public:
        ValueColumns() { add(_icon); add(_name); add(_value); add(_descr); add(_editable); };

        Gtk::TreeModelColumn<Glib::RefPtr<Gdk::Pixbuf> > _icon;
        Gtk::TreeModelColumn<Glib::ustring> _name;
        Gtk::TreeModelColumn<Glib::ustring> _value;
        Gtk::TreeModelColumn<Glib::ustring> _descr;
        Gtk::TreeModelColumn<bool> _editable;
    };
    ValueColumns _value_columns;

    struct GraphGroup;
    struct GraphPage {
      MYX_HEALTH_PAGE *page;
      Gtk::Label *caption;
      Gtk::Label *description;
      Gtk::VBox *box;
      Gtk::EventBox *evbox;
      Glib::RefPtr<Gtk::SizeGroup> sizer;
      std::list<GraphGroup*> groups;
    };
    
    struct GraphItem;
    struct GraphGroup {
      GraphPage *page;
      MYX_HEALTH_GROUP *group;
      Gtk::Frame *frame;
      Gtk::VBox *box;
      std::list<GraphItem*> graphs;
    };
    
    struct GraphItem {
      GraphGroup *group;
      MYX_HEALTH_GRAPH *def;
      Gtk::EventBox *evbox;
      BaseGraph *graph;
    };

    enum {
      VStatus=0,
      VServer=1
    };

    bool _fetching;
    bool _graphs_ready;

    MInstanceInfo *_instance;

    Gtk::TreeView *_type_tree[2];
    Gtk::TreeView *_value_tree[2];

    Gtk::Menu _page_menu;
    Gtk::Menu _group_menu;
    Gtk::Menu _graph_menu;
    
    MGGladeXML *_new_page_dlg;
    MGGladeXML *_new_group_dlg;
    MGGladeXML *_graph_dlg;

    GraphPage *_cur_graph_page;
    GraphGroup *_cur_graph_group;
    GraphItem *_cur_graph_item;
    
    // for lists
    MYX_VARIABLES *_var_values[2];
    MYX_VARIABLES_LISTING *_var_lists[2];

    // for graphs
    MYX_VARIABLES *_status_values;
    MYX_VARIABLES *_old_status_values;

    MYX_HEALTH_PAGES *_graph_defs;
    
    Glib::RefPtr<Gdk::Pixbuf> _type_icon;
    Glib::RefPtr<Gdk::Pixbuf> _var_ro_icon;
    Glib::RefPtr<Gdk::Pixbuf> _var_rw_icon;

    Gdk::Color _line_color;
    Gdk::Color _grid_color;
    Glib::RefPtr<Gtk::Style> _graph_style;
    Glib::RefPtr<Gdk::Pixmap> _level_fore, _level_back;
    Glib::RefPtr<Gdk::Pixbuf> _usage_fore, _usage_back;

    std::list<GraphPage*> _pages;
    
    void update_graphs(MYX_VARIABLES *server_vars);
    void refresh_graphs(MYX_VARIABLES *status_vars);

    void refresh_list(int index);

    Glib::ustring replace_variables(const Glib::ustring &expr);
    
    char *get_value_for_variable(MYX_VARIABLES *values, const char *name);
    
    bool check_formula(const Glib::ustring &text);
    
    void setup_graphs();
    void setup_type_tree(int index);
    void setup_value_list(Gtk::TreeView *tree);

    void add_page(MYX_HEALTH_PAGE *page);
    void add_group(GraphPage *page, MYX_HEALTH_GROUP *group);
    GraphItem *add_graph(GraphGroup *group, MYX_HEALTH_GRAPH *graph);
    
    void remove_page(GraphPage *rpage);
    void remove_group(GraphGroup *rgroup);
    void remove_graph(GraphItem *rgraph);
    
    void variable_edited(const Glib::ustring& path,
                         const Glib::ustring& new_text);
    
    void type_selected(int index);
    
    bool commit_graph_edit(MYX_HEALTH_GRAPH *graph);
    
    bool page_button_press(GdkEventButton *ev, GraphPage *page);
    bool group_button_press(GdkEventButton *ev, GraphGroup *group);
    bool graph_button_press(GdkEventButton *ev, GraphItem *graph);

    void add_graph_mi();
    void delete_graph_mi();
    void edit_graph_mi();
    void add_group_mi();
    void delete_group_mi();
    void rename_group_mi();
    void rename_page_mi();
    void add_page_mi();
    void delete_page_mi();
    void restore_defaults_mi();
    
    static bool status_callback(void*,void *);

    void handle_disconnect();
  public:
    MAHealthPanel(MAdministrator *app, MDataInterface *data);
    ~MAHealthPanel();

    virtual void show();
    virtual bool before_show();
    virtual bool before_hide();
    virtual bool before_quit();

    virtual bool init();

    virtual bool is_local_only() { return false; };
    virtual bool needs_connection() { return true; };

};

extern MAPanel *create_health_panel(MAdministrator *app, MDataInterface *data);

#endif /* _MAHEALTHPANEL_H_ */
