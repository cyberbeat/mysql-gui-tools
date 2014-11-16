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

#ifndef _MQWORKAREATAB_H_
#define _MQWORKAREATAB_H_

#include "MQBaseTab.h"

#include <gtkmm/paned.h>
#include <gtkmm/scrolledwindow.h>
#include <gtkmm/label.h>
#include <gtkmm/treepath.h>

namespace Gtk {
  class HTML;
};

class MQResultSetView;
class MGGladeXML;

class MQResultTab : public MQBaseTab {
    struct RSItem {
      // label to display in tab 
      Glib::ustring label;
      // query displayed for this resultset
      // rs only contains queries that were executed, while this may contain
      // queries that are being edited but were not executed yet.
      Glib::ustring query;

      Gtk::Frame frame;
      MQResultSetView *rs;

      Gtk::Paned *paned;

      sigc::connection scroll_con;
    };
    
    enum CompareAction {
      CopyToLeft,
      CopyToRight,
      CopyAllToLeft,
      CopyAllToRight,
      DeleteFromLeft,
      DeleteFromRight
    };

    MGGladeXML *_toolbar_xml;
    MGGladeXML *_toolmenu_xml;
    
    Gtk::VPaned _topPaned; // 1st slot is reserved for query-area
    Gtk::ScrolledWindow _topBoxScroll;
    Gtk::VBox _topBox;
  

    Gtk::Paned *_paned;

    std::vector<RSItem*> _rs_items;

    bool _vertical;

    sigc::connection _sync_con1, _sync_con2;

    void scrolled(MQResultSetView *sender);
    void activated(MQResultSetView *sender);
    
    void rotate_paned(bool vertical);

    virtual bool tab_will_close();

    Gtk::Paned *find_paned_with(MQResultSetView *rs);

    MQResultSetView *get_not_active();

    void row_changed(MQResultSetView *sender);

    bool copy_row_contents(MQResultSetView *from_rs,
                           MQResultSetView *to_rs,
                           const Gtk::TreePath &path);
    
    void copy_all_rows(MQResultSetView *from_rs,
                       MQResultSetView *to_rs);
    
    void delete_all_rows(MQResultSetView *from_rs,
                         MQResultSetView *that_are_missing_in_rs);
    
    void update_cmp_toolbar();
    void cmp_do_action(CompareAction action);
    void cmp_get_menu_pos(int &x, int &y, bool &push_in);
    void cmp_actions();
    void cmp_close();
    void cmp_go_next();
    void cmp_go_back();
    
  public:
    MQResultTab();
    virtual ~MQResultTab();

    virtual MQTabType get_type() { return TResultSet; };
    
    bool is_busy();
    bool is_vertical() { return _vertical; };

    void set_top_widget(Gtk::Widget *w);
    
    void set_sync_scrolling(bool flag=true);
    
    void add_resultset(MQResultSetView *rsv, bool vertical);
    int get_item_count() { return _rs_items.size(); };
    MQResultSetView *get(int i) { return _rs_items[i]->rs; };
    void remove_resultset(MQResultSetView *rsview);
    int get_index(MQResultSetView *rsview);

    bool contains_rsview(MQResultSetView *rsview);

    void set_label(MQResultSetView *rs, const Glib::ustring &text);
    void set_query_text(MQResultSetView *rs, const Glib::ustring &text);

    Glib::ustring get_query_text(MQResultSetView *rs);

    MQResultSetView *get_active();

    void show_compare_toolbar();
};


class MQHelpTab : public MQBaseTab {
    Gtk::HTML *_html;
    Gtk::ScrolledWindow _swin;

    std::string _loaded_file;
  public:
    MQHelpTab();
    virtual MQTabType get_type() { return THelp; };

    void show_help(const std::string &id, const std::string &file);
};


#endif /* _MQWORKAREATAB_H_ */
