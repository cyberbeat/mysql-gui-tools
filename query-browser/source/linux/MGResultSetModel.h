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

#ifndef _MGRESULTSETMODEL_H_
#define _MGRESULTSETMODEL_H_


#include "myx_public_interface.h"

#include "MYXResultSet.h"

#include <gtkmm/treemodel.h>
#include <gtkmm/treepath.h>


class MGResultSetModel : public Gtk::TreeModel, public Glib::Object, public MYXResultSet {
  public:
    enum SpecialColumns {
      RowEditable, // bool
      RowStatusIcon, // GdkPixbuf
      SpecialLast
    };

  protected:
    int _stamp;

    bool _edit_enabled;
    mutable bool _disable_checks;

    Gtk::TreeModel::ColumnRecord _column_record;

    std::vector<char*> _state_colors; // deleted, inserted
    std::vector<char*> _diff_colors; // missing, only in this, diff 

    virtual Gtk::TreeModelFlags get_flags_vfunc();
    virtual int get_n_columns_vfunc();
    virtual GType get_column_type_vfunc(int index);

    //virtual bool get_iter_vfunc(GtkTreeIter* iter, const Gtk::TreeModel::Path& path);
    virtual bool get_iter_vfunc (const Gtk::TreeModel::Path& path, Gtk::TreeModel::iterator& iter) const;
    //virtual Gtk::TreeModel::Path get_path_vfunc(const Gtk::TreeModel::iterator& iter);
    virtual Gtk::TreeModel::Path get_path_vfunc(const Gtk::TreeModel::iterator& iter) const;

    //virtual void get_value_vfunc(const Gtk::TreeModel::iterator& iter, int column, GValue* value);
    virtual void get_value_vfunc (const Gtk::TreeModel::iterator& iter, int column, Glib::ValueBase& value) const;

    //virtual bool iter_next_vfunc(GtkTreeIter* iter);
    virtual bool iter_next_vfunc (const Gtk::TreeModel::iterator& iter, Gtk::TreeModel::iterator& iter_next) const;
    //virtual bool iter_nth_child_vfunc(GtkTreeIter* iter, const GtkTreeIter* parent, int n);
    virtual bool iter_nth_child_vfunc (const Gtk::TreeModel::iterator& parent, int n, Gtk::TreeModel::iterator& iter) const;
   
    virtual bool iter_nth_root_child_vfunc (int n, Gtk::TreeModel::iterator& iter) const;
    //virtual bool iter_parent_vfunc(GtkTreeIter* iter, const GtkTreeIter* child);
    virtual bool iter_parent_vfunc (const Gtk::TreeModel::iterator& child, Gtk::TreeModel::iterator& iter) const;
    //virtual void ref_node_vfunc(GtkTreeIter* iter);
    virtual void ref_node_vfunc (const Gtk::TreeModel::iterator& iter) const;

    virtual bool iter_children_vfunc (const Gtk::TreeModel::iterator& parent, Gtk::TreeModel::iterator& iter) const;
    //virtual bool iter_has_child_vfunc(const GtkTreeIter* iter);
    virtual bool iter_has_child_vfunc (const Gtk::TreeModel::iterator& iter) const;
    //virtual int iter_n_children_vfunc(const GtkTreeIter* iter);
    virtual int iter_n_children_vfunc (const Gtk::TreeModel::iterator& iter) const;
    virtual int iter_n_root_children_vfunc () const;
    // unchanged
    virtual void set_value_impl(const Gtk::TreeModel::iterator& row, int column, const Glib::ValueBase& value);

    //virtual void unref_node_vfunc(GtkTreeIter* iter);
    virtual void unref_node_vfunc (const Gtk::TreeModel::iterator& iter) const;

    
    bool treeiter_valid(const Gtk::TreeModel::iterator& iter) const;
    bool treeiter_valid(const GtkTreeIter *iter) const;

    MGResultSetModel(const Gtk::TreeModelColumnRecord& columns,
                     MYX_RESULTSET *resultset);
    virtual ~MGResultSetModel();

  public:
    static void row_added_callback(MYXResultSet *rs, unsigned int row);
    static void row_deleted_callback(MYXResultSet *rs, unsigned int row);

  public:
    static Glib::RefPtr<MGResultSetModel> create(const Gtk::TreeModelColumnRecord& columns,
                                                 MYX_RESULTSET *resultset);

    void set_value(const Gtk::TreeModel::iterator& row, int column, const gpointer value, size_t value_length);
    bool get_value(const Gtk::TreeModel::iterator& row, int column, gpointer &value, size_t &value_length) const;

    void set_state_colors(const std::vector<Glib::ustring> &colors);
    void set_diff_colors(const std::vector<Glib::ustring> &colors);
    
    Gtk::TreeModel::iterator append();
    void erase(const Gtk::TreeModel::iterator &iter);

    void notify_resultset_changed();

    void set_editable(bool flag);
    bool get_editable();

    MYX_RS_ROW *get_iter_row(const Gtk::TreeModel::iterator &iter);

    void __disable_checks(bool flag) { _disable_checks= flag; };
};


#endif /* _MGRESULTSETMODEL_H_ */
