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


#include "MGResultSetModel.h"

#include "MGCellRendererBlob.h" // data2str()


static MYXResultSetCallbacks callbacks= {
  MGResultSetModel::row_added_callback,
    MGResultSetModel::row_deleted_callback,
    NULL
};



void MGResultSetModel::row_added_callback(MYXResultSet *rs, unsigned int row)
{
  Gtk::TreePath path;
  path.push_back(row);
  dynamic_cast<MGResultSetModel*>(rs)->row_inserted(path, dynamic_cast<MGResultSetModel*>(rs)->get_iter(path));
}


void MGResultSetModel::row_deleted_callback(MYXResultSet *rs, unsigned int row)
{
  Gtk::TreePath path;
  path.push_back(row);
  dynamic_cast<MGResultSetModel*>(rs)->row_deleted(path);
}


    //XXX TODO fix the model stuff so that there's only 1 action per cell
    // when it's submited to apply_actions

MGResultSetModel::MGResultSetModel(const Gtk::TreeModelColumnRecord& columns,
                                   MYX_RESULTSET *resultset)
: Glib::ObjectBase(typeid(MGResultSetModel)), //register a custom GType.
  Glib::Object(), //The custom GType is actually registered here.
  MYXResultSet(resultset, callbacks),
  _stamp(1), //When the model's stamp != the iterator's stamp then that iterator is invalid and should be ignored. Also, 0=invalid
  _edit_enabled(false),
  _disable_checks(true)
{
//  Gtk::TreeModel::add_interface(Glib::Object::get_type());

//  _column_record.add();
}


MGResultSetModel::~MGResultSetModel()
{ 
  for (unsigned int i= 0; i < _diff_colors.size(); i++)
    g_free(_diff_colors[i]);
  for (unsigned int i= 0; i < _state_colors.size(); i++)
    g_free(_state_colors[i]);
}


Glib::RefPtr<MGResultSetModel> MGResultSetModel::create(const Gtk::TreeModelColumnRecord& columns,
                                                        MYX_RESULTSET *resultset)
{
  return Glib::RefPtr<MGResultSetModel>(new MGResultSetModel(columns, resultset));
}


Gtk::TreeModelFlags MGResultSetModel::get_flags_vfunc()
{
  return Gtk::TREE_MODEL_ITERS_PERSIST;
}


int MGResultSetModel::get_n_columns_vfunc()
{
  return _resultset->columns_num_to_display*2+(int)SpecialLast;
}


GType MGResultSetModel::get_column_type_vfunc(int index)
{
  if (index & 1)
    return G_TYPE_STRING;
  else
  {
    switch (_resultset->columns[index/2].column_type)
    {
    case MYX_RSCT_BLOB:
    case MYX_RSCT_TEXT:
      return G_TYPE_STRING;
    default:
      return G_TYPE_STRING;
    }
  }
}


//bool MGResultSetModel::iter_next_vfunc(GtkTreeIter* iter)
bool MGResultSetModel::iter_next_vfunc(const Gtk::TreeModel::iterator& iter, 
  Gtk::TreeModel::iterator& iter_next) const
{
  /*
  if (_disable_checks || treeiter_valid(iter))
  {
    iter->stamp= _stamp;
    iter->user_data= (gpointer)((unsigned long)iter->user_data+1);
    return (unsigned long)iter->user_data < get_row_count() ? true : false;
  }
  iter->stamp= 0;
  return false;
  */
  if (_disable_checks || treeiter_valid(iter))
  {
    iter_next.set_stamp(_stamp);
    iter_next.gobj()->user_data= (gpointer)((unsigned long)iter.gobj()->user_data+1);
    return (unsigned long)iter_next.gobj()->user_data < get_row_count();
  }
  iter_next.set_stamp(0);
  return false;
}


bool MGResultSetModel::iter_children_vfunc(const Gtk::TreeModel::iterator& parent,
  Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  g_message("!");
  iter.set_stamp(0);
  //XXX
  return false;
}


//bool MGResultSetModel::iter_has_child_vfunc(const GtkTreeIter* iter)
bool MGResultSetModel::iter_has_child_vfunc (const Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  if (!iter && _resultset && get_row_count() > 0)
  {
    return true;
  }
  return false;
}


//int MGResultSetModel::iter_n_children_vfunc(const GtkTreeIter* iter)
int MGResultSetModel::iter_n_children_vfunc (const Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  if (!iter)
    return get_row_count();
  return 0;
}

int MGResultSetModel::iter_n_root_children_vfunc() const
{
  return get_row_count();
}


//bool MGResultSetModel::iter_nth_child_vfunc(GtkTreeIter* iter, const GtkTreeIter* parent, int n)
bool MGResultSetModel::iter_nth_child_vfunc (const Gtk::TreeModel::iterator& parent, 
  int n, Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  if (!parent)
  {
    iter.set_stamp(_stamp);
    iter.gobj()->user_data= (gpointer)(long)n;
    return true;
  }
  iter.set_stamp(0);
  return false;
}


bool MGResultSetModel::iter_nth_root_child_vfunc (int n, Gtk::TreeModel::iterator& iter) const
{
  if (n < get_row_count())
  {
    iter.set_stamp(_stamp);
    iter.gobj()->user_data= (gpointer)(long)n;
    return true;
  }
  return false;
}


//bool MGResultSetModel::iter_parent_vfunc(GtkTreeIter* iter, const GtkTreeIter* child)
bool MGResultSetModel::iter_parent_vfunc (const Gtk::TreeModel::iterator& child, 
  Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  g_message("++");
  iter.set_stamp(0);
  return false;
}


//void MGResultSetModel::ref_node_vfunc(GtkTreeIter* iter)
void MGResultSetModel::ref_node_vfunc (const Gtk::TreeModel::iterator& iter) const
// body unchanged
{
}


//void MGResultSetModel::unref_node_vfunc(GtkTreeIter* iter)
void MGResultSetModel::unref_node_vfunc (const Gtk::TreeModel::iterator& iter) const
// body unchanged
{
 // g_message("unrefnode");
}

//Gtk::TreeModel::Path MGResultSetModel::get_path_vfunc(const Gtk::TreeModel::iterator& iter)
Gtk::TreeModel::Path MGResultSetModel::get_path_vfunc(const Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  if (treeiter_valid(iter))
  {
    Path path;

    path.push_back((long)iter.gobj()->user_data);
    
    return path;
  }
  return Path();
}


//bool MGResultSetModel::get_iter_vfunc(GtkTreeIter* iter, const Gtk::TreeModel::Path& path)
bool MGResultSetModel::get_iter_vfunc (const Gtk::TreeModel::Path& path, 
  Gtk::TreeModel::iterator& iter) const
// body unchanged
{
  int len= path.size();

  if (len != 1)
  {
    g_message("bad path");
    iter.set_stamp(0);
    return false;
  }

  unsigned long row_index= path[0];

  if (row_index >= _resultset->rows_num)
  {
    if (row_index >= _resultset->rows_num + _new_rows)
    {
      iter.set_stamp(111);
      return false;
    }
  }
  iter.set_stamp(_stamp);
  iter.gobj()->user_data= (gpointer)row_index;

  return true;
}

bool MGResultSetModel::get_value(const Gtk::TreeModel::iterator& row, 
  int column, gpointer &value, size_t &value_length) const
{
  const GtkTreeIter *iter= row.gobj();

  if (_disable_checks || treeiter_valid(iter))
  {
    unsigned long r= (unsigned long)iter->user_data;

    if (column < _resultset->columns_num*2)
    {
      switch (column % 2)
      {
      case 0: // data
        {
          char *v;
          size_t l;
          get(r, column/2, v, l);
          value= v;
          value_length= l;
        }
        break;
      case 1: // color
        {
          char *color= NULL;

          // 1st check diff
          if (r < _resultset->rows_num)
          {
            MYX_RS_ROW *row= _resultset->rows + r;
            switch (row->diff&MYX_RD_MASK)
            {
            case MYX_RD_OTHER_ONLY:
              color= _diff_colors[0];
              break;
            case MYX_RD_THIS_ONLY:
              color= _diff_colors[1];
              break;
            case MYX_RD_DIFFERS:
              // check column specific differences
              if ((row->diff>>4) & (1<<(column/2)))
                color= _diff_colors[2];
              break;
            }
          }

          // 2nd. changes color overrides diff color
          switch (get_edit_status(r, column/2))
          {
          case MESUnchanged:
            break;
          case MESPlaceHolder:
            color= _state_colors[3];
            break;
          case MESAdded:
            color= _state_colors[1];
            break;
          case MESDeleted:
            color= _state_colors[0];
            break;
          case MESChanged:
            color= _state_colors[2];
            break;
          default:
            break;
          }

          value= color;
          value_length= 0;
        }
        break;
      }
      return true;
    }
  }
  else
    g_warning("invalid iterator!");
  
  return false;
}


//void MGResultSetModel::get_value_vfunc(const Gtk::TreeModel::iterator& iter_, int column, GValue* value)
void MGResultSetModel::get_value_vfunc (const Gtk::TreeModel::iterator& iter_, int column, Glib::ValueBase& value_) const
{
  const GtkTreeIter *iter= iter_.gobj();
  GValue *value= value_.gobj();
  
  if (_disable_checks || treeiter_valid(iter))
  {
    unsigned long r= (unsigned long)iter->user_data;
    
    // from 0 to 2*_resultset->columns_num, the columns correspond
    // to the content (text or blob ptr) and color of the resultset,
    // alternately.
    // values bigger than that, are special attributes as defined
    // in enum SpecialColumns
    
    if (column >= (int)_resultset->columns_num*2)
    {
      // editable attribute
      
      // hack: this is for manually setting special column values
      switch (column-(int)_resultset->columns_num*2)
      {
      case RowEditable:
        {
          bool editable= _edit_enabled;

          /* this is for the editable flag */
          if (editable)
          {
            if (!get_row_editable(r))
              editable= false;
          }
          g_value_init(value, G_TYPE_BOOLEAN);
          g_value_set_boolean(value, (gboolean)editable);
        }
        break;
      case RowStatusIcon:
//        break;
      default:
        g_value_init(value, G_TYPE_OBJECT);
        g_value_set_object(value, NULL);
        break;
      }
    }
    else if (column < (int)_resultset->columns_num*2)
    {
      // values
      
      bool tmp= _disable_checks;
      gpointer data= NULL;
      size_t size= 0;
      bool is_blob;
      switch (_resultset->columns[column/2].column_type)
      {
      case MYX_RSCT_BLOB:
      case MYX_RSCT_TEXT:
        is_blob= true;
        break;
      default:
        is_blob= false;
      }

      g_value_init(value, G_TYPE_STRING);
      
      _disable_checks= true;
      if (get_value(iter_, column, data, size))
      {
        if ((column % 2) == 1) // color
        {
          g_value_set_string(value, (gchar*)data);
        }
        else
        {
          if (is_blob) {
            g_value_set_string(value, data2str(data, size).c_str());
          } else {
            g_value_set_static_string(value, (gchar*)data);
          }
        }
      }
      else
      {
        if (is_blob) {
          g_value_set_string(value, data2str(NULL,0).c_str());
        }
      }
      _disable_checks= tmp;
    }
  }
  else
    g_warning("invalid iterator!");
}


void MGResultSetModel::set_value_impl(const Gtk::TreeModel::iterator& row, int column, const Glib::ValueBase& value)
{
  // dummy, not used by our code
}


void MGResultSetModel::set_value(const Gtk::TreeModel::iterator& row, int column, const gpointer value, size_t value_length)
{
  unsigned long row_num= (unsigned long)row.gobj()->user_data;

  switch ((column%2))
  {
  case 0:
    set(row_num, column/2, (char*)value, value_length);
    break;
  case 1:
    // color, ignore
    break;
  }
}


bool MGResultSetModel::treeiter_valid(const Gtk::TreeModel::iterator &iter) const
{
  return iter.get_stamp() == _stamp;
}


bool MGResultSetModel::treeiter_valid(const GtkTreeIter *iter) const
{
  return iter->stamp == _stamp;
}


void MGResultSetModel::notify_resultset_changed()
{
  _stamp++;
}


void MGResultSetModel::set_editable(bool flag)
{
  if (_edit_enabled != flag)
  {
    _edit_enabled= flag;
    
    set_placeholder_enabled(flag);
  }
}


bool MGResultSetModel::get_editable()
{
  return _edit_enabled;
}


void MGResultSetModel::set_state_colors(const std::vector<Glib::ustring> &colors)
{
  for (unsigned int i= 0; i < _state_colors.size(); i++)
    g_free(_state_colors[i]);

  _state_colors.reserve(colors.size());
  for (unsigned int i= 0; i < colors.size(); i++)
  {
    _state_colors[i]= g_strdup(colors[i].c_str());
  }
}


void MGResultSetModel::set_diff_colors(const std::vector<Glib::ustring> &colors)
{
  for (unsigned int i= 0; i < _diff_colors.size(); i++)
    g_free(_diff_colors[i]);

  _diff_colors.reserve(colors.size());
  for (unsigned int i= 0; i < colors.size(); i++)
  {
    _diff_colors[i]= g_strdup(colors[i].c_str());
  }
}


MYX_RS_ROW *MGResultSetModel::get_iter_row(const Gtk::TreeModel::iterator &iter)
{
  return _resultset->rows + (long)iter.gobj()->user_data;
}


Gtk::TreeModel::iterator MGResultSetModel::append()
{
  Gtk::TreeModel::iterator iter(this);
  unsigned long new_row= add_row();

  // make new iterator
  iter.set_stamp(_stamp);
  iter.gobj()->user_data= (gpointer)new_row;
  
  // this is generating a runtime error (!?)
  // emit signal
//  row_inserted(get_path(iter), iter);

  return iter;
}


void MGResultSetModel::erase(const Gtk::TreeModel::iterator &iter)
{
  unsigned long row= (unsigned long)iter.gobj()->user_data;

  delete_row(row);
}
