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

// NOTE: shared with OSX

#include "MYXResultSet.h"


class MYXResultSet::RSChange
{
  public:
    MYX_RS_ACTION *action;
    unsigned int column;
    bool has_error;

    RSChange(const MYX_RS_ACTION &act, int col)
      : column(col)
      {
        action= myx_query_create_action(act.action, act.row, act.column,
                                        act.new_value_length, act.new_value);
        
        has_error= false;
      }
    RSChange(MYX_RS_ACTION *act, int col)
      : action(act), column(col)
      {
        has_error= false;
      }
    ~RSChange()
    {
      if (action)
      {
        g_free(action->new_value);
        action->new_value= NULL;
        myx_query_free_action(action);
      }
    }

    void add_to_resultset(MYX_RESULTSET *rs)
    {
      myx_query_add_action(rs, action);
      g_free(action);
      action= NULL;
    }
    
    inline MYX_RS_ACTION_TYPE get_type() { return action->action; };
    inline size_t get_size() { return action->new_value_length; };
    inline char *get_value() { return action->new_value; };
    inline unsigned int get_row() { return action->row; };
};


MYXResultSet::MYXResultSet(MYX_RESULTSET *rs, const MYXResultSetCallbacks &cbacks)
  : _resultset(rs), _callbacks(cbacks), _new_rows(0), _placeholder(false)
{
}


MYXResultSet::~MYXResultSet()
{
  for (std::list<RSChange*>::iterator it= _changes.begin(); it != _changes.end(); ++it)
    delete *it;
}

    
unsigned int MYXResultSet::add_row()
{
  unsigned int row= get_row_count();
  _new_rows++;

  if (_callbacks.row_added)
    (*_callbacks.row_added)(this, row);
  
  return row;
}


void MYXResultSet::delete_row(unsigned int row)
{
  MYX_RS_ACTION *action;
  
  if (row < _resultset->rows_num + _new_rows - 1)
  {
    action= myx_query_create_action(MYX_RSA_DELETE, row, NULL, 0, NULL);

    RSChange *change= new RSChange(action, 0);

    _changes.push_front(change);
  }
}


bool MYXResultSet::row_has_changes(unsigned int row)
{
  for (std::list<RSChange*>::const_iterator it= _changes.begin();
       it != _changes.end(); ++it)
  {
    if ((*it)->get_row() == row)
      return true;
  }
  return false;
}


MYX_RS_COLUMN_TYPE MYXResultSet::get_column_type(unsigned int column)
{
  if (_resultset->columns_num <= column)
    return _resultset->columns[column].column_type;
  else
    return MYX_RSCT_INTEGER;
}


void MYXResultSet::set_placeholder_enabled(bool flag)
{
  if (_placeholder != flag)
  {
    _placeholder= flag;
  
    if (_placeholder)
      add_row();
    else
    {
      int i= _resultset->rows_num + _new_rows - 1;
      while (i >= (int)_resultset->rows_num)
      {
        if (row_has_changes(i))
        {
          break;
        }

        if (_callbacks.row_deleted)
          (*_callbacks.row_deleted)(this, i);
        _new_rows--;
        i--;
      }
    }
  }
}


MYXResultSet::RSChange* MYXResultSet::find_change(unsigned int row, unsigned int column) const
{
  for (std::list<RSChange*>::const_iterator it= _changes.begin();
       it != _changes.end(); ++it)
  {
    if ((*it)->get_row() == row && ((*it)->column == column ||
                                    (*it)->get_type() == MYX_RSA_DELETE))
      return *it;
  }
  return 0;
}


MYXRSEditStatus MYXResultSet::get_edit_status(unsigned int row, unsigned int column) const
{
  if (row >= _resultset->rows_num)
  {
    if (row == _resultset->rows_num + _new_rows - 1 && _placeholder)
      return MESPlaceHolder;
    else
    {
      RSChange *change= find_change(row, 0);

      if (change && change->get_type() == MYX_RSA_DELETE)
        return MESDeleted;
      else
        return MESAdded;
    }
  }
  else if (!_resultset->rows[row].fields)
    return MESAdded;
  else
  {
    RSChange *change= find_change(row, column);
    
    if (change && change->get_type() == MYX_RSA_DELETE)
      return MESDeleted;
    else if (change)
      return MESChanged;
    else
      return MESUnchanged;
  }
}


MYXRSCompareStatus MYXResultSet::get_compare_status(unsigned int row, unsigned int column)
{
  switch (_resultset->rows[row].diff&MYX_RD_MASK)
  {
    case MYX_RD_OTHER_ONLY:
      return MCSOnlyInOther;
    case MYX_RD_THIS_ONLY:
      return MCSOnlyInThis;
    case MYX_RD_DIFFERS:
      // check column specific differences
      if ((_resultset->rows[row].diff>>4) & (1<<column))
        return MCSDiffers;
    default:
      return MCSMatch;
  }
}


bool MYXResultSet::isNull(unsigned int row, unsigned int column)
{
  char *dummy= NULL;
  size_t dummys;
  
  if (!get(row, column, dummy, dummys))
    return false;
  
  return dummy == NULL;
}


bool MYXResultSet::get(unsigned int row, unsigned int column,
                       char *&value, size_t &size) const
{
  RSChange *change= find_change(row, column);
  if (change)
  {
    if (change->get_type() == MYX_RSA_DELETE)
    {
      if (_resultset->rows[row].fields && row < _resultset->rows_num)
      {
        size= _resultset->rows[row].fields[column].value_length;
        value= _resultset->rows[row].fields[column].value;
      }
      else
      {
        size= change->get_size();
        value= change->get_value();
      }
    }
    else
    {
      size= change->get_size();
      value= change->get_value();
    }
  }
  else
  {
    if (_resultset->rows && _resultset->rows[row].fields && row < _resultset->rows_num)
    {
      size= _resultset->rows[row].fields[column].value_length;
      value= _resultset->rows[row].fields[column].value;
    }
    else
    {
      size= 0;
      value= NULL;
      return false;
    }
  }
  return true;
}


void MYXResultSet::set(unsigned int row, unsigned int column,
                       const char *value, size_t size)
{
  RSChange *change= find_change(row, column);

  // check if same value as last change (or if it's deleted)
  if (change && (change->get_type() == MYX_RSA_DELETE ||
                 (change->get_size() == size &&
                  memcmp(change->get_value(), value, size)==0)))
    return;
  
  // check if same value as in resultset
  if (row < _resultset->rows_num)
  {
    if (_resultset->rows[row].fields == NULL)
      return;
    
//    if (_resultset->rows[row].fields[column].value_length == size
//        && memcmp(_resultset->rows[row].fields[column].value, value, size)==0)
//      return;
  }

  if (row == _resultset->rows_num + _new_rows - 1 && _placeholder)
    add_row();

  MYX_RS_ACTION *action= myx_query_create_action(row >= _resultset->rows_num ? MYX_RSA_ADD : MYX_RSA_UPDATE,
                                                 row, _resultset->columns+column,
                                                 size, value);
  
  _changes.push_front(new RSChange(action, column));
}


bool MYXResultSet::get_row_editable(unsigned int row) const
{
  RSChange *change= find_change(row, 0);
  
  if (change && change->get_type() == MYX_RSA_DELETE)
    return false;
  return true;
}


unsigned int MYXResultSet::get_row_count() const
{
  return _resultset->rows_num + _new_rows;
}


void MYXResultSet::pre_commit()
{
  // go through all actions requested and leave only those that
  // should actually be executed
  std::list<RSChange*>::iterator jter, iter= _changes.begin();
  while (iter != _changes.end())
  {
    RSChange *change= *iter;

    _changes.erase(iter);

    // locate all previous changes to the same cell and remove them
    jter= _changes.begin();
    while (jter != _changes.end())
    {
      std::list<RSChange*>::iterator jnext= jter;
      RSChange *ch= *jter;
      ++jnext;

      if (ch->get_row() == change->get_row() &&
          (change->get_type() == MYX_RSA_DELETE ||
           change->column == ch->column))
      {
        _changes.erase(jter);
      }
      jter= jnext;
    }

    // do not add DELETEs for rows that were newly added
    if ((change->get_row() < _resultset->rows_num && _resultset->rows[change->get_row()].fields!=NULL) ||
        change->get_type() != MYX_RSA_DELETE)
      change->add_to_resultset(_resultset);
    else
    {
      // go through all actions and decrement the row index of
      // the new items that come after this one
      jter= _changes.begin();
      while (++jter != _changes.end())
      {
        RSChange *ch= *jter;
        
        if (ch->action->row >= _resultset->rows_num 
            && ch->action->row > change->action->row)
          --ch->action->row;
      }
    }
    iter= _changes.begin();
  }
}


void MYXResultSet::pack()
{
  if (_callbacks.row_deleted)
  {
    for (unsigned int i= 0; i < _resultset->rows_num; i++)
    {
      if (!_resultset->rows[i].fields)
      {
        (*_callbacks.row_deleted)(this, _resultset->rows_num-i-1);
      }
    }
  }

  myx_query_pack_resultset(_resultset);
}
  

void MYXResultSet::post_commit(bool failed)
{  
  _new_rows= 0;
  if (_resultset->actions)
  {
    myx_query_update_resultset(_resultset);
    if (_resultset->actions)
    {
      if (failed)
      {
        // add back the actions that failed
        unsigned int i;
        for (i= 0; i < _resultset->actions->actions_num; i++)
        {
          if (_resultset->actions->actions[i].status == MYX_RSAS_NEW
              || _resultset->actions->actions[i].status == MYX_RSAS_FAILED)
          {
            if (_resultset->actions->actions[i].row >= _resultset->rows_num)
            {
              if (_new_rows < _resultset->actions->actions[i].row - (_resultset->rows_num-1))
              {
                g_message("recycle row %i", _resultset->actions->actions[i].row - (_resultset->rows_num-1));
                _new_rows= _resultset->actions->actions[i].row - (_resultset->rows_num-1);
              }
            }
            
            _changes.push_front(new RSChange(_resultset->actions->actions[i], _resultset->actions->actions[i].column-_resultset->columns));
          }
        }
        g_free(_resultset->actions->actions);
        g_free(_resultset->actions);
        _resultset->actions= NULL;
      }
    }
  }
  
  if (!failed)
  {
    myx_query_discard_actions(_resultset);
    pack();
  }
}


void MYXResultSet::discard_changes()
{
  _new_rows= 0;
  for (std::list<RSChange*>::iterator it= _changes.begin(); it != _changes.end(); ++it)
    delete *it;
  _changes.clear();

  if (_resultset->actions)
    myx_query_discard_actions(_resultset);
  
  myx_query_pack_resultset(_resultset);
}


bool MYXResultSet::has_changes()
{
  if (_changes.begin() == _changes.end())
    return false;
  return true;
}
