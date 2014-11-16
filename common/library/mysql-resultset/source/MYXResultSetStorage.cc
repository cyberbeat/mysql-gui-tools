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


#include "MYXResultSetStorage.h"


#if 0
#define DPRINT(s) g_message(s)
#else
#define DPRINT(s)
#endif

struct MYXRowBlock {
  unsigned int row_count;
  unsigned int rows_allocated;
  MYXRow rows[1];
};


#define DEFAULT_ROW_BLOCK_SIZE (4*1024 / sizeof(MYXField))



static void free_row_data(MYXRow *row, int column_count)
{
  if (row->fields)
  {
    for (int i= 0; i < column_count; i++)
      g_free(row->fields[i].value);
    g_free(row->fields);
  }
}


/*
static void free_change(MYXResultSetFieldChange *change)
{
  g_free(change->new_value.value);
}
 */




MYXResultSetStorage::MYXResultSetStorage(MYXResultSetSource *dataSource)
  : _source(dataSource)
{
  _row_block_size= DEFAULT_ROW_BLOCK_SIZE;
  
  _row_page_size= 100;
  
  _fetch_complete= false;
  _placeholder= false;

  reset_resultset();

  _columns= dataSource->get_columns();
  
  memset(&_callbacks, 0, sizeof(_callbacks));
}


MYXResultSetStorage::~MYXResultSetStorage()
{
  reset_resultset();
}



MYXRow *MYXResultSetStorage::get_row(bigint row) const
{
  bigint i= 0;
  for (std::vector<MYXRowBlock*>::const_iterator iter= _row_blocks.begin();
       iter != _row_blocks.end(); ++iter)
  {
    i+= (*iter)->row_count;
    if (i >= row)
    {
      return &((*iter)->rows[row - (i - (*iter)->row_count)]);
    }
  }
  return 0;
}


void MYXResultSetStorage::reset_resultset()
{
  for (std::map<bigint,FieldChange*>::iterator it= _changes.begin(); 
       it != _changes.end(); ++it)
    delete it->second;
  _changes.clear();
  
  for (std::vector<MYXResultSetSource::ColumnInfo*>::iterator it= _columns.begin();
       it != _columns.end(); ++it)
    delete *it;
  
  _last_change= 0;
  _undo_position= 0;
  
  for (std::vector<MYXRowBlock*>::iterator it= _row_blocks.begin();
       it != _row_blocks.end(); ++it)
  {
    for (unsigned int i= 0; i < (*it)->row_count; i++)
      free_row_data((*it)->rows+i, _columns.size());
    g_free(*it);
  }
  _row_blocks.clear();
  
  _starting_row= 0;
  _current_row_index= -1;

  _new_rows= 0;
  _row_count= 0;
}



void MYXResultSetStorage::set_callbacks(const MYXResultSetCallbacks &cbacks)
{
  _callbacks= cbacks;
}


bool MYXResultSetStorage::add_result_row(const char **fields, size_t *lengths)
{
  MYXRowBlock *block= 0;
  int columns_num= _columns.size();

  if (!_row_blocks.empty())
  {
    block= _row_blocks[_row_blocks.size()-1];
    if (block->row_count == block->rows_allocated)
      block= 0;
  }
  
  if (!block)
  {
    block= (MYXRowBlock*)g_malloc(sizeof(MYXRowBlock) + sizeof(MYXRow)*(_row_block_size-1));
    if (!block)
      return false;
    
    block->rows_allocated= _row_block_size;
    block->row_count= 0;
    _row_blocks.push_back(block);
  }

  MYXRow *row= block->rows + block->row_count;
  row->fields= (MYXField*)g_new0(MYXField, columns_num);
  if (!row->fields)
    return false;

  if (fields)
  {
    for (int i= 0; i < columns_num; i++)
    {
      if (lengths[i] > 0)
      {
        row->fields[i].value= (char*)g_memdup(fields[i], lengths[i]+1);
        if (!row->fields[i].value && fields[i])
          return false;
        row->fields[i].value[lengths[i]]= 0;
      }
      else
      {
        row->fields[i].value= (char*)g_memdup(fields[i], lengths[i]);
        if (!row->fields[i].value && fields[i])
          return false;
      }
      row->fields[i].length= lengths[i];
    }
  }
  block->row_count++;
  _row_count++;
  
  return true;
}



void MYXResultSetStorage::set_page_size(unsigned int size)
{
  if (_row_blocks.size() > 0)
  {
    g_warning("MYXResultSetStorage: cannot set page size once results are being fetched");
    return;
  }
  _row_page_size= size;
}


void MYXResultSetStorage::set_page_start(bigint row)
{
  _starting_row= row;
  
  //XXX
}



bool MYXResultSetStorage::seek_row(bigint row)
{
  if (row >= get_row_count() || row < 0)
    return false;

  _current_row_index= row;

  return true;
}


bool MYXResultSetStorage::move_first()
{
  if (get_row_count() > 0)
  {
    _current_row_index= -1;

    return true;
  }
  return false;
}


bool MYXResultSetStorage::move_last()
{
  if (get_row_count() > 0)
  {
    _current_row_index= get_row_count();
    return true;
  }
  return false;
}


bool MYXResultSetStorage::next_row(bool auto_fetch)
{
  if (!_fetch_complete && _current_row_index == get_row_count()-1)
  {
    if (!auto_fetch || fetch_next_page() <= 0)
      return false;
  }
  if (_current_row_index < get_row_count()-1)
  {
    _current_row_index++;
    return true;
  }
  return false;
}


bool MYXResultSetStorage::prior_row()
{
  if (_current_row_index > 0)
  {
    --_current_row_index;
    return true;
  }
  return false;
}


bool MYXResultSetStorage::fetch_all()
{
  while (!_fetch_complete)
    if (!fetch_next_page())
      return false;

  return true;
}


unsigned int MYXResultSetStorage::fetch_next_page()
{
  unsigned int cols= _columns.size();
  const char **row= g_new(const char*, cols);
  size_t *length= g_new(size_t, cols);
  unsigned int i;
  
  if (!row || !length)
    return 0;

  DPRINT("fetching next page...");
  
  for (i= 0; i < _row_page_size && _source->next(); i++)
  {
    if (!_source->get_row(row, length))
      return 0;

    add_result_row(row, length);
  }

  if (_source->eof())
    _fetch_complete= true;

  g_free(row);
  g_free(length);
  
  return i;
}


    
bigint MYXResultSetStorage::add_row()
{
  if (_fetch_complete)
  {
    bigint row= get_row_count();
    _new_rows++;

    if (_callbacks.row_added)
      (*_callbacks.row_added)(this, row);
    
    return row;
  }
  else
  {
    g_warning("MYXResultSetStorage: attempting to add row in a incomplete resultset");
    return -1;
  }
}


void MYXResultSetStorage::delete_row(bigint row)
{  
  if (row < _row_count + _new_rows - 1)
  {
    FieldChange *change= new FieldChange();

    change->type= CH_DELETE;
    change->column= -1;
    change->new_value.value= NULL;
    change->new_value.length= 0;

    change->next_field= NULL;
    change->next_change= _last_change;
    _last_change= change;

    _changes[row]= change;
  }
}

bool MYXResultSetStorage::row_has_changes(bigint row)
{
  if (_changes.find(row) != _changes.end())
    return true;
  
  return false;
}


int MYXResultSetStorage::get_column_count()
{
  return (int)_columns.size();
}


MYXRSColumnType MYXResultSetStorage::get_column_type(int column)
{
  if ((int)_columns.size() <= column)
    return _columns[column]->type;
  else
    return MYX_RSCT_INTEGER;
}


void MYXResultSetStorage::set_placeholder_enabled(bool flag)
{
  if (_placeholder != flag)
  {
    _placeholder= flag;
  
    if (_placeholder)
      add_row();
    else
    {
      bigint i= get_row_count();
      while (i >= _row_count)
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


MYXResultSetStorage::FieldChange* MYXResultSetStorage::find_change(bigint row, int column) const
{
  std::map<bigint,FieldChange*>::const_iterator iter= _changes.find(row);
  
  if (iter != _changes.end())
  {
    FieldChange *change= iter->second;
    while (change)
    {
      if (change->column == column || change->type == CH_DELETE)
        return change;
      change= change->next_field;
    }
  }
  return 0;
}



MYXRSCompareStatus MYXResultSetStorage::get_compare_status(bigint row, int column)
{
  /*
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
  }*/
}


MYXRSEditStatus MYXResultSetStorage::get_edit_status(bigint row, int column) const
{
  if (row >= _row_count)
  {
    if (row == _row_count + _new_rows - 1 && _placeholder)
      return MES_PLACE_HOLDER;
    else
    {
      FieldChange *change= find_change(row, 0);

      if (change && change->type == CH_ADD)
        return MES_DELETED;
      else
        return MES_ADDED;
    }
  }
  else if (!get_row(row)->fields)
    return MES_ADDED;
  else
  {
    FieldChange *change= find_change(row, column);
    
    if (change && change->type == CH_DELETE)
      return MES_DELETED;
    else if (change)
      return MES_CHANGED;
    else
      return MES_UNCHANGED;
  }
}


bool MYXResultSetStorage::get(bigint row, int column,
                              const char *&value, size_t &size) const
{
  FieldChange *change= find_change(row, column);
  if (change)
  {
    if (change->type == CH_DELETE && row < _row_count)
    {
      MYXRow *rowp= get_row(row);
      if (rowp->fields)
      {
        size= rowp->fields[column].length;
        value= rowp->fields[column].value;
      }
      else
      {
        size= change->new_value.length;
        value= change->new_value.value;
      }
    }
    else
    {
      size= change->new_value.length;
      value= change->new_value.value;
    }
    return true;
  }
  else if (row < _row_count)
  {
    MYXRow *rowp= get_row(row);

    if (rowp->fields)
    {
      size= rowp->fields[column].length;
      value= rowp->fields[column].value;
      return true;
    }
  }

  size= 0;
  value= 0;
  return false;
}


bool MYXResultSetStorage::get(int column, const char *&value, size_t &size) const
{
  return get(_current_row_index, column, value, size);
}


bigint MYXResultSetStorage::get_current_row() const
{
  return _current_row_index;
}


bool MYXResultSetStorage::set(bigint row, int column,
                              const char *value)
{
  return set(row, column, value, strlen(value));
}


bool MYXResultSetStorage::set(bigint row, int column,
                              const char *value, size_t size)
{
  FieldChange *change= find_change(row, column);

  // check if same value as last change (or if it's deleted)
  if (change && (change->type == CH_DELETE ||
                 (change->new_value.length == size &&
                  memcmp(change->new_value.value, value, size)==0)))
    return true; // no change since last change

  // check if same value as in resultset
  if (row < _row_count)
  {
    MYXRow *rowPtr= get_row(row);
    
    if (rowPtr->fields == NULL)
      return true;
    
    if (rowPtr->fields[column].length == size
        && memcmp(rowPtr->fields[column].value, value, size)==0)
      return true; // no change
  }

  // change was made in the placeholder, so add a new placeholder
  if (row == _row_count + _new_rows - 1 && _placeholder)
    add_row();
  
  change= new FieldChange();
  change->column= column;
  change->type= row >= _row_count ? CH_ADD : CH_EDIT;
  change->status= ST_PENDING;
  
  change->new_value.length= size;
  change->new_value.value= (char*)g_memdup(value, size+1);
  if (!change->new_value.value && value)
  {
    delete change;
    return false;
  }

  std::map<bigint,FieldChange*>::iterator rschange= _changes.find(row);
  if (rschange == _changes.end())
  {
    change->next_field= 0;
    _changes[row]= change;
  }
  else
  {
    FieldChange *ch, *prev;

    ch= rschange->second;
    prev= 0;
    
    while (ch && ch->column >= column)
    {
      prev= ch;
      ch= ch->next_field;
    }
    
    if (prev)
    {
      change->next_field= prev->next_field;
      prev->next_field= change;
    }
    else
    {
      change->next_field= rschange->second;
      rschange->second= change;
    }
  }

  change->next_change= _last_change;
  _last_change= change;
  
  return true;
}


bool MYXResultSetStorage::is_row_editable(bigint row) const
{
  FieldChange *change= find_change(row, 0);
  
  if (change && change->type == CH_DELETE)
    return false;
  return true;
}


bigint MYXResultSetStorage::get_row_count() const
{
  return _row_count + _new_rows;
}



//===========================================================================
// Editing
//===========================================================================

std::list<MYXResultSetStorage::RowChange> MYXResultSetStorage::prepare_commit()
{
  // go through all changes and:
  // - consolidate all changes from the same row
  // - update row numbers after deleted rows

  std::map<bigint,FieldChange*>::iterator jter, iter;
  std::list<RowChange> row_changes;

  iter= _changes.begin();
  while (iter != _changes.end())
  {
    RowChange rchange;
    FieldChange *change;
    bool deleted= false;
    bool added= false;
    
    change= iter->second;
    
    while (change)
    {
      if (change->status == ST_PENDING)
      {
        if (change->type == CH_DELETE)
        {
          deleted= true;
          break;
        }
        else if (change->type == CH_ADD)
        {
          added= true;
        }
      }
      change= change->next_field;
    }

    if (deleted) // row was deleted, add it as such
    {
      if (iter->first < _row_count)
      {
        rchange.row= iter->first;
        rchange.type= CH_DELETE;
        rchange.original_row= get_row(iter->first);

        row_changes.push_back(rchange);
      } // else, the row doesn't exist in the db

      // mark all other actions for the row as discarded
      change= iter->second;
      while (change)
      {
        change->status= ST_DISCARDED;
        change= change->next_field;
      }
    }
    else
    {
      bool has_changes= false;

      // in case of an added or updated row, consolidate all fields
      rchange.row= iter->first;
      rchange.type= added ? CH_ADD : CH_EDIT;
      for (unsigned int i= 0; i < _columns.size(); i++)
        rchange.fields.push_back(0);

      rchange.original_row= added ? 0 : get_row(iter->first);

      change= iter->second;
      while (change)
      {
        if (change->status == ST_PENDING)
        {
          rchange.fields[change->column]= &change->new_value;
          has_changes= true;
        }
        change= change->next_field;
      }

      if (has_changes)
        row_changes.push_back(rchange);
    }
    ++iter;
  }
  
  return row_changes;
}


void MYXResultSetStorage::finalize_commit(MYXResultSetCommitResult *result)
{
  bigint deleted_lines= 0;
  
  //XXX should these be executed during commit, right before the callbacks?
  
  // delete applied changes from the changes list
  // delete change rows where everything has been applied
  // update the row numbers for the remaining changes
 
  for (std::map<bigint,FieldChange*>::iterator iter= _changes.begin();
       iter != _changes.end(); ++iter)
  {
    FieldChange *ch= iter->second;
    FieldChange *prev= 0;
    
    while (ch)
    {
      FieldChange *next= ch->next_field;

      if (ch->status == ST_APPLIED)
      {
        if (prev)
          prev->next_field= ch->next_field;
        else
          iter->second= ch->next_field;

        if (ch->prev_change)
          ch->prev_change->next_change= ch->next_change;
        if (ch->next_change)
          ch->next_change->prev_change= ch->prev_change;
        if (_last_change == ch)
          _last_change= ch->next_change;

        // apply it to the live row list
        switch (ch->type)
        {
        case CH_ADD:
          {
            add_result_row(NULL, NULL);
            
            MYXRow *row= get_row(_row_count-1);
            
            memset(row->fields, 0, sizeof(char*) * _columns.size());

            // delete everything in this line, copying values
            while (iter->second)
            {
              next= iter->second->next_field;
              
              if (iter->second->prev_change)
                iter->second->prev_change->next_change= iter->second->next_change;
              if (iter->second->next_change)
                iter->second->next_change->prev_change= iter->second->prev_change;
              if (_last_change == iter->second)
                _last_change= iter->second->next_change;

              row->fields[iter->second->column]= iter->second->new_value;
              iter->second->new_value.value= 0;

              delete iter->second;
              iter->second= next;
            }
            next= 0;
            ch= 0;
          }
          break;

        case CH_EDIT:
          {
            MYXRow *row= get_row(iter->first);
            if (!row->fields)
              row->fields= g_new0(MYXField, _columns.size());
            if (row->fields[ch->column].value)
              g_free(row->fields[ch->column].value);

            // transfer ownership of the value data
            row->fields[ch->column]= ch->new_value;
            ch->new_value.value= 0;
          }
          break;

        case CH_DELETE:
          {
            if (iter->first < _row_count)
            {
              bigint row= iter->first;
              unsigned int row_index= 0;
              bigint i= 0;

              for (std::vector<MYXRowBlock*>::iterator row_block= _row_blocks.begin();
                   row_block != _row_blocks.end(); ++row_block)
              {
                i+= (*row_block)->row_count;

                if (i >= row)
                {
                  row_index= row - (i - (*row_block)->row_count);
                  
                  // free row data
                  MYXRow *row_data= (*row_block)->rows + row_index;

                  free_row_data(row_data, _columns.size());

                  // update the row list
                  memmove((*row_block)->rows + row_index,
                          (*row_block)->rows + row_index + 1,
                          sizeof(MYXRow));
              
                  (*row_block)->row_count--;
                  if ((*row_block)->row_count == 0)
                  {
                    g_free(*row_block);
                    _row_blocks.erase(row_block);
                  }
                  break;
                }
              }
            }

            // update line number info in result errors
            result->note_row_deletion(iter->first);
            
            deleted_lines++;

            // delete everything in this line
            while (iter->second)
            {
              next= iter->second->next_field;

              if (iter->second->prev_change)
                iter->second->prev_change->next_change= iter->second->next_change;
              if (iter->second->next_change)
                iter->second->next_field->prev_change= iter->second->prev_change;
              if (_last_change == iter->second)
                _last_change= iter->second->next_change;

              delete iter->second;
              iter->second= next;
            }
            next= 0;
            ch= 0;
            break;
          }
        }

        delete ch;
      }
      else if (ch->status == ST_DISCARDED)
      {
        if (prev)
          prev->next_field= ch->next_field;
        else
          iter->second= ch->next_field;

        if (ch->prev_change)
          ch->prev_change->next_change= ch->next_change;
        if (ch->next_change)
          ch->next_change->prev_change= ch->prev_change;
        if (_last_change == ch)
          _last_change= ch->next_change;
        
        g_free(ch->new_value.value);
        delete ch;
      }
      else
      {
        // only change prev if ch wasn't deleted
        prev= ch;
      }
      ch= next;
    }

    if (!iter->second)
      _changes.erase(iter);
  }
}



MYXResultSetCommitResult *MYXResultSetStorage::commit_changes()
{
  std::list<RowChange> changes;
  MYXResultSetCommitResult *result= new MYXResultSetCommitResult();
  bigint delete_count= 0;

  // prepare the actions
  changes= prepare_commit();

  // process actions
  for (std::list<RowChange>::iterator change= changes.begin(); change != changes.end(); ++change)
  {
    bool success= false;
    
    switch (change->type)
    {
    case CH_ADD:
      // go over all ADDs for the same row and commit at once
      success= db_insert_row(*change, result);
      if (success && _callbacks.row_added)
        (*_callbacks.row_added)(this, change->row - delete_count);
      break;

    case CH_EDIT:
      // commit the whole row at once
      success= db_update_row(*change, result);
      if (success && _callbacks.row_changed)
        (*_callbacks.row_changed)(this, change->row - delete_count);
      break;

    case CH_DELETE:
      // just delete it
      success= db_delete_row(*change, result);
      if (success && _callbacks.row_deleted)
      {
        (*_callbacks.row_deleted)(this, change->row - delete_count);
        delete_count++;
      }
      break;
    }

    FieldChange *ch= _changes[change->row];

    if (success)
    { // mark all changes for the row as applied
      while (ch)
      {
        if (ch->status == ST_PENDING)
          ch->status= ST_APPLIED;
        ch= ch->next_field;
      }
    }
    else  // mark all changes for the row as failed
    {
      while (ch)
      {
        if (ch->status == ST_PENDING)
          ch->status= ST_FAILED;
        ch= ch->next_field;
      }
    }
  }
  
  // cleanup the list of actions removing what was applied and updating
  // line numbers
  finalize_commit(result);

  return result;
}


void MYXResultSetStorage::discard_changes()
{
  _new_rows= 0;

  for (std::map<bigint,FieldChange*>::iterator iter= _changes.begin();
       iter != _changes.end(); ++iter)
  {
    g_free(iter->second->new_value.value);
    delete iter->second;
  }
  _changes.clear();
  _last_change= 0;
}


bool MYXResultSetStorage::has_changes()
{
  if (_changes.begin() == _changes.end())
    return false;
/*  
  for (std::map<bigint,FieldChange*>::const_iterator iter= _changes.begin();
       iter != _changes.end(); ++iter)
  {
    g_message("CH: %lli,%i %i (%i)", iter->first, iter->second->column, 
              iter->second->type,
              iter->second->status);
  }
  */
  return true;
}


bool MYXResultSetStorage::is_editable()
{
  if (_columns.size() > 0)
  {
    unsigned int i;
    for (i= 0; i < _columns.size(); i++)
    {
      if (_columns[i]->is_pk)
        return true;
    }
  }
  return false;
}



bool MYXResultSetStorage::start_update()
{
  return true;
}


bool MYXResultSetStorage::end_update()
{
  return true;
}



char *MYXResultSetStorage::prepare_pk_condition(char *buffer, RowChange &row)
{
  unsigned int i;
  char *start= buffer;

  *buffer= 0;
  
  // iterate over list of columns
  for (i= 0; i < _columns.size(); i++)
  {
    if (_columns[i]->is_pk)
    {
      if (*buffer)
        buffer= strmov(buffer, " AND ");
      buffer= strmov(buffer, "`");
      buffer= strmov(buffer, _columns[i]->name);
      buffer= strmov(buffer, "`=");

      if (row.original_row)
        buffer+= prepare_value(buffer, row.original_row->fields+i, i);
      else
      {
        buffer= start;
        break; // if there's no original_row this is a new row and this shouldn't be called
      }
    }
  }

  return (buffer == start) ? NULL : buffer;
}



MYXResultSetCommitResult::MYXResultSetCommitResult()
{
  _status= Success;
}


MYXResultSetCommitResult::~MYXResultSetCommitResult()
{
  for (std::list<InfoRecord*>::iterator it= _info.begin(); it != _info.end(); ++it)
    delete *it;
}


void MYXResultSetCommitResult::add_error(bigint row, MYXResultSetStorage::ChangeType action, const char *message, int code)
{
  InfoRecord *record= new InfoRecord();
  record->row= row;
  record->action= action;
  record->code= code;
  record->type= 2;
  record->message= g_strdup(message);

  _info.push_back(record);
}


void MYXResultSetCommitResult::add_warning(bigint row, MYXResultSetStorage::ChangeType action, const char *message, int code)
{
  InfoRecord *record= new InfoRecord();
  record->row= row;
  record->action= action;
  record->code= code;
  record->type= 1;
  record->message= g_strdup(message);

  _info.push_back(record);
}


void MYXResultSetCommitResult::add_note(bigint row, MYXResultSetStorage::ChangeType action, const char *message, int code)
{
  InfoRecord *record= new InfoRecord();
  record->row= row;
  record->action= action;
  record->code= code;
  record->type= 0;
  record->message= g_strdup(message);

  _info.push_back(record);
}


bool MYXResultSetCommitResult::has_warnings()
{
  for (std::list<InfoRecord*>::iterator iter= _info.begin();
       iter != _info.end(); ++iter)
  {
    if ((*iter)->type == 1)
      return true;
  }
  return false;
}


bool MYXResultSetCommitResult::has_errors()
{
  if (_status != Success)
    return true;
  for (std::list<InfoRecord*>::iterator iter= _info.begin();
       iter != _info.end(); ++iter)
  {
    if ((*iter)->type == 2)
      return true;
  }
  return false;
}


void MYXResultSetCommitResult::note_row_deletion(bigint row)
{
  for (std::list<InfoRecord*>::iterator iter= _info.begin();
       iter != _info.end(); ++iter)
  {
    if ((*iter)->row > row) // == should never happen!
    {
      (*iter)->row--;
    }
  }
}



void MYXResultSetCommitResult::dump()
{
  char *type[3]= {"NOTE", "WARNING", "ERROR"};
  for (std::list<InfoRecord*>::iterator iter= _info.begin();
       iter != _info.end(); ++iter)
  {
    printf("%s: row %lli: %s (%i)\n",
           type[(*iter)->type], (*iter)->row, (*iter)->message, (*iter)->code);
  }
}


void MYXResultSetCommitResult::out_of_memory()
{
  _status= EOutOfMemory;
}
