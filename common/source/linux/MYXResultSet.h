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


#ifndef _MYXRESULTSET_H_
#define _MYXRESULTSET_H_

#include "myx_public_interface.h"
#include <list>

enum MYXRSEditStatus {
  MESUnchanged,
  MESChanged,
  MESPlaceHolder,
  MESAdded,
  MESDeleted
};

enum MYXRSCompareStatus {
  MCSMatch,
  MCSDiffers,
  MCSOnlyInThis,
  MCSOnlyInOther
};


class MYXResultSet;

struct MYXResultSetCallbacks {
  void (*row_added)(MYXResultSet *rs, unsigned int row);
  void (*row_deleted)(MYXResultSet *rs, unsigned int row);
  void (*row_changed)(MYXResultSet *rs, unsigned int row);
};


class MYXResultSet {
  public:
    class RSChange;
    void *object;
    
  protected:

    MYX_RESULTSET *_resultset;
    
    MYXResultSetCallbacks _callbacks;

    unsigned int _new_rows;

    bool _placeholder;

    std::list<RSChange*> _changes;

    void pack();
    bool row_has_changes(unsigned int row);
    RSChange* find_change(unsigned int row, unsigned int column) const;
    
  public:
    MYXResultSet(MYX_RESULTSET *rs, const MYXResultSetCallbacks &cbacks);
    virtual ~MYXResultSet();
    
    unsigned int add_row();
    void delete_row(unsigned int row);
    
    void set_placeholder_enabled(bool flag);
    
    MYXRSEditStatus get_edit_status(unsigned int row, unsigned int column) const;
    MYXRSCompareStatus get_compare_status(unsigned int row, unsigned int column);
    
    bool isNull(unsigned int row, unsigned int column);
    bool get(unsigned int row, unsigned int column,
             char *&value, size_t &size) const;
    void set(unsigned int row, unsigned int column,
             const char *value, size_t size);
    
    MYX_RS_COLUMN_TYPE get_column_type(unsigned int column);
    
    bool get_row_editable(unsigned int row) const;

    unsigned int get_row_count() const;

    void pre_commit();
    void post_commit(bool failed);
    void discard_changes();
    
    bool has_changes();

    MYX_RESULTSET *get_resultset() { return _resultset; };
};


#endif /* _MYXRESULTSET_H_ */
