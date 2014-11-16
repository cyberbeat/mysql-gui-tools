/*
 *  MGRTTable.h
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/8/16.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#ifndef __MGRTTABLE_H__
#define __MGRTTABLE_H__

#include <MySQLGRT/MGRTValue.h>
#include <vector>
#include <map>
#include <string>

#include <MySQLGRT/MGRTObject.h>

enum RDBMSDataTypeGroup
{
  RDG_NUMERIC,
  RDG_STRING,
  RDG_TEXT,
  RDG_BLOB,
  RDG_DATETIME,
  RDG_GEO,
  RDG_VARIOUS,
  RDG_USER,
  RDG_STRUCTURED
};


class MGRTTable : public MGRTObject
{
  MGRTValue _simpleDataTypes;
  
  // temporary list of columns used for getting columns from their id's
  // necessary because the columns won't be visible to myx_grt_reference_lookup
  // until they're commited to the grt tree
  std::map<std::string,MGRTValue> _columns;
  
  const char *dataTypeIdForName(const char *name);

  MGRTValue getColumnWithId(const char *id);
public:
  MGRTTable(MYX_GRT *grt, MYX_GRT_VALUE *value);
  
  MGRTValue addColumn(const char *name);
  MGRTValue addIndex(const char *name);
  MGRTValue addFK(const char *name);
  
  void removeColumn(int index);
  void removeIndex(int index);
  void removeFK(int index);
  
  MGRTValue getColumn(int index);
  MGRTValue getColumn(const char *name);
  MGRTValue getIndex(int index);
  MGRTValue getFK(int index);
  
  int columnCount();
  int indexCount();
  int fkCount();
  
  void setDefaultCollation(const char *collation);
  
  void setColumnName(MGRTValue &column, const char *name);
  void setColumnType(MGRTValue &column, const char *type);
  void setColumnPK(MGRTValue &column, bool flag);
  bool columnIsPK(const MGRTValue &column);
  std::string formatColumnType(const MGRTValue &column);
  bool columnIsNumeric(const MGRTValue &column);
  const char *getColumnCharset(const MGRTValue &column);
  std::vector<const char*>getCharsetCollations(const MGRTValue &charsets, const char *charset);
  const char *getColumnCollation(const MGRTValue &column);
  MGRTValue getColumnFlags(const MGRTValue &column);
  std::string getEnabledColumnFlags(const MGRTValue &column);
  void setEnabledColumnFlags(MGRTValue &column, const std::string &flags);
  bool columnFlagState(const MGRTValue &column, const char *flag);
  void setColumnFlagState(MGRTValue &column, const char *flag, bool state);
  RDBMSDataTypeGroup columnTypeGroup(const MGRTValue &column);
  
  void moveColumn(int sindex, int dindex);

  bool addColumnToFK(const MGRTValue &fk);
  bool addColumnToFK(const MGRTValue &fk, const MGRTValue &column, const char *refColumn);
  int fkColumnCount(const MGRTValue &fk);
  
  const char *getFKColumn(const MGRTValue &fk, int index);
  const char *getFKRefColumn(const MGRTValue &fk, int index);
  void setFKRefTable(MGRTValue &fk, const MGRTValue &refTable);
  void setFKColumn(const MGRTValue &fk, int index, const char *name);
  void setFKRefColumn(const MGRTValue &fk, int index, const char *name);
  
  void removeColumnFromFK(const MGRTValue &fk, int index);
  void resetFKColumns(const MGRTValue &fk);
  
  void addColumnToIndex(const MGRTValue &index);
  void addColumnToIndex(const MGRTValue &index, const MGRTValue &column);
  int indexColumnCount(const MGRTValue &index);
  
  void removeColumnFromIndex(const MGRTValue &index, int coli);

  void updateIndexColumnNames();
    
  void setIndexColumn(const MGRTValue &index, int coli, const char *name);
  MGRTValue getIndexColumn(const MGRTValue &index, int coli);
};

#endif
