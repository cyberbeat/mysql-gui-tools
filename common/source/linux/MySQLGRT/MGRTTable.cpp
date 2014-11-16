/*
 *  MGRTTable.cpp
 *  MySQL GRT
 *
 *  Created by Alfredo Kojima on 05/8/16.
 *  Copyright 2005 MySQL AB. All rights reserved.
 *
 */

#include "MGRTTable.h"
#include <string>


MGRTTable::MGRTTable(MYX_GRT *grt, MYX_GRT_VALUE *value)
: MGRTObject(grt, value)
{
  MGRTValue catalog(ownerCatalog());
  if (catalog.isValid())
  {
    _simpleDataTypes.assign(catalog["simpleDatatypes"]);
  }
  
  MGRTValue columns(_object["columns"]);
  for (unsigned int i= 0; i < columns.count(); i++)
  {
    _columns[columns[i].dictId()]= columns[i];
  }
}


const char *MGRTTable::dataTypeIdForName(const char *name)
{
  unsigned int i;
  for (i= 0; i < _simpleDataTypes.count(); i++)
  {
    MGRTValue type(lookupObject(_simpleDataTypes[i]));

    if (strcmp(type["name"].asString(), name)==0)
      return type.dictId();
  }
  return NULL;
}


MGRTValue MGRTTable::addColumn(const char *name)
{
  MGRTValue columns= _object["columns"];
  bool first= columns.count() == 0;
  const char *datatypeName;
  MGRTValue column(MGRTValue::createObject(_grt, columns.listContentStruct(), name, _object));
  
  if (first)
  {
    datatypeName= "INT";
  }
  else
  {
    datatypeName= "VARCHAR";
  }
  
  _columns[column.dictId()]= column.grtValue();
  
  column.set("datatypeName", datatypeName);
  
  if (_simpleDataTypes.isValid())
    column.set("simpleType", dataTypeIdForName(datatypeName));
  
  column.set("defaultValueIsNull", 1);
  
  if (first)
  {
    column.set("isNullable", 0);
    column.set("autoIncrement", 1);
    column["flags"].append(MGRTValue("UNSIGNED"));
    column.set("precision", 11);
  }
  else
  {
    column.set("isNullable", 1);
    column.set("length", 40);
  }
  columns.append(column);  
  
  return column;
}


MGRTValue MGRTTable::addIndex(const char *name)
{
  MGRTValue indices(_object["indices"]);
  
  MGRTValue index(MGRTValue::createObject(_grt, indices.listContentStruct(), "new_index", _object));
  
  index.set("indexType", "INDEX");
  
  indices.append(index);
  
  return index;
}


MGRTValue MGRTTable::addFK(const char *name)
{
  MGRTValue fks(_object["foreignKeys"]);
  MGRTValue fk(MGRTValue::createObject(_grt, fks.listContentStruct(), "new_fk", _object));

  fk.set("updateRule", "NO ACTION");
  fk.set("deleteRule", "NO ACTION");
  
  fks.append(fk);
  
  return fk;
}



void MGRTTable::removeColumn(int index)
{
  std::map<std::string,MGRTValue>::iterator iter= _columns.find(_object["columns"][index].dictId());
  
  if (iter != _columns.end())
    _columns.erase(iter);
  else
    g_message("Error removing column, not found in internal list.");

  _object["columns"].remove(index);
}


void MGRTTable::removeIndex(int index)
{
  _object["indices"].remove(index);
}


void MGRTTable::removeFK(int index)
{
  _object["foreignKeys"].remove(index);
}


MGRTValue MGRTTable::getColumn(int index)
{
  return _object["columns"][index];
}


MGRTValue MGRTTable::getColumn(const char *name)
{
  return _object["columns"].listItemNamed(name);
}


MGRTValue MGRTTable::getColumnWithId(const char *id)
{
  return _object["columns"].listItem(id);
}


MGRTValue MGRTTable::getIndex(int index)
{
  return _object["indices"][index];
}


MGRTValue MGRTTable::getFK(int index)
{
  return _object["foreignKeys"][index];
}


void MGRTTable::setColumnPK(MGRTValue &column, bool isPK)
{  
  bool wasPK= columnIsPK(column);
  const char *structName;
  MGRTValue primaryKey;
  
  if (wasPK && !isPK)
  {
    primaryKey= MGRTValue::refObject(_grt, _object["primaryKey"]);
    
    if (primaryKey.isValid())
    {
      MGRTValue pkColumns(primaryKey["columns"]);
      const char *id= column.dictId();
      
      for (unsigned int i= 0; i < pkColumns.count(); i++)
      {
        MGRTValue pkColumn(MGRTValue::refObject(_grt, pkColumns[i]["referedColumn"].asString()));
        
        if (strcmp(pkColumn.dictId(), id)==0)
        {
          pkColumns.remove(i);
          break;
        }
        
        if (pkColumns.count() == 0)
          _object.remove("primaryKey");
      }
    }
  }
  else if (!wasPK && isPK)
  {  
    primaryKey= MGRTValue::refObject(_grt, _object["primaryKey"]);

    if (!primaryKey.isValid())
    {
      structName= _object.contentStructOfMember(_grt, "primaryKey");
      primaryKey= MGRTValue::createObject(_grt, structName,
                                          "PRIMARY", _object);

      myx_grt_reference_cache_add(_grt, primaryKey.grtValue());
      
      _object["indices"].append(primaryKey);

      primaryKey.set("isPrimary", 1);
      
      _object.set("primaryKey", primaryKey.dictId());
    }
    
    structName= primaryKey.contentStructOfMember(_grt, "columns");
    MGRTValue pkColumn(MGRTValue::createObject(_grt, structName,
                                               column["name"].asString(),
                                               primaryKey));
    
    pkColumn.set("referedColumn", column.dictId());
    
    myx_grt_reference_cache_add(_grt, column.grtValue());
    
    primaryKey["columns"].append(pkColumn);
  }
}


bool MGRTTable::columnIsPK(const MGRTValue &column)
{
  MGRTValue primaryKey(MGRTValue::refObject(_grt, _object["primaryKey"]));
  if (primaryKey.isValid())
  {
    MGRTValue pkColumns(primaryKey["columns"]);
    const char *id= column.dictId();
  
    for (unsigned int i= 0; i < pkColumns.count(); i++)
    {
      MGRTValue pkColumn(MGRTValue::refObject(_grt, pkColumns[i]["referedColumn"].asString()));

      if (strcmp(pkColumn.dictId(), id)==0)
        return true;
    }
  }
  return false;
}



std::string MGRTTable::formatColumnType(const MGRTValue &column)
{
  MGRTValue simpleType(lookupObject(column.get("simpleType","")));
  MGRTValue structuredType(column.get("structuredDatatype",""));
  std::string caption;
  char buffer[100];

  caption= column["datatypeName"].asString();
  
  if (simpleType.isValid())
  {
    if (simpleType["numericPrecision"].asInt() > 0)
    {    
      if (column["scale"].asInt() != 0) 
        snprintf(buffer, sizeof(buffer), "(%i,%i)", column["precision"].asInt(), column["scale"].asInt());
      else
        snprintf(buffer, sizeof(buffer), "(%i)", column["precision"].asInt());
      caption= caption + buffer;
    }
    else
    {     
      if (simpleType["characterMaximumLength"].asInt() > 0)
      {
        snprintf(buffer, sizeof(buffer), "(%i)", column["length"].asInt());
        caption= caption + buffer;
      }
      else if (column["datatypeExplicitParams"].isValid() && *column["datatypeExplicitParams"].asString())
        caption= caption + column["datatypeExplicitParams"].asString();
    }
  }
  else if (structuredType.isValid())
  {
  }
  else
  {
    char buffer[100];
    // if no simple or structured datatype is set,
    // simply take the parameters
    if (column["length"].asInt() != 0) 
    {
      snprintf(buffer, sizeof(buffer), "(%i)", column["length"].asInt());
      caption= caption + buffer;
    }
    else if (column["precision"].asInt() != 0)
    {
      if (column["scale"].asInt() != 0)
        snprintf(buffer, sizeof(buffer), "(%i,%i)", column["precision"].asInt(), column["scale"].asInt());
      else
        snprintf(buffer, sizeof(buffer), "(%i)", column["precision"].asInt());
      caption= caption + buffer;
    }
    else if (column["datatypeExplicitParams"].asString() != 0)
      caption= caption + column["datatypeExplicitParams"].asString();
  }

  return caption;
}


bool MGRTTable::columnIsNumeric(const MGRTValue &column)
{
  return columnTypeGroup(column) == RDG_NUMERIC;
}


void MGRTTable::setDefaultCollation(const char *coll)
{
  if (coll)
  {
    std::string charset(coll, (const char*)strchr(coll, '_'));
     _object.set("defaultCollationName", coll);
    _object.set("defaultCharsetName", charset.c_str());
  }
  else
  {
    _object.set("defaultCollationName", "");
    _object.set("defaultCharsetName", "");
  }
}

const char *MGRTTable::getColumnCharset(const MGRTValue &column)
{
  MGRTValue charset(column["characterSetName"]);
  if (charset.isValid())
    return charset;
  else
    return NULL;
}


std::vector<const char*>MGRTTable::getCharsetCollations(const MGRTValue &charsets, const char *charset)
{
  int i, c= charsets.count();
  std::vector<const char*> collations;
  
  for (i= 0; i < c; i++)
  {
    MGRTValue chs(MGRTValue::refObject(_grt, charsets[i].asString()));
    
    if (strcmp(chs["name"].asString(), charset)==0)
    {
      MGRTValue colls(chs["collations"]);
      unsigned int j;
      for (j= 0; j < colls.count(); j++)
        collations.push_back(colls[j].asString());
      break;
    }
  }
  return collations;
}


const char *MGRTTable::getColumnCollation(const MGRTValue &column)
{
  MGRTValue collation(column["collationName"]);
  if (collation.isValid())
    return collation;
  else
    return NULL;
}


MGRTValue MGRTTable::getColumnFlags(const MGRTValue &column)
{
  const char *typeId= column["simpleType"];
  MGRTValue simpleType(lookupObject(typeId));
  
  if (simpleType.isValid())
    return simpleType["flags"];

  return MGRTValue();
}


std::string MGRTTable::getEnabledColumnFlags(const MGRTValue &column)
{
  MGRTValue types(getColumnFlags(column));
  std::string flags;
  for (unsigned int i= 0; i < types.count(); i++)
  {
    if (columnFlagState(column, types[i].asString()))
    {
      if (flags.empty())
        flags= types[i].asString();
      else
        flags= flags+","+std::string(types[i].asString());
    }
  }
  return flags;
}


void MGRTTable::setEnabledColumnFlags(MGRTValue &column, const std::string &flags)
{
  MGRTValue types(getColumnFlags(column));
  unsigned int i, t;
  char **tokens= g_strsplit(flags.c_str(), ",", 0);
  
  for (i= 0; i < types.count(); i++)
  {
    bool on= false;
    
    for (t= 0; tokens[t]; t++)
    {
      if (strcasecmp(types[i].asString(), tokens[t])==0)
      {
        on= true;
        break;
      }
    }
    setColumnFlagState(column, types[i].asString(), on);
  }
  g_strfreev(tokens);
}

  
bool MGRTTable::columnFlagState(const MGRTValue &column, const char *flag)
{
  char *lflag= g_utf8_strdown(flag,strlen(flag));
  bool res= column.get(lflag, 0) != 0;
  g_free(lflag);
  return res;
}


void MGRTTable::setColumnFlagState(MGRTValue &column, const char *flag, bool state)
{
  bool isSet= columnFlagState(column, flag);
  char *lflag= g_utf8_strdown(flag,strlen(flag));

  if (state && !isSet)
    column.set(lflag, 1);
  else if (!state && isSet)
    column.set(lflag, 0);
  
  g_free(lflag);
}


RDBMSDataTypeGroup MGRTTable::columnTypeGroup(const MGRTValue &column)
{
  const char *typeId= column["simpleType"];
  MGRTValue simpleType(lookupObject(typeId));
  
  if (simpleType.isValid())
  {
    MGRTValue group(lookupObject(simpleType["group"].asString()));
    if (group.isValid())
    {
      const char *groupName= group["name"];
      if (strcmp(groupName, "numeric")==0)
        return RDG_NUMERIC;
      else if (strcmp(groupName, "string")==0)
        return RDG_STRING;
      else if (strcmp(groupName, "text")==0)
        return RDG_TEXT;
      else if (strcmp(groupName, "blob")==0)
        return RDG_BLOB;
      else if (strcmp(groupName, "datetime")==0)
        return RDG_DATETIME;
      else if (strcmp(groupName, "gis")==0)
        return RDG_GEO;
      else if (strcmp(groupName, "various")==0)
        return RDG_VARIOUS;
      else if (strcmp(groupName, "userdefined")==0)
        return RDG_USER;
      else if (strcmp(groupName, "structured")==0)
        return RDG_STRUCTURED;
    }
  }
  else if (column["structuredDatatype"].isValid())
    return RDG_STRUCTURED;

  return RDG_USER;
}


void MGRTTable::moveColumn(int sindex, int dindex)
{
  if (sindex != dindex)
  {
    MGRTValue column(getColumn(sindex));
  
    myx_grt_list_item_insert(_object["columns"].grtValue(), dindex, column.grtValue());
    
    if (sindex > dindex)
      sindex++;
    _object["columns"].remove(sindex);
  }
}


int MGRTTable::columnCount()
{
  return _object["columns"].count();
}

int MGRTTable::indexCount()
{
  return _object["indices"].count();
}

int MGRTTable::fkCount()
{
  return _object["foreignKeys"].count();
}

int MGRTTable::indexColumnCount(const MGRTValue &index)
{
  return index["columns"].count();
}

int MGRTTable::fkColumnCount(const MGRTValue &fk)
{
  return fk["columns"].count();
}


void MGRTTable::setColumnName(MGRTValue &column, const char *name)
{
  column.set("name", name);
  updateIndexColumnNames();
}


void MGRTTable::setColumnType(MGRTValue &column, const char *type)
{
  MGRTValue list(MGRTValue::createList());
  char *upperType= g_utf8_strup(type, strlen(type));
  MYX_GRT_ERROR error;
  MYX_GRT_VALUE *result;
  MGRTValue schema(MGRTValue::refObject(_grt, _object["owner"]));

  if (schema.isValid())
  {
    MGRTValue catalog(MGRTValue::refObject(_grt, schema["owner"]));
  
    if (catalog.isValid())
    {
      list.append(catalog["simpleDatatypes"]);
      list.append(column);
      list.append(MGRTValue(upperType));
    }
    else
    {
      g_message("invalid catalog");
      return;
    }
  }
  else
  {
    g_message("invalid schema");
    return;
  }

  result= myx_grt_function_get_and_call(_grt, "DbUtils", "setColumnDatatypeByString",0,
                                        list.grtValue(), &error);
  if (error != MYX_GRT_NO_ERROR)
    g_message("error parsing column datatype");
  if (result)
    myx_grt_value_release(result);
}


bool MGRTTable::addColumnToFK(const MGRTValue &fk)
{
  if (columnCount() > 0)
  {
    MGRTValue column(getColumn(0));
  
    fk["columns"].append(MGRTValue(column.dictId()));
    fk["referedColumnNames"].append(MGRTValue(""));
    return true;
  }
  return false;
}


bool MGRTTable::addColumnToFK(const MGRTValue &fk, const
                              MGRTValue &column, const char *refColumnName)
{
  fk["columns"].append(MGRTValue(column.dictId()));
  fk["referedColumnNames"].append(MGRTValue(refColumnName));
  
  MGRTValue schema(MGRTValue::refObject(_grt, _object["owner"].asString()));
  
  if (schema.isValid() && *fk["referedTableName"].asString())
  {
    MGRTValue table(schema["tables"].listItemNamed(fk["referedTableName"].asString()));
    if (table.isValid())
    {
      MGRTValue refColumn(table["columns"].listItemNamed(refColumnName));
      if (refColumn.isValid())
        fk["referedColumns"].append(MGRTValue(refColumn.dictId()));
      return true;
    }
  }
  fk["referedColumns"].append(MGRTValue(""));
  return true;
}


void MGRTTable::setFKRefTable(MGRTValue &fk, const MGRTValue &refTable)
{
  fk.set("referedTable", refTable.dictId());
  fk.set("referedTableName", refTable["name"].asString());

  resetFKColumns(fk);

  // XXX update the refcolumns list
}


void MGRTTable::removeColumnFromFK(const MGRTValue &fk, int index)
{
  fk["columns"].remove(index);
  fk["referedColumns"].remove(index);
  fk["referedColumnNames"].remove(index);
}


const char *MGRTTable::getFKColumn(const MGRTValue &fk, int index)
{
  if (_columns[fk["columns"][index].asString()].isValid())
    return _columns[fk["columns"][index].asString()]["name"].asString();
  else
    return NULL;
}


const char *MGRTTable::getFKRefColumn(const MGRTValue &fk, int index)
{
  return fk["referedColumnNames"][index].asString();
}


void MGRTTable::setFKColumn(const MGRTValue &fk, int index, const char *colid)
{
  fk["columns"].replace(index, colid);
}


void MGRTTable::setFKRefColumn(const MGRTValue &fk, int index, const char *name)
{
  fk["referedColumnNames"].replace(index, name);
}


void MGRTTable::resetFKColumns(const MGRTValue &fk)
{
  fk["columns"].clear();
  fk["referedColumnNames"].clear();
}


void MGRTTable::addColumnToIndex(const MGRTValue &index)
{
  MGRTValue indexColumn(MGRTValue::createObject(_grt, index["columns"].listContentStruct(), "column", _object));

  index["columns"].append(indexColumn);
}


void MGRTTable::addColumnToIndex(const MGRTValue &index, const MGRTValue &column)
{
  g_return_if_fail(myx_grt_struct_is_or_inherits_from(_grt, column.contentStruct(),"db.IndexColumn"));
  
  MGRTValue indexColumn(MGRTValue::createObject(_grt, index["columns"].listContentStruct(), column["name"], _object));

  indexColumn.set("referedColumn", column.dictId());
  indexColumn.set("name", column["name"].asString());
  
  index["columns"].append(indexColumn);
}


void MGRTTable::removeColumnFromIndex(const MGRTValue &index, int coli)
{
  index["indexColumns"].remove(coli);
}


void MGRTTable::updateIndexColumnNames()
{
  for (unsigned int j= 0; j < _object["indices"].count(); j++)
  {
    MGRTValue index(_object["indices"][j]);
    for (unsigned int i= 0; i < index["columns"].count(); i++)
    {
      MGRTValue refColumn(index["columns"][i]["referedColumn"]);
      if (refColumn.isValid())
      {
        MGRTValue column(getColumnWithId(refColumn.asString()));
        if (column.isValid())
        {
          index["columns"][i].set("name", column["name"].asString());
        }
      }
    }
  }
}


void MGRTTable::setIndexColumn(const MGRTValue &index, int coli, const char *name)
{
  MGRTValue column(getColumn(name));
  
  if (column.isValid())
  {
    index["columns"][coli].set("referedColumn", column.dictId());
    index["columns"][coli].set("name", (const char*)column["name"]);
  }
  else
    index["columns"][coli].set("referedColumn", "");
}


MGRTValue MGRTTable::getIndexColumn(const MGRTValue &index, int coli)
{
  return index["columns"][coli];
}
