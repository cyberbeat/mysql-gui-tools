-- ----------------------------------------------------------------------------------------
-- Copyright (C) 2004 MySQL AB
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-- ----------------------------------------------------------------------------------------


-- ----------------------------------------------------------------------------------------
-- @file Workbench.lua
-- @brief Module that contains program logic for MySQL Workbench
-- ----------------------------------------------------------------------------------------


-- ----------------------------------------------------------------------------------------
-- @brief Returns the information about this module
--
--   Every Grt module has to implement this function to return information about the 
-- module. Note that new functions that should be exposed to the Grt have to be listed 
-- here. Function that are not exposed should start with a underscore.
--
-- @return A dict that contains the name and the function names of the module
-- ----------------------------------------------------------------------------------------

function getModuleInfo()
  local moduleInfo= 
    {
      name= "DbUtils", 
      functions= {
        "mergeCatalogs::",
        "removeIgnoredObjectsFromCatalog::",
        "removeEmptySchemataFromCatalog::",
        "setColumnDatatypeByString::",
        "getColumnDatatypeAsString::",
        "getRoutineName::",
        "getRoutineType::",
        "getTriggerStatement::",
        "getTriggerEvent::",
        "getTriggerTiming::",
        "copyColumnType::",
        "getCatalogsChanges::",
        "generateTableTestData::"
      }, 
      extends= ''
    }

  return moduleInfo
end

-- ----------------------------------------------------------------------------------------
-- @brief Builds a tree of db.DatabaseSyncObject 
-- 
--   Builds a tree of db.DatabaseSyncObject which described the differences between
-- the original catalog and modified
--
-- @param originalCatalog
-- @param modifiedCatalog
--
-- @return the tree object on success or nil on error
-- ----------------------------------------------------------------------------------------
-- function getCatalogsChanges(args)
  
--  local t= grtV.newObj("db.DatabaseSyncObject", "syncObj", grt.newGuid(), "")
--  local originalCatalog= args[1]
--  local modifiedCatalog= args[2]  

--  t.dbObject= originalCatalog
--  t.modelObject= modifiedCatalog
  
--  return grt.success(t)
-- end

-- ----------------------------------------------------------------------------------------
-- @brief Merges two catalogs
--
--   Adds all schemata from the source catalog to the target catalog. If there are
-- duplicate schema names the user is asked to enter a unique name.
-- The owner field of added schemas is updated to the new catalog.
--
-- @param sourceCatalog
-- @param targetCatalog
--
-- @return 1 on success or an error
-- ----------------------------------------------------------------------------------------
function mergeCatalogs(args)
  if (args == nil) or (grtV.getn(args) ~= 2) then
    return grt.error(_("This function needs two catalogs as arguments."))
  end
  
  local sourceCatalog= args[1]
  local targetCatalog= args[2]
  local i
  
  -- loop over all schemata in the source catalog
  for i= 1, grtV.getn(sourceCatalog.schemata) do
    local sourceSchema= sourceCatalog.schemata[i]
    local nameIsUnique= false
    local j
    local schemaName= grtV.toLua(sourceSchema.name)
    
    -- loop until the schema name is unique
    while not(nameIsUnique) and (schemaName ~= "") do
      nameIsUnique= true
      
      -- check the source schema name against all schemata in the target catalog
      for j= 1, grtV.getn(targetCatalog.schemata) do
        if grtV.toLua(targetCatalog.schemata[j].name) == schemaName then
          nameIsUnique= false
          break
        end
      end
    
      -- if the name is already taken, ask the user for another name
      if not(nameIsUnique) then
        schemaName= input(string.format(_("The schema name \"%s\" is not unique. " ..
          "Please enter a new name or leave the name empty to skip this schema."), schemaName))
      end
    end
    
    -- if the user enters "", skip this schema
    if schemaName ~= "" then
      sourceSchema.name= schemaName

      sourceSchema.owner= targetCatalog

      -- add source schema to target catalog
      grtV.insert(targetCatalog.schemata, sourceSchema)
    end
  end
  
  -- Free cached GRT values on Lua side
  collectgarbage()
  
  return grt.success()
end

-- ----------------------------------------------------------------------------------------
-- @brief Removes all objects from a catalog that are on the given ignore list
--
--   Loops over all schemata and schema objects and removes the objects that are on
-- the ignore list
--
-- @param catalog
-- @param ignoreList
--
-- @return 1 on success or an error
-- ----------------------------------------------------------------------------------------
function removeIgnoredObjectsFromCatalog(args)
  if (args == nil) or (grtV.getn(args) ~= 2) then
    return grt.error(_("This function needs a catalog and an ignore list as arguments."))
  end
  
  local i
  local catalog= args[1]
  local ignoreList= args[2]
  
  -- loop over all schemata
  for i= 1, grtV.getn(catalog.schemata) do
    local schema= catalog.schemata[i]
    local schemaStructName= grtS.get(schema)
    local member= grtS.getMembers(schemaStructName)
    local j
    
    -- check all members of the schema struct
    for j= 1, table.getn(member) do
      local memberContentStruct= grtS.getMemberContentStruct(schemaStructName, member[j])
      
      -- if the member holds a list of objects that derive from "db.DatabaseObject" check it
      if (memberContentStruct ~= nil) and grtS.inheritsFrom(memberContentStruct, "db.DatabaseObject") then
        local objList= schema[member[j]]
        
        if objList ~= nil then
          local k
          
          -- loop over all objects in the list
          for k= grtV.getn(objList), 1, -1 do
            local objStructName= grtS.get(objList[k])
            
            -- check if this object is on the ignore list
            for l= 1, grtV.getn(ignoreList) do
              local ignoreString= objStructName .. ":" .. grtV.toLua(schema.name) .. "." .. 
                grtV.toLua(objList[k].name)
                
              -- if it is, remove it from the list
              if (ignoreString == grtV.toLua(ignoreList[l])) then
                grtV.remove(objList, k)
                break -- bug #18778
              end
            end
          end
        end
      end
    end
  end
  
  -- Free cached GRT values on Lua side
  collectgarbage()
  
  return grt.success()
end


-- ----------------------------------------------------------------------------------------
-- @brief Removes all schemata from the given catalog if they are empty
--
--   Removes all schemata from the given catalog if they contain no sub-objects
--
-- @param catalog a catalog object
--
-- @return 1 on success or an error
-- ----------------------------------------------------------------------------------------
function removeEmptySchemataFromCatalog(args)
  local catalog= args[1]
  local i
  local isEmpty= true
  
  -- loop over all schemata
  for i= grtV.getn(catalog.schemata), 1, -1 do
    local schema= catalog.schemata[i]
    local schemaStructName= grtS.get(schema)
    local j
    
    -- check all schemata member variables
    local members= grtS.getMembers(schemaStructName)
    for j= 1, grtV.getn(members) do
    
      -- if the member variable is a list
      if (grtS.getMemberType(schemaStructName, members[j]) == "list") then
        local memberContentType= grtS.getMemberContentStruct(schemaStructName, members[j])
        
        -- check content type of the list and the list count
        if ((memberContentType ~= nil) and grtS.inheritsFrom(memberContentType, "db.DatabaseObject")) and 
          (grtV.getn(schema[members[j]]) > 0) then
          isEmpty= false
          break
        end
      end
    end
    
    if isEmpty then
      print(string.format(_("Removing schema %s"), grtV.toLua(catalog.schemata[i].name)))
      grtV.remove(catalog.schemata, i)
    end
  end
  
  return grt.success()
end


-- ----------------------------------------------------------------------------------------
-- @brief Sets the datatype defined by a string to the given column
--
--   Assigns a datatype defined by a string like NUMERIC(7, 2) to the
-- given column
--
-- @param simpleDatatypeSource can be a RDBMS, a list or a ref list of SimpleDatatypes
-- @param column
-- @param datatypeString
--
-- @return 1 on success or an error
-- ----------------------------------------------------------------------------------------
function setColumnDatatypeByString(args)
  local simpleDatatypeSource= args[1]
  local column= args[2]
  local datatypeString= grtV.toLua(args[3])
  
  -- regex to split INT (10, 5)
  -- (\w*)\s*(\((\d+)\s*(\,\s*(\d+))?\))?
  -- 1.. INT
  -- 3.. 10 (optional)
  -- 5.. 5 (optional)
  local regExDatatype= "(\\w*)\\s*(\\((\\d+)\\s*(\\,\\s*(\\d+))?\\))?"
  
  -- regex to split ENUM ("red", "blue")
  -- (\w*)\s*(\(.*)?\)?
  -- 1.. ENUM
  -- 2.. ("red", "blue")
  local regExExplicitParams= "(\\w*)\\s*(\\(.*)?\\)?"
  
  -- get datatype name
  local datatypeName= grtU.regExVal(datatypeString, regExDatatype, 1)
  
  column.precision= 0
  column.scale= 0
  column.length= 0
  column.datatypeExplicitParams= ""

  if datatypeName ~= nil then
    local simpleDatatype
    column.datatypeName= datatypeName
    
    -- find the simple datatype with this datatypeName
    if grtV.typeOf(simpleDatatypeSource) == "dict" then
      simpleDatatype= grtV.getListItemByObjName(simpleDatatypeSource.simpleDatatypes, datatypeName)
    elseif (grtV.getContentType(simpleDatatypeSource) == "string") then
      simpleDatatype= grtV.getListRefValueByObjName(simpleDatatypeSource, datatypeName)
    else
      simpleDatatype= grtV.getListItemByObjName(simpleDatatypeSource, datatypeName)
    end
    
    if simpleDatatype ~= nil then
      column.simpleType= simpleDatatype
      
      -- get precision
      if grtV.toLua(simpleDatatype.numericPrecision) > 0 then
        local precision= grtU.regExVal(datatypeString, regExDatatype, 3)
        
        if precision ~= "" then
          column.precision= tonumber(precision)
        end
        
        -- get scale
        if grtV.toLua(simpleDatatype.numericScale) > 0 then
          local scale= grtU.regExVal(datatypeString, regExDatatype, 5)
          
          if scale ~= "" then
            column.scale= tonumber(scale)
          end
        end
      -- get length
      elseif grtV.toLua(simpleDatatype.characterMaximumLength) > 0 then
        local length= grtU.regExVal(datatypeString, regExDatatype, 3)
        
        if length ~= "" then
          column.length= tonumber(length)
        end        
      -- other stuff like ENUM()
      else
        column.datatypeExplicitParams= grtU.regExVal(datatypeString, regExExplicitParams, 2)
      end
    else
      column.datatypeExplicitParams= grtU.regExVal(datatypeString, regExExplicitParams, 2)
    end
  end
  
  return grt.success()
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the datatype of the given column as string
--
--   Returns the datatype of the given column as string like "NUMERIC(7, 2)"
--
-- @param rdbms
-- @param column
--
-- @return the datatype string on success or an error
-- ----------------------------------------------------------------------------------------
function getColumnDatatypeAsString(args)
  local column= args[1]
  local simpleDatatype= column.simpleType
  local typeString
  
  if (simpleDatatype ~= nil) and (grtV.typeOf(simpleDatatype) == "dict") then
    typeString= grtV.toLua(simpleDatatype.name)
    
    -- check precision
    if grtV.toLua(simpleDatatype.numericPrecision) > 0 then
      local precision= grtV.toLua(column.precision)
      
      if precision > 0 then
        typeString= typeString .. "(" .. precision
        
        -- check scale
        if grtV.toLua(simpleDatatype.numericScale) > 0 then
          local scale= grtV.toLua(column.scale)
          
          if (scale > 0) then
            typeString= typeString .. ", " .. scale
          end
        end
        
        typeString= typeString .. ")"
      end
      
    elseif grtV.toLua(simpleDatatype.characterMaximumLength) > 0 then
      typeString= typeString .. "(" .. grtV.toLua(column.length) .. ")"
    else
      local explParams= grtV.toLua(column.datatypeExplicitParams)
      
      typeString= typeString .. explParams
    end
  else
    local precision= grtV.toLua(column.precision)
    local length= grtV.toLua(column.length)
    local explParams= grtV.toLua(column.datatypeExplicitParams)
    
    typeString= grtV.toLua(column.datatypeName)
    
    if precision > 0 then
      local scale= grtV.toLua(column.scale)
      
      typeString= typeString .. "(" .. precision
      
      if scale > 0 then
        typeString= typeString .. ", " .. scale
      end
      
      typeString= typeString .. ")"
    elseif length > 0 then
      typeString= typeString .. "(" .. length .. ")"
    elseif explParams ~= "" then
      typeString= typeString .. explParams
    end
  end

  return grt.success(typeString)
end


-- ----------------------------------------------------------------------------------------
-- @brief Returns the name from the given routine SQL
--
--   Returns the name from the given routine SQL
--
-- @param sql
--
-- @return the name string on success or an error
-- ----------------------------------------------------------------------------------------
function getRoutineName(args)
  local sql= grtV.toLua(args[1])
  local name
  
  -- .create\s+(procedure|function)\s+(\w+)\s*\(
  name= grtU.regExVal(sql, ".*create\\s+(procedure|function|trigger)\\s+(\\w+)\\s*", 2)
  
  return grt.success(name)
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the trigger statement from the given trigger SQL
--
--   Returns the trigger statement from the given trigger SQL
--
-- @param sql
--
-- @return the trigger statement string on success or an error
-- ----------------------------------------------------------------------------------------
function getTriggerStatement(args)
  local sql= grtV.toLua(args[1])
  local name
  
    name= grtU.regExVal(sql, ".*create\\s+trigger\\s+\\w+\\s*(before|after)\\s*(insert|update|delete)\\s*on\\s*\\w+\\s*for\\s*each\\s*row\\r?\\n?((.|\\r|\\n|\\s)*)", 3)
  
  return grt.success(name)
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the trigger event from the given trigger SQL
--
--   Returns the trigger event from the given trigger SQL
--
-- @param sql
--
-- @return the trigger event string on success or an error
-- ----------------------------------------------------------------------------------------
function getTriggerEvent(args)
  local sql= grtV.toLua(args[1])
  local name
  
    name= grtU.regExVal(sql, ".*create\\s+trigger\\s+\\w+\\s*(before|after)\\s*(insert|update|delete)((.|\\r|\\n|\\s)*)", 2)
  
  return grt.success(name)
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the trigger timing from the given trigger SQL
--
--   Returns the trigger timing from the given trigger SQL
--
-- @param sql
--
-- @return the trigger timing string on success or an error
-- ----------------------------------------------------------------------------------------
function getTriggerTiming(args)
  local sql= grtV.toLua(args[1])
  local name
  
    name= grtU.regExVal(sql, ".*create\\s+trigger\\s+\\w+\\s*(before|after)((.|\\r|\\n|\\s)*)", 1)
  
  return grt.success(name)
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the name from the given routine SQL
--
--   Returns the name from the given routine SQL
--
-- @param sql
--
-- @return the name string on success or an error
-- ----------------------------------------------------------------------------------------
function getRoutineType(args)
  local sql= grtV.toLua(args[1])
  local routineType

  -- .create\s+(procedure|function)\s+(\w+)\s*\(
  routineType= grtU.regExVal(sql, ".*create\\s+(procedure|function|trigger)\\s+(\\w+)\\s*", 1)
  
  return grt.success(routineType)
end




-- ----------------------------------------------------------------------------------------
-- @brief Copies the column datatype from the source column to the dest column
--
-- @param sourceColumn
-- @param destColumn
-- 
-- ----------------------------------------------------------------------------------------
function copyColumnType(args)
  local source= args[1]
  local dest= args[1]

  dest["datatypeName"]= grtV.toLua(source["datatypeName"])
  if source["precision"] ~= nil then
    dest["precision"]= grtV.toLua(source["precision"])
  end 
  if source["scale"] ~= nil then
    dest["scale"]= grtV.toLua(source["scale"])
  end
  if source["length"] ~= nil then
    dest["length"]= grtV.toLua(source["length"])
  end
  if source["characterSetName"] ~= nil then
    dest["characterSetName"]= grtV.toLua(source["characterSetName"])
  end 
  if source["collationName"] ~= nil then
    dest["collationName"]= grtV.toLua(source["collationName"])
  end
  if source["flags"] ~= nil then
    dest["flags"]= grtV.newList("string")
    for i=1,grtV.getn(source["flags"]) do
      grtV.insert(dest["flags"], grtV.toLua(source["flags"][i]))
    end
  end
  if source["simpleType"] ~= nil then
    dest["simpleType"]= grtV.toLua(source["simpleType"])
  end
  if source["structuredDatatype"] ~= nil then
    dest["structuredDatatype"]= grtV.toLua(source["structuredDatatype"])
  end
  if source["datatypeExplicitParams"] ~= nil then
    dest["datatypeExplicitParams"]= grtV.toLua(source["datatypeExplicitParams"])
  end

  return grt.success()
end

-- ----------------------------------------------------------------------------------------
-- @brief Generates test data that can be used for tests, yet not all datatypes are handled
--
-- @param tbl  the db.Table to generate testdata for
-- @param numOfRows  number of rows to generate
-- @param fileName  the filename the INSERT statements get written to
-- 
-- ----------------------------------------------------------------------------------------
function generateTableTestData(tbl, numOfRows, fileName)
  local i, j
  local sql= ""
  local sqlHeader= "INSERT INTO " .. grtV.toLua(tbl.name) .. "("
  local colUnsigned= {}
  local sqlFile= io.open(fileName, "w")
  
  -- built INSERT header listing all columns
  for i= 1, grtV.getn(tbl.columns) do    
    local col= tbl.columns[i]
    if (i > 1) then
      sqlHeader= sqlHeader .. ", "
    end
    
    sqlHeader= sqlHeader .. grtV.toLua(col.name)
    
    -- cache unsigned flag
    colUnsigned[i]= 0
    for j= 1, grtV.getn(col.flags) do
      if grtV.toLua(col.flags[j]) == "unsigned" then
        colUnsigned[i]= 1
        break
      end
    end
  end
  sqlHeader= sqlHeader .. ") VALUES\n  ("
  
  sql= sqlHeader
  
  -- generate as much rows as needed
  for i= 1, numOfRows do
    for j= 1, grtV.getn(tbl.columns) do
      local col= tbl.columns[j]
      
      if (j > 1) then
        sql= sql .. ", "
      end
      
      if (grtV.toLua(col.autoIncrement) == 1) then
        -- deal with AutoIncrement values
        sql= sql .. i
      elseif (grtV.toLua(col.datatypeName) == "TINYINT") or 
        (grtV.toLua(col.datatypeName) == "SMALLINT") or
        (grtV.toLua(col.datatypeName) == "MEDIUMINT") or
        (grtV.toLua(col.datatypeName) == "INT") or
        (grtV.toLua(col.datatypeName) == "BIGINT") then
        -- deal with INTEGER values
        local val
        local intRange= 255
        
        if grtV.toLua(col.datatypeName) == "SMALLINT" then
          intRange= 65535
        elseif grtV.toLua(col.datatypeName) == "MEDIUMINT" then
          intRange= 16777215
        elseif grtV.toLua(col.datatypeName) == "MEDIUMINT" then
          intRange= 4294967295
        end
        
        if colUnsigned[i] == 0 then
          val= (math.random(intRange) - math.floor(intRange / 2))
        else
          val= math.random(intRange)        
        end
        
        sql= sql .. val
      elseif (grtV.toLua(col.datatypeName) == "VARCHAR") or
        (grtV.toLua(col.datatypeName) == "CHAR") then
        -- deal with string values
        local k
        local val= ""
        local len= grtV.toLua(col.length)        
        len= len - math.random(len - 1)
        
        for k= 1, len do
          val= val .. string.char(math.random(65, 90))
        end
        
        sql= sql .. "'" .. val .. "'"
      end
    end
    
    -- make sure to not excede maximum command length
    if string.len(sql) < 16000 and i < numOfRows then
      sql= sql .. "),\n  ("
    else
      sql= sql .. ");\n\n"
      sqlFile:write(sql)
      
      sql= sqlHeader
    end
    
    -- print status
    if i - math.floor(i/100)*100 == 0 then
      print("Rows processed: " .. i)
    end
  end
  
  if string.len(sql) ~= string.len(sqlHeader) then
    sql= sql .. ");\n\n"
    sqlFile:write(sql)
  end
  
  sqlFile:close()
end
