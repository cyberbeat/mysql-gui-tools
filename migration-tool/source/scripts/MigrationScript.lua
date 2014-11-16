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
-- @file MigrationScript.lua
-- @brief Script that handles a migration process
-- ----------------------------------------------------------------------------------------

-- ------------------------------------------------------------------
-- MySQL Migration Toolkit - Migration script
-- ------------------------------------------------------------------

Mig = {
  version= "1.1.9exp",
  sourceConn= nil,
  targetConn= nil,
  sourceSchemataNames= nil,
  selectedSchemataNames= nil,
  sourceObjects= nil,
  migrationMethods= nil,
  objTypes= nil
}

-- ------------------------------------------------------------------
-- The run method that is called to perform the migration

function Mig:run()
  self:printTitle("MySQL Migration Toolkit - Script Version " .. self.version)

  self:init()
  
  -- Source connection
  self:printTitle(_("Source database connection."), 1)
  self.sourceConn= self:getConnection("db.mgmt.JdbcDriver")
  if self.sourceConn == nil then
    return -1
  end
  
  -- Target connection
  self:printTitle(_("Target database connection."), 1)
  self.targetConn= self:getConnection("db.mgmt.JdbcDriver", {"TransformationModule"}) 
  if self.targetConn == nil then
    return -1
  end
  grtV.setGlobal("/migration/targetConnection", self.sourceConn)
  
  -- Get schemata list
  self:printTitle(_("Fetching source schemata."), 1)
  self.sourceSchemataNames= self:getSourceSchemata()
  if self.sourceSchemataNames == nil then
    return -1
  end
  grtV.setGlobal("/migration/sourceSchemataNames", self.sourceSchemataNames)
  
  -- Selected schemata
  self:printTitle(_("Schema selection."), 1)
  self.selectedSchemataNames= self:chooseSourceSchemata()
  if self.selectedSchemataNames == nil then
    return -1
  end
  grtV.setGlobal("/migration/selectedSchemataNames", self.selectedSchemataNames)
  
  -- Reverse engineering
  self:printTitle(_("Reverse engineering."), 1)
  local catalog= self:reverseEngineering()
  if catalog == nil then
    return -1
  end
  grtV.setGlobal("/migration/sourceCatalog", catalog)
  
  -- Get migration methods
  self:printTitle(_("Get migration methods."), 1)
  self.migrationMethods= self:getMigrationMethods()
  if self.migrationMethods == nil then
    return -1
  end
  grtV.setGlobal("/migration/migrationMethods", self.migrationMethods)  
  
  -- Let the user setup the ignore list
  self:printTitle(_("Setup ignore list."), 1)
  local ignoreList= self:getIgnoreList()
  if ignoreList == nil then
    return -1
  end
  grtV.setGlobal("/migration/ignoreList", ignoreList)
  
  -- Do migration
  self:printTitle(_("Performing migration."), 1)
  if not(self:doMigration()) then
    return -1
  end
  
  -- Generate objects
  self:printTitle(_("Generate target objects"), 1)
  if not(self:generateObjects()) then
    return -1
  end
  
  -- Bulk data transfer
  self:printTitle(_("Bulk data transfer"), 1)
  if not(self:bulkDataTransfer()) then
    return -1
  end

  self:printTitle(_("Migration finished."))
end

-- ------------------------------------------------------------------
-- Initialize the migration environment

function Mig:init()
  print(_("Initializing migration environment..."))

  Migration:initMigration()

  print(_("Initialisation complete."))

  self.root = grtV.getGlobal("/")
end

-- ------------------------------------------------------------------
-- Print a title

function Mig:printTitle(title, style)
  if style ~= 1 then
    print("")
    print(title)
    print(string.rep("-", string.len(title)))
  else
    print("")
    print(string.rep("*", string.len(title) + 4))
    print("* " .. title .. " *")
    print(string.rep("*", string.len(title) + 4))
  end
end

-- ------------------------------------------------------------------
-- Print an error

function Mig:printError(msg)
  if grtError ~= nil then
    print(msg)
    print(grtError.error)
    if grtError.detail ~= nil then
      print(grtError.detail)
    end
  end
end

function Mig:inputNumber(caption)
  local res= tonumber(input(caption))

  if res == nil then
    print("")
    print(_("Incorrect input. Please enter a number."))
    return self:inputNumber(caption)
  else
    return res
  end
end

-- ------------------------------------------------------------------
-- Select and test a connection

function Mig:getConnection(driverStruct, requiredModules)

  local conn= self:selectConnection(driverStruct, requiredModules) 
  
  if conn ~= nil then
    if self:testConnection(conn) ~= 1 then
      print("")
      local retry= self:inputNumber(_("Choose different connection: (1. Yes, 0. Abort) "))
      
      if retry == 0 then
        return nil
      else
        conn= self:getConnection(driverStruct, requiredModules)
      end
    end
  end
  
  return conn
end

function Mig:driverMeetsRequirements(driver, driverStruct, requiredModules)
  local allRequirementsMet= false
  
  -- check struct name
  if (driverStruct == nil) or (grtS.get(driver) == driverStruct) then        
    -- check required modules
    if requiredModules ~= nil then
      local moduleFound
      local k
      for k= 1, table.getn(requiredModules) do
        local l
        moduleFound= false
        
        for l= 1, grtV.getn(driver.defaultModules) do
          local moduleName= grtV.toLua(driver.defaultModules[requiredModules[k]])
          if (moduleName ~= nil) and (moduleName ~= "") then
            moduleFound= true
            break
          end
        end
        
        if not(moduleFound) then
          break
        end
      end
      
      if (moduleFound) then
        allRequirementsMet= true
      end
    else
      allRequirementsMet= true
    end
  end

  return allRequirementsMet
end

-- ------------------------------------------------------------------
-- Ask user to select a connection

function Mig:selectConnection(driverStruct, requiredModules)

  -- Let the user select the database system
  self:printTitle(_("Please choose a database system:"))
  
  local i
  local rdbms
  local rdbmsList= {}
  local count= 0;
  
  for i= 1, grtV.getn(self.root.rdbmsMgmt.rdbms) do
    local rdbms= self.root.rdbmsMgmt.rdbms[i]
    local driverCount= 0
    
    if (driverStruct == nil) and (requiredModules == nil) then
      allRequirementsMet= true
    else
      local j
      for j= 1, grtV.getn(rdbms.drivers) do
        local driver= rdbms.drivers[j]
        
        if self:driverMeetsRequirements(driver, driverStruct, requiredModules) then
          driverCount= driverCount + 1
        end
      end
    end
    
    if driverCount > 0 then
      table.insert(rdbmsList, rdbms)
      count= count + 1
      print(count .. ". " .. grtV.toLua(rdbms.caption))
    end
  end
  
  print(_("0. Abort"))
  print("")
  
  local rdbmsIndex= self:inputNumber(_("Source Database System: "))
  
  if (rdbmsIndex > 0) and (rdbmsIndex <= table.getn(rdbmsList)) then
    rdbms= rdbmsList[rdbmsIndex]
  else   
    return nil
  end
  
  -- Let the user select a stored connection or to create a new one
  self:printTitle(_("Please choose a connection:"))
  
  print(_("1. Create new connection"))
  
  local connCount= 1
  local connList= {}
  for i= 1, grtV.getn(self.root.rdbmsMgmt.storedConns) do
    if grtV.toLua(self.root.rdbmsMgmt.storedConns[i].driver.owner._id) == 
      grtV.toLua(rdbms._id) then
      
      local conn= self.root.rdbmsMgmt.storedConns[i]
      
      if self:driverMeetsRequirements(conn.driver, driverStruct, requiredModules) then
        print(connCount + 1 .. ". " .. grtV.toLua(conn.name))      
        table.insert(connList, conn)
        
        connCount= connCount + 1
      end
    end
  end
  
  print(_("0. Abort"))
  if connCount > 1 then
    print(_("-1.Delete a connection"))
  end
  print("")
  
  local connIndex= self:inputNumber(_("Connection: "))
  
  if connIndex == 0 then
    return nil
  elseif connIndex > 1 and connIndex <= connCount + 1 then
    -- display selected connection
    local conn= connList[connIndex - 1]
    
    self:printTitle(string.format(_("Selected connection: %s"), grtV.toLua(conn.name)))
    
    for i= 1, grtV.getn(conn.parameterValues) do
      local key= grtV.getKey(conn.parameterValues, i)
      
      if (key ~= "password") then
        print(key .. ": " .. grtV.toLua(conn.parameterValues[key]))
      else
        print(key .. ": ********")
      end
    end
    
    print("")
    
    local isCorrect= self:inputNumber(_("Accept connection: (1. Yes, 2. Choose another connection, 0. Abort) "))
    
    if isCorrect == 0 then
      return nil
    elseif isCorrect == 1 then
      return conn
    else
      return self:selectConnection(driverStruct, requiredModules)
    end
  elseif connIndex == -1 then
    -- Handle connection deletion
    local delIndex= self:inputNumber(string.format(_("Connection to delete: (2 - %d, 0 to cancel) "), connCount))
    
    if delIndex ~= 0 then
      grtV.remove(self.root.rdbmsMgmt.storedConns, delIndex - 1)
      
      RdbmsManagement:storeConns({self.root.rdbmsMgmt.storedConns})
    end
    
    return self:selectConnection(driverStruct, requiredModules)
  else
    -- create a new connection
    return Mig:newConnection(driverStruct, rdbms)
  end
end

-- ------------------------------------------------------------------
-- Ask user to select a connection

function Mig:newConnection(driverStruct, rdbms)
  self:printTitle(string.format(_("Creating new connection to %s ..."), grtV.toLua(rdbms.caption)))
  print(_("Please enter the connection parameters."))
  
  local i
  local driver
  local conn= grtV.newObj("db.mgmt.Connection", "newConnection", grt.newGuid(), "")
  
  -- Choose a driver
  if (grtV.getn(rdbms.drivers) == 1) then
    driver= rdbms.drivers[1]
  else
    print(_("Please choose a driver:"))
    
    local driverList= {}
    local count= 0
    
    for i= 1, grtV.getn(rdbms.drivers) do
      if (driverStruct == nil) or (grtS.get(rdbms.drivers[i]) == driverStruct) then    
        count= count + 1
        table.insert(driverList, rdbms.drivers[i])
        
        print(count .. ". " .. grtV.toLua(rdbms.drivers[i].caption))
      end
    end
    
    print(_("0. Abort"))
    print("")
    
    local driverIndex= self:inputNumber(_("Driver: "))
    
    if (driverIndex > 0) and (driverIndex <= count) then
      driver= driverList[driverIndex]
    else
      return nil      
    end
  end
  
  conn.driver= driver
  
  print("")
  
  -- Get parameters
  for i= 1, grtV.getn(driver.parameters) do
    local param= driver.parameters[i]
    local paramDefault= grtV.toLua(param.defaultValue)
    local paramValue
    local paramLookup= ""

    if grtV.toLua(param.lookupValueModule) ~= "" then
      local j
      local lookupList= grtM.callFunction(grtV.toLua(param.lookupValueModule), 
        grtV.toLua(param.lookupValueMethod), {conn}, 1)
      
      if lookupList ~= nil then
        for j= 1, grtV.getn(lookupList) do
          paramLookup= paramLookup .. grtV.toLua(lookupList[j])
          
          if j < grtV.getn(lookupList) then
            paramLookup= paramLookup .. ", "
          end
        end
      end
    end
    
    if paramDefault ~= "" then
      paramValue= input(grtV.toLua(param.caption) .. " [" .. paramDefault .. "] ")
      if paramValue == "" then
        paramValue= paramDefault
      end
    elseif paramLookup ~= "" then
      paramValue= input(grtV.toLua(param.caption) .. " (" .. paramLookup .. ") ")
    else
      paramValue= input(grtV.toLua(param.caption) .. " ")
    end

    conn.parameterValues[grtV.toLua(param.name)]= paramValue
  end
  
  -- Copy modules
  for i= 1, grtV.getn(driver.defaultModules) do
    local moduleName= grtV.getKey(driver.defaultModules, i)
    
    conn.modules[moduleName]= driver.defaultModules[moduleName]
  end
  
  print("")
  
  local connName= input(_("Connection name (leave blank not to store): "))
  
  if connName ~= "" then
    conn.name= connName
    grtV.insert(self.root.rdbmsMgmt.storedConns, conn)
    
    RdbmsManagement:storeConns({self.root.rdbmsMgmt.storedConns})
  end
  
  return conn
end

-- ------------------------------------------------------------------
-- Tests the connection

function Mig:testConnection(conn)
  self:printTitle(string.format(_("Testing connection to %s ..."), grtV.toLua(conn.driver.owner.caption)))

  local res= grtM.callFunction(grtV.toLua(conn.modules["ReverseEngineeringModule"]), "getVersion", {conn})
  if res == nil then
    print(string.format(_("The connection to the %s database could not be established."), 
      grtV.toLua(conn.driver.owner.caption))
    )
    
    self:printError(_("The following error occured."))
    
    return 0
  end

  -- store target version for the migration process
  grtV.setGlobal("/migration/targetVersion", res)
  print(_("Test completed successfully."))
  
  return 1
end

-- ------------------------------------------------------------------
-- Get list of source schemata

function Mig:getSourceSchemata()
  local schemaList= grtM.callFunction(grtV.toLua(self.sourceConn.modules["ReverseEngineeringModule"]), 
    "getSchemata", {self.sourceConn}, 1
  )
  
  if schemaList ~= nil then
    print(_("List of source schemata fetched successfully."))
    print("")
  end
  
  return schemaList
end

-- ------------------------------------------------------------------
-- Let the use choose from the list of source schemata

function Mig:chooseSourceSchemata()
  local schemaList= grtV.duplicate(self.sourceSchemataNames)

  self:printTitle(_("Choose the schemata to migrate ..."))
  
  local i
  
  for i= 1, grtV.getn(schemaList) do
    print(i .. ". " .. grtV.toLua(schemaList[i]))
  end
  
  print("0. Abort")
  print("")
  
  local selectedSchemataStr= self:inputNumber(_("Schemata: (ids seperate with ,) "))

  -- catch exit
  if selectedSchemataStr == 0 then
    return -1
  end

  local selectedSchemataIds= grt.split(selectedSchemataStr, ",")

  for i= grtV.getn(schemaList), 1, -1 do
    local key
    local value
    local selected= false
    
    for key, value in selectedSchemataIds do
      if i == value + 0 then
        selected= true
        break
      end
    end
    
    if not(selected) then
      grtV.remove(schemaList, i)
    end
  end
  
  print("")
  self:printTitle(_("Selected schema(ta):"))
  
  for i= 1, grtV.getn(schemaList) do
    print(grtV.toLua(schemaList[i]))
  end
  
  print("")
  local acceptIndex= self:inputNumber(_("Accept selection: (1. Yes, 2. Reselect, 0. Abort) "))
  
  if acceptIndex == 2 then
    schemaList= self:chooseSourceSchemata()
  end
  
  return schemaList
end

-- ------------------------------------------------------------------
-- Let the use choose from the list of source schemata

function Mig:reverseEngineering()
  self:printTitle(string.format(_("Reverse engineering %s ..."), grtV.toLua(self.sourceConn.driver.owner.caption)))
  
  local catalog= grtM.callFunction(grtV.toLua(self.sourceConn.modules.ReverseEngineeringModule),
    "reverseEngineer", {self.sourceConn, self.selectedSchemataNames}
  )

  if catalog ~= nil then
    print("")
    print(_("Reverse engineering completed successfully."))
    print("")
  else
    print("")
    self:printError(_("The following error occured during reverse engineering."))
  end
  
  return catalog
end

-- ------------------------------------------------------------------
-- Get the available migration methods

function Mig:getMigrationMethods()
  self:printTitle(_("Fetching available migration methods ..."))
  
  local methods= grtM.callFunction(grtV.toLua(self.sourceConn.modules.MigrationModule),
    "migrationMethods", nil
  )
  
  if methods == nil then
    print("")
    print(_("Could not fetch migration methods."))
    print("")
  else
    -- build source object list
    self.sourceObjects= {}
    self.objTypes= {}
    
    local schemaList= grtV.getGlobal("/migration/sourceCatalog/schemata")
    
    -- loop over all source schemata
    local i
    for i=1, grtV.getn(schemaList) do
      local schema= grtV.getGlobal("/migration/sourceCatalog/schemata/" .. i - 1)
      
      table.insert(self.objTypes, grtS.get(schema))
      
      local schemaStructMembers= grtS.getMembers(grtS.get(schema))
      
      local j      
      for j= 1, table.getn(schemaStructMembers) do
        if grtS.getMemberType(grtS.get(schema), schemaStructMembers[j]) == "list" then
          local memberContentStruct= grtS.getMemberContentStruct(grtS.get(schema), schemaStructMembers[j])

          -- only take objects from lists, that hold db.DatabaseObject dicts
          if (memberContentStruct ~= nil) and
            (grtS.inheritsFrom(memberContentStruct, "db.DatabaseObject")) then
          
            local k
            for k= 1, grtV.getn(schema[schemaStructMembers[j]]) do
              table.insert(self.sourceObjects, schema[schemaStructMembers[j]][k])
            
              if k == 1 then
                table.insert(self.objTypes, grtS.get(schema[schemaStructMembers[j]][k]))
              end
            end
          end
        end
      end
    end
    
    -- add default mappings for all objTypes
    local defaultMappings= grtV.getGlobal("/migration/mappingDefaults")
    for i= 1, table.getn(self.objTypes) do
      local j
      local bestMethod= nil
      local bestMethodRating= -1
      
      for j=1, grtV.getn(methods) do
        local sourceStructName= grtV.toLua(methods[j].sourceStructName)
        local targetPackageName= grtV.toLua(methods[j].targetPackageName)
        local methodRating= tonumber(grtV.toLua(methods[j].rating))
                
        if (grtS.inheritsFrom(self.objTypes[i], sourceStructName) or 
            (self.objTypes[i] == sourceStructName)) and 
          (grtV.toLua(self.targetConn.driver.owner.databaseObjectPackage) == targetPackageName) and
          (bestMethodRating < methodRating) then
          bestMethod= methods[j]
          bestMethodRating= methodRating
        end
      end
      
      if bestMethod ~= nil then
        -- create mapping entry
        local mapping= grtV.newObj("db.migration.Mapping", 
          "DefaultMapping" .. grtV.toLua(bestMethod.sourceStructName), grt.newGuid(), ""
        )
        
        mapping.sourceStructName= sourceStructName
        mapping.method= bestMethod
        mapping.methodName= bestMethod.name
        mapping.moduleName= moduleName
        mapping.params= bestMethod.paramGroups[1]
        
        grtV.insert(defaultMappings, mapping)
      end
    end
  end
  
  return methods
end

-- ------------------------------------------------------------------
-- Setting up ignore list

function Mig:getIgnoreList(ignoreList)
  if ignoreList == nil then
    ignoreList= grtV.newList("string")
  end
  
  self:printTitle(_("Your current ignore list:"))
  
  local i
  local j
  local ignoreListIds= {}
  local totalCount= 0
  
  for j= 1, table.getn(self.objTypes) do    
    local count= 0
    
    if not(grtS.inheritsFrom(self.objTypes[j], "db.Schema")) then
      print("")
      print(grtS.getCaption(self.objTypes[j]))
      
      for i= 1, grtV.getn(ignoreList) do
        if string.sub(grtV.toLua(ignoreList[i]), 1, string.len(self.objTypes[j])) == self.objTypes[j] then
          count= count + 1          
          totalCount= totalCount + 1
          
          print("   " .. totalCount .. ". " .. string.sub(grtV.toLua(ignoreList[i]), string.len(self.objTypes[j]) + 2))
          
          table.insert(ignoreListIds, totalCount, i)
        end
      end
      
      if count == 0 then
        print("   " .. _("None"))
      end 
    end
  end
  
  print("")
  print(_("1. Accept ignore list"))
  print(_("2. Add item to ignore list"))
  print(_("3. Delete item from ignore list"))
  print(_("0. Abort"))
  print("")
  
  local selection= self:inputNumber(_("Selection: "))
  
  if selection == 0 then
    return nil
  elseif selection == 1 then
    return ignoreList
  elseif selection == 2 then
    print("")
    
    -- ignore schema
    for j= 2, table.getn(self.objTypes) do    
      print(j - 1 .. ". " .. grtS.getCaption(self.objTypes[j]))
    end
    
    print("")
    
    local objType= self:inputNumber(string.format(
      _("Please enter the object type: (1 - %d, 0. Abort) "), table.getn(self.objTypes) - 1)
    )
    
    if objType == 0 then
      ignoreList= self:getIgnoreList(ignoreList)
    else
      local i
      local s= ""
      local objs= {}
      local index= 0
      
      -- ignore schema
      objType= objType + 1
      
      for i= 1, grtV.getn(self.sourceObjects) do
        if grtS.get(self.sourceObjects[i]) == self.objTypes[objType] then
          local onIgnoreList= false
          local j        
          
          -- check if obj is already on the ignore list
          for j= 1, grtV.getn(ignoreList) do
            objIgnoreStr= self.objTypes[objType] .. ":" .. grtV.toLua(self.sourceObjects[i].name)
            
            if grtV.toLua(Base:patternMatch({objIgnoreStr, grtV.toLua(ignoreList[j]), 1})) == 1 then
              onIgnoreList= true
              break
            end
          end
        
          -- if not, list it
          if not(onIgnoreList) then
            table.insert(objs, self.sourceObjects[i])
          
            index= index + 1
            s= s .. grt.alignLeft(index .. ". " .. grtV.toLua(self.sourceObjects[i].name) .. " ", 25)
              
            if math.mod(index, 4) == 0 then
              print(s)
              s= ""
            end
          end
        end
      end
      
      if s ~= "" then
        print(s)
      end
      
      print("")
      local ignoreObjs= input(_("Add to ignore list: (id or pattern - separate by , use no spaces) "))
      
      local ignoreObjsIds= grt.split(ignoreObjs, ",")
      
      for i=1, table.getn(ignoreObjsIds) do
        local index= tonumber(ignoreObjsIds[i])
        
        if index ~= nil then
          grtV.insert(ignoreList, self.objTypes[objType] .. ":" .. grtV.toLua(objs[index].name))  
        elseif (ignoreObjsIds[i] ~= nil) and (ignoreObjsIds[i] ~= "") then
          grtV.insert(ignoreList, self.objTypes[objType] .. ":" .. ignoreObjsIds[i])
        end
      end
      
      ignoreList= self:getIgnoreList(ignoreList)
    end
  elseif selection == 3 then
    print("")
    
    local index= self:inputNumber(_("Please enter the number of the entry to remove: (0 to cancel) "))
    
    if (index > 0) and (ignoreListIds[index] ~= nil) then
      grtV.remove(ignoreList, ignoreListIds[index])
    end
    
    ignoreList= self:getIgnoreList(ignoreList)
  end
  
  return ignoreList
end

-- ------------------------------------------------------------------
-- Perform migration

function Mig:doMigration()
  print("")
  
  grtM.callFunction(
    grtV.toLua(self.sourceConn.modules.MigrationModule), "migrate", 
    {"global::/migration", "global::/rdbmsMgmt/rdbms/" .. grtV.toLua(self.targetConn.driver.owner.name), "global::/migration/targetVersion"}
  )
  if grtError == nil then
    print(_("Migration completed successfully."))
    
    return true
  else
    self:printError(_("The following error occured during migration."))
    
    return false
  end
end

-- ------------------------------------------------------------------
-- Generate SQL

function Mig:generateObjects()
  print("")
  
  local selection= self:inputNumber(
    _("Create object online or write a SQL create script? (1. online, 2. script, 3. both, 0. abort) ")
  )
  
  if selection == 0 then
    return false
  else
    if (selection == 2) or (selection == 3) then
      print("")
      
      grtM.callFunction(
        grtV.toLua(self.targetConn.modules.TransformationModule), "generateSqlCreateStatements", 
        {"global::/migration/targetCatalog", "global::/migration/objectCreationParams"}
      )
      self:printError(_("The SQL create statements cannot be created."))
    
      -- write sql create script to file
      print("Write create script.")
      
      local script= grtM.callFunction(
        grtV.toLua(self.targetConn.modules.TransformationModule), "getSqlScript", 
        {"global::/migration/targetCatalog", "global::/migration/objectCreationParams"}
      )
      self:printError(_("The SQL create script cannot be created."))

      local f= io.open("creates.sql", "w+")
      if f ~= nil then
        f:write(grtV.toLua(script))
        
        f:flush()
        f:close()
      end
    end
    
    if (selection == 1) or (selection == 3) then
      -- create database objects online
      print("Create database objects.")
      
      grtM.callFunction(
        grtV.toLua(self.targetConn.modules.TransformationModule), "executeSqlStatements", 
        {self.targetConn, "global::/migration/targetCatalog", "global::/migration/creationLog"}
      )
      self:printError(_("The SQL create script cannot be executed."))
    end
  end
  
  return (grtError == nil)
end

-- ------------------------------------------------------------------
-- Generate SQL

function Mig:bulkDataTransfer()
  print("")
  
  local selection= self:inputNumber(
    _("Write a SQL insert script? (1. yes, 2. no, 0. abort) ")
  )

  -- set transfer parameters
  grtV.setGlobal("/migration/dataBulkTransferParams", {
      CreateScript= (selection == 1) and "yes" or "no", 
      TransferData= "no",
      ScriptFileName="inserts.sql"
    }
  )
  grtV.setContentType(grtV.getGlobal("/migration/dataBulkTransferParams"), "string")
  
  self:printTitle("Execute bulk data transfer")
  
  grtM.callFunction(
    grtV.toLua(self.sourceConn.modules.MigrationModule), "dataBulkTransfer", 
    {self.sourceConn, "global::/migration/sourceCatalog", 
      self.targetConn, "global::/migration/targetCatalog", 
      "global::/migration/dataBulkTransferParams",
      "global::/migration/dataTransferLog"
    }
  )
  self:printError(_("The bulk data transfer returned an error."))
  
  return true
end

-- ------------------------------------------------------------------
-- ------------------------------------------------------------------
-- Start the Migration script

Mig:run()
