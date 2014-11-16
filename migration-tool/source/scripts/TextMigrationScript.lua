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


run("lua/_textforms.lua")

GO_NEXT="next"
GO_BACK="back"
GO_QUIT="quit"

BACK_NEXT_BUTTONS= {{GO_BACK, _("Back")}, {GO_NEXT, _("Next")}}

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


function centerLabel(y, width, text)
    return dlg.Label((width - string.len(text))/2, y, text)
end


-- ------------------------------------------------------------------
-- The run method that is called to perform the migration

function Mig:run()
  local result
  local state= 1
  self:init()

  self:initscr()

  while true do
    if state == 1 then
      -- Source connection
      result= self:getConnection(_("Source database connection."), "db.mgmt.JdbcDriver")
      if result[1] == GO_BACK then
        return -1
      elseif result[1] == GO_QUIT then
        return -1
      else
        state= 2   
      end
      self.sourceConn= result[2]
    end
    if state == 2 then
      -- Target connection
      result= self:getConnection(_("Target database connection."), "db.mgmt.JdbcDriver", {"TransformationModule"})
      if result[1] == GO_BACK then
        state= 1
      elseif result[1] == GO_QUIT then
        return -1
      else
        self.targetConn= result[2]
        grtV.setGlobal("/migration/targetConnection", self.sourceConn)
        self:endscr()
        
        -- Get schemata list
        self:printTitle(_("Fetching source schemata."), 1)
        self.sourceSchemataNames= self:getSourceSchemata()
        if self.sourceSchemataNames == nil then
          return -1
        else
          state= 3
          grtV.setGlobal("/migration/sourceSchemataNames", self.sourceSchemataNames)
        end
      end
    end
    if state == 3 then
      self:initscr()
      -- Selected schemata
      result= self:selectSourceSchemata(_("Schema selection."))
      if result[1] == GO_QUIT then
        return -1
      elseif result[1] == GO_BACK then
        state= 2
      else
        state= 4
        self.selectedSchemataNames= result[2]
        grtV.setGlobal("/migration/selectedSchemataNames", self.selectedSchemataNames)
        self:endscr()
        -- Reverse engineering
        local catalog= self:reverseEngineering(_("Reverse engineering."))
        if catalog == nil then
          return -1
        end
        grtV.setGlobal("/migration/sourceCatalog", catalog)
        -- Get migration methods
        self.migrationMethods= self:getMigrationMethods(_("Get migration methods."))
        if self.migrationMethods == nil then
          return -1
        end
        grtV.setGlobal("/migration/migrationMethods", self.migrationMethods)
        self:initscr()
      end
    end
    if state == 4 then
      -- Let the user setup the ignore list
      result= self:selectIgnoreList(_("Setup Ignore List"))
      if result[1] == GO_QUIT then
        return -1
      elseif result[1] == GO_BACK then
        state= 3
      else
        state= 5
        grtV.setGlobal("/migration/ignoreList", result[2])
      end
    end
    if state == 5 then
      self:endscr()
      -- Do migration
      self:printTitle(_("Performing migration."), 1)
      if not(self:doMigration()) then
        return -1
      end
      self:initscr()
      state= 6
    end
    if state == 6 then
      -- Generate objects
      if not(self:generateObjects(_("Generate target objects"))) then
        return -1
      end

      -- Bulk data transfer
      if not(self:bulkDataTransfer(_("Bulk data transfer"))) then
        return -1
      end
    end
  end
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

-- ------------------------------------------------------------------
-- Select and test a connection

function Mig:getConnection(title, driverStruct, requiredModules)

  while 1 do
    local result= self:selectConnection(title, driverStruct, requiredModules)

    if result[1] == GO_NEXT then
      local conn= result[2]
      if self:testConnection(conn) ~= 1 then
        local form= SimpleForm:new()

        form:init(title, 40)
	        
        form:addLabel(_("There was an error with the connection"), SF_CENTER)
        form:addSpace()
        form:addLabel(_("Choose a different connection?"), SF_CENTER)
        form:addSpace()
        form:setButtons({}, {{GO_NEXT,"Yes"},{GO_QUIT,"Cancel"}})

        result= form:run()

        if result ~= GO_NEXT then
          return {GO_QUIT, nil}
        end
      else
        return {GO_NEXT, conn}
      end
    else
      return result
    end
  end
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

function Mig:selectDBSystem(title, driverStruct, requiredModules)
  local options
  local form
  local rdbmsList= {}
  
  -- Let the user select the database system
  options= {}

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
      table.insert(options, grtV.toLua(rdbms.caption))
    end
  end
  
  form= SimpleForm:new()
  form:init(title, 40)
  form:addLabel(_("Please choose a database system:"), SF_CENTER)
  form:addSpace()
  form:addListbox("lbox", options, 6)
  form:addSpace()
  form:setButtons({}, BACK_NEXT_BUTTONS)

  result= form:run()

  if result == GO_NEXT then
    return {result, rdbmsList[form:getValue("lbox")]}
  else
    return {result, nil}
  end
end


function Mig:selectStoredConnection(title, rdbms)
  local i
  local form

  -- Let the user select a stored connection or to create a new one
  while 1 do
    local clist= {}
    local options= {}
    for i= 1, grtV.getn(self.root.rdbmsMgmt.storedConns) do
      if grtV.toLua(self.root.rdbmsMgmt.storedConns[i].driver.owner._id) ==
      grtV.toLua(rdbms._id) then
        
        local conn= self.root.rdbmsMgmt.storedConns[i]

        table.insert(options, grtV.toLua(conn.name))
        table.insert(clist, i)
      end
    end

    table.insert(options, 1, _("Create new connection"))

    form= SimpleForm:new()
    form:init(title, 60)
    form:addLabel(_("Please choose a connection:"), SF_CENTER)
    form:addSpace()
    form:addListbox("lbox", options, 6)
    form:addSpace()

    form:setButtons({{"DELETE", "Delete Selected"}}, BACK_NEXT_BUTTONS)

    local result= form:run()

    if result == "DELETE" then
      local delIndex= form:getValue("lbox")

      if (delIndex > 1) and (delIndex ~= nil) then
        grtV.remove(self.root.rdbmsMgmt.storedConns, clist[delIndex-1])
      
        RdbmsManagement:storeConns({self.root.rdbmsMgmt.storedConns})
      end
    elseif result == GO_BACK then
      return {GO_BACK,nil}
    elseif result == GO_NEXT then
      local index= form:getValue("lbox")-1

      if index > 0 then
        return {GO_NEXT, self.root.rdbmsMgmt.storedConns[clist[index]]}
      else
        return {GO_NEXT, nil}
      end
    else
	return {result, nil}
    end
  end
end

function Mig:confirmConnection(title, conn)
  local i
  local form

  form= SimpleForm:new()
  form:init(title, 40)

  form:addLabel(string.format(_("Selected connection: %s"), grtV.toLua(conn.name)), SF_LEFT)
  form:addSpace()

  for i= 1, grtV.getn(conn.parameterValues) do
    local key= grtV.getKey(conn.parameterValues, i)

    if (key ~= "password") then
      form:addLabel(key .. ": " .. grtV.toLua(conn.parameterValues[key]), SF_LEFT)
    else
      form:addLabel(key .. ": ********", SF_LEFT)
    end
  end

  form:addSpace()
  
  form:addLabel(_("Accept connection?"), SF_LEFT)
  form:setButtons({}, {{GO_NEXT, "Yes"},{GO_BACK, "No"}})

  local result= form:run()

  return {result, nil}
end


function Mig:selectConnection(title, driverStruct, requiredModules)
  local res
  local rdbms
  local conn
  local step

  step= 1
  while 1 do
    if step == 1 then
      res= self:selectDBSystem(title, driverStruct, requiredModules)
      if res[1] == GO_BACK then
        return {GO_BACK, nil}
      elseif res[1] == GO_QUIT then
        return {GO_QUIT, nil}
      else
        step= 2
        rdbms= res[2]
      end
    end  
    if step == 2 then
      res= self:selectStoredConnection(title, rdbms)
      if res[1] == GO_NEXT then
        step= 3
        conn= res[2]
      elseif res[1] == GO_BACK then
        step= 1
      else
        return {GO_QUIT, nil}
      end
    end
    if step == 3 then
      if conn ~= nil then
        -- display selected connection
        res= self:confirmConnection(title, conn)
        if res[1] == GO_NEXT then
          return {GO_NEXT, conn}
        elseif res[1] == GO_QUIT then
          return {GO_QUIT, nil}
        else
          step= 2
        end
      else
        -- create a new connection
        res= Mig:newConnection(title, driverStruct, rdbms)

        if res[1] == GO_BACK then
          step= 2
        elseif res[1] == GO_NEXT then
          return {GO_NEXT, res[2]}
        else
          return {GO_QUIT, nil}  
        end
      end
    end
  end
end

-- ------------------------------------------------------------------
-- Ask user to select a connection

function Mig:selectDriver(title, driverStruct, rdbms)
  local options, form
  local driverList= {}
  -- Choose a driver
  options={}
  for i= 1, grtV.getn(rdbms.drivers) do
    if (driverStruct == nil) or (grtS.get(rdbms.drivers[i]) == driverStruct) then
      table.insert(driverList, rdbms.drivers[i])

      table.insert(options, grtV.toLua(rdbms.drivers[i].caption))
    end
  end

  form= SimpleForm:new()
  form:init(title, 60)

  form:addLabel(string.format(_("New Connection to %s"), grtV.toLua(rdbms.caption)), SF_CENTER)
  form:addLabel(_("Please select a driver to use for the connection:"), SF_CENTER)
  form:addSpace()

  form:addLabel(_("Driver:"), SF_LEFT)
  form:addListbox("driver", options, 5)
  form:addSpace()
    
  form:setButtons({}, BACK_NEXT_BUTTONS)

  local result= form:run()

  if result ~= GO_NEXT then
    return {result, nil}
  else
    return {GO_NEXT, driverList[form:getValue("driver")]}
  end
end


function Mig:newConnectionWithDriver(title, rdbms, driver)
  local result
  local form
  local i
  local options
  local conn= grtV.newObj("db.mgmt.Connection", "newConnection", grt.newGuid(), "")

  conn.driver= driver

  -- Get parameters
  form= SimpleForm:new()
  form:init(title, 60)
    
  form:addLabel(string.format(_("New Connection to %s"), grtV.toLua(rdbms.caption)), SF_CENTER)
  form:addLabel(_("Please enter the connection parameters."), SF_CENTER)
  form:addSpace()

  form:addLabel(_("Driver: ") .. grtV.toLua(driver.caption))
  form:addEntries({{"name", _("Connection Name:"), "", SFT_TEXT}})
  form:addLabel(_("(leave blank to not store)"), SF_RIGHT)

  options= {}

  local y= 8
  for i= 1, grtV.getn(driver.parameters) do
    local param= driver.parameters[i]
    local paramDefault= grtV.toLua(param.defaultValue)
    local paramValue
    local paramLookup= 0
    local suboptions
    local items= {}
    suboptions= {}
    if grtV.toLua(param.lookupValueModule) ~= "" then
      local j
      local lookupList= grtM.callFunction(grtV.toLua(param.lookupValueModule),
        grtV.toLua(param.lookupValueMethod), {conn}, 1)
      if lookupList ~= nil then
        for j= 1, grtV.getn(lookupList) do
          table.insert(suboptions, grtV.toLua(lookupList[j]))
          items:insert(grtV.toLua(lookupList[j]))
          paramLookup=1
        end
      end
    end
    if paramLookup==1 then
      table.insert(options, {grtV.toLua(param.name), grtV.toLua(param.caption), suboptions, SFT_LIST})
    else
      if grtV.toLua(param.name) == "password" then
        table.insert(options, {grtV.toLua(param.name), grtV.toLua(param.caption), "", SFT_PASSWORD})
      elseif grtV.toLua(param.name) == "port" then
        table.insert(options, {grtV.toLua(param.name), grtV.toLua(param.caption), paramDefault, SFT_NUMBER})
      else
        table.insert(options, {grtV.toLua(param.name), grtV.toLua(param.caption), paramDefault, SFT_TEXT})
      end
    end
  end

  form:addEntries(options)

  form:addSpace()
  form:setButtons({}, BACK_NEXT_BUTTONS)

  result= form:run()

  if result == GO_BACK then
    return {GO_BACK, nil}
  elseif result == GO_QUIT then
    return {GO_QUIT, nil}
  else
    for i= 1, grtV.getn(driver.parameters) do
      local param= driver.parameters[i]
      conn.parameterValues[grtV.toLua(param.name)]= form:getValue(grtV.toLua(param.name))
    end

    -- Copy modules
    for i= 1, grtV.getn(driver.defaultModules) do
      local moduleName= grtV.getKey(driver.defaultModules, i)
    
      conn.modules[moduleName]= driver.defaultModules[moduleName]
    end
 
    local connName= form:getValue("name")

    if connName ~= "" then
      conn.name= connName
      grtV.insert(self.root.rdbmsMgmt.storedConns, conn)

      RdbmsManagement:storeConns({self.root.rdbmsMgmt.storedConns})
    end
    return {GO_NEXT, conn}
  end
end


function Mig:newConnection(title, driverStruct, rdbms)
  local result  
  while 1 do
    local count= 0
    local driverIndex= nil
    for i= 1, grtV.getn(rdbms.drivers) do
      if (driverStruct == nil) or (grtS.get(rdbms.drivers[i]) == driverStruct) then
        count= count+1
        driverIndex= i
      end
    end
    if count > 1 then
      result= self:selectDriver(title, driverStruct, rdbms)

      if result[1] == GO_BACK then
        return {GO_BACK, nil}
      else
        result= self:newConnectionWithDriver(title, rdbms, result[2])
        if result[1] == GO_QUIT then
          return {GO_QUIT, nil}
        elseif result[1] == GO_NEXT then
          return result
        end
      end
    else
      result= self:newConnectionWithDriver(title, rdbms, rdbms.drivers[driverIndex])
      return result
    end
  end
end

-- ------------------------------------------------------------------
-- Tests the connection

function Mig:testConnection(conn)

  self:endscr()
  
  self:printTitle(string.format(_("Testing connection to %s ..."), grtV.toLua(conn.driver.owner.caption)))

  local res= grtM.callFunction(grtV.toLua(conn.modules["ReverseEngineeringModule"]), "getVersion", {conn})
  if res == nil then
    print(string.format(_("The connection to the %s database could not be established."), 
      grtV.toLua(conn.driver.owner.caption))
    )
    
    self:printError(_("The following error occured."))

    print(_("Press enter to continue."))
    input("")

    self:initscr()
    
    return 0
  end

  -- store target version for the migration process
  grtV.setGlobal("/migration/targetVersion", res)

  self:initscr()
  
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

function Mig:selectSourceSchemata(title)
  local schemaList= grtV.duplicate(self.sourceSchemataNames)
  local form
  local i
  local list

  form= SimpleForm:new()
  form:init(title, 60)

  list= {}
  for i= 1, grtV.getn(schemaList) do
    table.insert(list, {grtV.toLua(schemaList[i]), 0, i})
  end
  form:addLabel(_("Select the schemas to migrate:"), SF_LEFT)
  form:addSpace()
  form:addCheckList("list", list, 10)
  form:addSpace()
  form:setButtons({}, BACK_NEXT_BUTTONS)

  local result= form:run()

  if result ~= GO_NEXT then
    return {result, nil}
  end

  self:endscr()
  local selectedSchemas= form:getValue("list")
  for i= grtV.getn(schemaList), 1, -1 do
    local key
    local value
    local selected= false

    for key, value in selectedSchemas do
      if i == value + 0 then
        selected= true
        break
      end
    end
    
    if not(selected) then
      grtV.remove(schemaList, i)
    end
  end

  return {GO_NEXT, schemaList}
end

-- ------------------------------------------------------------------
-- Let the use choose from the list of source schemata

function Mig:reverseEngineering(title)

  self:printTitle(title, 1)
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

function Mig:getMigrationMethods(title)
  self:printTitle(title, 1)
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

function Mig:addIgnoreListItem(title, ignoreList)
  local form= SimpleForm:new()

  local items= {}
  
  form:init(title, 50)

  form:addLabel(_("Select objects to ignore:"), SF_LEFT)
  -- ignore schema

  local i
  local objs= {}
  local objType

  -- ignore schema
  for objType=2,table.getn(self.objTypes) do
    local subitems= {}
    for i= 1, grtV.getn(self.sourceObjects) do
      if grtS.get(self.sourceObjects[i]) == self.objTypes[objType] then
        local onIgnoreList= false
        local j
        objIgnoreStr= self.objTypes[objType] .. ":" .. grtV.toLua(self.sourceObjects[i].name)
        -- check if obj is already on the ignore list
        for j= 1, grtV.getn(ignoreList) do            
          if grtV.toLua(Base:patternMatch({objIgnoreStr, grtV.toLua(ignoreList[j]), 1})) == 1 then
            onIgnoreList= true
            break
          end
        end
        
        -- if not, list it
        if not(onIgnoreList) then
          table.insert(objs, self.sourceObjects[i])
          table.insert(subitems, {grtV.toLua(self.sourceObjects[i].name), objIgnoreStr})
        end
      end
    end
    table.insert(items, {self.objTypes[objType], subitems})
  end

  form:addCheckTree("ignore", items, 15)

  form:addSpace()

  form:setButtons({}, {{"CANCEL", "Cancel"},{"OK","OK"}})
  
  local result= form:run()

  if result == "OK" then
    local obj
    items= form:getValue("ignore")
    for i,obj in items do
      grtV.insert(ignoreList, obj)
    end
  end
  return ignoreList
end


function Mig:selectIgnoreList(title, ignoreList)
  local form
  
  if ignoreList == nil then
    ignoreList= grtV.newList("string")
  end

  form= SimpleForm:new()
  form:init(title, 60)

  form:addLabel(_("Your current ignore list:"), SF_LEFT)
  
  local i
  local j
  local totalCount= 0
  local itemList= {}
  
  for j= 1, table.getn(self.objTypes) do    
    local count= 0
    
    if not(grtS.inheritsFrom(self.objTypes[j], "db.Schema")) then
      local items= {}

      table.insert(itemList, {grtS.getCaption(self.objTypes[j]), items})
      
      for i= 1, grtV.getn(ignoreList) do
        if string.sub(grtV.toLua(ignoreList[i]), 1, string.len(self.objTypes[j])) == self.objTypes[j] then
          count= count + 1          
          totalCount= totalCount + 1

          table.insert(items, {string.sub(grtV.toLua(ignoreList[i]), string.len(self.objTypes[j]) + 2), grtV.toLua(ignoreList[i])})
        end
      end

      if table.getn(items) == 0 then
        table.insert(items, {"None", ""})
      end
    end
  end

  form:addCheckTree("list", itemList, 10)
  form:addLabel(_("Objects in the ignore list will not be migrated."), SF_LEFT)
  form:addLabel(_("Select objects and press Delete to remove from ignore list."), SF_LEFT)
  form:addSpace()

  form:setButtons({{"ADD","Add New"},{"DEL","Delete"}}, BACK_NEXT_BUTTONS)

  local result= form:run(true)

  if result == GO_NEXT then
    form:pop()
    return {GO_NEXT, ignoreList}
  elseif result == GO_BACK then
    form:pop()
    return {GO_BACK, nil}   
  elseif result == GO_QUIT then
    form:pop()
    return {GO_QUIT, nil}
  elseif result == "ADD" then
    ignoreList= self:addIgnoreListItem(title, ignoreList)
    form:pop()
    return self:selectIgnoreList(title, ignoreList)
  elseif result == "DEL" then
    local obj, item
    items= form:getValue("list")
    for i= grtV.getn(ignoreList),1,-1 do
      for j,item in items do
        if item == grtV.toLua(ignoreList[i]) then
          grtV.remove(ignoreList, i)
          break
        end
      end
    end
    return self:selectIgnoreList(title, ignoreList)
  end
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

function Mig:generateObjects(title)
  local form= SimpleForm:new()
  form:init(title, 40)

  form:addLabel("Select actions to perform:", SF_LEFT)
  form:addSpace()
  form:addCheck("online", _("Create objects online"))
  form:addCheck("script", _("Write a script file"))
  form:addSpace()
  form:setButtons({}, {{GO_NEXT, _("OK")}, {GO_QUIT, _("Abort")}})

  local result= form:run()
  
  if (result == GO_QUIT) then
    return false
  else
    self:endscr()
      
    if form:getValue("script")==1 then      
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
    
    if form:getValue("online")==1 then
      -- create database objects online
      print("Create database objects.")
      
      grtM.callFunction(
        grtV.toLua(self.targetConn.modules.TransformationModule), "executeSqlStatements", 
        {self.targetConn, "global::/migration/targetCatalog", "global::/migration/creationLog"}
      )
      self:printError(_("The SQL create script cannot be executed."))
    end
    print("Press <Enter> to continue.")
    input("")
    self:initscr()
  end
  
  return (grtError == nil)
end

-- ------------------------------------------------------------------
-- Generate SQL

function Mig:bulkDataTransfer(title)
  local form= SimpleForm:new()

  form:init(title, 40)

  form:addLabel(_("Write a SQL insert script?"))

  form:addSpace()
  form:setButtons({}, {{"yes","Yes"},{"no","No"}})

  local result= form:run()

  -- set transfer parameters
  grtV.setGlobal("/migration/dataBulkTransferParams", {
      CreateScript= result,
      TransferData= "no",
      ScriptFileName="inserts.sql"
    }
  )
  grtV.setContentType(grtV.getGlobal("/migration/dataBulkTransferParams"), "string")

  self:endscr()

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


function Mig:initscr()
  dlg.init()
  dlg.drawRootText(0, 0, "MySQL Migration Toolkit - Script Version " .. self.version)
  dlg.pushHelpLine(_(" <tab> - switch between elements  |  <space> - select  |  <return> - activate"))
end

function Mig:endscr()
  dlg.finish()
end

-- ------------------------------------------------------------------
-- ------------------------------------------------------------------
-- Start the Migration script

Mig:run()

Mig:endscr()
