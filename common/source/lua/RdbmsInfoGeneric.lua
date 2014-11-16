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
-- @file RdbmsInfoGeneric.lua
-- @brief Module that contains functionality for database management
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
  local moduleInfo= {
    name= "RdbmsInfoGeneric", 
    functions= {
      "getRdbmsInfo::"
    }, 
    extends= "RdbmsInfo"
  }

  return moduleInfo
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about Access
--
--   Returns a db.mgmt.Rdbms struct with infos about the rdbms
-- 
-- @return a new created db.mgmt.Rdbms GRT value struct 
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  local rdbmsMgmt= args[1]

  -- create Rdbms object
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "GenericJdbc", "{76FD8EDF-45C7-4A4C-9014-F7FF485BA904}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "Generic Jdbc"
  rdbms.databaseObjectPackage= "db"

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverGenericJdbc(rdbms))

  rdbms.defaultDriver= rdbms.drivers[1]

  return grt.success(rdbms)
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get the MS SQL driver
--
--   Helper function to return infos about the Jdbc driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverGenericJdbc(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "GenericJdbc", 
    "{27A7FC95-2459-444E-901A-3458CF82D808}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "Generic Jdbc"
  driver.description= "Generic Jdbc driver connection"

  -- Jdbc specific settings
  driver.className= ""
  driver.connectionStringTemplate= ""

  -- add driver parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "classname", "Class Name:", 
    "Classname of the driver to use.", "string", 1, 218, "", 0, 1))

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", 2, 218, "", 0, 1))

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "explicit_username", "Username:", 
    "Explicit username if not submitted in the connection string", "string", 3, 218, "", 0, 0))

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "explicit_password", "Password:", 
    "Explicit password", "string", 4, 218, "", 0, 0))    
    
  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "skipVersionDetection", "Skip version detection", 
    "Skips version detection as a workaround for broken drivers.", "boolean", -1, 318, "", 1, 0))
    
  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringGeneric",
      MigrationModule= "MigrationGeneric",
      TransformationModule= ""
    }

  if grt.moduleExists("BaseJava") then
    driver.isAccessable= true
    driver.isInstalled= 1
  else
    driver.isAccessable= false
    driver.isInstalled= false
  end

  return driver
end
