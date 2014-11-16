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
-- @file RdbmsInfoAccess.lua
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
    name= "RdbmsInfoAccess", 
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
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "Access", "{D150C038-494B-444F-9E96-4BF35E58762B}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "MS Access"
  rdbms.databaseObjectPackage= "db"

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverAccessJdbc(rdbms))

  rdbms.defaultDriver= rdbms.drivers[1]

  return grt.success(rdbms)
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get the Access driver
--
--   Helper function to return infos about the Jdbc driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverAccessJdbc(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "Access", 
    "{A8F2E8C2-415A-48C5-B8F8-95EE6E7D4FDB}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "MS Access"
  driver.description= "JDBC driver to connect to MS Access."

  -- Jdbc specific settings
  driver.className= "sun.jdbc.odbc.JdbcOdbcDriver"
  driver.connectionStringTemplate= "jdbc:odbc:Driver={Microsoft Access Driver (*.mdb)};DBQ=%databaseFile%;DriverID=22;READONLY=true;UID=%username%;PWD=%password%}"

  -- add driver parameters
  local param= __RdbmsInfo_lua.getDriverParameter(owner, "databaseFile", "Database File:", 
    "MS Access database file.", "file", 1, 218, "", 0, 1)
  param.paramTypeDetails=
    {
      fileType= "MS Access Files",
      fileExtension= "mdb",
      fileOpenDialogCaption, "Open MS Access File ..."
    }
  grtV.insert(driver.parameters, param)

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "username", "Username:", 
    "Name of the user to connect with.", "string", 2, 218, "", 0, 0))

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "password", "Password:", 
    "The user's password.", "password", 3, 218, "", 0, 0))

  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", -1, 218, "", 1, 0))

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringAccess",
      MigrationModule= "MigrationAccess",
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
