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
-- @file RdbmsInfoMaxdb.lua
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
    name= "RdbmsInfoMaxdb", 
    functions= {
      "getRdbmsInfo::"
    }, 
    extends= "RdbmsInfo"
  }

  return moduleInfo
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about MaxDB
--
--   Returns a db.mgmt.Rdbms struct with infos about the rdbms
-- 
-- @return a new created db.mgmt.Rdbms GRT value struct 
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  local rdbmsMgmt= args[1]

  -- create Rdbms object
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "MaxDB", "{672A46A8-3CE4-419B-ABFF-AE5B61205C3D}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "MaxDB Database Server"
  rdbms.databaseObjectPackage= "db.maxdb"
  
  -- create simple datatypes for Rdbms
  createSimpleDatatypes(rdbmsMgmt, rdbms)

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverMaxdbJdbcSid(rdbms))
  

  rdbms.defaultDriver= rdbms.drivers[1]

  return grt.success(rdbms)
end


-- ----------------------------------------------------------------------------------------
-- @brief Builds the list of simple datatypes
--
--   Helper function to build the list of simple datatypes
-- 
-- @param rdbmsMgmt the Grt value of the Rdbms Management
-- @param rdbms the Grt value of the Rdbms
-- ----------------------------------------------------------------------------------------
function createSimpleDatatypes(rdbmsMgmt, rdbms)

  local dt
  local owner= grtV.toLua(rdbms._id)
  local group
  
  -- --------------------------------------------------------------------------------------
  -- numeric group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "numeric")
    
    -- SMALLINT
    dt= grtV.newObj("db.SimpleDatatype", "SMALLINT", "{F225951E-D381-43B5-BF9C-9218EAC65E1A}", owner)
    dt.group= group
    dt.numericPrecision= 5    
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- INT[EGER]
    dt= grtV.newObj("db.SimpleDatatype", "INTEGER", "{25E3CD6F-124E-49E9-A5AE-E3F44614D05C}", owner)
    dt.group= group
    dt.numericPrecision= 10
    grtV.insert(dt.synonyms, "INT")
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- FLOAT
    dt= grtV.newObj("db.SimpleDatatype", "FLOAT", "{996F49DC-C4BC-4C97-B1A1-68E43C384919}", owner)
    dt.group= group    
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- FIXED
    dt= grtV.newObj("db.SimpleDatatype", "FIXED", "{B7CFB3A7-1634-4930-B64C-EC5B2FFF5958}", owner)
    dt.group= group
    dt.numericPrecision= 38
    dt.numericScale= 37
    grtV.insert(rdbms.simpleDatatypes, dt)   
    
  end
  

  -- --------------------------------------------------------------------------------------
  -- string group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "string")
    
    -- VARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "VARCHAR", "{521413DC-D60D-4EDE-94D3-7915374E3265}", owner)
    dt.group= group
    dt.characterMaximumLength= 8000
    grtV.insert(dt.flags, "ASCII")
    grtV.insert(dt.flags, "BYTE")
    grtV.insert(dt.flags, "UNICODE")    
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- CHAR[ACTER]
    dt= grtV.newObj("db.SimpleDatatype", "CHAR", "{F980C4A0-983B-41D4-8D2F-1C3122F42203}", owner)
    dt.group= group
    dt.characterMaximumLength= 8000
    grtV.insert(dt.synonyms, "CHARACTER")
    grtV.insert(dt.flags, "ASCII")
    grtV.insert(dt.flags, "BYTE")
    grtV.insert(dt.flags, "UNICODE")    
    grtV.insert(rdbms.simpleDatatypes, dt)
    
  end
  
  -- --------------------------------------------------------------------------------------
  -- text group
  -- do
  --  group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "text")
  --  
  --  -- LONG
  --  dt= grtV.newObj("db.SimpleDatatype", "LONG", "{0207CD2C-0789-4241-90DB-7C54B3B1B8F3}", owner)
  --  dt.group= group
  --  dt.characterMaximumLength= -32
  --  grtV.insert(dt.flags, "ASCII")    
  --  grtV.insert(dt.flags, "UNICODE")        
  --  grtV.insert(rdbms.simpleDatatypes, dt)
  --      
  -- end
  
  -- --------------------------------------------------------------------------------------
  -- blob group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "blob")
    
    -- LONG
    dt= grtV.newObj("db.SimpleDatatype", "LONG", "{5161C414-9EBE-4FC8-878E-1C48066D665A}", owner)
    dt.group= group
    dt.characterMaximumLength= -32
    grtV.insert(dt.flags, "ASCII")
    grtV.insert(dt.flags, "BYTE")
    grtV.insert(dt.flags, "UNICODE")        
    grtV.insert(rdbms.simpleDatatypes, dt)
    
  end
  
  -- --------------------------------------------------------------------------------------
  -- datetime group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "datetime")
    
    -- DATE
    dt= grtV.newObj("db.SimpleDatatype", "DATE", "{560898D6-776B-477F-901E-201DC5875812}", owner)
    dt.group= group
    dt.dateTimePrecision= 3
    grtV.insert(dt.flags, "EUR")
    grtV.insert(dt.flags, "INTERNAL")
    grtV.insert(dt.flags, "ISO")  
    grtV.insert(dt.flags, "JIS")
    grtV.insert(dt.flags, "USA")
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- TIME
    dt= grtV.newObj("db.SimpleDatatype", "TIME", "{CED28F00-9B19-47A6-9B2E-012C3F4C2F78}", owner)
    dt.group= group
    dt.dateTimePrecision= 3
    grtV.insert(dt.flags, "EUR")
    grtV.insert(dt.flags, "INTERNAL")
    grtV.insert(dt.flags, "ISO")  
    grtV.insert(dt.flags, "JIS")
    grtV.insert(dt.flags, "USA")
    grtV.insert(rdbms.simpleDatatypes, dt)
        
    -- TIMESTAMP
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP", "{E4CDFE32-E9D0-4D7D-98A3-EAEC9E04DAF0}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(dt.flags, "EUR")
    grtV.insert(dt.flags, "INTERNAL")
    grtV.insert(dt.flags, "ISO")  
    grtV.insert(dt.flags, "JIS")
    grtV.insert(dt.flags, "USA")
    grtV.insert(rdbms.simpleDatatypes, dt)

  end
  
  -- --------------------------------------------------------------------------------------
  
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get the MaxDB driver using Sid
--
--   Helper function to return infos about the Jdbc driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverMaxdbJdbcSid(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "MaxDB JDBC", 
    "{9FAE1CD7-8B0E-47E5-81E9-6A311AA89896}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "MaxDB JDBC Driver"
  driver.description= "MaxDB JDBC Driver to connect to MaxDB 7.5 and newer."

  driver.filesTarget= "./java/lib/"
  grtV.insert(driver.files, "sapdbc-7_6_00_12_4339.jar")
  driver.downloadUrl= "http://www.mysql.com/products/maxdb/"

  -- Jdbc specific settings
  driver.className= "com.sap.dbtech.jdbc.DriverSapDB"
  driver.connectionStringTemplate= "jdbc:sapdb://%host%:%port%/%instance%?user=%username%&password=%password%&sqlmode=%sqlmode%&cachelimit=%cachelimit%&timeout=%timeout%&isolation=%isolation%&autocommit=%autocommit%&reconnect=%reconnect%&cache=%cache%"

  -- add driver parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "instance", "Instance:", 
    "MaxDB Instance", "string", 1, 218, "", 0, 1))

  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 2, "7210")

  -- advanced parameters
  
-- TODO: check which of these are actually needed
  
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "sqlmode", "SQL mode", 
    "SQL mode. Possible values are ORACLE | INTERNAL.", "string", -1, 218, "INTERNAL", 1, 0))
    
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "cachelimit", "Cache limit", 
    "Cache limit of the connection.", "int", -1, 218, "32", 1, 0))

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "timeout", "Command timeout", 
    "Command timeout of the connection in seconds.", "int", -1, 218, "0", 1, 0))

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "isolation", "Isolation level", 
    "Isolation level of the connection. One of: TRANSACTION_READ_UNCOMMITTED, TRANSACTION_READ_COMMITTED, TRANSACTION_REPEATABLE_READ, TRANSACTION_SERIALIZABLE.", "string", -1, 218, "TRANSACTION_SERIALIZABLE", 1, 0))
    
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "autocommit", "Autocommit mode", 
    "Possible values are: on, off. on: A COMMIT is performed after every command. off: Transactions must be controlled with the methods commit() and rollback().", "string", -1, 218, "on", 1, 0))
    
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "reconnect", "Reconnect mode", 
    "Possible values are: on, off. on: The system automatically reconnects to the database instance after a command timeout. off: There is no automatic new connection.,", "string", -1, 218, "on", 1, 0))
    
--  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "trace", "Trace file", 
--    "Name and location of a debug trace file", "string", -1, 218, "", 1, 0))
    
--  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "tracesize", "Trace size limit", 
--    "Maximum number of lines in the file for the debug output. If this number is exceeded, the content of the file is overwritten cyclically", "int", -1, 218, "", 1, 0))
    
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "cache", "Cache size", 
    "Enables caching of some prepared statement informations. Possible value: all or a combinations of s,i,u and d.", "string", -1, 218, "all", 1, 0))
    
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "cachelimit", "Cache limit", 
    "Cache limit of the connection", "int", -1, 218, "32", 1, 0))
    
 -- TODO: unicode flag missing    
    
  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringMaxdb",
      MigrationModule= "MigrationMaxdb",
      TransformationModule= ""
    }

  if grt.moduleExists("BaseJava") then
    driver.isInstalled= grt.getRes(BaseJava:javaClassExists(driver.className))
  else
    driver.isInstalled= false
  end

  return driver
end

