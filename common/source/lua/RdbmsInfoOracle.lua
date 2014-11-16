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
-- @file RdbmsInfoOracle.lua
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
    name= "RdbmsInfoOracle", 
    functions= {
      "getRdbmsInfo::"
    }, 
    extends= "RdbmsInfo"
  }

  return moduleInfo
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about Oracle
--
--   Returns a db.mgmt.Rdbms struct with infos about the rdbms
-- 
-- @return a new created db.mgmt.Rdbms GRT value struct 
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  local rdbmsMgmt= args[1]

  -- create Rdbms object
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "Oracle", "{B9E7D193-5761-461A-84DF-3D31A78D8546}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "Oracle Database Server"
  rdbms.databaseObjectPackage= "db.oracle"
  
  -- create simple datatypes for Rdbms
  createSimpleDatatypes(rdbmsMgmt, rdbms)

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverOracleJdbcSid(rdbms))
  grtV.insert(rdbms.drivers, getDriverOracleJdbcService(rdbms))

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
    
    -- NUMERIC
    dt= grtV.newObj("db.SimpleDatatype", "NUMBER", "{0DDAFF86-5345-4666-87CC-ECC3CB7EFFD7}", owner)
    dt.group= group
    dt.numericPrecision= 38
    dt.numericScale= 127
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- FLOAT
    dt= grtV.newObj("db.SimpleDatatype", "FLOAT", "{0DDAFF86-5345-4666-87CC-ECC3CB7EFFD8}", owner)
    dt.group= group
    dt.numericPrecision= 38
    dt.numericScale= 127
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- DECIMAL
    dt= grtV.newObj("db.SimpleDatatype", "DECIMAL", "{0DDAFF86-5345-4666-87CC-ECC3CB7EFFD9}", owner)
    dt.group= group
    dt.numericPrecision= 38
    dt.numericScale= 127
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  

  -- --------------------------------------------------------------------------------------
  -- string group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "string")
    
    -- VARCHAR2
    dt= grtV.newObj("db.SimpleDatatype", "VARCHAR2", "{473A5724-6E51-4244-82E4-799429743DAD}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(dt.synonyms, "VARCHAR")
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- NVARCHAR2
    dt= grtV.newObj("db.SimpleDatatype", "NVARCHAR2", "{3ED03390-6061-40F0-954A-9694EBB52565}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- CHAR
    dt= grtV.newObj("db.SimpleDatatype", "CHAR", "{A67FAEF0-DAEE-48B3-81E9-AA74C2E22373}", owner)
    dt.group= group
    dt.characterMaximumLength= 2000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NCHAR
    dt= grtV.newObj("db.SimpleDatatype", "NCHAR", "{3C93E288-11AC-457A-B9C4-F00E6280F8F5}", owner)
    dt.group= group
    dt.characterMaximumLength= 2000
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- text group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "text")
    
    -- LONG
    dt= grtV.newObj("db.SimpleDatatype", "LONG", "{216BAD31-8522-4903-B0D4-1CD7EABE1F53}", owner)
    dt.group= group
    dt.characterMaximumLength= -31
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- CLOB
    dt= grtV.newObj("db.SimpleDatatype", "CLOB", "{FA34E017-40AF-44C5-B0E1-F1472B03905D}", owner)
    dt.group= group
    dt.characterMaximumLength= -32
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- NCLOB
    dt= grtV.newObj("db.SimpleDatatype", "NCLOB", "{EDC7AFE6-1DCC-47C2-9B91-5FB5CF0C7C55}", owner)
    dt.group= group
    dt.characterMaximumLength= -32
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- blob group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "blob")
    
    -- RAW
    dt= grtV.newObj("db.SimpleDatatype", "RAW", "{8359B274-3054-4872-886E-41E34E6B5D9C}", owner)
    dt.group= group
    dt.characterMaximumLength= -31
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- LONG RAW
    dt= grtV.newObj("db.SimpleDatatype", "LONG RAW", "{E4A07B3B-A7A1-4862-8C5C-855C63F28CEA}", owner)
    dt.group= group
    dt.characterMaximumLength= -32
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- BLOB
    dt= grtV.newObj("db.SimpleDatatype", "BLOB", "{F9BCD294-15B7-4907-8ADA-193BFD503F0C}", owner)
    dt.group= group
    dt.characterMaximumLength= -32
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- datetime group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "datetime")
    
    -- DATE
    dt= grtV.newObj("db.SimpleDatatype", "DATE", "{5C88EF8B-6C6D-4482-8110-29D4297FAEE7}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- TIMESTAMP
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP", "{FC542742-9DCF-4D76-AED8-EAE9D14720F3}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- TIMESTAMP WITH TIME ZONE
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP WITH TIME ZONE", "{29BF9F26-AE7F-444F-9741-9A47C3CB29F9}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- TIMESTAMP WITH LOCAL TIME ZONE
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP WITH LOCAL TIME ZONE", "{2864AB40-D507-4F42-9D8C-AEC54AB6258E}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- INTERVAL YEAR TO MONTH
    dt= grtV.newObj("db.SimpleDatatype", "INTERVAL YEAR TO MONTH", "{1F1BEA1B-A354-40E1-B2BE-54A60AD73CB9}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(rdbms.simpleDatatypes, dt)

    -- INTERVAL DAY TO SECOND
    dt= grtV.newObj("db.SimpleDatatype", "INTERVAL DAY TO SECOND", "{EA59F083-A72C-4AB7-A599-8DBB06C52804}", owner)
    dt.group= group
    dt.dateTimePrecision= 9
    grtV.insert(rdbms.simpleDatatypes, dt)    
  end
  
  -- --------------------------------------------------------------------------------------
  -- various group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "various")
    
    -- BFILE
    dt= grtV.newObj("db.SimpleDatatype", "BFILE", "{628DA687-3D07-4142-8FB3-8682DEF51272}", owner)
    dt.group= group
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get the Oracle driver using Sid
--
--   Helper function to return infos about the Jdbc driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverOracleJdbcSid(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "OracleThinSid", 
    "{C72E3867-3115-41BE-9DCF-102DDB62093F}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "Oracle Thin JDBC Driver using SID"
  driver.description= "Oracle Thin JDBC driver to connect to Oracle 9i and Oracle 10g servers."

  driver.filesTarget= "./java/lib/"
  grtV.insert(driver.files, "ojdbc14.jar")
  driver.downloadUrl= "http://www.oracle.com/technology/software/tech/java/sqlj_jdbc/htdocs/jdbc101020.html"

  -- Jdbc specific settings
  driver.className= "oracle.jdbc.OracleDriver"
  driver.connectionStringTemplate= "jdbc:oracle:thin:%username%/%password%@%host%:%port%:%sid%"

  -- add driver parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "sid", "SID:", 
    "Oracle system identifier", "string", 1, 218, "", 0, 1))

  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 2, "1521")

  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", -1, 218, "", 1, 0))
  
  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringOracle",
      MigrationModule= "MigrationOracle",
      TransformationModule= "",
      QueryModule= "QueryOracle"
    }

  if grt.moduleExists("BaseJava") then
    driver.isInstalled= grt.getRes(BaseJava:javaClassExists(driver.className))
  else
    driver.isInstalled= false
  end

  return driver
end

-- ----------------------------------------------------------------------------------------
-- @brief Function to get the Oracle driver using Service
--
--   Helper function to return infos about the Jdbc driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverOracleJdbcService(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "OracleThinService", 
    "{212808F5-EE3A-44F1-A437-40CA6A9BD63A}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "Oracle Thin JDBC Driver using Service"
  driver.description= "Oracle Thin JDBC driver to connect to Oracle 9i and Oracle 10g servers."

  driver.filesTarget= "./java/lib/"
  grtV.insert(driver.files, "ojdbc14.jar")
  driver.downloadUrl= "http://www.oracle.com/technology/software/tech/java/sqlj_jdbc/htdocs/jdbc101020.html"

  -- Jdbc specific settings
  driver.className= "oracle.jdbc.OracleDriver"
  driver.connectionStringTemplate= "jdbc:oracle:thin:%username%/%password%@//%host%:%port%/%service%"

  -- add driver parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "service", "Service:", 
    "Oracle service name", "string", 1, 218, "", 0, 1))

  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 2, "1521")

  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", -1, 218, "", 1, 0))
  

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringOracle",
      MigrationModule= "MigrationOracle",
      TransformationModule= ""
    }

  if grt.moduleExists("BaseJava") then
    driver.isAccessable= true
    driver.isInstalled= grt.getRes(BaseJava:javaClassExists(driver.className))
  else
    driver.isAccessable= false
    driver.isInstalled= false
  end

  return driver
end
