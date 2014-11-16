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
-- @file RdbmsInfoMssql.lua
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
    name= "RdbmsInfoMssql", 
    functions= {
      "getRdbmsInfo::"
    }, 
    extends= "RdbmsInfo"
  }

  return moduleInfo
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about Mssql
--
--   Returns a db.mgmt.Rdbms struct with infos about the rdbms
-- 
-- @return a new created db.mgmt.Rdbms GRT value struct 
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  local rdbmsMgmt= args[1]

  -- create Rdbms object
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "Mssql", "{C31762DD-0C04-45A0-8030-C05A2382E004}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "MS SQL Server"
  rdbms.databaseObjectPackage= "db.mssql"
  
  -- create simple datatypes for Rdbms
  createSimpleDatatypes(rdbmsMgmt, rdbms)

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverMssqlJdbc(rdbms))

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
  
    -- DECIMAL
    dt= grtV.newObj("db.SimpleDatatype", "DECIMAL", "{5D9C607E-6CB2-4F5B-AB11-40AAC9AC35E2}", owner)
    dt.group= group
    dt.numericPrecision= 28
    dt.numericScale= 28
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NUMERIC
    dt= grtV.newObj("db.SimpleDatatype", "NUMERIC", "{76446D77-57AA-48FC-AB86-D81B23D32D95}", owner)
    dt.group= group
    dt.numericPrecision= 28
    dt.numericScale= 28
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- FLOAT
    dt= grtV.newObj("db.SimpleDatatype", "FLOAT", "{1A50E390-1E17-4DBA-8781-626D629564CA}", owner)
    dt.group= group
    dt.numericPrecision= 15
    dt.numericScale= 15
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- REAL
    dt= grtV.newObj("db.SimpleDatatype", "REAL", "{16470D1F-C339-4235-9764-61C17A9A6E5B}", owner)
    dt.group= group
    dt.numericPrecision= 7
    dt.numericScale= 7
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- TINYINT
    dt= grtV.newObj("db.SimpleDatatype", "TINYINT", "{4C220187-EBAA-458F-81FC-B22656B0F1B6}", owner)
    dt.group= group
    dt.numericPrecision= 3
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- SMALLINT
    dt= grtV.newObj("db.SimpleDatatype", "SMALLINT", "{A3DD29A8-7FAF-42B5-A61B-D47BE005A31E}", owner)
    dt.group= group
    dt.numericPrecision= 5
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- INT
    dt= grtV.newObj("db.SimpleDatatype", "INT", "{3CA215AC-B2D1-4539-BF42-DF184FD5BA7C}", owner)
    dt.group= group
    dt.numericPrecision= 10
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- BIGINT
    dt= grtV.newObj("db.SimpleDatatype", "BIGINT", "{27E9AFDB-1798-48A4-85B7-6F1B26ED7C08}", owner)
    dt.group= group
    dt.numericPrecision= 19
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- string group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "string")
  
    -- CHAR
    dt= grtV.newObj("db.SimpleDatatype", "CHAR", "{DBA4AF53-EA75-4FEA-9602-955754FD6C5E}", owner)
    dt.group= group
    dt.characterMaximumLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- VARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "VARCHAR", "{A4D27A90-0803-4CCE-BEAA-E2A60B075017}", owner)
    dt.group= group
    dt.characterMaximumLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NCHAR
    dt= grtV.newObj("db.SimpleDatatype", "NCHAR", "{962B8C30-4F84-477B-940D-D8245F8E57FB}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NVARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "NVARCHAR", "{0A860931-EFDD-4FAF-A00C-D7FAC4F76856}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- text group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "text")
  
    -- TEXT
    dt= grtV.newObj("db.SimpleDatatype", "TEXT", "{83B6CCD8-9CB2-444A-BAAD-455F5DBC3B73}", owner)
    dt.group= group
    dt.characterMaximumLength= -31
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NTEXT
    dt= grtV.newObj("db.SimpleDatatype", "NTEXT", "{73903640-5E47-4182-87FF-C5A69372F0E6}", owner)
    dt.group= group
    dt.characterMaximumLength= -30
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- blob group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "blob")
  
    -- IMAGE
    dt= grtV.newObj("db.SimpleDatatype", "IMAGE", "{35366D07-7CED-450A-BC85-A26372731166}", owner)
    dt.group= group
    dt.characterOctetLength= -31
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- BINARY
    dt= grtV.newObj("db.SimpleDatatype", "BINARY", "{08177DE8-36A1-4BCA-9340-AF57A0E635E9}", owner)
    dt.group= group
    dt.characterOctetLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- VARBINARY
    dt= grtV.newObj("db.SimpleDatatype", "VARBINARY", "{F5A21752-AAF4-46A9-A9F0-3B117CA1CA56}", owner)
    dt.group= group
    dt.characterOctetLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- datetime group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "datetime")
  
    -- DATETIME
    dt= grtV.newObj("db.SimpleDatatype", "DATETIME", "{F1DA3FBD-C75A-4B05-AFF8-F897F3B82739}", owner)
    dt.group= group
    dt.dateTimePrecision= 8
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- SMALLDATETIME
    dt= grtV.newObj("db.SimpleDatatype", "SMALLDATETIME", "{757F5494-3FC5-4DB5-B844-C1F06C952279}", owner)
    dt.group= group
    dt.dateTimePrecision= 6
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- TIMESTAMP
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP", "{C593D7F9-F047-4142-9D6D-BA55C6BEB4A5}", owner)
    dt.group= group
    dt.dateTimePrecision= 8
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- various group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "various")
    
    -- BIT
    dt= grtV.newObj("db.SimpleDatatype", "BIT", "{64B57C32-49FE-47B4-A006-E2E68B948FE8}", owner)
    dt.group= group
    dt.numericPrecision= 1
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- MONEY
    dt= grtV.newObj("db.SimpleDatatype", "MONEY", "{73580086-E192-4621-ACA9-8FC435833325}", owner)
    dt.group= group
    dt.numericPrecision= 19
    dt.numericScale= 4
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- SMALLMONEY
    dt= grtV.newObj("db.SimpleDatatype", "SMALLMONEY", "{3D4F9D59-C34B-4D99-AAC8-38FF348B237B}", owner)
    dt.group= group
    dt.numericPrecision= 10
    dt.numericScale= 4
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- UNIQUEIDENTIFIER
    dt= grtV.newObj("db.SimpleDatatype", "UNIQUEIDENTIFIER", "{64C9B531-9E3C-4667-97A7-6992E395263E}", owner)
    dt.group= group
    dt.characterOctetLength= 16
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
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
function getDriverMssqlJdbc(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "jTDS102", 
    "{9437F83E-FF9E-4831-9132-B4502303C229}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "MS SQL JDBC Driver"
  driver.description= "JDBC driver to connect to MS SQL Server 2000 and 2005."

  driver.filesTarget= "./java/lib/"
  grtV.insert(driver.files, "jtds-1.2.jar")
  driver.downloadUrl= "http://sourceforge.net/project/showfiles.php?group_id=33291"

  -- Jdbc specific settings
  driver.className= "net.sourceforge.jtds.jdbc.Driver"
  driver.connectionStringTemplate= "jdbc:jtds:sqlserver://%host%:%port%/%database%;user=%username%;password=%password%" ..
    ";charset=utf-8;domain=%domain%"

  -- add driver parameters
  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 1, "1433")

  local param= __RdbmsInfo_lua.getDriverParameter(owner, "database", "Database:", 
    "Name of the database, e.g. Northwind.", "string", -1, 218, "", 0, 1)
  param.lookupValueModule= "ReverseEngineeringMssql"
  param.lookupValueMethod= "getCatalogs"
  grtV.insert(driver.parameters, param)

  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "domain", "Domain:", 
    "If specificed, NTLM authentication is used.", "string", -1, 218, "", 0, 0))

  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", -1, 218, "", 1, 0))
  

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringMssql",
      MigrationModule= "MigrationMssql",
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
