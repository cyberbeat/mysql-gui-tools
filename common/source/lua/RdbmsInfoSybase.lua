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
-- @file RdbmsInfoSybase.lua
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
    name= "RdbmsInfoSybase", 
    functions= {
      "getRdbmsInfo::"
    }, 
    extends= "RdbmsInfo"
  }

  return moduleInfo
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about Sybase
--
--   Returns a db.mgmt.Rdbms struct with infos about the rdbms
-- 
-- @return a new created db.mgmt.Rdbms GRT value struct 
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  local rdbmsMgmt= args[1]

  -- create Rdbms object
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "Sybase", "{C6D9F3AA-88F1-4460-860B-CF39A8F35F99}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "Sybase Server"
  rdbms.databaseObjectPackage= "db.Sybase"
  
  -- create simple datatypes for Rdbms
  createSimpleDatatypes(rdbmsMgmt, rdbms)

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverSybaseJdbc(rdbms))

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
    dt= grtV.newObj("db.SimpleDatatype", "DECIMAL", "{2BD15BF8-4454-4B23-9A49-7DE3371FC18C}", owner)
    dt.group= group
    dt.numericPrecision= 28
    dt.numericScale= 28
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NUMERIC
    dt= grtV.newObj("db.SimpleDatatype", "NUMERIC", "{376FBA25-32E8-421E-AB10-A0B13E07E2D1}", owner)
    dt.group= group
    dt.numericPrecision= 28
    dt.numericScale= 28
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- FLOAT
    dt= grtV.newObj("db.SimpleDatatype", "FLOAT", "{207E2F97-3239-4E18-8EAE-AD0D45306D0E}", owner)
    dt.group= group
    dt.numericPrecision= 15
    dt.numericScale= 15
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- DOUBLE PRECISION
    dt= grtV.newObj("db.SimpleDatatype", "DOUBLE PRECISION", "{7EB20593-A000-49D9-AB64-0869B15A76E9}", owner)
    dt.group= group
    dt.numericPrecision= 15
    dt.numericScale= 15
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- REAL
    dt= grtV.newObj("db.SimpleDatatype", "REAL", "{DFDE15C4-2D56-4B0E-92FB-85B8B573FCFE}", owner)
    dt.group= group
    dt.numericPrecision= 7
    dt.numericScale= 7
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- TINYINT
    dt= grtV.newObj("db.SimpleDatatype", "TINYINT", "{15DC5698-9288-49BB-9913-84F79F150D29}", owner)
    dt.group= group
    dt.numericPrecision= 3
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- SMALLINT
    dt= grtV.newObj("db.SimpleDatatype", "SMALLINT", "{7738F811-6B03-4B38-A47B-3B903207DF0E}", owner)
    dt.group= group
    dt.numericPrecision= 5
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- INT
    dt= grtV.newObj("db.SimpleDatatype", "INT", "{858DBBD8-6F0B-4ED3-AC1B-609678C22329}", owner)
    dt.group= group
    dt.numericPrecision= 10
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- BIGINT
    dt= grtV.newObj("db.SimpleDatatype", "BIGINT", "{9B48DF5A-7A4B-49D1-8D0F-9FF7E109FA2A}", owner)
    dt.group= group
    dt.numericPrecision= 19
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- string group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "string")
  
    -- CHAR
    dt= grtV.newObj("db.SimpleDatatype", "CHAR", "{78A362CD-7D20-461C-ADF2-8B8F0AAAB6C9}", owner)
    dt.group= group
    dt.characterMaximumLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- VARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "VARCHAR", "{D90B8F91-9738-433C-B0A0-F034FAD99BA4}", owner)
    dt.group= group
    dt.characterMaximumLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NCHAR
    dt= grtV.newObj("db.SimpleDatatype", "NCHAR", "{FD1C8F51-7518-4869-8071-8C1EACBECE7B}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NVARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "NVARCHAR", "{82034ACE-ABB0-4B59-8EEE-19AC83CE91A4}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- UNICHAR
    dt= grtV.newObj("db.SimpleDatatype", "UNICHAR", "{CB1E3BA3-E063-4293-8FD8-41D8F6CBB3A8}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- UNIVARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "UNIVARCHAR", "{34365D88-7539-4D30-A36C-38CD8409FC17}", owner)
    dt.group= group
    dt.characterMaximumLength= 4000
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- text group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "text")
  
    -- TEXT
    dt= grtV.newObj("db.SimpleDatatype", "TEXT", "{48258B17-E097-485E-804D-283D3134581F}", owner)
    dt.group= group
    dt.characterMaximumLength= -31
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- NTEXT
    dt= grtV.newObj("db.SimpleDatatype", "NTEXT", "{C1E49C98-7D35-41D5-9E49-8394D5D878E0}", owner)
    dt.group= group
    dt.characterMaximumLength= -30
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- blob group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "blob")
  
    -- IMAGE
    dt= grtV.newObj("db.SimpleDatatype", "IMAGE", "{47DBC71D-4442-46EE-8ED0-A963CD562924}", owner)
    dt.group= group
    dt.characterOctetLength= -31
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- BINARY
    dt= grtV.newObj("db.SimpleDatatype", "BINARY", "{968B1592-C662-4AFA-AF5F-82335D3B277B}", owner)
    dt.group= group
    dt.characterOctetLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- VARBINARY
    dt= grtV.newObj("db.SimpleDatatype", "VARBINARY", "{FE2DAE89-8D04-4B6F-9880-B1505F40906C}", owner)
    dt.group= group
    dt.characterOctetLength= 8000
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- datetime group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "datetime")
  
    -- DATETIME
    dt= grtV.newObj("db.SimpleDatatype", "DATETIME", "{C2EBE339-AEC9-4B0F-B9EC-00AF7256B532}", owner)
    dt.group= group
    dt.dateTimePrecision= 8
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- SMALLDATETIME
    dt= grtV.newObj("db.SimpleDatatype", "SMALLDATETIME", "{33F4BF4E-A939-406D-AB61-683AA93E06C0}", owner)
    dt.group= group
    dt.dateTimePrecision= 6
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- TIMESTAMP
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP", "{17511841-0224-4891-887E-66568F91E6E1}", owner)
    dt.group= group
    dt.dateTimePrecision= 8
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- DATE
    dt= grtV.newObj("db.SimpleDatatype", "DATE", "{38B2A199-2326-43B1-AC53-00A4489D2DEB}", owner)
    dt.group= group
    dt.dateTimePrecision= 4
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- TIME
    dt= grtV.newObj("db.SimpleDatatype", "TIME", "{861B0C5C-BB3C-43C5-9C4B-8A41E0101B04}", owner)
    dt.group= group
    dt.dateTimePrecision= 4
    grtV.insert(rdbms.simpleDatatypes, dt)
  end
  
  -- --------------------------------------------------------------------------------------
  -- various group
  do
    group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(rdbmsMgmt, "various")
    
    -- BIT
    dt= grtV.newObj("db.SimpleDatatype", "BIT", "{E9120415-E057-4AEC-8953-B0740CE70CAE}", owner)
    dt.group= group
    dt.numericPrecision= 1
    grtV.insert(rdbms.simpleDatatypes, dt)
    
    -- MONEY
    dt= grtV.newObj("db.SimpleDatatype", "MONEY", "{F7A37FF5-F7DE-4698-A0EF-5D5361AAB8C2}", owner)
    dt.group= group
    dt.numericPrecision= 19
    dt.numericScale= 4
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- SMALLMONEY
    dt= grtV.newObj("db.SimpleDatatype", "SMALLMONEY", "{F04113AB-FDD7-4B9A-8998-F5C54518455C}", owner)
    dt.group= group
    dt.numericPrecision= 10
    dt.numericScale= 4
    grtV.insert(rdbms.simpleDatatypes, dt)
  
    -- UNIQUEIDENTIFIER
    dt= grtV.newObj("db.SimpleDatatype", "UNIQUEIDENTIFIER", "{88E3F35F-1131-4618-B55E-FACE13E5E393}", owner)
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
function getDriverSybaseJdbc(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "jTDSsybase", 
    "{7A1DAC30-41C8-4E71-B489-CECCDA8E26C6}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "Sybase JDBC Driver"
  driver.description= "JDBC driver to connect to Sybase."

  driver.filesTarget= "./java/lib/"
  grtV.insert(driver.files, "jtds-1.2.jar")
  driver.downloadUrl= "http://sourceforge.net/project/showfiles.php?group_id=33291"

  -- Jdbc specific settings
  driver.className= "net.sourceforge.jtds.jdbc.Driver"
  driver.connectionStringTemplate= "jdbc:jtds:sybase://%host%:%port%/%database%;user=%username%;password=%password%" ..
    ";charset=utf-8"

  -- add driver parameters
  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 1, "5000")

  local param= __RdbmsInfo_lua.getDriverParameter(owner, "database", "Database:", 
    "Name of the database.", "string", -1, 218, "", 0, 1)
  param.lookupValueModule= "ReverseEngineeringSybase"
  param.lookupValueMethod= "getCatalogs"
  grtV.insert(driver.parameters, param)


  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", -1, 218, "", 1, 0))
  

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringSybase",
      MigrationModule= "MigrationSybase",
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
