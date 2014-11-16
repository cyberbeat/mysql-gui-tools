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
-- @file RdbmsInfoMysql.lua
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
    name= "RdbmsInfoMysql", 
    functions= {
      "getRdbmsInfo::",
      "getSimpleDatatypes::",
      "getCharacterSets::"
    }, 
    extends= "RdbmsInfo"
  }

  return moduleInfo
end

-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about MySQL
--
--   Creates a db.mgmt.Rdbms structed GRT value and adds all simple datatypes and 
-- drivers
-- 
-- @param owner a GRT object
--
-- @return a new created GRT value of struct "db.mgmt.Rdbms" containing the drivers infos
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  local rdbmsMgmt= args[1]

  -- create Rdbms object
  local rdbms= grtV.newObj("db.mgmt.Rdbms", "Mysql", "{6D75781B-52CF-4252-9B3D-9F28B75C09F7}", grtV.toLua(rdbmsMgmt._id))
  rdbms.caption= "MySQL Server"
  rdbms.databaseObjectPackage= "db.mysql"

  -- create simple datatypes for Rdbms
  createSimpleDatatypes(rdbms.simpleDatatypes, rdbmsMgmt)
  
  -- create character sets for Rdbms
  createCharacterSets(rdbms.characterSets, rdbmsMgmt)

  -- add driver to the Rdbms' list of drivers
  grtV.insert(rdbms.drivers, getDriverMysqlNative(rdbms))
  grtV.insert(rdbms.drivers, getDriverMysqlJdbc(rdbms))
  grtV.insert(rdbms.drivers, getDriverMysqlEmbedded(rdbms))

  rdbms.defaultDriver= rdbms.drivers[1]
  
  return grt.success(rdbms)
end


-- ----------------------------------------------------------------------------------------
-- @brief Adds all simple datatypes to a list
--
--   A function that adds all datatypes to the given list
--
-- @param datatypeList a GRT list that will hold all the datatypes
-- @param owner a GRT object
-- ----------------------------------------------------------------------------------------
function getSimpleDatatypes(args)
  if grtV.getn(args) == 2 then
    createSimpleDatatypes(args[1], args[2])
  else
    createSimpleDatatypes(args[1], nil)
  end
  
  return grt.success()
end

-- ----------------------------------------------------------------------------------------
-- @brief Adds all character sets to a list
--
--   A function that adds all character sets to the given list
--
-- @param characterSetList a GRT list that will hold all the character sets
-- @param owner a GRT object
-- ----------------------------------------------------------------------------------------
function getCharacterSets(args)
  if grtV.getn(args) == 2 then
    createCharacterSets(args[1], args[2])
  else
    createCharacterSets(args[1], nil)
  end
  
  return grt.success()
end


-- ----------------------------------------------------------------------------------------
-- @brief Builds the list of simple datatypes
--
--   Helper function to build the list of simple datatypes
-- 
-- @param datatypeList the list that will hold the datatypes
-- @param owner the owner, if any
-- ----------------------------------------------------------------------------------------
function createSimpleDatatypes(datatypeList, owner)

  local dt
  local group
  local owner_id
  
  if owner ~= nil then
    owner_id= grtV.toLua(owner._id)
  else
    -- if the function is called without and rdbmsMgmt then assign all simpleDatatypes to the group userdefined
    owner_id= ""
    
    -- Userdefined group
    group= grtV.newObj("db.DatatypeGroup", "userdefined", "{3D44C889-03BA-4783-A02D-ACDF4943257E}", "")
    group.caption= "Userdefined Types"
    group.description= "Datatypes defined by a user"
  end

  -- --------------------------------------------------------------------------------------
  -- numeric group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "numeric")
    end
  
    -- TINYINT
    dt= grtV.newObj("db.SimpleDatatype", "TINYINT", "{3C75C19F-FCA4-44DE-8881-F477FEA32E62}", owner_id)
    dt.group= group
    dt.numericPrecision= 3
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  
    -- SMALLINT
    dt= grtV.newObj("db.SimpleDatatype", "SMALLINT", "{B98335A2-603F-4042-9D59-88248D987309}", owner_id)
    dt.group= group
    dt.numericPrecision= 5
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  
    -- MEDIUMINT
    dt= grtV.newObj("db.SimpleDatatype", "MEDIUMINT", "{A5A2A320-DAAD-416E-862C-FAA9AD90DF8C}", owner_id)
    dt.group= group
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    dt.numericPrecision= 8
    grtV.insert(datatypeList, dt)
  
    -- INT
    dt= grtV.newObj("db.SimpleDatatype", "INT", "{014AB6FC-CA8A-4FA2-8BD9-E97D7F8D53AE}", owner_id)
    dt.group= group
    dt.numericPrecision= 10
    grtV.insert(dt.synonyms, "INTEGER")
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  
    -- BIGINT
    dt= grtV.newObj("db.SimpleDatatype", "BIGINT", "{B96B522A-48FE-431F-A0B4-D5E1B733FE3F}", owner_id)
    dt.group= group
    dt.numericPrecision= 20
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  
    -- FLOAT
    dt= grtV.newObj("db.SimpleDatatype", "FLOAT", "{BEA67203-20CC-4A53-A4EC-B2F31E84DE1E}", owner_id)
    dt.group= group
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  
    -- DOUBLE
    dt= grtV.newObj("db.SimpleDatatype", "DOUBLE", "{3873BD96-9C60-4BB0-90B4-45F4C76485D9}", owner_id)
    dt.group= group
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  
    -- DECIMAL
    dt= grtV.newObj("db.SimpleDatatype", "DECIMAL", "{8CB74E5F-921A-4F6E-95C6-3AD94F2BB36F}", owner_id)
    dt.group= group
    dt.numericPrecision= 64
    dt.numericScale= 30
    grtV.insert(dt.flags, "UNSIGNED")
    grtV.insert(dt.flags, "ZEROFILL")
    grtV.insert(datatypeList, dt)
  end

  -- --------------------------------------------------------------------------------------
  -- string group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "string")
    end
  
    -- CHAR
    dt= grtV.newObj("db.SimpleDatatype", "CHAR", "{4B160881-85B7-4C25-BBD1-36C9CA1A0E80}", owner_id)
    dt.group= group
    dt.characterMaximumLength= 255
    grtV.insert(dt.flags, "BINARY")
    grtV.insert(dt.flags, "UNICODE")
    grtV.insert(dt.flags, "ASCII")
    grtV.insert(datatypeList, dt)
  
    -- VARCHAR
    dt= grtV.newObj("db.SimpleDatatype", "VARCHAR", "{7198A7BD-968D-4BD9-B85C-4DE7504F0D83}", owner_id)
    dt.group= group
    dt.characterMaximumLength= 65535
    grtV.insert(dt.flags, "BINARY")
    grtV.insert(datatypeList, dt)
  
    -- BINARY
    dt= grtV.newObj("db.SimpleDatatype", "BINARY", "{4159AB8E-78E7-41E7-AAFD-0DE3766C2935}", owner_id)
    dt.group= group
    dt.characterOctetLength= 255
    grtV.insert(datatypeList, dt)
  
    -- VARBINARY
    dt= grtV.newObj("db.SimpleDatatype", "VARBINARY", "{CD41C6AA-4BAC-42E8-A4FC-655CAE3534D0}", owner_id)
    dt.group= group
    dt.characterOctetLength= 65535
    grtV.insert(datatypeList, dt)
  end

  -- --------------------------------------------------------------------------------------
  -- text group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "text")
    end
  
    -- TINYTEXT
    dt= grtV.newObj("db.SimpleDatatype", "TINYTEXT", "{B965148B-8E5B-46E9-999C-252D92B37B2C}", owner_id)
    dt.group= group
    dt.characterMaximumLength= 255
    grtV.insert(dt.flags, "BINARY")
    grtV.insert(datatypeList, dt)
  
    -- TEXT
    dt= grtV.newObj("db.SimpleDatatype", "TEXT", "{52734DB8-D81D-4593-A74C-891967DFAC63}", owner_id)
    dt.group= group
    dt.characterMaximumLength= 65535
    grtV.insert(dt.flags, "BINARY")
    grtV.insert(datatypeList, dt)
  
    -- MEDIUMTEXT
    dt= grtV.newObj("db.SimpleDatatype", "MEDIUMTEXT", "{7874D9C1-0F91-487C-822C-3ABC407452BE}", owner_id)
    dt.group= group
    dt.characterMaximumLength= -24
    grtV.insert(dt.flags, "BINARY")
    grtV.insert(datatypeList, dt)
  
    -- LONGTEXT
    dt= grtV.newObj("db.SimpleDatatype", "LONGTEXT", "{BFDB5048-852A-4985-8772-5123FB7C23FE}", owner_id)
    dt.group= group
    dt.characterMaximumLength= -32
    grtV.insert(dt.flags, "BINARY")
    grtV.insert(datatypeList, dt)
  end

  -- --------------------------------------------------------------------------------------
  -- blob group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "blob")
    end
  
    -- TINYBLOB
    dt= grtV.newObj("db.SimpleDatatype", "TINYBLOB", "{D2A508F3-9636-400D-816E-68E0A172204C}", owner_id)
    dt.group= group
    dt.characterOctetLength= 255
    grtV.insert(datatypeList, dt)
  
    -- BLOB
    dt= grtV.newObj("db.SimpleDatatype", "BLOB", "{6A28850F-C54C-4722-80A0-6187798B35C0}", owner_id)
    dt.group= group
    dt.characterOctetLength= 65535
    grtV.insert(datatypeList, dt)
  
    -- MEDIUMBLOB
    dt= grtV.newObj("db.SimpleDatatype", "MEDIUMBLOB", "{EBCCD9D0-1497-4EEE-97FC-0C96DB3D2EBE}", owner_id)
    dt.group= group
    dt.characterOctetLength= -24
    grtV.insert(datatypeList, dt)
  
    -- LONGBLOB
    dt= grtV.newObj("db.SimpleDatatype", "LONGBLOB", "{6C2E3841-EA8D-48E1-A727-B0B2E71857BF}", owner_id)
    dt.group= group
    dt.characterOctetLength= -32
    grtV.insert(datatypeList, dt)
  end

  -- --------------------------------------------------------------------------------------
  -- datetime group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "datetime")
    end
  
    -- DATETIME
    dt= grtV.newObj("db.SimpleDatatype", "DATETIME", "{9A2399C8-F3EA-4B6C-AAB1-830D3BFF164B}", owner_id)
    dt.group= group
    dt.dateTimePrecision= 8
    grtV.insert(datatypeList, dt)
  
    -- DATE
    dt= grtV.newObj("db.SimpleDatatype", "DATE", "{E3ACD2FE-A26F-4BB8-ADA6-433F92B0570D}", owner_id)
    dt.group= group
    dt.dateTimePrecision= 3
    grtV.insert(datatypeList, dt)
  
    -- TIME
    dt= grtV.newObj("db.SimpleDatatype", "TIME", "{65642303-376E-4C4A-B200-7E0DFAA47344}", owner_id)
    dt.group= group
    dt.dateTimePrecision= 3
    grtV.insert(datatypeList, dt)
  
    -- YEAR
    dt= grtV.newObj("db.SimpleDatatype", "YEAR", "{868AC4F9-F4BB-44AD-8512-4EBAD5E5D661}", owner_id)
    dt.group= group
    dt.dateTimePrecision= 1
    grtV.insert(datatypeList, dt)
  
    -- TIMESTAMP
    dt= grtV.newObj("db.SimpleDatatype", "TIMESTAMP", "{D8A1FD40-794F-464E-8E4A-E14C38D9C924}", owner_id)
    dt.group= group
    dt.dateTimePrecision= 4
    grtV.insert(datatypeList, dt)
  end

  -- --------------------------------------------------------------------------------------
  -- gis group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "gis")
    end
  
    -- GEOMETRY
    dt= grtV.newObj("db.SimpleDatatype", "GEOMETRY", "{DC0345B1-52D4-497C-AFA1-17A00BCAA7FE}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- LINESTRING
    dt= grtV.newObj("db.SimpleDatatype", "LINESTRING", "{739D4BC4-0188-46BB-84AB-DB97CE01B67F}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- POLYGON
    dt= grtV.newObj("db.SimpleDatatype", "POLYGON", "{50C760E2-97A4-4BB0-ABA5-E0A2D11E683D}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- MULTIPOINT
    dt= grtV.newObj("db.SimpleDatatype", "MULTIPOINT", "{86A060EB-F367-4890-8FFA-00840F281791}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- MULTILINESTRING
    dt= grtV.newObj("db.SimpleDatatype", "MULTILINESTRING", "{04006513-2EA3-4785-A541-8C25E68E5E0A}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- MULTIPOLYGON
    dt= grtV.newObj("db.SimpleDatatype", "MULTIPOLYGON", "{1D1FE4C9-6A0B-423C-A048-8830C430A93E}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- GEOMETRYCOLLECTION
    dt= grtV.newObj("db.SimpleDatatype", "GEOMETRYCOLLECTION", "{E1609E12-9B2A-4EF7-9265-00D6D3BE0026}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  end

  -- --------------------------------------------------------------------------------------
  -- various group
  do
    if owner ~= nil then
      group= __RdbmsManagement_lua.getDatatypeGroupByNameLua(owner, "various")
    end
  
    -- BIT
    dt= grtV.newObj("db.SimpleDatatype", "BIT", "{855F4FED-94F7-4E28-812C-6882B28571AF}", owner_id)
    dt.group= group
    dt.numericPrecision= 2
    grtV.insert(datatypeList, dt)
  
    -- BOOLEAN
    dt= grtV.newObj("db.SimpleDatatype", "BOOLEAN", "{F496575F-5F75-4825-A059-7DFC75DB3DFD}", owner_id)
    dt.group= group
    dt.numericPrecision= 1
    grtV.insert(datatypeList, dt)
  
    -- ENUM
    dt= grtV.newObj("db.SimpleDatatype", "ENUM", "{D442887A-6402-4873-ADC5-6F875085C0A6}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  
    -- SET
    dt= grtV.newObj("db.SimpleDatatype", "SET", "{D8F350D8-9522-478F-9233-28C52E26A067}", owner_id)
    dt.group= group
    grtV.insert(datatypeList, dt)
  end
  
end

-- ----------------------------------------------------------------------------------------
-- @brief Builds the list of character sets
--
--   Helper function to build the list of character sets
-- 
-- @param list the list that will hold the character sets
-- @param owner the owner, if any
-- ----------------------------------------------------------------------------------------

function createCharacterSets(list, owner)
  local characterSetsXml
  local characterSets
  local i
  
  -- this XML string is generated from the value returned by 
  -- ReverseEngineeringMysql:getCharacterSets()
  do
    characterSetsXml= [[<?xml version="1.0"?>
<data>
  <value type="list" content-type="dict" content-struct-name="db.CharacterSet">
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{43903B6E-F2D0-4529-9B67-E0717BF4EF9F}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">big5_chinese_ci</value>
        <value type="string">big5_bin</value>
      </value>
      <value type="string" key="defaultCollation">big5_chinese_ci</value>
      <value type="string" key="description">Big5 Traditional Chinese</value>
      <value type="string" key="name">big5</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{3AC6FA9A-82A4-4F7A-98B9-1F12CFB202BF}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">dec8_swedish_ci</value>
        <value type="string">dec8_bin</value>
      </value>
      <value type="string" key="defaultCollation">dec8_swedish_ci</value>
      <value type="string" key="description">DEC West European</value>
      <value type="string" key="name">dec8</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{9D4131DD-68A4-40BC-9D3F-82B4F7FA17B1}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp850_general_ci</value>
        <value type="string">cp850_bin</value>
      </value>
      <value type="string" key="defaultCollation">cp850_general_ci</value>
      <value type="string" key="description">DOS West European</value>
      <value type="string" key="name">cp850</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{6ABFE754-D874-49FF-AC49-386F49B841AC}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">hp8_english_ci</value>
        <value type="string">hp8_bin</value>
      </value>
      <value type="string" key="defaultCollation">hp8_english_ci</value>
      <value type="string" key="description">HP West European</value>
      <value type="string" key="name">hp8</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{E2E563B6-7240-4D2B-A665-CE22EBD29E9F}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">koi8r_general_ci</value>
        <value type="string">koi8r_bin</value>
      </value>
      <value type="string" key="defaultCollation">koi8r_general_ci</value>
      <value type="string" key="description">KOI8-R Relcom Russian</value>
      <value type="string" key="name">koi8r</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{AEFED982-F313-40D4-ABA9-11583190507F}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">latin1_german1_ci</value>
        <value type="string">latin1_swedish_ci</value>
        <value type="string">latin1_danish_ci</value>
        <value type="string">latin1_german2_ci</value>
        <value type="string">latin1_bin</value>
        <value type="string">latin1_general_ci</value>
        <value type="string">latin1_general_cs</value>
        <value type="string">latin1_spanish_ci</value>
      </value>
      <value type="string" key="defaultCollation">latin1_swedish_ci</value>
      <value type="string" key="description">ISO 8859-1 West European</value>
      <value type="string" key="name">latin1</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{A4E5546A-E0F8-4601-9DC3-D39C7DE1FD72}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">latin2_czech_cs</value>
        <value type="string">latin2_general_ci</value>
        <value type="string">latin2_hungarian_ci</value>
        <value type="string">latin2_croatian_ci</value>
        <value type="string">latin2_bin</value>
      </value>
      <value type="string" key="defaultCollation">latin2_general_ci</value>
      <value type="string" key="description">ISO 8859-2 Central European</value>
      <value type="string" key="name">latin2</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{2F780E59-D0CF-4175-8473-C86D73B33ED6}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">swe7_swedish_ci</value>
        <value type="string">swe7_bin</value>
      </value>
      <value type="string" key="defaultCollation">swe7_swedish_ci</value>
      <value type="string" key="description">7bit Swedish</value>
      <value type="string" key="name">swe7</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{E986FF81-4A4E-4A5D-B792-8D70F05A1BDE}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">ascii_general_ci</value>
        <value type="string">ascii_bin</value>
      </value>
      <value type="string" key="defaultCollation">ascii_general_ci</value>
      <value type="string" key="description">US ASCII</value>
      <value type="string" key="name">ascii</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{41103BE4-2115-4129-A4F3-0EC70E1A7CA6}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">ujis_japanese_ci</value>
        <value type="string">ujis_bin</value>
      </value>
      <value type="string" key="defaultCollation">ujis_japanese_ci</value>
      <value type="string" key="description">EUC-JP Japanese</value>
      <value type="string" key="name">ujis</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{9069C68C-56AA-40EA-9C14-1542D5226355}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">sjis_japanese_ci</value>
        <value type="string">sjis_bin</value>
      </value>
      <value type="string" key="defaultCollation">sjis_japanese_ci</value>
      <value type="string" key="description">Shift-JIS Japanese</value>
      <value type="string" key="name">sjis</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{A9BCA02E-A707-4832-A9A2-61F0A56F25C2}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">hebrew_general_ci</value>
        <value type="string">hebrew_bin</value>
      </value>
      <value type="string" key="defaultCollation">hebrew_general_ci</value>
      <value type="string" key="description">ISO 8859-8 Hebrew</value>
      <value type="string" key="name">hebrew</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{091F9B2C-FC20-47D4-B7EC-E31EF07828FF}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">tis620_thai_ci</value>
        <value type="string">tis620_bin</value>
      </value>
      <value type="string" key="defaultCollation">tis620_thai_ci</value>
      <value type="string" key="description">TIS620 Thai</value>
      <value type="string" key="name">tis620</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{FECE8C4D-B95C-4D06-B5E5-7C685B512607}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">euckr_korean_ci</value>
        <value type="string">euckr_bin</value>
      </value>
      <value type="string" key="defaultCollation">euckr_korean_ci</value>
      <value type="string" key="description">EUC-KR Korean</value>
      <value type="string" key="name">euckr</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{52DEA621-FBB4-4FC5-88C6-936CC3F53579}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">koi8u_general_ci</value>
        <value type="string">koi8u_bin</value>
      </value>
      <value type="string" key="defaultCollation">koi8u_general_ci</value>
      <value type="string" key="description">KOI8-U Ukrainian</value>
      <value type="string" key="name">koi8u</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{FD0CE66F-4075-48FB-AF79-C75453274791}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">gb2312_chinese_ci</value>
        <value type="string">gb2312_bin</value>
      </value>
      <value type="string" key="defaultCollation">gb2312_chinese_ci</value>
      <value type="string" key="description">GB2312 Simplified Chinese</value>
      <value type="string" key="name">gb2312</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{AF1906F2-20B5-491C-8450-14F730DBECD8}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">greek_general_ci</value>
        <value type="string">greek_bin</value>
      </value>
      <value type="string" key="defaultCollation">greek_general_ci</value>
      <value type="string" key="description">ISO 8859-7 Greek</value>
      <value type="string" key="name">greek</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{2BA9756D-C757-4770-B795-B1A4943359EC}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp1250_general_ci</value>
        <value type="string">cp1250_czech_cs</value>
        <value type="string">cp1250_croatian_ci</value>
        <value type="string">cp1250_bin</value>
      </value>
      <value type="string" key="defaultCollation">cp1250_general_ci</value>
      <value type="string" key="description">Windows Central European</value>
      <value type="string" key="name">cp1250</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{B5603A47-2936-4144-BA74-5D4B6959F47F}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">gbk_chinese_ci</value>
        <value type="string">gbk_bin</value>
      </value>
      <value type="string" key="defaultCollation">gbk_chinese_ci</value>
      <value type="string" key="description">GBK Simplified Chinese</value>
      <value type="string" key="name">gbk</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{A50D3C42-2BE0-49C7-A751-59ADDC334F01}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">latin5_turkish_ci</value>
        <value type="string">latin5_bin</value>
      </value>
      <value type="string" key="defaultCollation">latin5_turkish_ci</value>
      <value type="string" key="description">ISO 8859-9 Turkish</value>
      <value type="string" key="name">latin5</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{38416973-B84F-48DF-A766-01022A48B7CB}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">armscii8_general_ci</value>
        <value type="string">armscii8_bin</value>
      </value>
      <value type="string" key="defaultCollation">armscii8_general_ci</value>
      <value type="string" key="description">ARMSCII-8 Armenian</value>
      <value type="string" key="name">armscii8</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{FEDC8872-11CC-47CA-810A-6CCC2FAC6624}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">utf8_general_ci</value>
        <value type="string">utf8_bin</value>
        <value type="string">utf8_unicode_ci</value>
        <value type="string">utf8_icelandic_ci</value>
        <value type="string">utf8_latvian_ci</value>
        <value type="string">utf8_romanian_ci</value>
        <value type="string">utf8_slovenian_ci</value>
        <value type="string">utf8_polish_ci</value>
        <value type="string">utf8_estonian_ci</value>
        <value type="string">utf8_spanish_ci</value>
        <value type="string">utf8_swedish_ci</value>
        <value type="string">utf8_turkish_ci</value>
        <value type="string">utf8_czech_ci</value>
        <value type="string">utf8_danish_ci</value>
        <value type="string">utf8_lithuanian_ci</value>
        <value type="string">utf8_slovak_ci</value>
        <value type="string">utf8_spanish2_ci</value>
        <value type="string">utf8_roman_ci</value>
        <value type="string">utf8_persian_ci</value>
      </value>
      <value type="string" key="defaultCollation">utf8_general_ci</value>
      <value type="string" key="description">UTF-8 Unicode</value>
      <value type="string" key="name">utf8</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{B37E43B5-C83C-49A6-81FD-21424E22A0CC}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">ucs2_general_ci</value>
        <value type="string">ucs2_bin</value>
        <value type="string">ucs2_unicode_ci</value>
        <value type="string">ucs2_icelandic_ci</value>
        <value type="string">ucs2_latvian_ci</value>
        <value type="string">ucs2_romanian_ci</value>
        <value type="string">ucs2_slovenian_ci</value>
        <value type="string">ucs2_polish_ci</value>
        <value type="string">ucs2_estonian_ci</value>
        <value type="string">ucs2_spanish_ci</value>
        <value type="string">ucs2_swedish_ci</value>
        <value type="string">ucs2_turkish_ci</value>
        <value type="string">ucs2_czech_ci</value>
        <value type="string">ucs2_danish_ci</value>
        <value type="string">ucs2_lithuanian_ci</value>
        <value type="string">ucs2_slovak_ci</value>
        <value type="string">ucs2_spanish2_ci</value>
        <value type="string">ucs2_roman_ci</value>
        <value type="string">ucs2_persian_ci</value>
      </value>
      <value type="string" key="defaultCollation">ucs2_general_ci</value>
      <value type="string" key="description">UCS-2 Unicode</value>
      <value type="string" key="name">ucs2</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{69774387-87EB-4929-95F7-666E7081C341}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp866_general_ci</value>
        <value type="string">cp866_bin</value>
      </value>
      <value type="string" key="defaultCollation">cp866_general_ci</value>
      <value type="string" key="description">DOS Russian</value>
      <value type="string" key="name">cp866</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{EFA25F7E-F252-4083-A7E7-0E0B51D2C7A6}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">keybcs2_general_ci</value>
        <value type="string">keybcs2_bin</value>
      </value>
      <value type="string" key="defaultCollation">keybcs2_general_ci</value>
      <value type="string" key="description">DOS Kamenicky Czech-Slovak</value>
      <value type="string" key="name">keybcs2</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{82133B64-E91A-4EAB-96E7-5211E354E818}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">macce_general_ci</value>
        <value type="string">macce_bin</value>
      </value>
      <value type="string" key="defaultCollation">macce_general_ci</value>
      <value type="string" key="description">Mac Central European</value>
      <value type="string" key="name">macce</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{3006DAE4-DDE9-4979-B982-8284D885ED2A}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">macroman_general_ci</value>
        <value type="string">macroman_bin</value>
      </value>
      <value type="string" key="defaultCollation">macroman_general_ci</value>
      <value type="string" key="description">Mac West European</value>
      <value type="string" key="name">macroman</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{6C8C05CC-8859-4974-B756-497697DB875F}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp852_general_ci</value>
        <value type="string">cp852_bin</value>
      </value>
      <value type="string" key="defaultCollation">cp852_general_ci</value>
      <value type="string" key="description">DOS Central European</value>
      <value type="string" key="name">cp852</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{99441A25-E2A8-4B71-A311-AA46C216D6A9}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">latin7_estonian_cs</value>
        <value type="string">latin7_general_ci</value>
        <value type="string">latin7_general_cs</value>
        <value type="string">latin7_bin</value>
      </value>
      <value type="string" key="defaultCollation">latin7_general_ci</value>
      <value type="string" key="description">ISO 8859-13 Baltic</value>
      <value type="string" key="name">latin7</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{B670A3DE-8FD2-4508-A437-4EE82D3F184A}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp1251_bulgarian_ci</value>
        <value type="string">cp1251_ukrainian_ci</value>
        <value type="string">cp1251_bin</value>
        <value type="string">cp1251_general_ci</value>
        <value type="string">cp1251_general_cs</value>
      </value>
      <value type="string" key="defaultCollation">cp1251_general_ci</value>
      <value type="string" key="description">Windows Cyrillic</value>
      <value type="string" key="name">cp1251</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{EF353851-0BAE-450A-8751-D96E58402C08}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp1256_general_ci</value>
        <value type="string">cp1256_bin</value>
      </value>
      <value type="string" key="defaultCollation">cp1256_general_ci</value>
      <value type="string" key="description">Windows Arabic</value>
      <value type="string" key="name">cp1256</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{25D735B3-3DBA-4EF7-95C9-B94E5A480D40}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp1257_lithuanian_ci</value>
        <value type="string">cp1257_bin</value>
        <value type="string">cp1257_general_ci</value>
      </value>
      <value type="string" key="defaultCollation">cp1257_general_ci</value>
      <value type="string" key="description">Windows Baltic</value>
      <value type="string" key="name">cp1257</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{F312E41B-8ECF-4B2D-B084-B22186F114AE}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">binary</value>
      </value>
      <value type="string" key="defaultCollation">binary</value>
      <value type="string" key="description">Binary pseudo charset</value>
      <value type="string" key="name">binary</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{7A92AB4C-BB3F-47B9-BC98-4F55CF8690A3}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">geostd8_general_ci</value>
        <value type="string">geostd8_bin</value>
      </value>
      <value type="string" key="defaultCollation">geostd8_general_ci</value>
      <value type="string" key="description">GEOSTD8 Georgian</value>
      <value type="string" key="name">geostd8</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{3EE67D94-4290-4A37-A0A3-73E2602C2971}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">cp932_japanese_ci</value>
        <value type="string">cp932_bin</value>
      </value>
      <value type="string" key="defaultCollation">cp932_japanese_ci</value>
      <value type="string" key="description">SJIS for Windows Japanese</value>
      <value type="string" key="name">cp932</value>
    </value>
    <value type="dict" struct-name="db.CharacterSet">
      <value type="string" key="_id">{EF2A81A5-CB47-46E3-9FC5-203D7DA8EA10}</value>
      <value type="list" content-type="string" key="collations">
        <value type="string">eucjpms_japanese_ci</value>
        <value type="string">eucjpms_bin</value>
      </value>
      <value type="string" key="defaultCollation">eucjpms_japanese_ci</value>
      <value type="string" key="description">UJIS for Windows Japanese</value>
      <value type="string" key="name">eucjpms</value>
    </value>
  </value>
</data>

]]
  end

  characterSets= grtV.fromXml(characterSetsXml)
  
  for i= 1, grtV.getn(characterSets) do
    if owner ~= nil then
      characterSets[i].owner= owner
    end
    
    grtV.insert(list, characterSets[i])
  end
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get the native MySQL driver
--
--   Helper function to return infos about the native MySQL driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverMysqlNative(owner)

  -- create driver object
  local driver= grtV.newObj("db.mgmt.NativeDriver", "MysqlNative", 
    "{D65C7567-0B84-4AC6-A1A4-0A4BB8C9F3F2}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "MySQL Native Driver"
  driver.description= "MySQL native driver using the MySQL client library"

  driver.filesTarget= "."
  grtV.insert(driver.files, "libmysql.dll")
  driver.downloadUrl= "http://dev.mysql.com/downloads/mysql/"

  -- add driver parameters
  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 1, "3306")

  -- add additional parameters
  local param= __RdbmsInfo_lua.getDriverParameter(owner, "schema", "Default Schema:", 
    "The schema that will be used as default schema", "string", -1, 218, "", 0, 0)
  param.lookupValueModule= "ReverseEngineeringMysql"
  param.lookupValueMethod= "getSchemata"
  grtV.insert(driver.parameters, param)

  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "useCompression", "Use compression protocol", 
    "Select this option for WAN connections.", "boolean", -1, 318, "", 1, 0))
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "useSSL", 
    "Use SSL if available (the client library needs to support it)", 
    "This option turns on SSL encryption if the client library supports it", "boolean", -1, 318, "", 1, 0))
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "useAnsiQuotes", "Use ANSI quotes to quote identifiers.", 
    "If enabled this option overwrites the serverside settings.", "tristate", -1, 318, "", 1, 0))
  if grt.getResLua(Base:getOsTypeName()) == "WINDOWS" then
    grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "socket", "Named Pipe:", 
      "Use the specified named pipe instead of a TCP/IP connection.", 
      "string", -1, 218, "", 1, 0))
  else
    grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "socket", "Socket Name:", 
      "Use the specified socket instead of a TCP/IP connection.", 
      "string", -1, 218, "", 1, 0))
  end

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringMysql",
      MigrationModule= "MigrationGeneric",
      TransformationModule= "TransformationMysql",
      QueryModule= "QueryMysql"
    }

  driver.isInstalled= 1

  return driver
end

-- ----------------------------------------------------------------------------------------
-- @brief Function to get the Jdbc MySQL driver
--
--   Helper function to return infos about the Jdbc driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverMysqlJdbc(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.JdbcDriver", "MysqlJdbc31", 
    "{8E33CDBA-2B8D-4221-96C4-506D398BC377}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "MySQL JDBC Driver 5.0"
  driver.description= "MySQL JDBC Driver"

  driver.filesTarget= "./java/lib/"
  grtV.insert(driver.files, "mysql-connector-java-5.0.4-bin.jar")
  driver.downloadUrl= "http://dev.mysql.com/downloads/connector/j/5.0.html"

  -- Jdbc specific settings
  driver.className= "com.mysql.jdbc.Driver"
  driver.connectionStringTemplate= "jdbc:mysql://%host%:%port%/?user=%username%&password=%password%" ..
    "&useServerPrepStmts=false&characterEncoding=UTF-8"

  -- add driver parameters
  __RdbmsInfo_lua.addDriverParamDefaults(driver, driver.parameters, 1, "3306")

  -- add additional parameters
  local param= __RdbmsInfo_lua.getDriverParameter(owner, "schema", "Default Schema:", 
    "The schema that will be used as default schema", "string", -1, 218, "", 0, 0)
  param.lookupValueModule= "ReverseEngineeringMysqlJdbc"
  param.lookupValueMethod= "getSchemata"
  grtV.insert(driver.parameters, param)

  -- advanced parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "jdbcConnStr", "Connection String:", 
    "Jdbc Connection String", "string", -1, 218, "", 1, 0))

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringMysqlJdbc",
      MigrationModule= "MigrationMysql",
      TransformationModule= "TransformationMysqlJdbc"
    }

  driver.isInstalled= 1

  return driver
end

-- ----------------------------------------------------------------------------------------
-- @brief Function to get the embedded MySQL driver
--
--   Helper function to return infos about the embedded MySQL driver
-- 
-- @param owner the Grt value of the Rdbms
--
-- @return a new created GRT value of struct "db.mgmt.Driver" containing the driver infos
-- ----------------------------------------------------------------------------------------
function getDriverMysqlEmbedded(owner)
  -- create driver object
  local driver= grtV.newObj("db.mgmt.NativeDriver", "MysqlEmbedded", 
    "{64B098D9-57F1-44C6-9B6D-41852D159A38}", grtV.toLua(owner._id))

  -- set driver values
  driver.caption= "MySQL Native Driver for Embedded"
  driver.description= "MySQL native driver for the use with Embedded"

  driver.filesTarget= "."
  grtV.insert(driver.files, "libmysqld.dll")
  driver.downloadUrl= "http://dev.mysql.com/downloads/mysql/"

  -- add driver parameters
  grtV.insert(driver.parameters, __RdbmsInfo_lua.getDriverParameter(owner, "datadir", "Data Directory:", 
    "The MySQL data directory to use", "dir", 1, 218, "", 0, 1))

  -- add additional parameters
  local param= __RdbmsInfo_lua.getDriverParameter(owner, "schema", "Default Schema:", 
    "The schema that will be used as default schema", "string", 2, 218, "", 0, 0)
  param.lookupValueModule= "ReverseEngineeringMysql"
  param.lookupValueMethod= "getSchemata"
  grtV.insert(driver.parameters, param)

  driver.defaultModules= 
    {
      ReverseEngineeringModule= "ReverseEngineeringMysql",
      MigrationModule= "MigrationGeneric",
      TransformationModule= "TransformationMysql"
    }

  driver.isInstalled= 1

  return driver
end
