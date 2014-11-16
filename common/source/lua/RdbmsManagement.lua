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
-- @file RdbmsManagement.lua
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
    name= "RdbmsManagement", 
    functions= {
      "getManagementInfo::",
      "storeConns::",
      "getDatatypeGroupByName::",
      "getDefaultDriverByName::"
    }, 
    extends= ""
  }

  return moduleInfo
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns a dict containing connection management information
--
--   Returns a structed dict that contains a list of all drivers and stored connections.
-- New drivers have to be added here
--
-- @return a db.mgmt.Management dict
-- ----------------------------------------------------------------------------------------
function getManagementInfo()
  -- Create new management object
  local rdbmsMgmt= grtV.newObj("db.mgmt.Management", "rdbmsManagement", grt.newGuid(), "")

  -- -------------------------------
  -- Get datatype groups
  getDatatypeGroups(rdbmsMgmt)

  -- -------------------------------
  -- Get rdbms information
  local modules= grtM.get("RdbmsInfo")
  local i

  -- Call the getRdbmsInfo function of all modules that extend RdbmsInfo
  for i= 1, table.getn(modules) do
    local rdbms= grtM.callFunction(modules[i], "getRdbmsInfo", rdbmsMgmt)
    grtV.insert(rdbmsMgmt.rdbms, rdbms)
  end

  -- -------------------------------
  -- Prepare a list of stored connections
  local storedConnsFilename= grt.getResLua(Base:getAppDataDir()) .. "Connections.xml"
  if grt.fileExists(storedConnsFilename) then
    rdbmsMgmt.storedConns= grtV.load(storedConnsFilename)
  end

  return grt.success(rdbmsMgmt)
end


-- ----------------------------------------------------------------------------------------
-- @brief Stores the given connection list to disk
--
--   Stores the given connection list to the user's connection file on disk so they 
-- can be loaded by getManagementInfo().
--
-- @param connectionList connection list GRT value to store
-- 
-- @return success
-- ----------------------------------------------------------------------------------------
function storeConns(args)
  if args == nil then
    return grt.error("The first argument has to be a list of db.mgmt.Connection objects")
  end
  
  -- force the target directory to be present
  Base:createDir({Base:getAppDataDir()})
  
  if grtError == nil then
    local storedConnsFilename= grtV.toLua(Base:getAppDataDir()) .. "Connections.xml"
    grtV.save(args[1], storedConnsFilename)

    return grt.success()
  else
    return grt.error("The destination directory cannot be created.")
  end
end


-- ----------------------------------------------------------------------------------------
-- @brief Returns the datatype group defined by a name
--
--   Returns the datatype group defined by the submitted name
--
-- @param mgmtPath global path of the rdbms management value
-- @param groupName the name of the datatype group
--
-- @return the datatype group or an error if there is no such group
-- ----------------------------------------------------------------------------------------
function getDatatypeGroupByName(args)
  local groupList= args[1].datatypeGroups
  local i

  if groupList ~= nil then
    for i= 1, grtV.getn(groupList) do
      if (string.lower(grtV.toLua(args[2])) == grtV.toLua(groupList[i].name)) then
        return grt.success(groupList[i])
      end
    end
  end

  return grt.error(_("No such datatype group"))
end


-- ----------------------------------------------------------------------------------------
-- @brief Fills the given list with driver information
--
--   Helper function to fill the given list with driver information
--
-- @param drivers a typed list that will hold the drivers
-- ----------------------------------------------------------------------------------------
function getDatatypeGroupByNameLua(rdbmsMgmt, groupName)
  local groupList
  local i
  
  if rdbmsMgmt ~= nil then
    groupList= rdbmsMgmt.datatypeGroups
  
    if groupList ~= nil then
      for i= 1, grtV.getn(groupList) do
        if (string.lower(groupName) == grtV.toLua(groupList[i].name)) then
          return groupList[i]
        end
      end
    end
  end
  
  return ""
end

-- ----------------------------------------------------------------------------------------
-- @brief Fills the given list with driver information
--
--   Helper function to fill the given list with driver information
--
-- @param drivers a typed list that will hold the drivers
-- ----------------------------------------------------------------------------------------
function getDatatypeGroups(rdbmsMgmt)
  local groupList= rdbmsMgmt.datatypeGroups
  local group

  -- Numeric group
  group= grtV.newObj("db.DatatypeGroup", "numeric", "{A0B5D6E6-E94C-482A-8608-8EF7AF4FB560}", grtV.toLua(rdbmsMgmt._id))
  group.caption= _("Numeric Types")
  group.description= _("Datatypes to store numbers of different sizes")
  grtV.insert(groupList, group)

  -- String group
  group= grtV.newObj("db.DatatypeGroup", "string", "{92F0945A-4658-47A0-A3EA-423F4AB13694}", grtV.toLua(rdbmsMgmt._id))
  group.caption= _("Strings")
  group.description= _("Datatypes to store shorter text")
  grtV.insert(groupList, group)

  -- Text group
  group= grtV.newObj("db.DatatypeGroup", "text", "{F48E12A6-C67D-43EC-8FA6-EC30D75E5528}", grtV.toLua(rdbmsMgmt._id))
  group.caption= _("Long Text Types")
  group.description= _("Datatypes to store long text")
  grtV.insert(groupList, group)

  -- Blob group
  group= grtV.newObj("db.DatatypeGroup", "blob", "{640A39CD-A559-4366-BBCD-734F638B361D}", grtV.toLua(rdbmsMgmt._id))
  group.caption= _("Blob Types")
  group.description= _("Datatypes to store binary data")
  grtV.insert(groupList, group)

  -- Datetime group
  group= grtV.newObj("db.DatatypeGroup", "datetime", "{06431E19-2E93-4DE3-8487-A8DF4A25DA96}", grtV.toLua(rdbmsMgmt._id))
  group.caption= "Date and Time Types"
  group.description= "Datatypes to store date and time values"
  grtV.insert(groupList, group)

  -- Geo group
  group= grtV.newObj("db.DatatypeGroup", "gis", "{C1ABB46A-9AF9-420C-B31B-95AFE921C085}", grtV.toLua(rdbmsMgmt._id))
  group.caption= "Geographical Types"
  group.description= "Datatypes to store geographical information"
  grtV.insert(groupList, group)

  -- Various group
  group= grtV.newObj("db.DatatypeGroup", "various", "{BBE4BC2C-D462-46B7-AC2E-9F0CE17C3686}", grtV.toLua(rdbmsMgmt._id))
  group.caption= "Various Types"
  group.description= "Various datatypes"
  grtV.insert(groupList, group)

  -- Userdefined group
  group= grtV.newObj("db.DatatypeGroup", "userdefined", "{3D44C889-03BA-4783-A02D-ACDF4943257E}", grtV.toLua(rdbmsMgmt._id))
  group.caption= "Userdefined Types"
  group.description= "Datatypes defined by a user"
  grtV.insert(groupList, group)
  
  -- Structured Datatypes group
  group= grtV.newObj("db.DatatypeGroup", "structured", "{039C02E4-4FBE-4D5C-9E89-39A3E74E77F7}", grtV.toLua(rdbmsMgmt._id))
  group.caption= "Structured Types"
  group.description= "Structured datatypes consisting of a collection of simple and other structured datatypes"
  grtV.insert(groupList, group)
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the default driver of the RDBMS with the given name
--
--   Looks up the defaut driver of the RDBMS with the given name in 
-- in /rdbmsMgmt/rdbms
--
-- @param the default driver or an error if no RDBMS with this name can be found
-- ----------------------------------------------------------------------------------------
function getDefaultDriverByName(args)
  local rdbmsList= grtV.getGlobal("/rdbmsMgmt/rdbms")
  local rdbmsName= grtV.toLua(args[1])
  local rdbms= grtV.getListItemByObjName(rdbmsList, rdbmsName)
  
  if rdbms ~= nil then
    return grt.success(rdbms.defaultDriver)
  else
    return grt.error(string.format(_("Cannot find RDBMS with the name %s."), rdbmsName))
  end
end

