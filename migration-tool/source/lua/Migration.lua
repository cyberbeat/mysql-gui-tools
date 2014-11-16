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
-- @file Migration.lua
-- @brief Module that contains program logic for MySQL Migration
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
  local moduleInfo= 
    {
      name= "Migration", 
      functions= {
        "initMigration::",
        "shutdownMigration::"
      }, 
      extends= ""
    }

  return moduleInfo
end

-- ----------------------------------------------------------------------------------------
-- @brief Initalizes the application
--
--   Creates an object of the struct db.workbench.Environment and assigns it to the
-- global /workbench
--
-- @return 1 on success or an error
-- ----------------------------------------------------------------------------------------
function initMigration()

  -- Generate a new db.migration.Migration object and set it to the global /migration
  grtV.setGlobal("/migration", grtV.newObj("db.migration.Migration", "Migration", grt.newGuid(), ""))

  -- Get all available RDBMS drivers
  grtV.setGlobal("/rdbmsMgmt", grt.getRes(RdbmsManagement:getManagementInfo()))
  
  -- Load generic datatype mapping
  if grt.fileExists("xml/GenericDatatypeMapping.xml") then
    grtV.setGlobal("/migration/genericDatatypeMappings", grtV.load("xml/GenericDatatypeMapping.xml"))
  end

  -- Free GRT values cached on Lua side
  collectgarbage()

  return grt.success()
end

-- ----------------------------------------------------------------------------------------
-- @brief Performs actions when the application is shut down
--
--   Performs several actions shortly before the application is shut down
--
-- @return 1 on success or an error
-- ----------------------------------------------------------------------------------------
function shutdownMigration(args)
  RdbmsManagement:storeConns({grtV.getGlobal("/rdbmsMgmt/storedConns")})

  return grt.success()
end
