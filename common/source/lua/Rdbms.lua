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
-- @file BaseLua.lua
-- @brief Module that contains base functionality for Lua
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
      name= 'Rdbms', 
      functions= {
        'connect::',
        'close::'
      }, 
      extends= ''
    }

  return moduleInfo
end

-- ----------------------------------------------------------------------------------------
-- @brief Opens a connection to a database
--
--   Opens a connection to a database
--
-- @return the connection object
-- ----------------------------------------------------------------------------------------
function connect(args)
  local conn
  local connGlobalString= ""
  
  if grtV.getn(args) == 1 then
    if type(grtV.toLua(args[1])) == "string" then
      connGlobalString= grtV.toLua(args[1])
      conn= grtV.getGlobal(string.sub(connGlobalString, 9))
    else
      conn= args[1]
    end
  elseif grtV.getn(args) == 3 then
    local name= grtV.toLua(args[1])
    local rdbmsType= grtV.toLua(args[2])
    local params= grtV.toLua(args[3])
    local driver
    
    conn= grtV.newObj("db.mgmt.Connection", name, "", "")
    
    driver= RdbmsManagement:getDefaultDriverByName({rdbmsType})
    if driver == nil then
      return grt.error(string.format(_("There is no rdbms available with the name %s"), rdbmsType))
    end
    
    conn.driver= driver
    conn.modules= grtV.duplicate(conn.driver.defaultModules)
    conn.parameterValues= params
    
    grtV.lookupAdd(conn)
  else
    return grt.error(_("This function takes (db.mgmt.Connection) or " ..
      "(name, rdbmsType, {params}) as parameters."))
  end
  
  -- get query module
  local queryModule= conn.modules.QueryModule
  if queryModule == nil then
    queryModule= conn.driver.defaultModules.QueryModule
  end
  
  if queryModule == nil then
    return grt.error(_("The connection does not have a query module."))
  end
  
  -- call query module function to open the database connection
  if connGlobalString ~= "" then
    conn= grtM.callFunction(grtV.toLua(queryModule), "connOpen", {connGlobalString})
  else
    conn= grtM.callFunction(grtV.toLua(queryModule), "connOpen", {conn})
  end
  
  if grtError ~= nil then
    return grtError
  end

  return grt.success(conn)
end

-- ----------------------------------------------------------------------------------------
-- @brief Closes the connection to a database
--
--   Close the connection to a database
--
-- @return success or error
-- ----------------------------------------------------------------------------------------
function close(args)
  local conn
  
  if grtV.getn(args) == 1 then
    conn= args[1]
  else
    return grt.error(_("This function takes (db.mgmt.Connection) as parameter."))
  end

  -- get query module
  local queryModule= conn.modules.QueryModule
  if queryModule == nil then
    queryModule= conn.driver.defaultModules.QueryModule
  end
  
  if queryModule == nil then
    return grt.error(_("The connection does not have a query module."))
  end
  
  -- call query module function to close the database connection
  conn= grtM.callFunction(grtV.toLua(queryModule), "connClose", {conn})
  if grtError ~= nil then
    return grtError
  end
  
  return grt.success()
end

