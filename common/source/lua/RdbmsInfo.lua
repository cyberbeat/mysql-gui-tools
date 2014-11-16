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
-- @file RdbmsInfo.lua
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
  local moduleInfo= 
    {
      name= "RdbmsInfo", 
      functions= {
        "getRdbmsInfo::"
      }, 
      extends= ""
    }

  return moduleInfo
end


-- ----------------------------------------------------------------------------------------
-- @brief Function to get information about a rdbms
--
--   Returns a db.mgmt.Rdbms struct with infos about the rdbms
-- 
-- @return a new created db.mgmt.Rdbms GRT value struct 
-- ----------------------------------------------------------------------------------------
function getRdbmsInfo(args)
  return grt.error("A specific implementation has to called.")
end


-- ----------------------------------------------------------------------------------------
-- @brief Returns a new created driver parameter
--
--   Helper function to create a new driver parameter
--
-- @param owner the owner GRT value (the driver)
-- @param name the name of the parameter (username, password, port, host, ...)
-- @param caption the caption of the parameter
-- @param desc the description
-- @param paramType the type of the param (can be "string", "int", "file")
-- @param row the layout row in which to create the widget for this parameter
-- @param width the width of the widget
-- @param defaultValue the default value or "" if there is no default value
-- @param layoutAdvanced if this is set to 1 the parameter is listed in the advanced section
-- @param required if this is set to 1 the parameter must be set before the user can continue
-- 
-- @return a new created GRT value of struct "db.mgmt.DriverParameter"
-- ----------------------------------------------------------------------------------------
function getDriverParameter(owner, name, caption, desc, paramType,
  row, width, defaultValue, layoutAdvanced, required)

  local param= grtV.newObj("db.mgmt.DriverParameter", name, grt.newGuid(), grtV.toLua(owner._id))

  param.caption= caption
  param.description= desc
  param.paramType= paramType
  param.defaultValue= defaultValue
  param.layoutAdvanced= 0
  param.layoutRow= row
  param.layoutWidth= width
  param.layoutAdvanced= layoutAdvanced
  param.required= required

  return param
end

-- ----------------------------------------------------------------------------------------
-- @brief Adds default driver parameter to the driver parameter list
--
--   Helper function to add the default parameters host, port, username, password to
-- the given driver parameter list
--
-- @param owner the owner GRT value (the driver)
-- @param params the parameter list GRT value
-- @param startRow in which row the default parameter should begin (set to 2 to allow
--   another parameter to be in the first line)
-- @param defaultPort the default port number
-- ----------------------------------------------------------------------------------------
function addDriverParamDefaults(owner, params, startRow, defaultPort)
  grtV.insert(params, getDriverParameter(owner, "host", "Hostname:", 
    "Name or IP address of the server machine", "string", startRow, 118, "", 0, 1))

  grtV.insert(params, getDriverParameter(owner, "port", "Port:", 
    "TCP/IP port", "int", startRow, 46, defaultPort, 0, 1))

  grtV.insert(params, getDriverParameter(owner, "username", "Username:", 
    "Name of the user to connect with.", "string", startRow + 1, 218, "", 0, 1))

  grtV.insert(params, getDriverParameter(owner, "password", "Password:", 
    "The user's password.", "password", startRow + 2, 218, "", 0, 0))
end
