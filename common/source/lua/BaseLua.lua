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
      name= 'BaseLua', 
      functions= {
        'engineVersion::'
      }, 
      extends= ''
    }

  return moduleInfo
end

-- ----------------------------------------------------------------------------------------
-- @brief Returns the version of the used engine
--
--   Returns the version of the used engine
--
-- @return the Lua version string
-- ----------------------------------------------------------------------------------------
function engineVersion(args)
  local lua_version= _G["_VERSION"]

  return grt.success(lua_version)
end
