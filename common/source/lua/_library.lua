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
-- @file _library.lua
-- @brief A collection of common auxiliary functions that is loaded when the Lua loader 
-- gets initialized
-- ----------------------------------------------------------------------------------------


grt= {GrtLuaLibraryVersion= "1.0"}


-- ----------------------------------------------------------------------------------------
-- @brief dummy implemenation for _()
--
--   This is a placeholder till a real _() function is implemented
--
-- Example:
--   print(_("test"))
--
-- @param txt the text that has to be translated
--
-- @return the original text
-- ----------------------------------------------------------------------------------------
function _(txt)
  return txt
end


-- ----------------------------------------------------------------------------------------
-- @brief Prepares a return value
--
--   Every function that is exposed to the Grt needs to return its value in a dict that
-- has a "value" entry. Use this function to prepare the result values. The function can
-- also be called without a parameter. In that case 1 will be returned to the Grt.
--
-- Example:
--   function calcDouble(args)
--     return _success(args[1] * 2)
--   end
--
-- @param result  the result value that should be returned to the Grt.
--
-- @return the result value prepared to be returned to the Grt
-- ----------------------------------------------------------------------------------------
function grt.success(result)
  return result and {value= result} or {value= 1}
end


-- ----------------------------------------------------------------------------------------
-- @brief Returns an error to the Grt
--
--   Use this function to pass an error message back to the Grt.
--
-- Example:
--   function calcDouble(args)
--     return args[1] and 
--       grt.success(args[1] * 2) or 
--       grt.error("You need to pass a number as argument")
--   end
--
-- @param errorText  the error message
-- @param errorDetails  detailed information about the error
--
-- @return the error prepared to be returned to the Grt
-- ----------------------------------------------------------------------------------------
function grt.error(errorText, errorDetails)
  return {error= errorText, detail= errorDetails}
end


-- ----------------------------------------------------------------------------------------
-- @brief Prints the error message if a GRT module function call failed
--
--   Checks if the grtError and prints the error if it is set. Passes the result back
--
-- @param result  the result from a GRT module function call
--
-- @return the result
-- ----------------------------------------------------------------------------------------
function grt.getRes(result)
  if (grtError ~= nil) then
    print(grtError.error)

    if (grtError.detail ~= nil) then
      print(grtError.detail)
    end
  end

  return result
end


-- ----------------------------------------------------------------------------------------
-- @brief Extracts the return value of a Grt function as a Lua value
--
--   A Grt function returns its result as a dict in the "value" entry. This function
-- extracts the value and converts it to a Lua value
--
-- @return the result converted to a Lua object
-- ----------------------------------------------------------------------------------------
function grt.getResLua(result)
  return grtV.toLua(grt.getRes(result))
end


-- ----------------------------------------------------------------------------------------
-- @brief Checks if an GRT error has occured and if so, prints the error adn exits
--
--   Checks if the grtError and prints the error if it is set. Exists with error code 1
--
-- @param errorString  the error string that will be printed before the error message
-- ----------------------------------------------------------------------------------------
function grt.exitOnError(errorString)
  if (grtError ~= nil) then
    print("ERROR: " .. errorString)
    
    print(grtError.error)

    if (grtError.detail ~= nil) then
      print(grtError.detail)
    end
    
    exit(1)
  end
end


-- ----------------------------------------------------------------------------------------
-- @brief Returns a new generated GUID
--
--   Calls the Grt function getGuid() from the module Base to retrieve a GUID. The Grt
-- function result is then converted to a Lua string
--
-- @return a new generated GUID as lua string
-- ----------------------------------------------------------------------------------------
function grt.newGuid()
  return grt.getResLua(Base:getGuid())
end


-- ----------------------------------------------------------------------------------------
-- @brief Checks if a files exists
--
--   Tries to open the file. If the file exists it will close the file again and 
-- return true. If it does not exists it will return false (as Lua will return false 
-- when nothing is returned explicitly
--
-- @param filename the file to check
--
-- @return true if the files exists, false if not
-- ----------------------------------------------------------------------------------------
function grt.fileExists(filename)
  local f = io.open(filename, "r")

  if f then
    f:close()

    return true
  end
  
  return false
end


-- ----------------------------------------------------------------------------------------
-- @brief Splits a Lua string into a list of Lua strings.
--
--   Splits the given string based on the separator and puts the tokens into a list
--
-- @param str the string to split
-- @param sep the separator character
--
-- @return returns the list of tokens
-- ----------------------------------------------------------------------------------------
function grt.split(str, sep)
  local t= {}
  local function helper(token) table.insert(t, token) end

  helper((string.gsub(str, "(.-)(" .. sep .. ")", helper)))

  return t
end

-- ----------------------------------------------------------------------------------------
-- @brief Aligns the given string to the left and adds blanks.
--
--   Splits the given string based on the separator and puts the tokens into a list
--
-- @param str the string to align left
-- @param width the length of the new string
--
-- @return returns the aligned string
-- ----------------------------------------------------------------------------------------
function grt.alignLeft(str, width)
  local s
  
  if str == nil then
    s= string.rep(" ", width)
  else
    local l= string.len(str)
    
    if l < width then
      s= str .. string.rep(" ", width - l)
    elseif l > width then
      s= string.sub(str, 1, width)
    else
      s= str
    end
  end
  
  return s
end

-- ----------------------------------------------------------------------------------------
-- @brief Invoke a remote agent function and wait it to finish.
--
--   Invokes a remote function, polling the agent until it is finished.
-- Once finished, it will return the exit status and result value.
-- 
-- The first parameter has to be a table {hostname= "192.168.1.100", port= 12345}
--
-- Example:
--   agent= {hostname= "192.168.1.100", port= 12345}
--   res= grt.callRemoteFunction(agent, "BaseJava", "engineVersion", nil, false)
--
-- @param agent a table with connection information, e.g. {hostname= "192.168.1.100", port= 12345}
-- @param module name of the module to call
-- @param funcname name of the module function to call
-- @param argument the arguments to pass to the module function (have to be in a list)
-- @param syncGlobals if true the global object trees are synced on both sides
--
-- @return {error : error_status, result : result_value }
-- ----------------------------------------------------------------------------------------
function grt.callRemoteFunction(agent, module, funcname, argument, syncGlobals)
  local session= grtA.connect(agent.hostname, agent.port)
  local result= nil
  
  if session then
    if syncGlobals then
      grtA.setGlobal(session)
    end
  
    if grtA.invoke(session, module, funcname, argument) == 0 then
      while 1 do
        print("checking")

        st= grtA.check(session)
        if st == 0 then     -- nothing executing!?
          grtA.close(session)
          print("Unexpected state in remote agent.")
          break
        elseif st == 2 then   -- executing
          sleep(50)
          grtA.messages(session)
        elseif st == 3 then   -- finished
	  result= grtA.finish(session)
          break
        else                -- anything else
          grtA.close(session)
          print("Error waiting remote function to finish.")
	  return nil
        end
      end
    else
      print("Error invoking remote method.")
      grtA.close(session)
      return nil
    end
    
    print("sync globals")

    if syncGlobals then
      grtA.getglobal(session)
    end

    grtA.close(session)
  end

  return result
end


-- ----------------------------------------------------------------------------------------
-- @brief Checks if a module exists
--
--   Goes through the list of modules to see if the module with the given name exists
--
-- @param moduleName the name of the module
--
-- @return returns true if the module exists
-- ----------------------------------------------------------------------------------------
function grt.moduleExists(moduleName)
  local moduleList= grtM.get()
  local i
  
  for i= 1, table.getn(moduleList) do
    if moduleList[i] == moduleName then
      return true
    end 
  end

  return false
end

