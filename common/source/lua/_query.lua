-- ----------------------------------------------------------------------------------------
-- Copyright (C) 2005, 2006 MySQL AB
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
-- @file _query.lua
-- @brief This library contains the query class
-- ----------------------------------------------------------------------------------------

Query= {
    grtQuery= nil,
    grtRes= nil
  }
  
function Query:new(args)
  qryObj= {}
  
  -- set meta table to inherit parent class behaviour
  setmetatable(qryObj, self)
  self.__index = self
  
  -- check arguments
  local name= "Query"
  local argCount= 0

  if type(args) == "string" then
    qryObj.grtQuery= grtV.newObj("db.query.Query", name, "", "")
    qryObj:setSql(args)
  else
    if args ~= nil then
      argCount= table.getn(args)
      if argCount >= 2 then
        name= args[2]
      end
    end
    
    if argCount >= 1 then
      if type(args[1]) == "string" then
        qryObj.grtQuery= grtV.newObj("db.query.Query", name, "", "")
        qryObj:setSql(args[1])
      else
        qryObj.grtQuery= args[1]
      end
    else
      qryObj.grtQuery= grtV.newObj("db.query.Query", name, "", "")
    end
  end
  
  return qryObj
end

function Query:close()
  RdbmsResultSet:close({self.grtRes})
end

function Query:setSql(sql)
  self.grtQuery.sql= sql
end


function Query:getSql()
  return self.grtQuery.sql
end


function Query:setConnection(conn)
  if (conn ~= nil) and (grtS.get(conn) ~= "db.mgmt.Connection") then
    return grt.error("This function takes a db.mgmt.Connection as parameter.")
  end
  
  if conn._id == nil then
    conn._id= grt.newGuid()
  end

  -- make sure the conn is in the lookup cache
  grtV.lookupAdd(conn)
  
  self.grtQuery.connection= conn
  
  -- get query module
  local queryModule= conn.modules.QueryModule
  if queryModule == nil then
    queryModule= self:getConnection().driver.defaultModules.QueryModule
  end
  
  self.grtQuery.moduleName= queryModule
end


function Query:getConnection()
  return self.grtQuery.connection
end


function Query:print(conn)
  if conn ~= nil then
    self:setConnection(conn)
  end
  
  if self.grtQuery.moduleName == nil then
    return grt.error("The connection does not have a query module.")
  end
  
  grtM.callFunction(grtV.toLua(self.grtQuery.moduleName), "queryPrint", 
    {self:getConnection(), self:getSql()})
end


function Query:execute(conn, bufferSize)
  if conn ~= nil then
    self:setConnection(conn)
  end
  
  if self.grtQuery.moduleName == nil then
    return grt.error("The connection does not have a query module.")
  end
  
  self.grtRes= RdbmsResultSet:open({self.grtQuery, bufferSize})
  print(self.grtQuery.moduleName)
  grtM.callFunction(grtV.toLua(self.grtQuery.moduleName), "queryFetchResultSet", {self.grtRes})
  
  return self.grtRes
end


function Query:executeAndFetch(conn, bufferSize)
  self:execute(conn, bufferSize)
  
  while(qry2:fetchNextBlock()) do 
  end
end

function Query:fetchNextBlock()
  if self.grtRes ~= nil then
    grtM.callFunction(grtV.toLua(self.grtQuery.moduleName), "queryFetchResultSet", {self.grtRes})
  end
  
  if (self:status() == 2) then
    return true
  else
    return false
  end
end


function Query:currentRowCount()
  if self.grtRes ~= nil then
    return grtV.toLua(RdbmsResultSet:currentRowCount({self.grtRes}))
  else
    return 0
  end
end


function Query:status()
  if self.grtRes ~= nil then
    return grtV.toLua(RdbmsResultSet:status({self.grtRes}))
  else
    return 0
  end
end


function Query:fieldAsString(index)
  if self.grtRes ~= nil then
    return grtV.toLua(RdbmsResultSet:fieldAsString({self.grtRes, index - 1}))
  else
    return ""
  end
end


function Query:moveNext()
  if self.grtRes ~= nil then
    if grtV.toLua(RdbmsResultSet:moveNext({self.grtRes})) == 1 then
      return true
    else
      return false
    end
  else
    return false
  end
end


function Query:movePrior()
  if self.grtRes ~= nil then
    if grtV.toLua(RdbmsResultSet:movePrior({self.grtRes})) == 1 then
      return true
    else
      return false
    end
  else
    return false
  end
end


function Query:moveFirst()
  if self.grtRes ~= nil then
    if grtV.toLua(RdbmsResultSet:moveFirst({self.grtRes})) == 1 then
      return true
    else
      return false
    end
  else
    return false
  end
end


function Query:moveLast()
  if self.grtRes ~= nil then
    if grtV.toLua(RdbmsResultSet:moveLast({self.grtRes})) == 1 then
      return true
    else
      return false
    end
  else
    return false
  end
end


function Query:columnCount()
  if self.grtRes ~= nil then
    return grtV.getn(self.grtRes.columns)
  else
    return 0
  end
end
