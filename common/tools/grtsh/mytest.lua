
-- list registered modules to screen
print("List of modules")
lsm()

-- print contents of module to screen
print("Showing module mysql")
showm("mysql")


-- connection information for MySQL
conn= {username='root',password='a',hostname='localhost'}

-- setup the parameter to be passed. the parameter of functions are dictionaries that
-- contain function specific items. getSchemaList only needs connection_info
arg= {connect_info=conn}

-- call the function from the module mysql
sl= mysql:getSchemaList(arg)

-- sl= mysql:getSchemaList::({connect_info={username='root',password='root',hostname='localhost'}})

-- print the result, which is also a dictionary. including when an error happens
print("Schemas in mysql server:")
show(sl)

-- setup parameter again.
arg= {connect_info=conn, catalog='def', schema='mysql'}

-- call function
assets= mysql:getSchemaAssets(arg)

-- save the result to a file
save(assets, "assets.xml")

-- load the content of the file back
tmp= load("assets.xml")

-- save again, so we can compare to see if everything is ok
save(tmp, "assets2.xml")

-- convert the GRT_VALUE to a lua table
tbl= unwrap(assets)

-- set some entries from the returned assets in the root object
setobj("/tables", getobj(assets, "/tables"))
setobj("/views", getobj(assets, "/views"))


-- save state
save(getobj("/"), "root.xml")


-- print some stuf
print("Definition of Column User is:")
show(getobj("/tables/user/columns/User"))
print("Definition of Column Host is:")
show(getobj("/tables/user/columns/Host"))



-- some stuff to make navigation easier..

-- "enter" the tables object
cd("/tables")
-- list contents
print("Contents of", pwd())
ls()
-- "enter" the user table's column list. could also be cd("/tables/user/columns")
cd("user/columns")
-- show contents
print("Contents of", pwd())
ls()
-- show value of one 
print("Follows contents of User")
show(getobj("User"))

-- register structs in file
stload("structs_base.xml")

stload("structs_db_base.xml")

-- assign a struct to a dict
usercol= getobj("User")

stset(usercol, "MyxColumn")

print("Assigned struct",stget(usercol),"to object User")
print("Validating...")
if stvalidate(usercol, stget(usercol), 0) ~= 0 then
	print("Validated OK")
else
	print("Validation Error")
end

-- same as above
stvalidate(usercol, 0)

