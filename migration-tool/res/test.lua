
print("Testing GRT-SH")

-- refresh environment
refresh()

-- get list of object IDs
objs= get_oids()

orcl=nil

-- go over retrieved oids
print("Objects:")
for idx,oid in objs do
	-- print object ID and class
	print("OID: "..oid)
	obj= object(oid)
        print("Class: "..obj._class_)
	print("Contents:")
	for a,b in obj do
		print("   "..a)
	end
        print("")

	if obj._class_ == "OrclReverseEngineering" then
		orcl=obj
	end
end

-- if we have an Oracle rev eng clas
if orcl ~=  nil then
	--schemata= orcl:getSchemata("jdbc:oracle:thin:system/sys@mikesthinkpad:1521:mtt")

        -- print results
        --table.foreachi(schemata, print)

        catalog= orcl:reverseEngineer("jdbc:oracle:thin:system/sys@mikesthinkpad:1521:mtt", {"SCOTT"})
end
