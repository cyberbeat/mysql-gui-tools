# Name: 6768_schema_privileges
# Target: windows
##
#MyConnect(MyConnect(VAR("conn")))

MyConnect({"user":"root","host":"localhost","port":3307})
Delay(2)

MyQuery("DROP DATABASE IF EXISTS test1")


#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(20, 45))
#Delay(1)
#Key(Return)
#Delay(2)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(58, 252))

#Delay(1)
#ClickAt((290, 573), button=3)
#Delay(2)
#ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(167, 237))
#Delay(1)

MouseRelease((314, 879), button=1)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(95, 119), button=3)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(124, 189))
Delay(1)

Type('test1')
Key(Return)
Delay(2)
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(77, 86))
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(35, 8))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl"), offset=(217, 13))
Delay(3)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivSchemataFrame.CatalogVST"), offset=(24, 81))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAvailListView"), offset=(39, 6))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.AssignSchemaPrivBtn"), offset=(10, 12))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.ApplyChangesBtn"), offset=(52, 12))
Delay(2)
CheckDBQuery("select count(*) from mysql.db where db = 'test1'", [[1]]) 
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(56, 262))
Delay(2)


ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(36, 67), button=3)
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(64, 92))
Delay(2)

#DoubleClickAt((253, 472), button=3)
#Delay(2)
#ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(69, 94))
#Delay(2)

Key(Return)
Delay(2)
Key(Return)
Delay(2)
Key(Return)
Delay(2)
CheckDBQuery("select count(*) from mysql.db where db = 'test1'", [[0]]) 
