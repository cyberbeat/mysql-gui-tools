# Name: 14919_priv_table_escape
# Target: windows
##

MyConnect({"user":"root","host":"localhost"})

#MouseRelease((-774, 370), button=1)
#Delay(1)
#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(29, 94))
#Key(Return)
#Delay(1)

MyQuery("DROP DATABASE IF EXISTS test_test")

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(36, 253))
Delay(2)

#ClickAt((263, 520), button=3)
#Delay(1)
#ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(120, 177))
#Delay(1)

MouseRelease((314, 879), button=1)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(95, 119), button=3)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(124, 189))
Delay(1)

Type('test')
KeyPress(16)
Type('-')
KeyRelease(16)
Type('test')
Key(Tab)
Key(Return)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(61, 82))
Delay(3)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(36, 12))
Delay(3)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl"), offset=(209, 14))
Delay(3)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivSchemataFrame.CatalogVST"), offset=(37, 78))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAvailListView"), offset=(47, 9))
Delay(3)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.AssignSchemaPrivBtn"), offset=(13, 15))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.ApplyChangesBtn"), offset=(77, 8))

CheckDBQuery("SELECT COUNT(*) FROM mysql.db WHERE db = 'test\_test'", [[1]]) 
MyQuery("DROP DATABASE IF EXISTS test_test")
MyQuery("DELETE FROM mysql.db WHERE db = 'test\_test'")