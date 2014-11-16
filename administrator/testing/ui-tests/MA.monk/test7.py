# Name: 17739_view_drop_on_edit
# Target: windows
##

MyConnect({"user":"root","host":"localhost"})

MyQuery("DROP VIEW IF EXISTS `test`.`v1`")
MyQuery("CREATE VIEW `test`.`v1` AS SELECT 1")

#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(46, 61))
#Delay(1)
#Key(Return)
#Delay(3)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(54, 257))
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(31, 47))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl"), offset=(197, 10))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.ViewsTabSheet.Panel15.Panel16.RefreshViewsBtn"), button=1, offset=(21, 13))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.ViewsTabSheet.ViewsVST"), offset=(79, 21))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.ViewsTabSheet.Panel15.Panel16.EditViewBtn"), offset=(76, 10))
Delay(2)
ClickObject(PATH("EditorSqlForm.SqlUCE"), offset=(494, 5))
Delay(2)
Key(Return)
Delay(2)
ClickObject(PATH("EditorSqlForm.SqlUCE"), offset=(236, 19))
Delay(1)
Type('1')
ClickObject(PATH("EditorSqlForm.BottomPnl.ExecuteSQLBtn"), offset=(14, 11))
Delay(1)
Key(Return)
Delay(1)

CheckDBQuery("select count(*) from information_schema.views where table_schema = 'test' and table_name='v1'", [[1]]) 
CheckDBQuery("select count(*) from information_schema.views where table_schema = 'test' and table_name='v11'", [[1]])

MyQuery("DROP VIEW `test`.`v1`")
MyQuery("DROP VIEW `test`.`v11`")

