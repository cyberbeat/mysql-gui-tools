# Name: 16990_wrong_collation
# Target: windows
##

MyConnect({"user":"root","host":"localhost"})

MyQuery("DROP TABLE IF EXISTS `test`.`test_16990`")
MyQuery("CREATE TABLE `test`.`test_16990` (`id` INTEGER PRIMARY KEY, `c` VARCHAR(45) CHARACTER SET cp1251 COLLATE cp1251_ukrainian_ci NOT NULL) ENGINE=MyISAM")

#Delay(2)
#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(53, 83))
#Delay(2)
#Key(Return)
#Delay(2)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(60, 249))
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(36, 47))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.TablesTabSheet.TablesVST"), offset=(181, 24))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.TablesTabSheet.Panel3.EditTableBtn"), offset=(48, 10))
Delay(2)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnVST"), offset=(31, 52))
Delay(2)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl"), offset=(171, 13))
Delay(2)
ClickObject(PATH("EditorTableForm.TableEditorPnl.BottomPnl.CloseBtn"), button=1, offset=(37, 8))
Delay(2)

p = GetSnoop().widgetProperties(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl.TntTabSheet1.ColumnCollateCBox"))

MyQuery("DROP TABLE IF EXISTS `test`.`test_16990`")

if p["Text"] != "cp1251_ukrainian_ci":
  Abort("invalid collation")