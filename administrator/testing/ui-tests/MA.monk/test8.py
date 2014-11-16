# Name: 17650_index_duplication
# Target: windows
##

MyConnect({"user":"root","host":"localhost"})

MyQuery("DROP TABLE IF EXISTS `test`.`test_17650`")
MyQuery("CREATE TABLE `test`.`test_17650` (id INT)")

#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(105, 5))
#Delay(1)
#Key(Return)
#Delay(2)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(40, 255))
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.CatalogSubTreePnl.SchemataFrame.CatalogVST"), offset=(28, 48))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl"), offset=(66, 11))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.TablesTabSheet.Panel3.RefreshBtn"), button=1, offset=(24, 11))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.TablesTabSheet.TablesVST"), offset=(52, 33))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.CatalogPnl.CatalogPageControl.SchemaSheet.SchemaPageControl.TablesTabSheet.Panel3.EditTableBtn"), offset=(11, 10))
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnVST"), offset=(67, 32))
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl"), button=1, offset=(27, 11))
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl.TntTabSheet2.AddIndexBtn"), offset=(6, 8))
Delay(1)
Key(Return)
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl.TntTabSheet2.TntGroupBox1.AddIndexColumnBtn"), offset=(8, 10))
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.BottomPnl.ApplyChangesBtn"), offset=(67, 12))
Delay(1)
Key(Return)
Delay(2)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnVST"), offset=(60, 28))
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl.TntTabSheet2.AddIndexBtn"), offset=(11, 9))
Delay(1)
Key(Return)
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.GirdOptionPnl.MainPageControl.ColumnTabSheet.ColumnsPageControl.TntTabSheet2.TntGroupBox1.AddIndexColumnBtn"), offset=(5, 8))
Delay(1)
ClickObject(PATH("EditorTableForm.TableEditorPnl.BottomPnl.ApplyChangesBtn"), offset=(67, 12))
Delay(1)
Key(Return)
Delay(2)

CheckDBQuery("select count(*) from information_schema.statistics where table_schema='test' and table_name='test_17650' and index_name='Index_1'", [[1]]) 
CheckDBQuery("select count(*) from information_schema.statistics where table_schema='test' and table_name='test_17650' and index_name='Index_2'", [[1]]) 


MyQuery("DROP TABLE `test`.`test_17650`")

