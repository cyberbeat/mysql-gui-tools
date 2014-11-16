# Name: 22593_available_privs_disappear
# Target: windows
##

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(68, 87))
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(80, 66), button=3)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(105, 79))
Delay(1)
Type('a')
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.ApplyChangesBtn"), offset=(28, 10))
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(84, 163), button=3)
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(102, 174))
Delay(1)
Type('b')
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.ApplyChangesBtn"), offset=(23, 7))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl"), offset=(225, 12))
Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(30, 27))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivSchemataFrame.CatalogVST"), offset=(59, 11))

ok = 0

try:
  ListItemPosAtRow(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAvailListView"), 0)
  ok = ok + 1
except:
  pass

try:
  ListItemPosAtRow(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAssignedListView"), 0)
  ok = ok + 1
except:
  pass

if (ok == 0):
  Abort("Privilege lists are empty")

Delay(1)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(28, 43))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivSchemataFrame.CatalogVST"), offset=(44, 10))
Delay(1)

ok = 0
try:
  ListItemPosAtRow(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAvailListView"), 0)
  ok = ok + 1
except:
  pass

try:
  ListItemPosAtRow(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAssignedListView"), 0)
  ok = ok + 1
except:
  pass

if (ok == 0):
  Abort("Privilege lists are empty")


ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(27, 24))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivSchemataFrame.CatalogVST"), offset=(50, 9))
Delay(1)

ok = 0
try:
  ListItemPosAtRow(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAvailListView"), 0)
  ok = ok + 1
except:
  pass

try:
  ListItemPosAtRow(PATH("MainForm.DockPnl.UserPnl.UserPageControl.SchemataPrivSheet.SchemaPrivAssignedListView"), 0)
  ok = ok + 1
except:
  pass

if (ok == 0):
  Abort("Privilege lists are empty")


MyConnect({"user":"root","host":"localhost"})

MyQuery("DROP USER A") 
MyQuery("DROP USER B") 