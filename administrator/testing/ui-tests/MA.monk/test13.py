# Name: 20911_missing_privileges
# Target: windows
##

#Delay(2)
#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(53, 83))
#Delay(2)
#Key(Return)
#Delay(2)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(90, 89))
Delay(2)
ClickObject(PATH("MainForm.SidebarPnl.SubTreePnl.SubTreePnl.UserTreeView"), offset=(28, 10))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl"), offset=(134, 8))
Delay(2)

c1 = GetSnoop().listItemPosWithCaption(PATH("MainForm.DockPnl.UserPnl.UserPageControl.GlobalPrivSheet.GlobalAssignedPrivListView"), 0, "CREATE_ROUTINE")
c2 = GetSnoop().listItemPosWithCaption(PATH("MainForm.DockPnl.UserPnl.UserPageControl.GlobalPrivSheet.GlobalPrivListView"), 0, "CREATE_ROUTINE")

a1 = GetSnoop().listItemPosWithCaption(PATH("MainForm.DockPnl.UserPnl.UserPageControl.GlobalPrivSheet.GlobalAssignedPrivListView"), 0, "ALTER_ROUTINE")
a2 = GetSnoop().listItemPosWithCaption(PATH("MainForm.DockPnl.UserPnl.UserPageControl.GlobalPrivSheet.GlobalPrivListView"), 0, "ALTER_ROUTINE")

if (c1 == None) and (c2 == None):
  Abort("privilege CREATE_ROUTINE is not available")

if (a1 == None) and (a2 == None):
  Abort("privilege ALTER_ROUTINE is not available")