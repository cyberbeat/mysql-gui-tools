# Name: 9866_username_16_char_limit
# Target: windows
##

MyConnect({"user":"root","host":"localhost"})

#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(0, 56))
#Delay(2)
#Key(Return)
#Delay(2)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(81, 83))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.NewUserBtn"), offset=(54, 8))
Delay(2)
Type('n0123456789012345')
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.ApplyChangesBtn"), offset=(57, 12))
Delay(2)
#ClickObject(PATH("TMyxModalDialog_01D351F0.TTntButton_01DDA630"), offset=(13, 14))
Key(Tab)
Delay(2)
Key(Return)
#ClickObject(PATH("TMyxModalDialog_01D351F0.TTntButton_01DDA630"), offset=(25, 12))
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.UserPageControl.UserInfoSheet.UserInfoScrollBox.UserInfoLoginGBox"), offset=(217, 33))
Delay(2)
Key(BackSpace)
Delay(2)
ClickObject(PATH("MainForm.DockPnl.UserPnl.Panel3.BottomBtnPnl.ApplyChangesBtn"), offset=(17, 7))

CheckDBQuery("select count(*) from mysql.user where User = 'N012345678901234'", [[1]]) 
MyQuery("drop user 'N012345678901234'")
