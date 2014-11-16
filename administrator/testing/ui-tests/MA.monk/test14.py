# Name: 17921_local_infile_setting
# Target: windows
##

#ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(79, 45))
#Delay(1)
#Key(Return)
#Delay(2)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(62, 63))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.StartupParametersPnl.StartupParamsPageControl"), offset=(596, 9))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.StartupParametersPnl.StartupParamsPageControl.StartupParamsPage7.SB7.GBox7_0.SEd7_0_6"), offset=(13, 11))
Delay(1)
Key(BackSpace)
Delay(1)
Type('1')
Delay(1)
ClickObject(PATH("MainForm.DockPnl.StartupParametersPnl.ButtonPnl.ApplyChangesBtn"), offset=(29, 13))
Delay(1)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(80, 37))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.ServiceControlPnl.PageControl.StartStopServiceSheet.ServiceStatusGBox.StopServiceBtn"), offset=(85, 14))
Delay(10)
ClickObject(PATH("MainForm.DockPnl.ServiceControlPnl.PageControl.StartStopServiceSheet.ServiceStatusGBox.StartServiceBtn"), offset=(85, 14))
Delay(10)

MyConnect({"user":"root","host":"localhost"})

CheckDBQuery("SHOW VARIABLES LIKE 'local_infile'", [["local_infile", "ON"]]) 

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(62, 63))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.StartupParametersPnl.StartupParamsPageControl"), offset=(596, 9))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.StartupParametersPnl.StartupParamsPageControl.StartupParamsPage7.SB7.GBox7_0.SEd7_0_6"), offset=(13, 11))
Delay(1)
Key(BackSpace)
Delay(1)
Type('0')
Delay(1)
ClickObject(PATH("MainForm.DockPnl.StartupParametersPnl.ButtonPnl.ApplyChangesBtn"), offset=(29, 13))
Delay(1)

ClickObject(PATH("MainForm.SidebarPnl.AdminTreeView"), offset=(80, 37))
Delay(1)
ClickObject(PATH("MainForm.DockPnl.ServiceControlPnl.PageControl.StartStopServiceSheet.ServiceStatusGBox.StopServiceBtn"), offset=(85, 14))
Delay(10)
ClickObject(PATH("MainForm.DockPnl.ServiceControlPnl.PageControl.StartStopServiceSheet.ServiceStatusGBox.StartServiceBtn"), offset=(85, 14))
Delay(10)

MyConnect({"user":"root","host":"localhost"})

CheckDBQuery("SHOW VARIABLES LIKE 'local_infile'", [["local_infile", "OFF"]]) 
