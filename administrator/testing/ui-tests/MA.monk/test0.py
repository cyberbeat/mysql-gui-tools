# Name: init
# Target: windows
##
#MyConnect(MyConnect(VAR("conn")))

ClickObject(PATH("ConnectToInstanceForm.ConnectToHostPnl"), offset=(20, 45))
Delay(1)
Key(Return)
Delay(2)
