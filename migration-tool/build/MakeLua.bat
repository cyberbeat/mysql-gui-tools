REM CommandInterpreter: $(COMSPEC)
@echo off

copy ..\..\mysql-gui-common\source\lua\*.lua ..\bin\windows\lua\
copy ..\source\scripts\*.* ..\bin\windows\scripts\
copy ..\source\lua\*.* ..\bin\windows\lua\
