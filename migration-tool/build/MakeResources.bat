REM CommandInterpreter: $(COMSPEC)
@echo off

cd ..\..\mysql-gui-common\res\windows
echo Building common resource

REM --------------------------------------
brcc32 mysqlcommon.rc
if not errorlevel 1 goto next1
echo An error occured!
pause

:next1
REM --------------------------------------
brcc32 WindowsXP.rc
if not errorlevel 1 goto next2
echo An error occured!
pause

:next2
echo .

REM --------------------------------------
REM --------------------------------------
cd ..\..\..\mysql-migration-tool\res\windows
echo Building application resource

REM --------------------------------------
brcc32 mysqlmigration.rc
if not errorlevel 1 goto endOfScript
echo An error occured!
pause

:endOfScript
echo .
cd ..\..\build
