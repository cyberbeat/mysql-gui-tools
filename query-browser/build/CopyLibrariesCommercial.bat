REM CommandInterpreter: $(COMSPEC)
@echo off

echo Building commercial libraries...
copy ..\..\mysql-gui-common\library\Lib_Commercial\libmysqlx.dll ..\bin\windows\libmysqlx.dll
copy ..\library\Lib_Commercial\libmysqlqb.dll ..\bin\windows\libmysqlqb.dll

if exist ..\bin\windows\msvcr71d.dll del ..\bin\windows\msvcr71d.dll
copy ..\..\mysql-gui-win-res\lib\windows\msvcr71.dll ..\bin\windows\msvcr71.dll

if exist ..\bin\windows\msvcp71d.dll del ..\bin\windows\msvcp71d.dll
copy ..\..\mysql-gui-win-res\lib\windows\msvcp71.dll ..\bin\windows\msvcp71.dll

echo .
