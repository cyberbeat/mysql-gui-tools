REM CommandInterpreter: $(COMSPEC)
@echo off

copy ..\..\mysql-gui-common\library\Lib_Debug\libmysqlx.dll ..\bin\windows\libmysqlx.dll
copy ..\..\mysql-gui-common\library_util\Lib_Debug\libmysqlutil.dll ..\bin\windows\libmysqlutil.dll
copy ..\library\Lib_Debug\libmysqlqb.dll ..\bin\windows\libmysqlqb.dll

if exist ..\bin\windows\msvcr71.dll del ..\bin\windows\msvcr71.dll
copy ..\..\mysql-gui-win-res\lib\windows\msvcr71d.dll ..\bin\windows\msvcr71d.dll

if exist ..\bin\windows\msvcp71.dll del ..\bin\windows\msvcp71.dll
copy ..\..\mysql-gui-win-res\lib\windows\msvcp71d.dll ..\bin\windows\msvcp71d.dll

echo .
