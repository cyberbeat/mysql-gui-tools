@echo off

pushd ..\..\mysql-gui-common\build
call MakeGrtShellDebug.bat
popd

copy ..\..\mysql-gui-common\bin\windows\grtsh.exe ..\bin\windows\.
