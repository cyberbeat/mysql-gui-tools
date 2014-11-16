@echo off

pushd ..\..\mysql-gui-common\build
call MakeGrtShellRelease.bat
popd

copy ..\..\mysql-gui-common\bin\windows\grtsh.exe ..\bin\windows\.
