@echo off

pushd ..\..\mysql-gui-common\build
call MakeXGrtShellRelease.bat
popd

copy ..\..\mysql-gui-common\bin\windows\XGrtSh.exe ..\bin\windows\.
