@echo off

rem Copy WB library to the application output path (here the work bench), to allow using it for debugging.
copy Lib_Debug\libmysqlgrtwbmodule.dll ..\..\mysql-workbench\bin\windows\libmysqlgrtwbmodule.dll

rem Copy WB library to the temporary output folder too, so it is available for Delphi packages.
rem copy Lib_Debug\libmysqlgrtwbmodule.dll ..\..\Output\libmysqlgrtwbmodule.dll