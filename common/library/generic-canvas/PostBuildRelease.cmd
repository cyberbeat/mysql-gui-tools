@echo off

rem Copy GC library to the application output path (here the work bench), to allow using it for debugging.
copy Lib_Release\libmysqlgc.dll ..\..\mysql-workbench\bin\windows\libmysqlgc.dll

rem Copy GC library to the temporary output folder too, so it is available for Delphi packages.
copy Lib_Release\libmysqlgc.dll ..\..\Output\libmysqlgc.dll