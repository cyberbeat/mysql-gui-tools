@echo off

if not exist ..\bin goto ERROR

cd ..\setup\windows\

rem Set SETUP_VERSION
if not exist set_source_dir.cmd goto ERROR3
call set_source_dir.cmd

rem override SOURCE_DIR
Set SOURCE_DIR="..\..\bin\windows"

rem check if source_dir exists
if not exist %SOURCE_DIR% goto ERROR2

echo Creating the GPL version of the query browser msi file...
rem Cleaning is necessary because *.wixobj files must be remade if the license
rem type changes.
nmake /NOLOGO -f Makefile.mak clean
nmake /NOLOGO -f Makefile.mak LICENSE_TYPE=commercial all
if errorlevel 1 (
  echo Building the setup-files failed. Error messages should have been provided above.
) else (
  echo Build was successful. You can find the generated files in the bin\dist\ directory.
)

echo Moving msi file to bin\dist\ ...
move mysql_query_browser.msi ..\..\bin\dist\mysql-query-browser-com-%SETUP_VERSION%-win.msi 1> nul

echo Generating md5sum ...
cd ..\..\bin\dist
..\..\..\mysql-gui-win-res\bin\md5sum.exe mysql-query-browser-com-%SETUP_VERSION%-win.msi > mysql-query-browser-com-%SETUP_VERSION%-win.msi.md5

cd ..\..\build\

pause
EXIT /B 0


:ERROR
echo An error has occurred.
pause
EXIT /B 1

:ERROR2
echo The source directory %SOURCE_DIR% cannot be found.
pause
EXIT /B 1

:ERROR3
echo You have to call the SetVersion.cmd batch file in the repos root to set the version correctly.
cd ..
pause
EXIT /B 1
