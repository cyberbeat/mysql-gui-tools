@echo off

pushd ..\setup\windows\

if not exist ..\releases mkdir ..\releases

rem Set SETUP_VERSION
if not exist set_source_dir.cmd goto ERROR3
call set_source_dir.cmd

rem override SOURCE_DIR
Set SOURCE_DIR="..\..\..\release"

echo Creating noinstall.zip ...
pushd ..\..\..

rem rename directory
rename release "MySQL GUI Tools %SETUP_VERSION_MAIN%"
if not exist "MySQL GUI Tools %SETUP_VERSION_MAIN%" goto ERROR4

del mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win-noinstall.zip 1> nul 2> nul
win-external-libs\bin\zip -r9 mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win-noinstall.zip "MySQL GUI Tools %SETUP_VERSION_MAIN%"\*
move mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win-noinstall.zip common\setup\releases\mysql-gui-tools-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip 1> nul

rename "MySQL GUI Tools %SETUP_VERSION_MAIN%" release

popd

:MSI

rem check if source_dir exists
if not exist %SOURCE_DIR% goto ERROR2

echo Creating the GPL version of the msi file...
rem Cleaning is necessary because *.wixobj files must be remade if the license type changes.
make /NOLOGO -f Makefile.mak clean
make /NOLOGO -f Makefile.mak LICENSE_TYPE=gpl all

if %errorlevel% == 1 (
  echo Building the setup-files failed. Error messages should have been provided above.
) else (
  echo Build was successful. You can find the generated files in the bin\dist\ directory.
)

echo Moving msi file to setup\releases\ ...
move mysql_gui_tools.msi ..\releases\mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win32.msi 1> nul

echo Generating md5sums ...
pushd ..\releases
..\..\..\win-external-libs\bin\md5sum.exe mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win32.msi > mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win32.msi.md5
..\..\..\win-external-libs\bin\md5sum.exe mysql-gui-tools-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip > mysql-gui-tools-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip.md5
popd


popd

EXIT /B 0

:ERROR2
echo The release dir cannot be found.
popd
pause
EXIT /B 1

:ERROR3
echo You have to call the SetVersion.cmd batch file in the repos root to set the version correctly.
popd
pause
EXIT /B 1

:ERROR4
echo The bin directory cannot be renamed
popd
pause
EXIT /B 1
