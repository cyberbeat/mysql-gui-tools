@echo off

pushd ..\setup\windows\

if not exist ..\releases mkdir ..\releases

rem Set SETUP_VERSION
if not exist set_source_dir.cmd goto ERROR3
call set_source_dir.cmd

rem override SOURCE_DIR
Set SOURCE_DIR="..\..\..\release"

copy ..\..\res\MySQLEULA.txt %SOURCE_DIR%\. 1> nul 2> nul
copy ..\..\res\MySQLEULA.rtf %SOURCE_DIR%\. 1> nul 2> nul
del %SOURCE_DIR%\COPYING 1> nul 2> nul


:NoInstall

echo Creating noinstall.zip ...
pushd ..\..\..

rem rename directory
rename release "MySQL GUI Tools %SETUP_VERSION_MAIN%"
if not exist "MySQL GUI Tools %SETUP_VERSION_MAIN%" goto ERROR4

del mysql-gui-tools-%SETUP_VERSION_BUNDLE%-win-noinstall.zip 1> nul 2> nul
zip -r9 mysql-gui-tools-com-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip "MySQL GUI Tools %SETUP_VERSION_MAIN%"\*
move mysql-gui-tools-com-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip common\setup\releases\mysql-gui-tools-com-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip 1> nul
echo Zip created.
pause
rename "MySQL GUI Tools %SETUP_VERSION_MAIN%" release

popd

:MSI

rem check if source_dir exists
if not exist %SOURCE_DIR% goto ERROR2

echo Creating the commercial version of the msi file...
rem Cleaning is necessary because *.wixobj files must be remade if the license
rem type changes.
make /NOLOGO -f Makefile.mak clean
make /NOLOGO -f Makefile.mak LICENSE_TYPE=commercial all
if errorlevel 1 (
  echo BUilding the setup-files failed. Error messages should have been provided above.
) else (
  echo Build was successful. You can find the generated files in the common\setup\releases directory.
)

echo Moving msi file to setup\releases\ ...
move mysql_gui_tools.msi ..\releases\mysql-gui-tools-com-%SETUP_VERSION_BUNDLE%-win32.msi 1> nul

echo Generating md5sums ...
pushd ..\releases
..\..\..\win-external-libs\bin\md5sum.exe mysql-gui-tools-com-%SETUP_VERSION_BUNDLE%-win32.msi > mysql-gui-tools-com-%SETUP_VERSION_BUNDLE%-win32.msi.md5
..\..\..\win-external-libs\bin\md5sum.exe mysql-gui-tools-com-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip > mysql-gui-tools-com-noinstall-%SETUP_VERSION_BUNDLE%-win32.zip.md5
popd


:RemoveCommercialFiles

del %SOURCE_DIR%\MySQLEULA.txt 1> nul 2> nul
del %SOURCE_DIR%\MySQLEULA.rtf 1> nul 2> nul
copy ..\..\res\COPYING %SOURCE_DIR%\. 1> nul 2> nul

popd

rem pause
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
