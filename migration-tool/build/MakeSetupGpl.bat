@echo off

rem build jar files
call MakeJars.bat

rem copy modules source, so user can modify it
copy ..\source\java\com\mysql\grt\modules\*.java ..\bin\windows\java\com\mysql\grt\modules\

rem copy eclipse project
copy ..\source\java\res\.classpath ..\bin\windows\java\.
copy ..\source\java\res\.project ..\bin\windows\java\.

rem delete .jar files that must not be shipped
del ..\bin\windows\java\lib\ojdbc14.jar


if not exist ..\bin goto ERROR

if not exist ..\bin\dist mkdir ..\bin\dist

cd ..\setup\windows\

rem Set SETUP_VERSION
if not exist set_source_dir.cmd goto ERROR3
call set_source_dir.cmd

rem override SOURCE_DIR
Set SOURCE_DIR="..\..\bin\windows"

echo Creating noinstall.zip ...
cd ..\..\bin

rem rename directory
rename windows "MySQL Migration Toolkit %SETUP_VERSION_MAIN%"
if not exist "MySQL Migration Toolkit %SETUP_VERSION_MAIN%" goto ERROR4

del mysql-migration-toolkit-%SETUP_VERSION%-win-noinstall.zip 1> nul 2> nul
zip -r9 mysql-migration-toolkit-%SETUP_VERSION%-win-noinstall.zip "MySQL Migration Toolkit %SETUP_VERSION_MAIN%"\*
move mysql-migration-toolkit-%SETUP_VERSION%-win-noinstall.zip dist\mysql-migration-toolkit-noinstall-%SETUP_VERSION%-win32.zip 1> nul

rename "MySQL Migration Toolkit %SETUP_VERSION_MAIN%" windows

cd ..\setup\windows\

rem check if source_dir exists
if not exist %SOURCE_DIR% goto ERROR2

echo Creating the GPL version of the msi file...
rem Cleaning is necessary because *.wixobj files must be remade if the license
rem type changes.
nmake /NOLOGO -f Makefile.mak clean
nmake /NOLOGO -f Makefile.mak LICENSE_TYPE=gpl all
if errorlevel 1 (
  echo BUilding the setup-files failed. Error messages should have been provided above.
) else (
  echo Build was successful. You can find the generated files in the bin\dist\ directory.
)

echo Moving msi file to bin\dist\ ...
move mysql_migration_toolkit.msi ..\..\bin\dist\mysql-migration-toolkit-%SETUP_VERSION%-win32.msi 1> nul

echo Generating md5sums ...
cd ..\..\bin\dist
..\..\..\mysql-gui-win-res\bin\md5sum.exe mysql-migration-toolkit-%SETUP_VERSION%-win32.msi > mysql-migration-toolkit-%SETUP_VERSION%-win32.msi.md5
..\..\..\mysql-gui-win-res\bin\md5sum.exe mysql-migration-toolkit-noinstall-%SETUP_VERSION%-win32.zip > mysql-migration-toolkit-noinstall-%SETUP_VERSION%-win32.zip.md5

cd ..\..\build\

del ..\bin\windows\java\com\mysql\grt\modules\*.java

pause
EXIT /B 0


:ERROR
del ..\bin\windows\java\com\mysql\grt\modules\*.java
echo An error has occurred.
pause
EXIT /B 1

:ERROR2
del ..\bin\windows\java\com\mysql\grt\modules\*.java
echo The source directory %SOURCE_DIR% cannot be found.
pause
EXIT /B 1

:ERROR3
del ..\bin\windows\java\com\mysql\grt\modules\*.java
echo You have to call the SetVersion.cmd batch file in the repos root to set the version correctly.
cd ..
pause
EXIT /B 1

:ERROR4
del ..\bin\windows\java\com\mysql\grt\modules\*.java
echo The bin directory cannot be renamed
cd ..
pause
EXIT /B 1