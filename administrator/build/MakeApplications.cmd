@echo off

rem Starting point here is the build directory. The components must be built already.

echo .
echo ========================================================================================
echo Creating MySQL Administrator application...
echo Working dir is:
chdir
echo ========================================================================================

echo.
echo ### Bulding MySQL System Tray Monitor...
del ..\..\Output\*.dcu /S /Q 1> nul
echo.
pushd ..\source\windows\systemtraymonitor
dcc32 MySQLSystemTrayMonitor.dpr
popd

echo.
echo ### Bulding MySQL Administrator...
del ..\..\Output\*.dcu /S /Q 1> nul
echo.
pushd ..\source\windows
dcc32 MySQLAdministrator.dpr
popd

echo ========================================================================================
echo Finished building MySQL Administrator application.
echo ========================================================================================
echo .
