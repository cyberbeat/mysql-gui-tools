@echo off
cls

echo .
echo MySQL Administrator Debug Build Script 1.0
echo --------------------------------------------------------------------
echo This script will create a runtime image of the MySQL Administrator
echo in the .\bin directory. All resources, mysql gui libraries and the
echo binary will be build using the debug settings.
echo .

echo Cleaning previous builds...
rmdir bin /S /Q 1> nul
echo .

echo Preparing application folder ...
call PrepareApplicationFolder.cmd
echo.
if errorlevel 1 goto error

rem Compile all resource files.
pushd source\windows
call ..\..\..\common\build\BuildResourceFiles.cmd MySQLAdministrator.dpr
popd
echo.
if errorlevel 1 goto error

pushd source\windows\systemtraymonitor
call ..\..\..\..\common\build\BuildResourceFiles.cmd MySQLSystemTrayMonitor.dpr
popd
echo.
if errorlevel 1 goto error

echo Compiling common libraries...
pushd ..\common\
call BuildDebug.cmd
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Making Administrator library (DLL)
echo -------------------------------------------------------
pushd library
bdsproj2mak libmysqladmin.bdsproj
make -s -flibmysqladmin.mak clean_Debug_Build
make -s -flibmysqladmin.mak Debug_Build
popd
echo =======================================================
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building Administrator application...
echo -------------------------------------------------------
pushd source\windows
dcc32 -$A8 -$B- -$C+ -$D+ -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L+ -$M- -$N+ -$O- -$P+ -$Q+ -$R+ -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\bin\windows -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\Output -LN..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -U..\..\..\common\source\windows;..\..\..\common\source\windows\Common;..\..\..\common\source\windows\png;..\..\..\common\source\windows\TNT;..\..\..\common\source\windows\VirtualTreeView\Source;..\..\..\common\source\windows\embeddedwb;..\..\..\common\source\windows\UniCodeEditor\Source;..\..\..\common\source\windows\Tools;..\..\..\common\obj\windows;%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O..\..\..\common\obj\Windows -I..\..\..\common\source\windows\Common --no-config MySQLAdministrator.dpr -Q
echo.
if errorlevel 1 (
  popd
  goto error
)

echo -------------------------------------------------------
echo Building System Tray Monitor application...
echo -------------------------------------------------------

cd systemtraymonitor
dcc32 -$A8 -$B- -$C+ -$D+ -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L+ -$M- -$N+ -$O- -$P+ -$Q+ -$R+ -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\bin\windows -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\..\Output -LN..\..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -U..\..\..\..\common\source\windows;..\..\..\..\common\source\windows\Common;..\..\..\..\common\source\windows\png;..\..\..\..\common\source\windows\TNT;..\..\..\..\common\source\windows\VirtualTreeView\Source;..\..\..\..\common\source\windows\embeddedwb;..\..\..\..\common\source\windows\UniCodeEditor\Source;..\..\..\..\common\source\windows\Tools;..\..\..\..\common\obj\windows;%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O..\..\..\..\common\obj\Windows -I..\..\..\..\common\source\windows\Common --no-config MySQLSystemTrayMonitor.dpr -Q
popd
echo.

if not errorlevel 1 goto finish

:error
echo ##### Batch build cannot continue due to an error. #####
goto end

:finish
echo -------------------------------------------------------
echo Finished Administrator and System Tray Monitor successfully.

:end
echo =======================================================
