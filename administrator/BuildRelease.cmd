@echo off
cls

echo .
echo MySQL Administrator Release Build Script 1.0
echo --------------------------------------------------------------------------------
echo This script will create a runtime image of the MySQL Administrator
echo in a special release folders. All resources, mysql gui libraries and the
echo binary will be build using the release settings.
echo .

call ..\common\build\ParseCommandline.cmd %1 %2 %3 %4 %5 %6 %7 %8 %9

echo Cleaning previous builds...
if exist ..\release\intermediate del ..\release\intermediate\*.* /S /Q 1> nul
if exist ..\release\MySQLAdministrator.exe del ..\release\MySQLAdministrator.exe
if exist ..\release\MySQLSystemTrayMonitor.exe del ..\release\MySQLSystemTrayMonitor.exe
echo .

echo --------------------------------------------------------------------------------
echo Preparing application folder ...
echo --------------------------------------------------------------------------------
pushd build
call PrepareReleaseFolder.cmd
popd
echo.
if errorlevel 1 goto error

if %copyThirdParty% == 1 (
  echo --------------------------------------------------------------------------------
  echo Copying third-party libraries...
  echo --------------------------------------------------------------------------------
  pushd ..\common\build
  call Copy3rdPartyLibraries.cmd ..\..\release
  popd
  echo.
  if errorlevel 1 goto error
)

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

if %compileCommon% == 1 (
  echo Compiling common libraries...
  pushd ..\common\
  call BuildRelease.cmd
  popd
  echo.
  if errorlevel 1 goto error
)

echo ================================================================================
echo Making Administrator library (DLL)
echo --------------------------------------------------------------------------------
pushd library
bdsproj2mak libmysqladmin.bdsproj
make -s -flibmysqladmin.mak clean_Release_Build
make -s -flibmysqladmin.mak Release_Build
popd
echo ================================================================================
echo.
if errorlevel 1 goto error

rem Due to Vista's new 64 bit folder we need to determine the 32bit BDS/Delphi/BCB folder explicitely.
rem This assumes we only deal with 32bit BDS.
if "%ProgramFiles(x86)%" == "" (
  set ApplicationPath="%ProgramFiles%"
)  else (
  set ApplicationPath="%ProgramFiles(x86)%"
)

echo ================================================================================
echo Building Administrator application
echo --------------------------------------------------------------------------------
pushd source\windows
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K+ -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\release -N0..\..\..\release\intermediate -U%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Lib\Indy10";%ApplicationPath%"\borland\bds\4.0\Imports" -O%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";common\obj\windows -R%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";..\..\..\common\source\windows\Unicode\Resources -NSBorland.Vcl -LE..\..\..\release -LN..\..\..\release -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NO..\..\..\release\intermediate -NB..\..\..\release\intermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -U..\..\..\common\source\windows;..\..\..\common\source\windows\Common;..\..\..\common\source\windows\png;..\..\..\common\source\windows\TNT;..\..\..\common\source\windows\VirtualTreeView\Source;..\..\..\common\source\windows\embeddedwb;..\..\..\common\source\windows\UniCodeEditor\Source;..\..\..\common\source\windows\Tools;..\..\..\common\source\windows\Unicode\Source;..\..\..\common\obj\windows;%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -O..\..\..\common\obj\Windows -I..\..\..\common\source\windows\Common --no-config MySQLAdministrator.dpr -Q
echo.
if errorlevel 1 (
  popd
  goto error
)

echo --------------------------------------------------------------------------------
echo Building System Tray Monitor application...
echo --------------------------------------------------------------------------------

cd systemtraymonitor
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K+ -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\release -N0..\..\..\..\release\intermediate -U%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -O%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";common\obj\windows -R%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";..\..\..\..\common\source\windows\Unicode\Resources -NSBorland.Vcl -LE..\..\..\..\release -LN..\..\..\..\release -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NO..\..\..\..\release\intermediate -NB..\..\..\..\release\intermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -U..\..\..\..\common\source\windows;..\..\..\..\common\source\windows\Common;..\..\..\..\common\source\windows\png;..\..\..\..\common\source\windows\TNT;..\..\..\..\common\source\windows\VirtualTreeView\Source;..\..\..\..\common\source\windows\embeddedwb;..\..\..\..\common\source\windows\UniCodeEditor\Source;..\..\..\..\common\source\windows\Tools;..\..\..\..\common\source\windows\Unicode\Source;..\..\..\..\common\obj\windows;%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -O..\..\..\..\common\obj\Windows -I..\..\..\..\common\source\windows\Common;..\..\..\common\source\windows\Unicode\Resources --no-config MySQLSystemTrayMonitor.dpr -Q
popd
echo.

if %cleanUp% == 1 (
  echo --------------------------------------------------------------------------------
  echo Do some housekeeping...
  echo --------------------------------------------------------------------------------
  pushd ..\common\build
  call CleanupRelease.cmd
  popd
)

if not errorlevel 1 goto finish

:error
echo ##### Batch build cannot be fully completed due to an error. #####
goto end

:finish
echo --------------------------------------------------------------------------------
echo Finished Administrator and System Tray Monitor successfully.

:end
echo ================================================================================
