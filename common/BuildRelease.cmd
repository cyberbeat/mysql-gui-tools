@echo off

rem MySQL Common GUI Build Script
rem --------------------------------------------------------------------
rem This script will build all commonly used tools, libraries and
rem Delphi components which are needed to build
rem a MySQL GUI tool. Target folder is ..\Output
rem --------------------------------------------------------------------

rem Due to Vista's new 64 bit folder we need to determine the 32bit BDS/Delphi/BCB folder explicitely.
rem This assumes we only deal with 32bit BDS.
if "%ProgramFiles(x86)%" == "" (
  set ApplicationPath="%ProgramFiles%"
)  else (
  set ApplicationPath="%ProgramFiles(x86)%"
)
rem echo %ApplicationPath%"\borland\bds\4.0\lib"

rem Build Tools only for internal use, so we don't need them in the release folder.
echo --------------------------------------------------------------------------------
echo Building dxgettext.exe
echo --------------------------------------------------------------------------------
pushd tools\dxgettext\
dcc32 -$A8 -$B- -$C+ -$D+ -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L+ -$M- -$N+ -$O- -$P+ -$Q+ -$R+ -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -E..\..\..\Output -N0intermediate -U%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -O%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";common\obj\windows -R%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config dxgettext.dpr -Q
popd
echo.
if errorlevel 1 goto error

echo --------------------------------------------------------------------------------
echo Building MySQLLibInterfaceMapper.exe
echo --------------------------------------------------------------------------------
pushd tools\LibInterfaceMapper\
if not exist bin mkdir bin
if not exist bin\LibInterfaceMapper_Templates mkdir bin\LibInterfaceMapper_Templates
xcopy ..\..\res\windows\LibInterfaceMapper\LibInterfaceMapper_Templates\*.* bin\LibInterfaceMapper_Templates /S /Q /Y
dcc32 -$A8 -$B- -$C+ -$D+ -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L+ -$M- -$N+ -$O- -$P+ -$Q+ -$R+ -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -Ebin -N0intermediate -U%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -O%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";common\obj\windows -R%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config MySQLLibInterfaceMapper.dpr -Q
popd
echo.
if errorlevel 1 goto error

echo --------------------------------------------------------------------------------
echo Building SetVersion.exe
echo --------------------------------------------------------------------------------
pushd tools\setversion\
dcc32 -$A8 -$B- -$C+ -$D+ -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L+ -$M- -$N+ -$O- -$P+ -$Q+ -$R+ -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -E..\..\..\Output -N0intermediate -U%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -O%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports";common\obj\windows -R%ApplicationPath%"\borland\bds\4.0\lib";%ApplicationPath%"\borland\bds\4.0\Imports" -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config setversion.dpr -Q
popd
echo.
if errorlevel 1 goto error

rem C and C++ libraries
echo --------------------------------------------------------------------------------
echo Making C and C++ libraries
echo --------------------------------------------------------------------------------

pushd library\generic-canvas\ftgl\
bdsproj2mak ftgl_static_lib.bdsproj
make -s -fftgl_static_lib.mak clean_Release_Build
make -s -fftgl_static_lib.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\utilities
bdsproj2mak libmysqlutil.bdsproj
make -s -flibmysqlutil.mak clean_Release_Build
make -s -flibmysqlutil.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\sql-parser
bdsproj2mak libmysqlsqlparser.bdsproj
make -s -flibmysqlsqlparser.mak clean_Release_Build
make -s -flibmysqlsqlparser.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\base-library
bdsproj2mak libmysqlx.bdsproj
make -s -flibmysqlx.mak clean_Release_Build
make -s -flibmysqlx.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\generic-canvas
bdsproj2mak libmysqlgc.bdsproj
make -s -flibmysqlgc.mak clean_Release_Build
make -s -flibmysqlgc.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\generic-runtime
bdsproj2mak libmysqlgrt.bdsproj
make -s -flibmysqlgrt.mak clean_Release_Build
make -s -flibmysqlgrt.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\grt-modules
bdsproj2mak libmysqlgrtbuiltinmodule.bdsproj
make -s -flibmysqlgrtbuiltinmodule.mak clean_Release_Build
make -s -flibmysqlgrtbuiltinmodule.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd library\grt-wb-controller
bdsproj2mak libmysqlgrtwbmodule.bdsproj
make -s -flibmysqlgrtwbmodule.mak clean_Release_Build
make -s -flibmysqlgrtwbmodule.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd tools\grtsh
bdsproj2mak grtsh.bdsproj
make -s -fgrtsh.mak clean_Release_Build
make -s -fgrtsh.mak Release_Build
popd
echo.
if errorlevel 1 goto error

goto finish

:error
echo ##### There was an error #####

:finish
echo --------------------------------------------------------------------------------
echo Finished building tools, common libraries and packages.
echo --------------------------------------------------------------------------------
