@echo off

rem MySQL Common GUI Build Script
rem --------------------------------------------------------------------
rem This script will build all commonly used tools, libraries and
rem Delphi components which are needed to build
rem a MySQL GUI tool. Target folder is ..\Output
rem --------------------------------------------------------------------

echo Clean previous builds ...
del ..\Output\*.~bpl /S /Q 2> NUL
del ..\Output\*.dcp /S /Q 2> NUL

rem Tools
echo =======================================================
echo Building dxgettext.exe
echo -------------------------------------------------------
pushd tools\dxgettext\
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -E..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config dxgettext.dpr -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building MySQLLibInterfaceMapper.exe
echo -------------------------------------------------------
pushd tools\LibInterfaceMapper\
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -Ebin -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config MySQLLibInterfaceMapper.dpr -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building SetVersion.exe
echo -------------------------------------------------------
pushd tools\setversion\
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -E..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config setversion.dpr -Q
popd
echo.
if errorlevel 1 goto error

rem C and C++ libraries
echo =======================================================
echo Making C and C++ libraries
echo -------------------------------------------------------
pushd library\generic-canvas\ftgl\
bdsproj2mak ftgl_static_lib.bdsproj
make -s -fftgl_static_lib.mak clean_Debug_Build
make -s -fftgl_static_lib.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\utilities
bdsproj2mak libmysqlutil.bdsproj
make -s -flibmysqlutil.mak clean_Debug_Build
make -s -flibmysqlutil.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\base-library
bdsproj2mak libmysqlx.bdsproj
make -s -flibmysqlx.mak clean_Debug_Build
make -s -flibmysqlx.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\generic-canvas
bdsproj2mak libmysqlgc.bdsproj
make -s -flibmysqlgc.mak clean_Debug_Build
make -s -flibmysqlgc.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\generic-runtime
bdsproj2mak libmysqlgrt.bdsproj
make -s -flibmysqlgrt.mak clean_Debug_Build
make -s -flibmysqlgrt.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\sql-parser
bdsproj2mak libmysqlsqlparser.bdsproj
make -s -flibmysqlsqlparser.mak clean_Debug_Build
make -s -flibmysqlsqlparser.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\grt-modules
bdsproj2mak libmysqlgrtbuiltinmodule.bdsproj
make -s -flibmysqlgrtbuiltinmodule.mak clean_Debug_Build
make -s -flibmysqlgrtbuiltinmodule.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd library\grt-wb-controller
bdsproj2mak libmysqlgrtwbmodule.bdsproj
make -s -flibmysqlgrtwbmodule.mak clean_Debug_Build
make -s -flibmysqlgrtwbmodule.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

pushd tools\grtsh
bdsproj2mak grtsh.bdsproj
make -s -fgrtsh.mak clean_Debug_Build
make -s -fgrtsh.mak Debug_Build
popd
echo.
if errorlevel 1 goto error

rem Packages
echo =======================================================
echo Building EmbeddedWB package
echo -------------------------------------------------------
pushd source\windows\embeddedwb
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\Output -LN..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config EmbeddedWBPackageD10.dpk -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building PNG package
echo -------------------------------------------------------
pushd source\windows\png
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\Output -LN..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -O..\..\..\obj\windows --no-config pngD10.dpk -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building TNT package
echo -------------------------------------------------------
pushd source\windows\TNT\Packages\d10
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\..\..\Output -LN..\..\..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config TntUnicodeVcl.dpk -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building Virtual Treeview package
echo -------------------------------------------------------
pushd source\windows\VirtualTreeview\Packages\BDS 2006
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\..\..\Output -LN..\..\..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -I..\..\..\Common  --no-config VirtualTreesR.dpk -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building UCE package
echo -------------------------------------------------------
pushd source\windows\UniCodeEditor\Packages\BDS 2006
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\..\..\Output -LN..\..\..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -I..\..\..\Common --no-config UniCodeEditorD10.dpk -Q
popd
echo.
if errorlevel 1 goto error

echo =======================================================
echo Building Tools package
echo -------------------------------------------------------
pushd source\windows\tools
dcc32 -$A8 -$B- -$C- -$D- -$E- -$F- -$G+ -$H+ -$I- -$J- -$K- -$L- -$M- -$N+ -$O+ -$P+ -$Q- -$R- -$S- -$T- -$U- -$V+ -$W- -$X+ -$Y- -$Z1 -H+ -W -$M4096,1048576 -$K4194304 -B -CG -E..\..\..\..\Output -N0intermediate -U%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -O%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports;common\obj\windows -R%ProgramFiles%\borland\bds\4.0\lib;%ProgramFiles%\borland\bds\4.0\Imports -NSBorland.Vcl -LE..\..\..\..\Output -LN..\..\..\..\Output -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -I..\Common --no-config toolsD10.dpk -Q
popd
echo.
if errorlevel 1 goto error

goto finish

:error
echo ##### There was an error #####

:finish
echo -------------------------------------------------------
echo Finished building tools, common libraries and packages.
echo =======================================================
