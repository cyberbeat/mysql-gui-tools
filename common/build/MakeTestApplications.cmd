@echo off

echo ================================================================================
echo Building all tests...
date /t
time /t
echo ================================================================================

echo.
echo --------------------------------------------------------------------------------
echo Making common static libraries
echo --------------------------------------------------------------------------------
pushd ..\library\sql-parser\
bdsproj2mak libmysqlsqlparser_static.bdsproj
make -s -flibmysqlsqlparser_static.mak clean_Release_Build
make -s -flibmysqlsqlparser_static.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd ..\library\base-library\
bdsproj2mak libmysqlx_static.bdsproj
make -s -flibmysqlx_static.mak clean_Release_Build
make -s -flibmysqlx_static.mak Release_Build
popd
echo.
if errorlevel 1 goto error

pushd ..\library\utilities\
bdsproj2mak libmysqlutil_static.bdsproj
make -s -flibmysqlutil_static.mak clean_Release_Build
make -s -flibmysqlutil_static.mak Release_Build
popd
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Making common test application
echo --------------------------------------------------------------------------------
pushd ..\library\testing\
bdsproj2mak CommonTest.bdsproj
make -s -fCommonTest.mak clean_Release_Build
make -s -fCommonTest.mak Release_Build
popd
echo.
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Compiling common DUnit test application
echo --------------------------------------------------------------------------------
pushd ..\source\windows\testing\General
dcc32 -$A8 -$B- -$C+ -$D- -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L- -$M- -$N+ -$O+ -$P+ -$Q+ -$R- -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -$M4096,1048576 -$K4194304 -B -CC -DCONSOLE_TESTRUNNER -Ebin -N0intermediate -U"..\..\Common;..\..\Unicode\Source;..\..\UniCodeEditor\Source;%ProgramFiles%\borland\bds\4.0\Source\DUnit\src;%ProgramFiles%\borland\bds\4.0\lib" -O"..\..\Common;..\..\Unicode\Source;%ProgramFiles%\borland\bds\4.0\Source\DUnit\src;%ProgramFiles%\borland\bds\4.0\lib" -I"..\..\Common;..\..\Unicode\Source;..\..\UniCodeEditor\Source;%ProgramFiles%\borland\bds\4.0\Source\DUnit\src" -R"..\..\Common;..\..\Unicode\Resources;..\..\UniCodeEditor\Source;%ProgramFiles%\borland\bds\4.0\Source\DUnit\src;%ProgramFiles%\borland\bds\4.0\lib" -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W+ -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST -I..\..\Common --no-config GeneralDUnitTest.dpr
popd
echo.
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Making MySQL Administrator static library
echo --------------------------------------------------------------------------------
pushd ..\..\administrator\library\
bdsproj2mak libmysqladmin_static.bdsproj
make -s -flibmysqladmin_static.mak clean_Release_Build
make -s -flibmysqladmin_static.mak Release_Build
popd
echo.
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Compiling MySQL Administrator test application
echo --------------------------------------------------------------------------------
pushd ..\..\administrator\testing\test-application\
bdsproj2mak MySQLAdministratorTest.bdsproj
make -s -fMySQLAdministratorTest.mak clean_Release_Build
make -s -fMySQLAdministratorTest.mak Release_Build
popd
echo.
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Making MySQL Query Browser static library
echo --------------------------------------------------------------------------------
pushd ..\..\query-browser\library\
bdsproj2mak libmysqlqb_static.bdsproj
make -s -flibmysqlqb_static.mak clean_Release_Build
make -s -flibmysqlqb_static.mak Release_Build
popd
echo.
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Compiling MySQL Query Browser test application
echo --------------------------------------------------------------------------------
pushd ..\..\query-browser\testing\test-application\
bdsproj2mak MySQLQueryBrowserTest.bdsproj
make -s -fMySQLQueryBrowserTest.mak clean_Release_Build
make -s -fMySQLQueryBrowserTest.mak Release_Build
popd
echo.
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo Compiling MySQL Query Browser DUnit test application
echo --------------------------------------------------------------------------------
pushd ..\..\query-browser\testing\test-application\
dcc32 -$A8 -$B- -$C+ -$D+ -$E- -$F- -$G+ -$H+ -$I+ -$J- -$K+ -$L+ -$M- -$N+ -$O- -$P+ -$Q+ -$R+ -$S- -$T- -$U- -$V+ -$W+ -$X+ -$Y+ -$Z4 -H+ -W -$M4096,1048576 -$K4194304 -B -CC -Ebin -N0intermediate -U"%ProgramFiles%\borland\bds\4.0\lib";"%ProgramFiles%\borland\bds\4.0\Imports" -O"%ProgramFiles%\borland\bds\4.0\lib";"%ProgramFiles%\borland\bds\4.0\Imports";common\obj\windows -R"%ProgramFiles%\borland\bds\4.0\lib";"%ProgramFiles%\borland\bds\4.0\Imports" -NSBorland.Vcl -LEintermediate -LNintermediate -AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE; -NOintermediate -NBintermediate -W-UNSAFE_TYPE -W-UNSAFE_CODE -W-UNSAFE_CAST  --no-config MySQLQueryBrowserDUnitTest.dpr -Q
popd
echo.
if errorlevel 1 goto error

echo ================================================================================
echo Finished building all tests...
date /t
time /t
echo ================================================================================
echo.

:error
