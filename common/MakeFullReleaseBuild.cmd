@echo off

echo ================================================================================
echo MySQL Release Prepare Build Script
echo ================================================================================
echo This script will build all commonly used tools, libraries and
echo applications needed for the setup.
echo Output folder is ..\release
echo ================================================================================

echo Cleaning previous builds ...
if exist ..\release rmdir ..\release /Q /S

pushd ..\administrator\
call BuildRelease.cmd /c+ /3+ /r-
popd
if errorlevel 1 goto error

pushd ..\query-browser\
call BuildRelease.cmd /c- /3- /r-
popd
if errorlevel 1 goto error

pushd ..\migration-tool\
call BuildRelease.cmd /c- /3- /r-
popd
if errorlevel 1 goto error

rem pushd ..\workbench\
rem call BuildRelease.cmd /c- /3- /r+
rem popd
rem if errorlevel 1 goto error

pushd build
call UpdateDocs.cmd
popd

goto finish

:error
echo --------------------------------------------------------------------------------
echo ##### Making setup folder aborted due to an error #####
goto end

:finish
echo --------------------------------------------------------------------------------
echo Successfully finished making setup folder.

:end
echo ================================================================================
