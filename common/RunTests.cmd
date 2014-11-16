@echo off

pushd build
call MakeTestApplications.cmd
popd
if errorlevel 1 goto error

echo.
echo ================================================================================
echo Running all tests using Server = 10.100.1.192, Port = 3309
date /t
time /t
echo ================================================================================

echo.
echo --------------------------------------------------------------------------------
echo Common tests
echo --------------------------------------------------------------------------------
pushd library\testing\bin
for %%x in (*.exe) do %%x --host=10.100.1.192 --port=3309 --data=..\test-data --data=..\..\..\res --data=..\..\unit-tests\test-data
popd
echo.
if errorlevel 1 goto error
pushd source\windows\testing\General\bin
for %%x in (*.exe) do %%x --host=10.100.1.192 --port=3309 --data=..\test-data --data=..\..\..\res --data=..\..\unit-tests\test-data
popd
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo MySQL Administrator tests
echo --------------------------------------------------------------------------------
pushd ..\administrator\testing\test-application\bin
for %%x in (*.exe) do %%x --host=10.100.1.192 --port=3309 --data=..\test-data --data=..\..\..\res --data=..\..\unit-tests\test-data
popd
if errorlevel 1 goto error

echo.
echo --------------------------------------------------------------------------------
echo MySQL Query Browser tests
echo --------------------------------------------------------------------------------
pushd ..\query-browser\testing\test-application\bin
for %%x in (*.exe) do %%x --host=10.100.1.192 --port=3309 --data=..\test-data --data=..\..\..\res --data=..\..\unit-tests\test-data
popd
if not errorlevel 1 goto finish

:error
echo An Error occured!

:finish

echo.
echo ================================================================================
time /t
echo Finished testing
echo ================================================================================
