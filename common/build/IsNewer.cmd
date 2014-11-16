@echo off

rem Script to check two files if the first is newer than the second one.
rem If the the first one does not exists or is not newer then errorlevel gets 0 on exit.
rem If the first file is newer or the second file does not exist then errorlevel gets 1 on exit.

if not exist %1 (
  set errorlevel=0
  goto finish
)

for /F "eol=; tokens=1,2,3,4,5 delims=.: " %%i in ("%~t1") do (
  set date1=%%k%%j%%i%%l%%m
)

if not exist %2 (
  set errorlevel=1
  goto finish
)

for /F "eol=; tokens=1,2,3,4,5 delims=.: " %%i in ("%~t2") do (
  set date2=%%k%%j%%i%%l%%m
)

if "%date1%" gtr "%date2%" (
  set errorlevel=1
) else (
  set errorlevel=0
)

:finish
