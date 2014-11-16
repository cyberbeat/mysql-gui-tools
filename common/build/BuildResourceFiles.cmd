@echo off

rem This script parses the given input file, which must be a Delphi dpr file
rem and builds all resources listed in that file.

echo ----------------------------------------------------------------
echo Compiling resource files...
echo ----------------------------------------------------------------
for /F "eol=/ tokens=1,2,3,4,5 delims=' " %%i in (%~1) do (
  if "%%i" == "{$R" (
    if not "%%j" == "*.res}" (
      echo Doing %%k ...
      brcc32 -fo%%j %%k > nul
      echo.
      if errorlevel 1 goto error
    )
  )
)
goto finish

:error
echo ##### There was an error #####

:finish
echo ----------------------------------------------------------------
echo Finished building resource files
echo ----------------------------------------------------------------
