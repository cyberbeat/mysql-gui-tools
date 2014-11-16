@echo off

rem Helper script to parse the given commands and setting variables used for release builds.
rem These variables can then be used by the caller to determine all requiered actions.
rem Recognized parameters are:
rem /c+- - Compile (+) or do not compile (-) common libraries, default is /c+
rem /3+- - Copy or do not copy third-party libraries (DLLs), default is /3+
rem /r+- - Remove or do not remove intermediate files (libs, objs, dcus etc.) after the build, default is /r+

rem Initialize variables with default values.
set compileCommon=1
set copyThirdParty=1
set cleanUp=1

:loop
if [%1] == [] goto end

if "%1" == "/c-" set compileCommon=0
if "%1" == "/C-" set compileCommon=0
if "%1" == "/3-" set copyThirdParty=0
if "%1" == "/r-" set cleanUp=0

shift
goto :loop

:end
