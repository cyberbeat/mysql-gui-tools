@echo off
rem Prerequesites:
rem You need to have msival2 installed and 
rem in your path.
rem
rem The following two locations need to be adapted
rem to your system

set MERGE_CUB=C:\Programme\MsiVal2\mergemod.cub
set ALL_CUB=C:\Programme\MsiVal2\darice.cub

rem The following variables normally do not need to be
rem changed.
set MSM_GPL=..\bin\dist\mysql_administrator_gpl.msm
set MSM_COM=..\bin\dist\mysql_administrator_commercial.msm
set MSI_GPL=..\bin\dist\mysql_administrator_gpl.msi
set MSI_COM=..\bin\dist\mysql_administrator_commercial.msi

if exist ..\bin\dist goto DirExists
echo "Error: The directory ..\bin\dist does not exist!"
pause
exit /b 1

:DirExists

if not exist %MSM_GPL% goto test2
echo Testing %MSM_GPL%...
msival2 %MSM_GPL% %MERGE_CUB% -f
if errorlevel 1 (
  echo Some tests failed. Error/Warning messages should have been provided above.
) else (
  echo All tests passed.
)

:test1
if not exist %MSM_COM% goto test3
echo Testing %MSM_COM%...
msival2 %MSM_COM% %MERGE_CUB% -f
if errorlevel 1 (
  echo Some tests failed. Error/Warning messages should have been provided above.
) else (
  echo All tests passed.
)

:test3
if not exist %MSI_GPL% goto test4
echo Testing %MSI_GPL%...
msival2 %MSI_GPL% %ALL_CUB% -f
if errorlevel 1 (
  echo Some tests failed. Error/Warning messages should have been provided above.
) else (
  echo All tests passed.
)

:test4
if not exist %MSI_COM% goto end
echo Testing %MSI_COM%...
msival2 %MSI_COM% %ALL_CUB% -f)
if errorlevel 1 (
  echo Some tests failed. Error/Warning messages should have been provided above.
) else (
  echo All tests passed.
)

:end
pause