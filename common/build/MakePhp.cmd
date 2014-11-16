REM CommandInterpreter: $(COMSPEC)
@echo off

REM --------------------------------------
echo Clean .class files ...

del /S /Q ..\bin\windows\php 1> nul 2> nul

echo .


copy ..\res\grt\php.ini ..\bin\windows\php\.


REM --------------------------------------
echo Copying Php files...


xcopy ..\source\php\*.php ..\bin\windows\php\ /S /Y /Q /EXCLUDE:xcopy_exclude.txt

if not errorlevel 1 goto endOfScript
echo An error occured!
pause


:endOfScript
echo .

