REM CommandInterpreter: $(COMSPEC)
@echo off

REM --------------------------------------
echo Clean .class files ...

del /S /Q ..\bin\windows\lua 1> nul 2> nul

echo .



REM --------------------------------------
echo Copying Lua files...


xcopy ..\source\lua\*.lua ..\bin\windows\lua\ /S /Y /Q /EXCLUDE:xcopy_exclude.txt

if not errorlevel 1 goto endOfScript
echo An error occured!
pause


:endOfScript
echo .
