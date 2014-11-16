@echo off

rem if there is no argument, display usage
if "%1"=="" goto usage

echo Making %1 ...

rem if not exist .\%1 mkdir .\%1
rem if not exist .\%1\LC_MESSAGES mkdir .\%1\LC_MESSAGES

:step1
echo Merge with gui-common.po with tool.po ...
.\bin\msgcat -s -o .\%1\LC_MESSAGES\tmp.po .\%1\LC_MESSAGES\mysql-administrator-template.po .\%1\LC_MESSAGES\mysql-gui-common-template.po
if not errorlevel 1 goto step2
echo An Error occured!
pause
goto endOfScript

:step2
echo Generate binary MO file for %1 ...
.\bin\msgfmt -o .\%1\LC_MESSAGES\default.mo .\%1\LC_MESSAGES\tmp.po
if not errorlevel 1 goto endOfScript
echo An Error occured!
pause
goto endOfScript

:usage
echo Usage: MakeMo LanguageId
echo .
echo Generates a default.mo file from .po files for a given language
echo .

:endOfScript

if exist .\%1\LC_MESSAGES\tmp.po del .\%1\LC_MESSAGES\tmp.po
echo .