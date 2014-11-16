@echo off

echo Cleaning up after release build...
del ..\..\release\*.lib /Q > nul 2> nul
del ..\..\release\*.tds /Q > nul 2> nul
rmdir ..\..\release\intermediate /S /Q > nul 2> nul
