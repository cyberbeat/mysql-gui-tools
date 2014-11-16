@echo off
@call CScript //nologo test_all.js %*
exit /B %ERRORLEVEL%