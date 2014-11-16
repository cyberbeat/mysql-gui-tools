pushd ..\source\windows\SetVersion

dcc32 SetVersion.dpr
if not errorlevel 1 goto endOfScript

echo Error while building MySQL SetVersion
pause

:endOfScript
popd

