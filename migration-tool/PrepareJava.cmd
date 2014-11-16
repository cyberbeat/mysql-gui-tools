@echo off

pushd ..\common\build
call PrepareJavaCode.cmd
if errorlevel 1 goto finished

call MakeJava.cmd

:finished
popd