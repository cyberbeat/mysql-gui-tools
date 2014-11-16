@echo off

call prepare_java.cmd

cd build
call MakeGrtShellRelease.bat
call MakeXGrtShellRelease.bat

call makesetupgpl.bat
cd ..
