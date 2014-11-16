@echo off

pushd %1

echo Executing batch file in folder:
cd

if not exist bin mkdir bin
if not exist bin\LibInterfaceMapper_Templates mkdir bin\LibInterfaceMapper_Templates
if not exist bin\LibInterfaceMapper_Templates\pascal mkdir bin\LibInterfaceMapper_Templates\pascal

copy ..\..\res\Windows\LibinterfaceMapper\LibInterfaceMapper_Templates\pascal\* bin\LibInterfaceMapper_Templates\pascal

popd

