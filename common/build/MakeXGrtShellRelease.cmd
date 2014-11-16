@echo off

pushd ..\source\windows\xgrtsh
copy XGrtShRelease.cfg XGrtSh.cfg
copy XGrtShRelease.dof XGrtSh.dof

dcc32 XGrtSh.dpr
popd
