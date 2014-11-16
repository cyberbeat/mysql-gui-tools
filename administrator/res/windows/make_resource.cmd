@echo off

brcc32 mysqladmin.rc
rc mysqlsystemtraymonitor.rc
brcc32 MySQLAdministrator.rc

pause
