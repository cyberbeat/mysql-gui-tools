@echo off

rem only build the setup files if there is no upload cmd
if [%1] == [] (

pushd build
call makesetupgpl.cmd
popd

)

if [%2] == [] (set version=INVALID) else (set version=%2)

rem uploade files to build server if parameter specified
if "%1" == "upload" (
echo Uploading setup files to final location...
pushd setup\releases

scp mysql-gui-tools-%version%-win32.msi mysqldev@build.mysql.com:/build/stage/Downloads/MySQLGUITools
ssh mysqldev@build.mysql.com chmod 644 /build/stage/Downloads/MySQLGUITools/mysql-gui-tools-%version%-win32.msi

scp mysql-gui-tools-%version%-win32.msi.md5 mysqldev@build.mysql.com:/build/stage/Downloads/MySQLGUITools
ssh mysqldev@build.mysql.com chmod 644 /build/stage/Downloads/MySQLGUITools/mysql-gui-tools-%version%-win32.msi.md5

scp mysql-gui-tools-noinstall-%version%-win32.zip mysqldev@build.mysql.com:/build/stage/Downloads/MySQLGUITools
ssh mysqldev@build.mysql.com chmod 644 /build/stage/Downloads/MySQLGUITools/mysql-gui-tools-noinstall-%version%-win32.zip

scp mysql-gui-tools-noinstall-%version%-win32.zip.md5 mysqldev@build.mysql.com:/build/stage/Downloads/MySQLGUITools
ssh mysqldev@build.mysql.com chmod 644 /build/stage/Downloads/MySQLGUITools/mysql-gui-tools-noinstall-%version%-win32.zip.md5
)

rem uploade files to support server if parameter specified
if "%1" == "support" (
echo Uploading setup files to support FTP server...
pushd setup\releases

scp mysql-gui-tools-%version%-win32.msi mysqldev@production.mysql.com:/supportftp/pub/mysql/download/gui-tools/
ssh mysqldev@production.mysql.com chmod 644 /supportftp/pub/mysql/download/gui-tools/mysql-gui-tools-%version%-win32.msi

scp mysql-gui-tools-%version%-win32.msi.md5 mysqldev@production.mysql.com:/supportftp/pub/mysql/download/gui-tools/
ssh mysqldev@production.mysql.com chmod 644 /supportftp/pub/mysql/download/gui-tools/mysql-gui-tools-%version%-win32.msi.md5

scp mysql-gui-tools-noinstall-%version%-win32.zip mysqldev@production.mysql.com:/supportftp/pub/mysql/download/gui-tools/
ssh mysqldev@production.mysql.com chmod 644 /supportftp/pub/mysql/download/gui-tools/mysql-gui-tools-noinstall-%version%-win32.zip

scp mysql-gui-tools-noinstall-%version%-win32.zip.md5 mysqldev@production.mysql.com:/supportftp/pub/mysql/download/gui-tools/
ssh mysqldev@production.mysql.com chmod 644 /supportftp/pub/mysql/download/gui-tools/mysql-gui-tools-noinstall-%version%-win32.zip.md5
)

popd