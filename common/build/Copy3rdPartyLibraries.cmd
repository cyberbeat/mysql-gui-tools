@echo off

rem Copies all third-party libraries (DLLs) used by our tools
rem Parameter %1 contains the target path (..\..\Output is default)

if [%1] == [] (
  set targetPath=..\..\output\
) else (
  set targetPath=%1\
)

if not exist %targetPath%libglib-2.0-0.dll copy ..\..\win-external-libs\lib\glib-2.0\libglib-2.0-0.dll %targetPath%
if not exist %targetPath%libgthread-2.0-0.dll copy ..\..\win-external-libs\lib\glib-2.0\libgthread-2.0-0.dll %targetPath%
if not exist %targetPath%intl.dll copy ..\..\win-external-libs\lib\glib-2.0\intl.dll %targetPath%

if not exist %targetPath%iconv.dll copy ..\..\win-external-libs\lib\libxml\iconv.dll %targetPath%
if not exist %targetPath%libxml2.dll copy ..\..\win-external-libs\lib\libxml\libxml2.dll %targetPath%
if not exist %targetPath%zlib.dll copy ..\..\win-external-libs\lib\libxml\zlib.dll %targetPath%

if not exist %targetPath%libexpat.dll copy ..\..\win-external-libs\lib\expat\libexpat.dll %targetPath%

if not exist %targetPath%libfcgi.dll copy ..\..\win-external-libs\lib\fcgi\libfcgi.dll %targetPath%

if not exist %targetPath%libmySQL.dll copy ..\..\win-external-libs\lib\mysql\opt\libmysql.dll %targetPath%

if not exist %targetPath%libpng12.dll copy ..\..\win-external-libs\lib\libpng\libpng12.dll %targetPath%

if not exist %targetPath%lua50.dll copy ..\..\win-external-libs\lib\lua\lua50.dll %targetPath%

if not exist %targetPath%pcre3.dll copy ..\..\win-external-libs\lib\pcre\pcre3.dll %targetPath%

if not exist %targetPath%python24.dll copy ..\..\win-external-libs\lib\python\python24.dll %targetPath%

if not exist %targetPath%zlib1.dll copy ..\..\win-external-libs\lib\zlib\zlib1.dll %targetPath%

if not exist %targetPath%freetype6.dll copy ..\..\win-external-libs\lib\freetype2\freetype6.dll %targetPath%

if not exist %targetPath%php5ts.dll copy ..\..\win-external-libs\lib\php-5.0.3\php5ts.dll %targetPath%

rem Finally a few system and Borland DLLs.
if not exist %targetPath%cc3270.dll copy ..\..\win-external-libs\lib\windows\cc3270.dll %targetPath%
if not exist %targetPath%cc3270mt.dll copy ..\..\win-external-libs\lib\windows\cc3270mt.dll %targetPath%
if not exist %targetPath%msvcp71.dll copy ..\..\win-external-libs\lib\windows\msvcp71.dll %targetPath%
if not exist %targetPath%msvcr71.dll copy ..\..\win-external-libs\lib\windows\msvcr71.dll %targetPath%

rem Since it does not fit elsewhere copy also the COPYING file here.
if not exist %targetPath%COPYING copy ..\COPYING %targetPath%
