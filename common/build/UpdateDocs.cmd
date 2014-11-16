@echo off

rem Since the docs won't be updated anymore and the setup to get them is not transparent (publication of the repositories is planned for Launchpad)
rem we just copy the (manually transferred) docs to the release folder.
rem scp docbuild@docsrva.mysql.com:/home/docbuild/docs-built/administrator/en/chm/administrator.chm ..\..\release\MySQLAdministrator.chm
rem scp docbuild@docsrva.mysql.com:/home/docbuild/docs-built/query-browser/en/chm/query-browser.chm ..\..\release\MySQLQueryBrowser.chm
rem scp docbuild@docsrva.mysql.com:/home/docbuild/docs-built/migration-toolkit/en/chm/migration-toolkit.chm ..\..\release\MySQLMigrationTool.chm
rem scp docbuild@docsrva.mysql.com:/home/docbuild/docs-built-confidential/workbench/en/chm/workbench.chm ..\..\release\MySQLWorkbench.chm

copy ..\docs\administrator-en.chm ..\..\release\MySQLAdministrator.chm
copy ..\docs\query-browser-en.chm ..\..\release\MySQLQueryBrowser.chm
copy ..\docs\migration-toolkit-en.chm ..\..\release\MySQLMigrationTool.chm
