
SUBDIRS=library source res images po

mysqladmin_in_files= MySQLAdministrator.desktop.in
mysqladmindatadir= $(datadir)/applications
mysqladmindata_DATA=MySQLAdministrator.desktop

DISTCLEANFILES=MySQLAdministrator.desktop

MySQLAdministrator.desktop: MySQLAdministrator.desktop.in config.status
	sed -e s@#mydatadir#@$(mydatadir)@g -e s@#bindir#@$(bindir)@g MySQLAdministrator.desktop.in > $@

