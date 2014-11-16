
SUBDIRS=library source res images po

mysqlqb_in_files= MySQLQueryBrowser.desktop.in
mysqlqbdatadir= $(datadir)/applications
mysqlqbdata_DATA=MySQLQueryBrowser.desktop

DISTCLEANFILES=MySQLQueryBrowser.desktop


MySQLQueryBrowser.desktop: MySQLQueryBrowser.desktop.in config.status
	sed -e s@#mydatadir#@$(mydatadir)@g -e s@#bindir#@$(bindir)@g MySQLQueryBrowser.desktop.in > $@

