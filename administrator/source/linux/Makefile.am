
bin_SCRIPTS=mysql-administrator

EXTRA_DIST=mysql-administrator

bin_PROGRAMS=mysql-administrator-bin

mysql_administrator_bin_SOURCES=\
	MGUserBrowserList.cc\
	MInstanceInfo.cc\
	MDataInterface.cc\
	MDynamicInterface.cc\
	MAdministrator.cc\
	MAPanel.cc\
	MAPreferences.cc\
	MAPreferencesEditor.cc\
	util.cc\
	main.cc\
	MAServerInformationPanel.cc\
	MAServiceControlPanel.cc\
	MAUserAdministrationPanel.cc\
	MAStartupParametersPanel.cc\
	MAServerConnectionsPanel.cc\
	MAHealthPanel.cc\
	MAServerLogsPanel.cc\
	MABackupPanel.cc\
	MARestorePanel.cc\
	MAReplicationStatusPanel.cc\
	MACatalogsPanel.cc\
	MADummyPanel.cc


INCLUDES=@GNOME_CFLAGS@ @MYSQL_CFLAGS@ @PCRE_CFLAGS@\
	-I$(top_srcdir)/../mysql-gui-common/source/linux\
	-I$(top_srcdir)/../mysql-gui-common/library/base-library/include\
	-I$(top_srcdir)/../mysql-gui-common/library/utilities/shared_include\
	-I$(top_srcdir)/../mysql-gui-common/library/utilities/include\
	-I$(top_srcdir)/library/include\
	-DPREFIX=\""$(PREFIX)"\"\
        -DDATADIRNAME=\""$(DATADIRNAME)"\"\
	-DCOMMONDIRNAME=\""$(MG_COMMONDIR)"\"\
	-DDOCDIR=\""$(DOCDIR)"\"


# mysql_administrator_bin_LDFLAGS=-pthread

mysql_administrator_bin_LDADD=\
	$(top_srcdir)/library/source/libadmin.a\
	$(top_srcdir)/../mysql-gui-common/source/linux/libmacommongui.a\
	$(top_srcdir)/../mysql-gui-common/library/base-library/source/.libs/libmysqlx.a\
	$(top_srcdir)/../mysql-gui-common/library/sql-parser/source/.libs/libsqlparser.a\
	$(top_srcdir)/../mysql-gui-common/library/utilities/source/.libs/libguiutil.a\
	@MYSQL_LIBS@ @GNOME_LIBS@ @UTIL_LIBS@ @PCRE_LIBS@

