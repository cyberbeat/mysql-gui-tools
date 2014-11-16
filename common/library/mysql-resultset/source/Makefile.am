

noinst_LTLIBRARIES= libsqlrs.la libgrtsqlrs.la

CFLAGS:= $(CFLAGS) $(TEST_CFLAGS)
CXXFLAGS:= $(CXXFLAGS) $(TEST_CFLAGS)

libsqlrs_la_SOURCES=\
	MYXResultSetStorage.cc\
	MYXMySQLResultSetStorage.cc\
	MYXMySQLResultSetSource.cc


libgrtsqlrs_la_SOURCES=myx_sql_resultset.cc

INCLUDES=-I../include -I$(top_srcdir)/library/base-library/include @MYSQL_INCLUDE@\
	 -I$(top_srcdir)/library/generic-runtime/include\
	-I$(top_srcdir)/library/utilities/include\
	 -I$(top_srcdir)/library/utilities/shared_include\
	@GLIB_CFLAGS@ @PCRE_CFLAGS@
