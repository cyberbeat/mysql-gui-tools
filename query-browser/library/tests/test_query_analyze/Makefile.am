
noinst_PROGRAMS = test_query_analyze

INCLUDES= -I../include -I../public_interface/linux\
        -I$(top_srcdir)/../mysql-gui-common/source/linux\
        -I$(top_srcdir)/../mysql-gui-common/library/include\
        -I$(top_srcdir)/../mysql-gui-common/library/shared_include\
	@MYSQL_CFLAGS@ @GLIB_CFLAGS@ @PCRE_CFLAGS@

test_query_analyze_SOURCES= test_query_analyze.cpp

test_query_analyze_LDADD= $(top_srcdir)/../mysql-gui-common/library/source/libmysqlx.a\
	@MYSQL_LIBS@ @PCRE_LIBS@ @GNOME_LIBS@
