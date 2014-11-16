
noinst_PROGRAMS = test_strip_sql

INCLUDES= -I../../include -I../../public_interface/linux\
        -I$(top_srcdir)/../common/source/linux\
        -I$(top_srcdir)/../common/library/include\
        -I$(top_srcdir)/../common/library/shared_include\
	@MYSQL_CFLAGS@ @GLIB_CFLAGS@ @PCRE_CFLAGS@

test_strip_sql_SOURCES= test_strip_sql.cpp

test_strip_sql_LDADD= $(top_srcdir)/../mysql-query-browser/library/source/libqb.a\
	$(top_srcdir)/../common/library/source/libmysqlx.a\
	@MYSQL_LIBS@ @PCRE_LIBS@ @GNOME_LIBS@
