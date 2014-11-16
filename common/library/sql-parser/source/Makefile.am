

noinst_LTLIBRARIES= libsqlparser.la

CFLAGS:= $(CFLAGS) $(TEST_CFLAGS)
CXXFLAGS:= $(CXXFLAGS) $(TEST_CFLAGS) -Wno-unknown-pragmas

libsqlparser_la_SOURCES=\
	myx_lex_helpers.cpp\
	myx_sql_parser.tab.cc \
	myx_sql_scanner.lex.c \
	MyxSQLTreeItem.cpp\
	MyxStatementParser.cpp


INCLUDES=-I../include -I$(top_srcdir)/library/base-library/include @MYSQL_INCLUDE@\
         -I$(top_srcdir)/library/generic-runtime/include\
        -I$(top_srcdir)/library/utilities/include\
         -I$(top_srcdir)/library/utilities/shared_include\
        @GLIB_CFLAGS@

