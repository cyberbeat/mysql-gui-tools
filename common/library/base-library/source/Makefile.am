
noinst_LTLIBRARIES = libmysqlx.la

CFLAGS:= $(CFLAGS) -Wall -Wsign-compare $(TEST_CFLAGS)
CXXFLAGS:= $(CXXFLAGS) -Wall $(TEST_CFLAGS)


INCLUDES=-I../include \
	-I$(top_srcdir)/library/utilities/shared_include\
	-I$(top_srcdir)/library/utilities/include\
	-I$(top_srcdir)/library/sql-parser/include\
	@MYSQL_INCLUDE@ @GLIB_CFLAGS@ @PCRE_CFLAGS@

libmysqlx_la_SOURCES=\
	myx_exporter.c\
	myx_catalogs.c\
	myx_library.c\
	myx_network.c\
	myx_database_model.c\
	myx_recordset.c\
	myx_query.cpp\
	myx_shell.c\
	myx_ser_aux_functions.c\
	myx_xml_option_functions.c\
	myx_xml_user_connections.c\
	myx_sql_highlighting.c\
        myx_simple_sql_parsing.c\
	myx_xml_charset.c\
	myx_xml_datatype.c
        
