
noinst_LTLIBRARIES = libguiutil.la

CFLAGS:= $(CFLAGS) $(TEST_CFLAGS)
CXXFLAGS:= $(CXXFLAGS) $(TEST_CFLAGS)

INCLUDES= -Wall -Wsign-compare\
         -I../include -I../shared_include \
        @MYSQL_INCLUDE@ @GLIB_CFLAGS@ @PCRE_CFLAGS@

libguiutil_la_SOURCES=\
	myx_util.c\
	myx_util_functions.c\
	myx_xml_util_functions.c\
	myx_international_file.c\
        ../shared_source/myx_shared_util_functions.c


