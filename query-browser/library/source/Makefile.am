
noinst_LIBRARIES = libqb.a

INCLUDES= -I../include -I../public_interface/linux\
        -I$(top_srcdir)/../common/source/linux\
        -I$(top_srcdir)/../common/library/base-library/include\
        -I$(top_srcdir)/../common/library/utilities/shared_include\
        -I$(top_srcdir)/../common/library/utilities/include\
	@MYSQL_CFLAGS@ @GLIB_CFLAGS@ @PCRE_CFLAGS@

libqb_a_SOURCES=\
	myx_qb_library.c\
        myx_xml_bookmark.c\
        myx_xml_history.c\
        myx_strip_query.cpp\
        myx_xml_functionlist.c\
        myx_script_editor.c\
        myx_script_viewer.c\
	myx_profiling.c
