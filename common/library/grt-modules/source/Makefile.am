
noinst_LTLIBRARIES= libgrtmods.la

CFLAGS:=$(CFLAGS) -Wno-unknown-pragmas $(TEST_CFLAGS)
CXXFLAGS:=$(CXXFLAGS) -Wno-unknown-pragmas $(TEST_CFLAGS)


INCLUDES=-I$(top_srcdir)/library/generic-runtime/include \
	-I$(top_srcdir)/library/grt-modules/include \
	@GLIB_CFLAGS@ \
	-I$(top_srcdir)/library/utilities/include \
	-I$(top_srcdir)/library/utilities/shared_include \
	-I$(top_srcdir)/library/base-library/include \
	-I$(top_srcdir)/library/sql-parser/include \
	@MYSQL_INCLUDE@ @PCRE_CFLAGS@ @LUA_CFLAGS@\
	@GRT_CFLAGS@ @ENABLED_MODULES@

libgrtmods_la_SOURCES=\
	myx_grt_base.c\
	myx_grt_mysql.c\
	myx_grt_mysql_query.c\
	myx_grt_mysql_reveng.c\
	myx_grt_mysql_reveng_script.c\
	myx_grt_mysql_reveng_script_helper.cpp\
	myx_grt_mysql_transformation.c

#libxgrtmods_a_SOURCES=\
#	myx_grt_forms_linux.c
