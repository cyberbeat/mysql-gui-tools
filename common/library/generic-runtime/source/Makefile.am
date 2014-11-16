

if BUILD_NEWT
grttextui=libgrttextui.la
endif

pkglibdir=$(libdir)/mysql-gui

noinst_LTLIBRARIES=libgrt.la $(grttextui)

if BUILD_JAVA
pkglib_LTLIBRARIES=libmyx_grt_java.la
endif


if BUILD_NEWT
lua_textdialogs=-DLUA_TEXT_DIALOGS
libgrttextui_la_SOURCES=lua_dialogs.c
endif

if BUILD_FASTCGI
fcgi=myx_grt_fcgi.c
endif

CFLAGS:= $(CFLAGS) $(TEST_CFLAGS)
CXXFLAGS:= $(CXXFLAGS) $(TEST_CFLAGS)

INCLUDES=-I$(top_srcdir)/library/generic-runtime/include @GLIB_CFLAGS@ \
	-I$(top_srcdir)/library/utilities/include\
	-I$(top_srcdir)/library/utilities/shared_include \
	-I$(top_srcdir)/library/generic-runtime/newt \
	@MYSQL_INCLUDE@ @PCRE_CFLAGS@ @LUA_CFLAGS@ \
	@GRT_CFLAGS@ @ENABLED_MODULES@ \
	$(lua_textdialogs)


libgrt_la_SOURCES=myx_grt.c\
	myx_grt_module.c\
	myx_grt_value.c\
	myx_grt_private.c\
        myx_grt_java.c\
	myx_grt_builtin.c\
	myx_grt_struct.c\
	myx_grt_shell.c\
        myx_grt_lua.c\
        myx_grt_lua_shell.c\
        myx_grt_lua_shell_help.c\
	lxp/lxplib.c\
	myx_grt_php.c\
	myx_grt_python.c\
	myx_grt_python_common.c\
	myx_grt_python_shell.c\
	myx_grt_agent.cc\
	MNServerSocket.cc\
	MNSocket.cc\
	$(fcgi)


if BUILD_JAVA
# no extra sources
libmyx_grt_java_la_SOURCES=
libmyx_grt_java_la_LIBADD=\
	libgrt.la \
	$(top_srcdir)/library/base-library/source/libmysqlx.la \
	$(top_srcdir)/library/sql-parser/source/libsqlparser.la \
	$(top_srcdir)/library/utilities/source/libguiutil.la \
	@GLIB_LIBS@ @PCRE_LIBS@ @GRT_LIBS@ @MYSQL_LIBS@
libmyx_grt_java_la_LDFLAGS=-module
endif
