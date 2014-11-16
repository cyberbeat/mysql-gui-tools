
noinst_PROGRAMS= grtsh 
#agentest

grtsh_SOURCES= grtshell.cpp

#agentest_SOURCES= agentest.cc

if BUILD_NEWT
newt_libs= $(top_builddir)/library/generic-runtime/source/.libs/libgrttextui.a\
	$(top_builddir)/library/generic-runtime/newt/libmnewt.a \
	@SLANG_LIB@ -ltermcap -lncurses
newt_flag= -DENABLE_TEXTUI
endif

if BUILD_FASTCGI
fcgi_libs=-lfcgi
endif

if USE_READLINE
rl_libs=-lncurses -lreadline -lhistory -ltermcap
grtsh_CPPFLAGS=-DUSE_READLINE 
endif


INCLUDES=-I$(top_srcdir)/library/grt-modules/include\
	-I$(top_srcdir)/library/base-library/include \
	-I$(top_srcdir)/library/mysql-resultset/include\
	-I$(top_srcdir)/library/utilities/include\
        -I$(top_srcdir)/library/utilities/shared_include\
	-I$(top_srcdir)/library/generic-runtime/include\
	$(newt_flag)\
	@GTHREAD_CFLAGS@ @MYSQL_INCLUDE@ @PCRE_CFLAGS@ \
        @ENABLED_MODULES@ @GRT_CFLAGS@\
	@GTK_CFLAGS@ 

grtsh_LDADD=\
	$(top_builddir)/library/grt-modules/source/libgrtmods.la \
	$(top_builddir)/library/mysql-resultset/source/libgrtsqlrs.la \
	$(top_builddir)/library/mysql-resultset/source/libsqlrs.la \
	$(top_builddir)/library/sql-parser/source/libsqlparser.la \
	$(top_builddir)/library/generic-runtime/source/libgrt.la \
	$(top_builddir)/library/base-library/source/libmysqlx.la \
	$(top_builddir)/library/utilities/source/libguiutil.la \
	@GTHREAD_LIBS@ @MYSQL_LIBS@ @PCRE_LIBS@ \
	@GRT_LIBS@\
	$(newt_libs)\
	$(rl_libs)\
	$(fcgi_libs)\
	-ldl -lstdc++

#xgrtsh_LDADD=\
#	$(top_srcdir)/library_grt_modules/source/libgrtmods.a \
#	$(top_srcdir)/library_grt/source/libgrt.a \
#	$(top_srcdir)/library/source/libmysqlx.a \
#	@GTHREAD_LIBS@ @MYSQL_LIBS@ @PCRE_LIBS@ \
#	@JNI_LDFLAGS@ @GTK_LIBS@\
#	-lncurses -lreadline -lhistory -ltermcap\
#	@LUA_LDFLAGS@ -ldl -lstdc++
#

#agentest_LDADD=\
#	$(top_srcdir)/library/grt-modules/source/libgrtmods.la \
#	$(top_srcdir)/library/generic-runtime/source/libgrt.la \
#	$(top_srcdir)/library/base-library/source/libmysqlx.la \
#	$(top_srcdir)/library/utilities/source/libguiutil.la \
#	@GTHREAD_LIBS@ @MYSQL_LIBS@ @PCRE_LIBS@ \
#	@GRT_LIBS@
#	$(rl_libs)\
#	-lstdc++
#
