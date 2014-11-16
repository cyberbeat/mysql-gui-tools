
bin_PROGRAMS= mabackup

mabackup_SOURCES=mabackup.cc


INCLUDES=@GLIB_CFLAGS@ @MYSQL_CFLAGS@ @PCRE_CFLAGS@\
        -I$(top_srcdir)/../common/library/base-library/include\
        -I$(top_srcdir)/../common/library/utilities/shared_include\
        -I$(top_srcdir)/../common/library/utilities/include\
        -I$(top_srcdir)/library/include\
        -DPREFIX=\""$(PREFIX)"\"\
        -DDATADIRNAME=\""$(DATADIRNAME)"\"\
        -DCOMMONDIRNAME=\""$(MG_COMMONDIR)"\"


mabackup_LDADD=\
        $(top_srcdir)/library/source/libadmin.a\
        $(top_srcdir)/../common/library/base-library/source/.libs/libmysqlx.a\
        $(top_srcdir)/../common/library/utilities/source/.libs/libguiutil.a\
        $(top_srcdir)/../common/library/sql-parser/source/.libs/libsqlparser.a\
        @MYSQL_LIBS@ @PCRE_LIBS@ @GLIB_LIBS@


