
bin_SCRIPTS=mysql-query-browser
bin_PROGRAMS=mysql-query-browser-bin


EXTRA_DIST=mysql-query-browser

mysql_query_browser_bin_SOURCES=\
	main.cc\
	html.cc\
	MGCellRendererBlob.cc\
	MGCellRendererText.cc\
	MQPreferences.cc\
	MQPreferencesEditor.cc\
	MGResultSetModel.cc\
	MQQueryDispatcher.cc\
	MQMainWindow.cc\
	MQResultSetView.cc\
	MQWorkArea.cc\
	MQBaseTab.cc\
	MQResultTab.cc\
	MQScriptEditorTab.cc\
	MQBookmarks.cc\
	MQHistory.cc\
	MQBookmarkBrowser.cc\
	MQHistoryBrowser.cc\
	MQFunctionBrowser.cc\
	MQNewBookmarkDialog.cc\
	MQIndicatorCellRenderer.cc\
	MQQueryParameters.cc\
	MGCodeEditor.cc\
	MQBaseModule.cc\
	MQSavedState.cc\
	MGCompletionList.cc


INCLUDES=@GNOME_CFLAGS@ @MYSQL_CFLAGS@ @PCRE_CFLAGS@\
        -I$(top_srcdir)/../mysql-gui-common/source/linux\
        -I$(top_srcdir)/../mysql-gui-common/library/base-library/include\
        -I$(top_srcdir)/../mysql-gui-common/library/utilities/shared_include\
        -I$(top_srcdir)/../mysql-gui-common/library/utilities/include\
        -I$(top_srcdir)/library/include\
	-I$(top_srcdir)/source/linux/gtksourceview\
	-DPREFIX=\""$(PREFIX)"\"\
	-DDATADIRNAME=\""$(DATADIRNAME)"\"\
        -DCOMMONDIRNAME=\""$(MG_COMMONDIR)"\"\
	-DDOCDIR=\""$(DOCDIR)"\"


mysql_query_browser_bin_LDADD=$(top_builddir)/library/source/libqb.a\
	$(top_builddir)/../mysql-gui-common/source/linux/libqbcommongui.a\
	$(top_builddir)/../mysql-gui-common/library/base-library/source/.libs/libmysqlx.a\
	$(top_builddir)/../mysql-gui-common/library/utilities/source/.libs/libguiutil.a\
	$(top_builddir)/../mysql-gui-common/library/sql-parser/source/.libs/libsqlparser.a\
	$(top_builddir)/source/linux/gtksourceview/gtksourceview/.libs/libgtksourceview-1.0.a\
        @MYSQL_LIBS@ @GNOME_LIBS@ @PCRE_LIBS@ 

#
# Can't add this as a AC_CONFIG_SUBDIRS in configure.in because
# toplevel autogen.sh is breaking the configure scripts in gtksourceview
# If a solution is found, this could be removed.
#
$(top_builddir)/source/linux/gtksourceview/gtksourceview/.libs/libgtksourceview-1.0.a:
	(cd gtksourceview; ./configure --enable-compile-warnings=yes --enable-static --disable-shared --disable-build-tests; make CFLAGS="$(CFLAGS) -fno-strict-aliasing")

clean-generic:
	-make -C gtksourceview distclean
	(cd gtksourceview; rm -fr autom4te.cache config.log)

