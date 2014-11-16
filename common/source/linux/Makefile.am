

if BUILD_GC
wbstuff=libwbcommongui.a libwboardcommon.a
endif

noinst_LIBRARIES=libmacommongui.a libqbcommongui.a $(wbstuff)

libmacommongui_a_SOURCES=\
	myg_gtk_utils.cc\
	myg_gtkutils.cc\
        myg_utils.cc\
	MGAboutPanel.cc\
	MYXInterface.cc\
	MGExecSU.cc\
	MGGladeXML.cc\
	MGConnectDialog.cc\
	MGBrowserList.cc\
	MGFileBrowserList.cc\
	MGTableBrowserList.cc\
	MGSchemaBrowserList.cc\
	MGGraphPlotter.cc\
	MGMeterGraphPlotter.cc\
	MGHMeterGraphPlotter.cc\
	MGPreferences.cc\
	MGPreferencesEditor.cc\
	MGTimeGraphPlotter.cc\
	MGCellRenderer.cc\
	MGImage.cc\
	MGImageCheckButton.cc\
	MDispatcher.cc\
	MCrontab.cc\
	MGTableEditor.cc\
	MGCharsetPickDialog.cc

libqbcommongui_a_SOURCES=\
	myg_gtk_utils.cc\
	myg_gtkutils.cc\
        myg_utils.cc\
	MGAboutPanel.cc\
	MGSQLTextBuffer.cc\
	MYXInterface.cc\
	MYXResultSet.cc\
	MGGladeXML.cc\
	MGConnectDialog.cc\
	MGBrowserList.cc\
	MGFileBrowserList.cc\
	MGTableBrowserList.cc\
	MGSchemaBrowserList.cc\
	MGPreferences.cc\
	MGPreferencesEditor.cc\
	MGCellRenderer.cc\
	MGImage.cc\
	MGImageButton.cc\
	MGImageCheckButton.cc\
	MGFloatingWidget.cc\
	MGTableEditor.cc\
	MGSchemaBrowserHelper.cc\
	MDispatcher.cc\
	MGBlobEditor.cc\
	MGCharsetPickDialog.cc\
	MGTreeTooltip.cc


if BUILD_GC
libwbcommongui_a_SOURCES=\
	myg_gtk_utils.cc\
	myg_gtkutils.cc\
        myg_utils.cc\
	MGAboutPanel.cc\
	MYXInterface.cc\
	MGGladeXML.cc\
	MGPreferences.cc\
	MGPreferencesEditor.cc\
	MGImage.cc\
	MGImageButton.cc\
	MGImageCheckButton.cc\
	MGShellView.cc\
	MGCellRenderer.cc\
	MGCellRendererCombo.cc\
	canvas/MGCanvas.cc\
	canvas/MWBCanvas.cc\
	canvas/MGCanvasScroller.cc\
	MySQLGRT/MGRT.cc\
	MySQLGRT/MGRTShell.cc\
	MySQLGRT/MGRTConnectDialog.cc\
	MySQLGRT/MGRTValueTree.cc\
	MySQLGRT/MGRTObject.cpp\
	MySQLGRT/MGRTSchema.cpp\
	MySQLGRT/MGRTTable.cpp\
	MySQLGRT/MGRTView.cpp\
	MySQLGRT/MGRTRoutine.cpp\
	MySQLGRT/MGRTObjectEditor.cc\
	MySQLGRT/MGRTSchemaEditor.cc\
	MySQLGRT/MGRTTableEditor.cc\
	MySQLGRT/MGRTViewEditor.cc\
	MySQLGRT/MGRTRoutineGroupEditor.cc\
	MySQLGRT/MGRTRoutineEmbeddedEditor.cc\
	MySQLGRT/MGRTRevEngFilterPane.cc\
	MySQLGRT/MGRTPropertiesList.cc\
	MySQLGRT/MGRTWizardBase.cc

libwboardcommon_a_SOURCES=\
        MGGladeXML.cc\
        canvas/MGCanvas.cc\
        canvas/MGCanvasScroller.cc



endif

DEFINES=-D_REENTRANT

INCLUDES=@GNOME_CFLAGS@ @MYSQL_INCLUDE@ @PCRE_CFLAGS@ @ENABLED_MODULES@ @GCDEPS_CFLAGS@ @GRT_CFLAGS@\
	-I$(top_srcdir)/library/base-library/include\
	-I$(top_srcdir)/library/utilities/include\
	-I$(top_srcdir)/library/utilities/shared_include\
	-I$(top_srcdir)/library/generic-runtime/include\
	-I$(top_srcdir)/library/sql-parser/include\
	-I$(top_srcdir)/library/grt-modules/include\
	-I$(top_srcdir)/library/grt-wb-controller/include\
	-I$(top_srcdir)/library/generic-canvas/include\
	-I$(top_srcdir)/library/generic-canvas/ftgl/include\
	-I..\
	-DDATADIRNAME=\""$(DATADIRNAME)"\"\
        -DCOMMONDIRNAME=\""$(MG_COMMONDIR)"\"


# noinst_PROGRAMS=texecsu

#texecsu_CXXFLAGS=-DTEST
#texecsu_LDADD=@GNOME_LIBS@ -L. -lmacommongui -lutil @MYSQL_LIBS@
#texecsu_SOURCES=MGExecSU.cc
#
