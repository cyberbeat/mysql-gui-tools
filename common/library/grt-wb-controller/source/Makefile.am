

noinst_LTLIBRARIES=libgrtworkbench.la

CFLAGS:= $(CFLAGS) $(TEST_CFLAGS)
CXXFLAGS:= $(CXXFLAGS) $(TEST_CFLAGS)

INCLUDES=@MYSQL_INCLUDE@ @GLIB_CFLAGS@ @GCDEPS_CFLAGS@ @GRT_CFLAGS@ @PCRE_CFLAGS@\
	-I$(top_srcdir)/library/base-library/include\
	-I$(top_srcdir)/library/utilities/include\
	-I$(top_srcdir)/library/utilities/shared_include\
	-I$(top_srcdir)/library/generic-runtime/include\
	-I$(top_srcdir)/library/generic-canvas/include\
	-I$(top_srcdir)/library/generic-canvas/ftgl/include\
	@ENABLED_MODULES@\
	-I../include

libgrtworkbench_la_SOURCES=\
	myx_grt_wb_controller.cc\
	myx_grt_wb_bridge.cc\
	GraphRenderer.cpp
