

noinst_LTLIBRARIES = libgcanvas.la



CXXFLAGS:=$(CXXFLAGS) -Wall -Wno-switch-enum -Wno-switch $(TEST_CFLAGS)

INCLUDES= -I../ftgl/include\
         -I../include @GLIB_CFLAGS@ @GCDEPS_CFLAGS@\
        -DUSE_FONTCONFIG

libgcanvas_la_SOURCES=\
	myx_gc_animation.cpp\
	myx_gc_base.cpp\
	myx_gc_bsp.cpp\
	myx_gc_canvas.cpp\
	myx_gc_connection.cpp\
	myx_gc_feedback.cpp\
	myx_gc_figure.cpp\
	myx_gc_figure_parser.cpp\
	myx_gc_font_manager.cpp\
	myx_gc_gl_helper.cpp\
	myx_gc_layer.cpp\
	myx_gc_layout.cpp\
	myx_gc_model.cpp\
	myx_gc_primitives.cpp\
	myx_gc_style.cpp\
	myx_gc_svgparser.cpp\
	myx_gc_texture.cpp\
	myx_gc_utilities.cpp\
	myx_gc_variant.cpp\
	myx_gc_view.cpp\
	gl2ps/gl2ps.c
