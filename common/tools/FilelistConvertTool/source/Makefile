CXX=g++



TOOL_CFLAGS=`pkg-config libxml-2.0 --cflags` -I/usr/include/libxml2
TOOL_LIBS=`pkg-config libxml-2.0 --libs` -lxml2




flconv: filelist_convert_tool.cpp
	$(CXX) $^ -o $@ $(TOOL_CFLAGS) $(TOOL_LIBS) -ggdb3
