CC=colorgcc
DEFINES=-DENABLE_JAVA_MODULES -DENABLE_LUA_MODULES

JAVA_CFLAGS=
JAVA_LIBS=-L/opt/j2sdk1.4.2_04/jre/lib/i386/server/ -ljvm
LUA_LIBS=-llua -llualib
CXXFLAGS=-Wall $(DEFINES) `pkg-config glib-2.0 libxml-2.0 libglade-2.0 gtkmm-2.4 --cflags`\
	-I../include -I../../../common/library/include -ggdb\
	-I../../../common/library/generic-runtime/include\
	-I../../../common/library/base-library/include\
	-I../../../common/source/linux\
	`mysql_config --cflags` `pcre-config --cflags` $(JAVA_CFLAGS)

LIBS=`pkg-config glib-2.0 libxml-2.0 --libs` `mysql_config --libs`\
	-L../../../common/library/source -lmysqlx\
	-L../../../common/source/linux -lmacommongui\
	`pcre-config --libs`\
	$(JAVA_LIBS) $(LUA_LIBS)

ALL: mt


mt: MigrationTool.o main.o
	$(CXX) -o $@ $^ $(LIBS)


mttest: mttest.o
	$(CC) $^ -L../../../common/library_grt/source -lgrt $(CFLAGS) $(LIBS) -o $@


clean:
	/bin/rm $(OBJS)
