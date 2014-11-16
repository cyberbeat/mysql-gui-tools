

INCLUDES=`pkg-config glib-2.0 --cflags` `mysql_config --cflags` `pcre-config --cflags`\
	-I../../../mysql-gui-common/library_grt/include -I../../../mysql-gui-common/library/include

LIBS= -L../../../mysql-gui-common/library_grt/source -lgrt\
	-L../../../mysql-gui-common/library/source -lmysqlx\
	`pcre-config --libs`\
	`pkg-config glib-2.0 libxml-2.0 --libs`  -llualib -llua \

CXX=colorgcc
CXXFLAGS=-Wall -ggdb $(INCLUDES)

OBJECTS=MNSocket.o MNServerSocket.o MTAgent.o main.o




mtagent: $(OBJECTS)
	g++ -o $@ $^  $(LIBS) 

clean:
	rm -f $(OBJECTS)
