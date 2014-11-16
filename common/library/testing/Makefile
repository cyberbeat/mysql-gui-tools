TEST_TIMEOUT=60

CXX=g++
CXXFLAGS=-Wall -Wno-unknown-pragmas -ggdb3 -fpermissive
INCLUDES=-Iinclude `mysql_config --cflags` -I../base-library/include -I../utilities/include -I../utilities/shared_include\
	-I../generic-runtime/include\
	-I../sql-parser/include\
	 `pkg-config glib-2.0 libxml-2.0 --cflags` `pcre-config --cflags` 

GRT_LIBS=../generic-runtime/source/.libs/libgrt.a -luuid `pkg-config gthread-2.0 --libs`

LIBS=`mysql_config --libs` `pkg-config libxml-2.0 glib-2.0 --libs`\
	$(GRT_LIBS)\
	 ../base-library/source/.libs/libmysqlx.a ../utilities/source/.libs/libguiutil.a\
	`pcre-config --libs` ../sql-parser/source/.libs/libsqlparser.a


all: test_all
	


update: flconv CommonTest.bdsproj
	./flconv --mode=dump --list-file=filelist.tmp --proj-file=CommonTest.bdsproj
	sed -e s'#\\#/#g' filelist.tmp > filelist
	echo >> filelist
	rm -f filelist.tmp


update-bds: flconv CommonTest.bdsproj
	sed -e s'#/#\\#g' filelist > filelist.tmp
	echo >> filelist.tmp
	./flconv --mode=update --list-file=filelist.tmp --proj-file=CommonTest.bdsproj

flconv:
	make -C ../../tools/FilelistConvertTool/source flconv
	cp ../../tools/FilelistConvertTool/source/flconv .

include filelist

OBJECTS=$(addsuffix .o,$(basename $(SOURCES)))


test_all: $(OBJECTS)
	$(CXX) -o $@ $^ $(LIBS) $(TEST_CFLAGS)

%.o: %.cpp
	$(CXX) -o $@ -c $^ $(CXXFLAGS) $(INCLUDES) $(TEST_CFLAGS)


test: test_all
	./run_tests.py $(TEST_TIMEOUT) --data=test-data --data=res --data=../../res

clean:
	rm -f $(OBJECTS)

