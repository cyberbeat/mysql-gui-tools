
TEST_TIMEOUT=120000


GCDIR=../../../mysql-gui-common/

TUT_INCLUDES=-I$(GCDIR)/library/testing/include/

CXX=g++
CXXFLAGS=-Wall -Wno-unknown-pragmas -fpermissive
INCLUDES=-Iinclude `mysql_config --cflags` $(TUT_INCLUDES) `pkg-config glib-2.0 libxml-2.0 --cflags` -I../../library/include -I$(GCDIR)/library/base-library/include -I$(GCDIR)/library/utilities/include -I$(GCDIR)/library/utilities/shared_include `pcre-config --cflags` -I$(GCDIR)/library/sql-parser/include
LIBS=`mysql_config --libs` `pkg-config libxml-2.0 glib-2.0 --libs` `pcre-config --libs` ../../library/source/libadmin.a\
	 $(GCDIR)/library/utilities/source/.libs/libguiutil.a  $(GCDIR)/library/base-library/source/.libs/libmysqlx.a \
 	$(GCDIR)/library/sql-parser/source/.libs/libsqlparser.a

FLCONV_DIR=$(GCDIR)/tools/FilelistConvertTool/source
FLCONV=$(FLCONV_DIR)/flconv
PROJFILE=MySQLAdministratorTest.bdsproj


all: test_all
	


update: flconv $(PROJFILE)
	./flconv --mode=dump --list-file=filelist.tmp --proj-file=$(PROJFILE)
	sed -e s'#\\#/#g' filelist.tmp > filelist
	echo >> filelist
	rm -f filelist.tmp


update-bds: flconv $(PROJFILE)
	./flconv --mode=update --list-file=filelist --proj-file=$(PROJFILE)

flconv:
	make -C $(FLCONV_DIR)
	ln -s $(FLCONV) .

include filelist

OBJECTS=$(addsuffix .o,$(basename $(SOURCES)))


test_all: $(OBJECTS)
	$(CXX) -o $@ $^ $(LIBS) $(TEST_CFLAGS)

test: test_all
	$(GCDIR)/library/testing/run_tests.py $(TEST_TIMEOUT) --data=res --data=../unit-tests/test-data 

%.o: %.cpp
	$(CXX) -o $@ -c $^ $(CXXFLAGS) $(INCLUDES) $(TEST_CFLAGS)

clean:
	rm -f $(OBJECTS)
