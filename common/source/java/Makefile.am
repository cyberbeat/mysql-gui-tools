
javalibdir=$(mydatadir)/java/lib
javadatadir=$(mydatadir)/java/com/mysql/grt/modules

javalib_DATA=lib/mysql-grt-java-1.0.0-bin.jar lib/jtds-1.2.jar  lib/junit.jar  lib/mysql-connector-java-5.0.4-bin.jar  lib/sapdbc-7_6_00_12_4339.jar

javadata_DATA=com/mysql/grt/modules/*.class


lib/mysql-grt-java-1.0.0-bin.jar:
	(cd $(top_srcdir); ./mkjava.sh all)

com/mysql/grt/modules/*.class:
	(cd $(top_srcdir); ./mkjava.sh all)

clean-local:
	-(cd $(top_srcdir); ./mkjava.sh clean)
	-rm -fr lib
