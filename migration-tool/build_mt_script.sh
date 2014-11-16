
version=$(grep version= source/scripts/MigrationScript.lua|sed -e 's/.*"\([^"]*\)".*/\1/')

libdir=lib
if test "$(arch)" = "ppc"; then
  target=macosx10.3-ppc
elif test "$(arch)" = "i686"; then
  if test "$(uname -s)" = "Linux"; then
    target=linux-i386
  fi
  jarch="i386"
elif test "$(arch)" = "x86_64"; then
  if test "$(uname -s)" = "Linux"; then
    target=linux-x86_64
    libdir=lib64
  fi
  jarch="amd64"
fi

if test x"$target" = x; then
  echo "Unknown architecture/OS"
  exit
fi

echo "Creating jar with Java portion..."
sh make_grtjar.sh

echo "=========================================="

echo "Copying files..."
rm -fr mysql-migration-toolkit-script-$version 
mkdir mysql-migration-toolkit-script-$version
cd mysql-migration-toolkit-script-$version

mkdir -p java/lib
mv ../mysql-grt-java-1.0.0-bin.jar java/lib
cp ../../common/res/java/*jar java/lib

mkdir -p java/com/mysql/grt/modules/
cp ../../common/source/java/com/mysql/grt/modules/*java java/com/mysql/grt/modules/
#cp ../../common/source/java/com/mysql/grt/modules/*java java/com/mysql/grt/modules
(cd java; javac -g:none -nowarn -classpath .:lib:lib/junit.jar:lib/mysql-grt-java-1.0.0-bin.jar com/mysql/grt/modules/*)

mkdir xml
cp ../../common/res/grt/*xml xml
cp ../res/grt/*xml xml

mkdir scripts
cp ../source/scripts/*lua scripts

mkdir lua
cp ../../common/source/lua/* lua
cp ../source/lua/*lua lua

if test `uname -s` = Darwin; then
	libname=myx_grt_java.dylib
        tlibname=$libname
else
	libname=libmyx_grt_java.so
        tlibname=myx_grt_java.so
fi
#make -C ../../common/library/generic-runtime/source $libname
cp ../../common/library/generic-runtime/source/.libs/$libname java/
if test $libname != $tlibname; then (cd java; ln -s $libname $tlibname); fi
cp ../../common/tools/grtsh/grtsh .
cp ../README.script .

cat <<EOF> run_migration_simple
#!/bin/sh

# Change the following paths to your local installation of JRE 1.6
if test "x\$JRE_LIB_BASE" = x; then
    JRE_LIB_BASE="/usr/java/jdk1.6.0/jre/lib/$jarch/"
fi

if [ ! -d \$JRE_LIB_BASE ]; then
    echo "JRE not found. Please make sure JRE (1.6.0 recommended) is installed and update the \$0 script to point to the correct path"
    exit 1
fi

JRE_LIB_PATHS="\$JRE_LIB_BASE:\$JRE_LIB_BASE/server"

LD_LIBRARY_PATH="java:\$JRE_LIB_PATHS:\$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

export GRT_JVM_PATH="\$JRE_LIB_BASE/server/libjvm.so"

./grtsh -x scripts/MigrationScript.lua
EOF
chmod +x run_migration_simple

cat <<EOF> run_migration
#!/bin/sh

# Change the following paths to your local installation of JRE 1.6
if test "x\$JRE_LIB_BASE" = x; then
   JRE_LIB_BASE="/usr/java/jdk1.6.0/jre/lib/$jarch/"
fi

if [ ! -d \$JRE_LIB_BASE ]; then
    echo "JRE not found. Please make sure JRE (1.6.0 recommended) is installed and update the \$0 script to point to the correct path"
    exit 1
fi

JRE_LIB_PATHS="\$JRE_LIB_BASE:\$JRE_LIB_BASE/server"


LD_LIBRARY_PATH="java:\$JRE_LIB_PATHS:\$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

export GRT_JVM_PATH="\$JRE_LIB_BASE/server/libjvm.so"

./grtsh -x scripts/TextMigrationScript.lua
EOF
chmod +x run_migration


cd ..
#echo "Packaging..."
#tar czf mysql-migration-toolkit-script-$version-$target.tar.gz mysql-migration-toolkit-script-$version

