#/bin/bash
#
# Java stuff build script
#


targetdir="source/java"
srcdir="."
grtsh="$srcdir/tools/grtsh/grtsh"
javac="javac"


struct_files="base canvas db db.mysql forms db.mgmt db.migration db.oracle db.mssql db.maxdb db.sybase db.query model db.workbench"


function help() {
  echo "Commands:"
  echo "prep     - prepare sources for build"
  echo "build    - build sources"
  echo "jar      - make a jar of the built sources"
  echo "all      - prep, build, jar"
  echo "cleantmp - clean temporary files created build (eg: generated Java sources)"
  echo "clean    - clean temporary and compiled files"
}



function copy_jars() {
  echo "Copying JAR files..."
  mkdir -p $targetdir/lib

  cp $srcdir/res/java/*jar $targetdir/lib
}


function clean_tmp_files() {
  echo "Cleaning up old builds from $targetdir/com..."
  for file in $struct_files; do
    dir=$(echo $file|sed -e s@\\.@/@g)
    rm -fr $targetdir/com/mysql/grt/$dir
  done
}

function clean_all() {
  clean_tmp_files
}


function prepare_srcs() {
  echo "Autogenerating Java code from XML definitions from $srcdir/res/grt to $targetdir/com..."

  if ! test -f $grtsh; then
    echo "You must first build $grtsh"
    exit
  fi

  clean_tmp_files

  for file in $struct_files; do
    echo "Processing $file ..."
    dir=$(echo $file|sed -e s@\\.@/@g)
    $grtsh -j "$srcdir/res/grt/structs.$file.xml" "$targetdir/com/mysql/grt/$dir/"
    if [ $? -ne 0 ]; then
      echo "Error generating code for $file."
      exit 1
    fi
  done
  echo "Autogeneration done."
}


function build_srcs() {
  echo "Building Java code in $targetdir..."

  echo "Removing old files..."
  find $targetdir -name \*.class -exec /bin/rm {} \;
  
  java_srcs="$targetdir/com/mysql/grt/*.java $targetdir/com/mysql/grt/modules/*.java"
  for file in $struct_files; do
    dir=$(echo $file|sed -e s@\\.@/@g)
    java_srcs="$java_srcs $targetdir/com/mysql/grt/$dir/*.java"
  done

  echo "Compiling..."
  $javac -g:none -nowarn -classpath "$targetdir/lib/junit.jar" $java_srcs
  if [ $? -ne 0 ]; then
    echo "Error compiling Java code."
    exit 1
  fi
  echo "Build done."
}


function copy_modules() {
  if test x"$1" = x; then
    echo "Missing destination directory for modules"
    exit 1
  fi
  cp $targetdir/com/mysql/grt/modules/*.class $1
}


function make_jar() {
  echo "Creating jar file..."

  if type gtar; then
    TAR=gtar
  else
    TAR=tar
  fi

  path="mysql-grt-java-1.0.0-bin.jar"
  mkdir -p "$targetdir/tmpjar/com"
  (cd "$targetdir";$TAR -cf - --exclude=modules --exclude=.svn com| $TAR xf - -C tmpjar)
  (cd "$targetdir/tmpjar"; jar cf "$path" com/)
  mv "$targetdir/tmpjar/$path" "$targetdir/lib"
  rm -fr "$targetdir/tmpjar"
  
  echo "Jar file $path created."
}


cmd=$1

case $cmd in
  prep)
    copy_jars
    prepare_srcs
    ;;
  build)
    build_srcs
    ;;
  copy_modules)
    copy_modules $2
    ;;
  cleantmp)
    clean_tmp_files
    ;;
  jar)
    make_jar
    ;;
  all)
    copy_jars
    prepare_srcs
    build_srcs
    make_jar
    ;;
  clean)
    clean_all
    ;;
  *)
    echo "$0: You must specify a command."
    help
    exit 1
  ;;
esac

