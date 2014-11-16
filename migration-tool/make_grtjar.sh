
mtroot=./
mcroot=../common

GRTSH=$mcroot/tools/grtsh/grtsh

rm -fr jbuild
mkdir jbuild

echo "Copying standard jar files..."
mkdir jbuild/lib
cp $mcroot/res/java/*.jar jbuild/lib

mkdir -p jbuild/com/mysql/grt/{canvas,forms,base,db/mysql,db/mgmt,db/migration,db/oracle,db/mssql}
echo "Generating Java source from GRT struct definition files..."
$GRTSH -j $mcroot/res/grt/structs.canvas.xml      jbuild/com/mysql/grt/canvas/
$GRTSH -j $mcroot/res/grt/structs.forms.xml       jbuild/com/mysql/grt/forms/
$GRTSH -j $mcroot/res/grt/structs.base.xml        jbuild/com/mysql/grt/base/
$GRTSH -j $mcroot/res/grt/structs.db.xml          jbuild/com/mysql/grt/db/
$GRTSH -j $mcroot/res/grt/structs.db.mysql.xml    jbuild/com/mysql/grt/db/mysql/
$GRTSH -j $mcroot/res/grt/structs.db.mgmt.xml     jbuild/com/mysql/grt/db/mgmt/
$GRTSH -j $mcroot/res/grt/structs.db.migration.xml jbuild/com/mysql/grt/db/migration/
$GRTSH -j $mcroot/res/grt/structs.db.oracle.xml   jbuild/com/mysql/grt/db/oracle/
$GRTSH -j $mcroot/res/grt/structs.db.mssql.xml    jbuild/com/mysql/grt/db/mssql/

jbdir=`pwd`/jbuild
echo "Copying GRT Java code..."
(cd $mcroot/source/java; tar cf - com |tar xf - -C $jbdir --exclude .svn)
echo "Copying MT Java code..."
(cd $mtroot/source/java; tar cf - com |tar xf - -C $jbdir --exclude .svn)

echo "Compiling Java source..."
(cd jbuild; javac -g:none -nowarn -classpath .:lib:lib/junit.jar \
        com/mysql/grt/*.java com/mysql/grt/modules/*.java com/mysql/grt/db/*.java com/mysql/grt/db/*/*.java )

echo "Creating JAR File..."
(cd jbuild; jar cf ../mysql-grt-java-1.0.0-bin.jar com/)
echo "Done."
