#! /bin/sh

test_fail=0

for dirname in $( ls ); do
  if [ -a ./$dirname/test.sh ]; then
    cd $dirname
    ./test.sh
    if [ "x$?" != "x0" ]; then
      test_fail=1
    fi
    cd ..
  fi
done

if [ "x$test_fail" = "x0" ]; then
echo ---------------------------------------------- mysql-query-browser tests pass!-
exit 0
else
echo ---------------------------------------------- mysql-query-browser tests fail!-
exit 1
fi



