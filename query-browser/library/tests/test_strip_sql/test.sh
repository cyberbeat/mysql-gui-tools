#!/bin/sh

cp ../../../../common/tests/common.sh ./test_linked.sh
cat ./test_priv.sh >> ./test_linked.sh
chmod +x ./test_linked.sh
./test_linked.sh $*
fc_result=$?
rm ./test_linked.sh
exit $fc_result
