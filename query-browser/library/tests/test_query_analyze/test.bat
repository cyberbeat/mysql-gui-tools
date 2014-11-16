@echo off
copy test.js test_linked.js > nul 2> nul
type ..\..\..\..\mysql-gui-common\tests\common.js  >   test_linked.js
type test.js                                    >>  test_linked.js
@call CScript //nologo test_linked.js %*
del test_linked.js
