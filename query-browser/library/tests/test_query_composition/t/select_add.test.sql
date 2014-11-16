-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_ADD
select * from t1 t1_alias, t2 t2_alias
-----------------
 where a=10;
-----------------

-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_ADD
SELECT * FROM t1 t
-----------------
 where a=10;
-----------------


-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_ADD
UPDATE t1 t SET a=10
-----------------

-----------------


-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_ADD
DELETE FROM t1 t
-----------------

-----------------

-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_ADD
DELETE FROM t1 t WHERE t.a=1
-----------------

-----------------

-----------------
add_table
test

mysql
help_category
MYX_QTAT_SELECT_ADD
SELECT * FROM mysql.help_category h
-----------------

-----------------


