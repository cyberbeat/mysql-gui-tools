
-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_ADD
select * from /*c1*/ t1 /*c3*/ t1_alias /*c4*/ , /*c5*/ t2 /*c6*/ t2_alias /*c7*/
-----------------
 where a=10
-----------------

-----------------
add_table
test

test
t3
MYX_QTAT_SELECT_JOIN
select * from /*c1*/ t1 /*c3*/ t1_alias /*c4*/ , /*c5*/ t2 /*c6*/ t2_alias /*c7*/
-----------------
 where a=10
-----------------
