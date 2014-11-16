-----------------
add_table
mysql

mysql
host
MYX_QTAT_SELECT_JOIN
select * from db db
-----------------
 where db.user='root';
-----------------

-----------------
add_table
mysql

mysql
help_category
MYX_QTAT_SELECT_JOIN
SELECT * FROM mysql.help_category h
-----------------

-----------------

-----------------
add_table
mysql

mysql
help_category
MYX_QTAT_SELECT_JOIN
SELECT * FROM mysql.help_category h, help_category he
WHERE he.help_category_id=h.help_category_id
-----------------

-----------------

-----------------
add_table
mysql

mysql
help_category
MYX_QTAT_SELECT_JOIN
SELECT * FROM mysql.help_category h, help_category he, help_category h0
WHERE he.help_category_id=h.help_category_id AND h0.help_category_id=h.help_category_id
-----------------

-----------------

