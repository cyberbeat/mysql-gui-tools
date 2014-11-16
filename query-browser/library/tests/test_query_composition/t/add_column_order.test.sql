-----------------
add_column
system
mysql
help_relation
help_keyword_id
MYX_QCT_ORDER_CLAUSE
SELECT * FROM mysql.help_relation hr
-----------------

-----------------

-----------------
add_column
system
mysql
help_relation
help_topic_id
MYX_QCT_ORDER_CLAUSE
SELECT * FROM mysql.help_relation hr
ORDER BY hr.help_keyword_id
-----------------

-----------------

-----------------
add_column
system
mysql
help_topic
name
MYX_QCT_ORDER_CLAUSE
UPDATE mysql.help_topic ht
SET ht.help_topic_id=1
-----------------

-----------------


-----------------
add_column
system
mysql
help_topic
content
MYX_QCT_ORDER_CLAUSE
UPDATE mysql.help_topic ht
SET ht.help_topic_id=1
ORDER BY ht.name
-----------------

-----------------


-----------------
add_column
system
mysql
help_topic
name
MYX_QCT_ORDER_CLAUSE
DELETE FROM mysql.help_topic ht
-----------------

-----------------


-----------------
add_column
system
mysql
help_topic
content
MYX_QCT_ORDER_CLAUSE
DELETE FROM mysql.help_topic ht
ORDER BY ht.name
-----------------

-----------------
