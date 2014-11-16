-----------------
add_column
system
mysql
help_relation
help_keyword_id
MYX_QCT_WHERE_CLAUSE
SELECT * FROM mysql.help_relation hr
-----------------

-----------------

-----------------
add_column
system
mysql
help_keyword
help_keyword_id
MYX_QCT_WHERE_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
-----------------

WHERE hr.help_keyword_id=
-----------------

-----------------
add_column
system
mysql
help_keyword
help_keyword_id
MYX_QCT_WHERE_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
-----------------

WHERE hr.help_keyword_id<
-----------------

-----------------
add_column
system
mysql
help_keyword
help_keyword_id
MYX_QCT_WHERE_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
-----------------

WHERE hr.help_keyword_id>
-----------------

-----------------
add_column
system
mysql
help_keyword
name
MYX_QCT_WHERE_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
WHERE hr.help_keyword_id=hk.help_keyword_id
-----------------

-----------------

-----------------
add_column
system
mysql
help_topic
name
MYX_QCT_WHERE_CLAUSE
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
MYX_QCT_WHERE_CLAUSE
UPDATE mysql.help_topic ht
SET ht.help_topic_id=1
WHERE name='a'
-----------------

-----------------

-----------------
add_column
system
mysql
help_topic
name
MYX_QCT_WHERE_CLAUSE
DELETE FROM mysql.help_topic ht
-----------------

-----------------

-----------------
add_column
system
mysql
help_topic
content
MYX_QCT_WHERE_CLAUSE
DELETE FROM mysql.help_topic ht
WHERE name='a'
-----------------

-----------------
