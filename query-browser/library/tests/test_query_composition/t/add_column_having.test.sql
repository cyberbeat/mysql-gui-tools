-----------------
add_column
system
mysql
help_relation
help_keyword_id
MYX_QCT_HAVING_CLAUSE
SELECT * FROM mysql.help_relation hr
GROUP BY hr.help_topic_id
-----------------

-----------------

-----------------
add_column
system
mysql
help_keyword
help_keyword_id
MYX_QCT_HAVING_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
GROUP BY hr.help_topic_id
-----------------

HAVING hr.help_keyword_id=
-----------------

-----------------
add_column
system
mysql
help_keyword
help_keyword_id
MYX_QCT_HAVING_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
GROUP BY hr.help_topic_id
-----------------

HAVING hr.help_keyword_id<
-----------------

-----------------
add_column
system
mysql
help_keyword
help_keyword_id
MYX_QCT_HAVING_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
GROUP BY hr.help_topic_id
-----------------

HAVING hr.help_keyword_id>
-----------------

-----------------
add_column
system
mysql
help_keyword
name
MYX_QCT_HAVING_CLAUSE
SELECT help_relation.help_keyword_id 
FROM mysql.help_relation hr, mysql.help_keyword hk
GROUP BY hr.help_topic_id
HAVING hr.help_keyword_id=hk.help_keyword_id
-----------------

-----------------


