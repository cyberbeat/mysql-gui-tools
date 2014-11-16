use test;

SET NAMES cp1251;

drop table if exists 
  test.t8, 
  test.t7, 
  test.t6, 
  test.t5, 
  test.t4, 
  test.t3, 
  test.t2, 
  test.t1;

-- tables for checking of join composition

create table t1 (t1_id int, primary key (t1_id))    engine=innodb;

create table t2 (t2_id int, primary key (t2_id), 
                 t1_id int, index(t1_id),
                 FOREIGN KEY (t1_id)
                 REFERENCES t1(t1_id)) engine=innodb;

create table t3 (t2_id int, primary key (t2_id), 
                 silly_name int, index(silly_name),
                 FOREIGN KEY (silly_name)
                 REFERENCES t1(t1_id)) engine=innodb;

-- tables for checking of quotas in the composition

create table t4 (`field with complex name `` \\ "` int, 
                 primary key (`field with complex name `` \\ "`)) engine=innodb;

create table t5 (t5_id int, primary key (t5_id), 
                 `field with complex name `` \\ "` int, 
                 index(`field with complex name `` \\ "`),
                 FOREIGN KEY (`field with complex name `` \\ "`)
                 REFERENCES t4(`field with complex name `` \\ "`)) engine=innodb;

create table t6 (t6_id int, primary key (t6_id), 
                 silly_name int, index(silly_name),
                 FOREIGN KEY (silly_name)
                 REFERENCES t4(`field with complex name `` \\ "`)) engine=innodb;


create table t7 (`field with complex name `` \\ " 2` int,
                 primary key (`field with complex name `` \\ " 2`)) engine=myisam;

create table t8 (t8_id int, primary key (t8_id), 
                 `field with complex name `` \\ " 2` int,
                 index(`field with complex name `` \\ " 2`),
                 FOREIGN KEY (`field with complex name `` \\ " 2`)
                 REFERENCES t7(`field with complex name `` \\ "`)) engine=myisam;

-- tables with utf-8 symbols

DROP TABLE IF EXISTS `таблица2`;
CREATE TABLE `таблица2` (
  `поле1` VARCHAR (50) collate utf8_bin
) DEFAULT CHARACTER SET utf8 COLLATE=utf8_bin;

INSERT INTO `таблица2` VALUES ('значение1'),('значение2');

/*!40000 ALTER TABLE `таблица2` ENABLE KEYS */;

DROP TABLE IF EXISTS `таблица1`;
CREATE TABLE `таблица1` (
  `поле2` VARCHAR(50) collate utf8_bin
) DEFAULT CHARACTER SET utf8 COLLATE=utf8_bin;

INSERT INTO `таблица1` VALUES ('значение3'),('значение4'),('значение5');
