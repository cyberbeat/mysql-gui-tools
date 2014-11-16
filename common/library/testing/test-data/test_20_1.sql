CREATE DATABASE IF NOT EXISTS `ma_tests` /*!40100 DEFAULT CHARACTER SET utf8 */;

DROP TABLE IF EXISTS `ma_tests`.`dbm_test`;CREATE TABLE  `ma_tests`.`dbm_test` (
  `id_dbm_test` int(10) unsigned NOT NULL auto_increment COMMENT 'id column',
  `name` varchar(45) NOT NULL default 'Tom' COMMENT 'test',
  `date_test` timestamp NOT NULL default CURRENT_TIMESTAMP,
  `rating` int(10) unsigned NOT NULL default '12',
  `test1` varchar(45) NOT NULL default 'test',
  `test2` varchar(45) default NULL,
  PRIMARY KEY  (`id_dbm_test`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
