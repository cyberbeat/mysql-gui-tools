CREATE DATABASE IF NOT EXISTS `ma_tests` /*!40100 DEFAULT CHARACTER SET utf8 */;

use ma_tests;
DROP TABLE IF EXISTS `foo`;
CREATE TABLE  `ma_tests`.`foo` (
  `idfoo` int(10) unsigned NOT NULL auto_increment,
  `col1` year(4) NOT NULL,
  `col2` char(4) NOT NULL,
  PRIMARY KEY  (`idfoo`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

