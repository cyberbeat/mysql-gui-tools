CREATE DATABASE IF NOT EXISTS `ma_tests` /*!40100 DEFAULT CHARACTER SET utf8 */;

DROP TABLE IF EXISTS `ma_tests`.`foo`;
CREATE TABLE  `ma_tests`.`foo` (
  `idfoo` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(45) NOT NULL,
  PRIMARY KEY  (`idfoo`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `ma_tests`.`foo2`;
CREATE TABLE  `ma_tests`.`foo2` (
  `idfoo2` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(45) NOT NULL,
  PRIMARY KEY  (`idfoo2`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

DROP TABLE IF EXISTS `ma_tests`.`ba`;
CREATE TABLE  `ma_tests`.`ba` (
  `idba` int(10) unsigned NOT NULL auto_increment,
  `idfoo` int(10) unsigned NOT NULL,
  `name` varchar(45) NOT NULL,
  `idfoo2` int(10) unsigned NOT NULL,
  PRIMARY KEY  (`idba`,`idfoo`,`idfoo2`),
  KEY `FK_ba_2` (`idfoo2`),
  KEY `FK_ba_1` (`idfoo`),
  CONSTRAINT `FK_ba_2` FOREIGN KEY (`idfoo2`) REFERENCES `foo2` (`idfoo2`) ON DELETE CASCADE,
  CONSTRAINT `FK_ba_1` FOREIGN KEY (`idfoo`) REFERENCES `foo` (`idfoo`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
