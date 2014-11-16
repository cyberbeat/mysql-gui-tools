/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=NO_AUTO_VALUE_ON_ZERO */;
CREATE DATABASE /*!32312 IF NOT EXISTS*/ `test`;
USE `test`;

CREATE TABLE `carthasproduct` (
  `idonlinecustomer` int(10) unsigned NOT NULL default '0',
  `idproduct` int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (`idonlinecustomer`,`idproduct`)
) TYPE=MyISAM;

INSERT INTO `carthasproduct` (`idonlinecustomer`,`idproduct`) VALUES (2,2),(2,3);

CREATE TABLE `creditcard` (
  `idcreditcard` int(10) unsigned NOT NULL auto_increment,
  `company` varchar(45) default NULL,
  PRIMARY KEY  (`idcreditcard`)
) TYPE=MyISAM;

INSERT INTO `creditcard` (`idcreditcard`,`company`) VALUES (1,'VISA'),(2,'Mastercard');

CREATE TABLE `forumpost` (
  `idforumpost` int(10) unsigned NOT NULL default '0',
  `idforumtopic` int(10) unsigned NOT NULL default '0',
  `idforumpost_parent` int(10) unsigned NOT NULL default '0',
  `idonlinecustomer` int(10) unsigned NOT NULL default '0',
  `title` varchar(45) default NULL,
  `paragraphs` text,
  `createdate` datetime default NULL,
  PRIMARY KEY  (`idforumpost`)
) TYPE=MyISAM;

CREATE TABLE `forumtopic` (
  `idforumtopic` int(10) unsigned NOT NULL default '0',
  `title` varchar(80) default NULL,
  PRIMARY KEY  (`idforumtopic`)
) TYPE=MyISAM;
