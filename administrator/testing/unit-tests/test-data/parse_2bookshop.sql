/* 
SQLyog v3.71
Host - localhost : Database - bookstore
**************************************************************
Server version 5.0.1-alpha-nt
*/

create database if not exists `bookstore`;

use `bookstore`;

/*
Table structure for allorder
*/

drop table if exists `allorder`;
CREATE TABLE `allorder` (
  `Id` int(11) NOT NULL auto_increment,
  `orderId` varchar(20) NOT NULL default '',
  `BookNo` int(11) NOT NULL default '0',
  `Amount` int(11) default NULL,
  PRIMARY KEY  (`Id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.allorder
*/

INSERT INTO `allorder` VALUES (13,'1094536714171',12,1);
INSERT INTO `allorder` VALUES (12,'1094536714171',13,1);
INSERT INTO `allorder` VALUES (11,'1094527039078',15,1);
INSERT INTO `allorder` VALUES (14,'1094536714171',11,1);
INSERT INTO `allorder` VALUES (15,'1094536751531',14,1);
INSERT INTO `allorder` VALUES (16,'1094536751531',11,1);

/*
Table structure for book
*/

drop table if exists `book`;
CREATE TABLE `book` (
  `Id` int(11) NOT NULL auto_increment,
  `BookName` varchar(40) NOT NULL default '',
  `BookClass` int(11) NOT NULL default '0',
  `Author` varchar(25) default NULL,
  `Publish` varchar(150) default NULL,
  `BookNo` varchar(30) default NULL,
  `Content` text,
  `Prince` float default NULL,
  `Amount` int(11) default NULL,
  `Leav_number` int(11) default NULL,
  `RegTime` datetime NOT NULL default '0000-00-00 00:00:00',
  `picture` varchar(200) default NULL,
  PRIMARY KEY  (`Id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.book
*/

INSERT INTO `book` VALUES (10,'中文Dramweaver MX2004白金教学',1,'徐立','兵器工业出版社','1','Dreamweaver的最新版本Dreamweaver MX 2004是一款专业的网页编辑器，它的出现使用户发现创作网页原来可以这样轻松！我们不能说它是十全十美的，但是它却实现了网页设计师的梦想，为我们提供了无限的创作空间。',36,500,499,'2004-08-30 00:00:00','pic\\b1.jpg');
INSERT INTO `book` VALUES (11,'3ds max 6轻松课堂实录',1,'高军锋','中国宇航出版社','2','数码艺术设计活宝贝丛书。适用范围：三维动画爱好者快速自学成才的指导书；社会三维动画初级培训班首选教材；高等院校电脑动画专业师生配套教材。 ',66,500,498,'2004-08-30 00:00:00','pic\\b2.jpg');
INSERT INTO `book` VALUES (12,'Java 程序设计实用教程',1,'马迪芳','北京交通大学出版社','3','高等学校计算机科学与技术教材。本书可作为高等院校计算机及其相关专业的教学用书，同时也适用于Java2的初学者和具有一定Java编程经验的开发人员。 ',27,500,499,'2004-08-30 00:00:00','pic\\b3.jpg');
INSERT INTO `book` VALUES (13,'摄影家西游记',2,'阮义忠','中国摄影出版社','4','一位影像工作者的旅行手札！宏伟壮丽的西班牙广场绝对是摄影家不应错过的好地方！',26,500,499,'2004-08-30 00:00:00','pic\\b4.jpg');
INSERT INTO `book` VALUES (14,'邓小平手迹选',3,NULL,'中国档案出版社','5','该书一函四册，八开本，分特藏版和豪华版两版，限量印制。其中特藏版限印800套，用独块樟木，传统手工艺制作，做工精细，质量考究，凸现线装书的古朴、典雅的品格。豪华版限印5000套。两种书，每套均有收藏证书，编号发行。该书是一部集史料价值、收藏价值和纪念意义的高档礼品书。现该书已被国家新闻出版中署列为邓小平同志诞辰100周年的重点图书\r\n江泽民为该书题名\r\n分特藏版和豪华版\r\n传统工艺樟木制箱\r\n质量考究古朴典雅 ',980,500,499,'2004-08-30 00:00:00','pic\\b5.jpg');
INSERT INTO `book` VALUES (15,'IT软件项目管理',1,'王强','清华大学出版社','7302089191','反映IT软件工程实践的管理类图书.',22,500,497,'0000-00-00 00:00:00','pic\\b6.jpg');

/*
Table structure for bookadmin
*/

drop table if exists `bookadmin`;
CREATE TABLE `bookadmin` (
  `AdminUser` varchar(20) default NULL,
  `AdminPass` varchar(50) default NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.bookadmin
*/

INSERT INTO `bookadmin` VALUES ('admin','admin');

/*
Table structure for bookclass
*/

drop table if exists `bookclass`;
CREATE TABLE `bookclass` (
  `Id` int(11) NOT NULL auto_increment,
  `ClassName` varchar(30) NOT NULL default '',
  PRIMARY KEY  (`Id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.bookclass
*/

INSERT INTO `bookclass` VALUES (3,'文学');
INSERT INTO `bookclass` VALUES (2,'摄影');
INSERT INTO `bookclass` VALUES (1,'计算机类');
INSERT INTO `bookclass` VALUES (4,'数学');

/*
Table structure for orders
*/

drop table if exists `orders`;
CREATE TABLE `orders` (
  `Id` int(11) NOT NULL auto_increment,
  `orderId` varchar(20) NOT NULL default '',
  `UserId` int(11) NOT NULL default '0',
  `SubmitTime` datetime NOT NULL default '0000-00-00 00:00:00',
  `ConsignmentTime` datetime default NULL,
  `TotalPrice` float default NULL,
  `content` text,
  `IPAddress` varchar(20) default NULL,
  `IsPayoff` int(11) default NULL,
  `IsSales` int(11) default NULL,
  PRIMARY KEY  (`Id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.orders
*/

INSERT INTO `orders` VALUES (13,'1094536714171',1,'2004-09-07 13:58:34','2004-09-07 13:58:41',119,'急需，请速发货！！','127.0.0.1',1,1);
INSERT INTO `orders` VALUES (12,'1094527039078',1,'2004-09-07 11:17:19','2004-09-07 11:17:26',22,'急需，请速发货！！','127.0.0.1',2,1);
INSERT INTO `orders` VALUES (14,'1094536751531',1,'2004-09-07 13:59:11','2004-09-07 13:59:18',1046,'急需，请速发货！！','127.0.0.1',1,1);

/*
Table structure for sequence
*/

drop table if exists `sequence`;
CREATE TABLE `sequence` (
  `id` int(11) NOT NULL default '0'
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.sequence
*/

INSERT INTO `sequence` VALUES (1);

/*
Table structure for shop_user
*/

drop table if exists `shop_user`;
CREATE TABLE `shop_user` (
  `Id` int(11) NOT NULL auto_increment,
  `UserName` varchar(20) NOT NULL default '',
  `PassWord` varchar(50) NOT NULL default '',
  `Names` varchar(20) default NULL,
  `Sex` char(2) default NULL,
  `Address` varchar(150) default NULL,
  `Phone` varchar(25) default NULL,
  `Post` varchar(8) default NULL,
  `Email` varchar(50) default NULL,
  `RegTime` datetime default NULL,
  `RegIpAddress` varchar(20) default NULL,
  PRIMARY KEY  (`Id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

/*
Table data for bookstore.shop_user
*/

INSERT INTO `shop_user` VALUES (1,'wxy','wxy','w','w','w',NULL,'444','e','2004-08-19 00:00:00','111');
INSERT INTO `shop_user` VALUES (2,'admin','1','www','?','www','555666','100000','wxy@3cn.com.cn',NULL,'127.0.0.1');

