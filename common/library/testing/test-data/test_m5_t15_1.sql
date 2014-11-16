drop schema if exists bug23012;
create schema bug23012;
use bug23012;

CREATE TABLE `tab` (
  `id` INTEGER
)
ENGINE = INNODB;

DELIMITER $$

DROP PROCEDURE IF EXISTS `a` $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `a`(IN a INT)
BEGIN
  select a from dual;
  insert into tab values(a);
END $$

DELIMITER ;

DELIMITER $$

DROP PROCEDURE IF EXISTS `b` $$
CREATE DEFINER=`root`@`localhost` PROCEDURE `b`(IN b INT)
BEGIN
  select b from dual;
  insert into tab values(b);
END $$

DELIMITER ;
