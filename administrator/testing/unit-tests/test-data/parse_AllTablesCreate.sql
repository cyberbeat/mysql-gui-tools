-- ----------------------------------------------------------------------
-- SQL create script
-- ----------------------------------------------------------------------

SET FOREIGN_KEY_CHECKS = 0;

DROP DATABASE IF EXISTS `bhop_3_0`;

CREATE DATABASE /*!32312 IF NOT EXISTS */ `bhop_3_0`
  CHARACTER SET latin1 COLLATE latin1_swedish_ci;

CREATE TABLE `bhop_3_0`.`automotive01` (
  `productid` BIGINT(16) NULL,
  `make` VARCHAR(64) NULL,
  `model` VARCHAR(64) NULL,
  `firstyear` DECIMAL(6, 2) NULL,
  `lastyear` DECIMAL(6, 2) NULL,
  `vehiclespec` VARCHAR(128) NULL,
  `position` VARCHAR(32) NULL,
  CONSTRAINT `fk_automotive01_productid` FOREIGN KEY `fk_automotive01_productid` (`productid`)
    REFERENCES `bhop_3_0`.`product` (`productid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`bhprivileges` (
  `privid` VARCHAR(16) NOT NULL,
  `description` VARCHAR(32) NULL,
  PRIMARY KEY (`privid`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`bhuser` (
  `userid` BIGINT(16) NOT NULL,
  `username` VARCHAR(48) NOT NULL,
  `url` VARCHAR(36) NULL,
  `nameoncard` VARCHAR(48) NULL,
  `creditcardtype` VARCHAR(40) NULL,
  `creditcardnumber` VARCHAR(40) NULL,
  `creditcardexpiration` VARCHAR(36) NULL,
  `referralinfo` VARCHAR(64) NULL,
  `registerdate` VARCHAR(24) NULL,
  `registersiteid` VARCHAR(8) NOT NULL,
  `registerip` VARCHAR(16) NULL,
  `status` VARCHAR(1) NULL DEFAULT '1',
  `sellerproductnumber` BIGINT(16) NULL DEFAULT 0,
  `password` VARCHAR(96) NOT NULL,
  `hint` VARCHAR(50) NULL,
  `firstname` VARCHAR(24) NULL,
  `lastname` VARCHAR(24) NULL,
  `companyname` VARCHAR(64) NULL,
  `dayphone` VARCHAR(24) NULL,
  `eveningphone` VARCHAR(24) NULL,
  `mobilephone` VARCHAR(64) NULL,
  `street` VARCHAR(24) NULL,
  `suite` VARCHAR(36) NULL,
  `city` VARCHAR(24) NULL,
  `state` VARCHAR(24) NULL,
  `zip` VARCHAR(16) NULL,
  `country` VARCHAR(36) NULL,
  `shipstreet` VARCHAR(48) NULL,
  `shipsuite` VARCHAR(36) NULL,
  `shipcity` VARCHAR(24) NULL,
  `shipstate` VARCHAR(24) NULL,
  `shipzip` VARCHAR(16) NULL,
  `shipcountry` VARCHAR(36) NULL,
  `email` VARCHAR(64) NULL,
  `usersession` VARCHAR(48) NULL,
  PRIMARY KEY (`userid`),
  UNIQUE INDEX `sys_c003105` (`username`(48)),
  CONSTRAINT `fk_bhuser_siteid` FOREIGN KEY `fk_bhuser_siteid` (`registersiteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`bid` (
  `bidid` BIGINT(16) NOT NULL,
  `productid` BIGINT(16) NULL,
  `buyerid` BIGINT(16) NULL,
  `bidquantity` DECIMAL(38, 0) NULL,
  `getquantity` DECIMAL(38, 0) NULL,
  `bidprice` DECIMAL(38, 2) NULL,
  `bidtime` VARCHAR(24) NULL,
  `bidip` VARCHAR(16) NULL,
  `bidsiteid` VARCHAR(8) NULL,
  `bidstatus` VARCHAR(4) NULL,
  PRIMARY KEY (`bidid`),
  CONSTRAINT `fk_bid_bidsiteid` FOREIGN KEY `fk_bid_bidsiteid` (`bidsiteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_bid_buyerid` FOREIGN KEY `fk_bid_buyerid` (`buyerid`)
    REFERENCES `bhop_3_0`.`bhuser` (`userid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_bid_productid` FOREIGN KEY `fk_bid_productid` (`productid`)
    REFERENCES `bhop_3_0`.`product` (`productid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`category` (
  `categoryid` VARCHAR(12) NOT NULL,
  `categoryname` VARCHAR(64) NOT NULL,
  `categorysummary` VARCHAR(128) NULL,
  `producttype` VARCHAR(24) NOT NULL DEFAULT 'Default',
  PRIMARY KEY (`categoryid`),
  CONSTRAINT `fk_category_producttype` FOREIGN KEY `fk_category_producttype` (`producttype`)
    REFERENCES `bhop_3_0`.`producttype` (`producttype`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`emaillist` (
  `email` VARCHAR(64) NOT NULL,
  `registersiteid` VARCHAR(8) NOT NULL,
  `registerdate` VARCHAR(24) NOT NULL,
  PRIMARY KEY (`email`, `registersiteid`),
  CONSTRAINT `fk_maillist_siteid` FOREIGN KEY `fk_maillist_siteid` (`registersiteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`filter` (
  `filterid` VARCHAR(48) NOT NULL,
  `filter_val` LONGTEXT NULL,
  PRIMARY KEY (`filterid`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`image` (
  `imageid` BIGINT(16) NOT NULL,
  `userid` VARCHAR(24) NULL,
  `imageregisterdate` VARCHAR(24) NULL,
  `imagesiteid` VARCHAR(8) NULL,
  `imagename` VARCHAR(256) NULL,
  `imageurl` VARCHAR(256) NULL,
  PRIMARY KEY (`imageid`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`product` (
  `productid` BIGINT(16) NOT NULL,
  `producttitle` VARCHAR(256) NULL,
  `productquantity` DECIMAL(38, 0) NULL,
  `productauctionmode` VARCHAR(8) NULL,
  `productfirstpagefeature` VARCHAR(4) NULL,
  `productcategoryfeature` VARCHAR(4) NULL,
  `productbold` VARCHAR(4) NULL,
  `productcategory` VARCHAR(12) NULL,
  `productimageurl` VARCHAR(256) NULL,
  `productdescription` VARCHAR(4000) NULL,
  `productmanufacture` VARCHAR(128) NULL,
  `productmanufacturepart` VARCHAR(128) NULL,
  `productcondition` VARCHAR(16) NULL,
  `productwarranty` VARCHAR(128) NULL,
  `productcity` VARCHAR(24) NULL,
  `productstate` VARCHAR(24) NULL,
  `productzip` VARCHAR(16) NULL,
  `productcountry` VARCHAR(36) NULL,
  `productreopen` VARCHAR(4) NULL,
  `productshipwhere` VARCHAR(128) NULL,
  `productpaymentmethod` VARCHAR(128) NULL,
  `productshipmentmethod` VARCHAR(128) NULL,
  `productshipmentcost` VARCHAR(48) NULL,
  `productshipterm` VARCHAR(128) NULL,
  `productstartupprice` DECIMAL(38, 2) NULL,
  `productcurrentprice` DECIMAL(38, 2) NULL,
  `productreserveprice` DECIMAL(38, 2) NULL,
  `productincrement` DECIMAL(38, 2) NULL,
  `productregistersiteid` VARCHAR(8) NULL,
  `productregisterip` VARCHAR(16) NULL,
  `productclick` DECIMAL(38, 0) NULL,
  `productbids` DECIMAL(38, 0) NULL,
  `sellerid` BIGINT(16) NOT NULL,
  `siteid` VARCHAR(8) NULL,
  `productstatus` VARCHAR(4) NULL,
  `productminquantity` DECIMAL(38, 0) NULL,
  `productfirstbid` DECIMAL(38, 2) NULL,
  `productemail` VARCHAR(64) NULL,
  `productcompanyurl` VARCHAR(128) NULL,
  `productleftquantity` DECIMAL(38, 0) NULL,
  `productclosesite` VARCHAR(4) NULL,
  `productregisterdate` VARCHAR(24) NULL,
  `productstarttime` VARCHAR(24) NULL,
  `productendtime` VARCHAR(24) NULL,
  `productclosedtime` VARCHAR(24) NULL,
  `productexttime` VARCHAR(24) NULL DEFAULT '0000-00-00-00-10-00',
  `productextnum` DECIMAL(38, 0) NULL,
  `productextmax` DECIMAL(38, 0) NULL,
  `producttype` VARCHAR(24) NOT NULL DEFAULT 'Default',
  `productcurrency` VARCHAR(10) NULL,
  `productimageurl2` VARCHAR(256) NULL,
  `productimageurl3` VARCHAR(256) NULL,
  `productimageurl4` VARCHAR(256) NULL,
  `productimageurl5` VARCHAR(256) NULL,
  `productimageurl6` VARCHAR(256) NULL,
  `productimageurl7` VARCHAR(256) NULL,
  `productimageurl8` VARCHAR(256) NULL,
  `productimageurl9` VARCHAR(256) NULL,
  `productimageurl10` VARCHAR(256) NULL,
  `productimageurl11` VARCHAR(256) NULL,
  `productimageurl12` VARCHAR(256) NULL,
  `productimageurl13` VARCHAR(256) NULL,
  `productimageurl14` VARCHAR(256) NULL,
  `productimageurl15` VARCHAR(256) NULL,
  `productimageurl16` VARCHAR(256) NULL,
  `productimageurl17` VARCHAR(256) NULL,
  `productimageurl18` VARCHAR(256) NULL,
  `productimageurl19` VARCHAR(256) NULL,
  `productimageurl20` VARCHAR(256) NULL,
  `productimageurl21` VARCHAR(256) NULL,
  `productimageurl22` VARCHAR(256) NULL,
  `productimageurl23` VARCHAR(256) NULL,
  `productimageurl24` VARCHAR(256) NULL,
  `productimageurl25` VARCHAR(256) NULL,
  `productimageurl26` VARCHAR(256) NULL,
  `productimageurl27` VARCHAR(256) NULL,
  `productimageurl28` VARCHAR(256) NULL,
  `productimageurl29` VARCHAR(256) NULL,
  `productimageurl30` VARCHAR(256) NULL,
  `productbuyitnow` VARCHAR(24) NULL,
  `productbuyitnowtime` VARCHAR(24) NULL,
  `productretailvalue` DECIMAL(38, 2) NULL,
  `productcategory2` VARCHAR(12) NULL,
  `productcategory3` VARCHAR(12) NULL,
  `productcategory4` VARCHAR(12) NULL,
  PRIMARY KEY (`productid`),
  CONSTRAINT `fk_product_producttype` FOREIGN KEY `fk_product_producttype` (`producttype`)
    REFERENCES `bhop_3_0`.`producttype` (`producttype`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_product_sellerid` FOREIGN KEY `fk_product_sellerid` (`sellerid`)
    REFERENCES `bhop_3_0`.`bhuser` (`userid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_product_sitecat` FOREIGN KEY `fk_product_sitecat` (`productregistersiteid`, `productcategory`)
    REFERENCES `bhop_3_0`.`sitecategory` (`siteid`, `categoryid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_product_siteid` FOREIGN KEY `fk_product_siteid` (`productregistersiteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`producttype` (
  `producttype` VARCHAR(24) NOT NULL,
  `description` VARCHAR(64) NULL,
  PRIMARY KEY (`producttype`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`salesformat` (
  `salesformatid` INT(4) NOT NULL,
  `description` VARCHAR(64) NULL,
  PRIMARY KEY (`salesformatid`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`site` (
  `sitesecureurl` VARCHAR(128) NULL,
  `siteid` VARCHAR(8) NOT NULL,
  `sitefirstname` VARCHAR(24) NULL,
  `sitelastname` VARCHAR(24) NULL,
  `sitecompanyname` VARCHAR(36) NULL,
  `sitestreet` VARCHAR(48) NULL,
  `sitesuite` VARCHAR(36) NULL,
  `sitecity` VARCHAR(24) NULL,
  `sitestate` VARCHAR(24) NULL,
  `sitezip` VARCHAR(16) NULL,
  `sitecountry` VARCHAR(36) NULL,
  `sitedayphone` VARCHAR(16) NULL,
  `siteemail` VARCHAR(64) NULL,
  `siteurl` VARCHAR(128) NULL,
  `siteregisterdate` VARCHAR(24) NULL,
  `siteproductnumber` DECIMAL(38, 0) NULL,
  `sitestatus` VARCHAR(8) NULL,
  `siteclose` VARCHAR(4) NULL,
  PRIMARY KEY (`siteid`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`sitecategory` (
  `siteid` VARCHAR(8) NOT NULL,
  `categoryid` VARCHAR(12) NOT NULL,
  `sitecategoryparentid` VARCHAR(12) NULL,
  `sitecategoryleaf` VARCHAR(1) NOT NULL DEFAULT '0',
  `sitecategoryproductnumber` DECIMAL(38, 0) NULL DEFAULT '0',
  `sitecategoryproductmaxbuy` INT(8) NULL,
  `sitecategorystatus` VARCHAR(1) NOT NULL DEFAULT '1',
  PRIMARY KEY (`siteid`, `categoryid`),
  CONSTRAINT `fk_sc_categiryid` FOREIGN KEY `fk_sc_categiryid` (`categoryid`)
    REFERENCES `bhop_3_0`.`category` (`categoryid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_sc_scparentid` FOREIGN KEY `fk_sc_scparentid` (`sitecategoryparentid`)
    REFERENCES `bhop_3_0`.`category` (`categoryid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_sc_siteid` FOREIGN KEY `fk_sc_siteid` (`siteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`sitefilter` (
  `siteid` VARCHAR(8) NOT NULL,
  `filterid` VARCHAR(48) NOT NULL,
  PRIMARY KEY (`siteid`, `filterid`),
  CONSTRAINT `fk_sitefilter_privid` FOREIGN KEY `fk_sitefilter_privid` (`filterid`)
    REFERENCES `bhop_3_0`.`filter` (`filterid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_sitefilter_siteid` FOREIGN KEY `fk_sitefilter_siteid` (`siteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`siteformat` (
  `siteid` VARCHAR(8) NOT NULL,
  `salesformatid` INT(4) NOT NULL,
  `defformat` VARCHAR(1) NULL,
  PRIMARY KEY (`siteid`, `salesformatid`),
  CONSTRAINT `fk_siteformat_salesformatid` FOREIGN KEY `fk_siteformat_salesformatid` (`salesformatid`)
    REFERENCES `bhop_3_0`.`salesformat` (`salesformatid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_siteformat_siteid` FOREIGN KEY `fk_siteformat_siteid` (`siteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`transaction` (
  `transactionid` BIGINT(16) NOT NULL,
  `bidid` BIGINT(16) NULL,
  `buyerid` BIGINT(16) NULL,
  `sellerid` BIGINT(16) NULL,
  `productid` BIGINT(16) NULL,
  `productquantity` DECIMAL(38, 0) NULL,
  `productprice` DECIMAL(38, 2) NULL,
  `transactiontime` VARCHAR(24) NULL,
  `buyerinvoicetime` VARCHAR(24) NULL,
  `sellerinvoicetime` VARCHAR(24) NULL,
  `siteid` VARCHAR(8) NULL,
  `transactionstatus` VARCHAR(4) NULL,
  `productcurrency` VARCHAR(10) NULL,
  PRIMARY KEY (`transactionid`),
  CONSTRAINT `fk_transaction_bidid` FOREIGN KEY `fk_transaction_bidid` (`bidid`)
    REFERENCES `bhop_3_0`.`bid` (`bidid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_transaction_buyerid` FOREIGN KEY `fk_transaction_buyerid` (`buyerid`)
    REFERENCES `bhop_3_0`.`bhuser` (`userid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_transaction_productid` FOREIGN KEY `fk_transaction_productid` (`productid`)
    REFERENCES `bhop_3_0`.`product` (`productid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_transaction_sellerid` FOREIGN KEY `fk_transaction_sellerid` (`sellerid`)
    REFERENCES `bhop_3_0`.`bhuser` (`userid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_transaction_siteid` FOREIGN KEY `fk_transaction_siteid` (`siteid`)
    REFERENCES `bhop_3_0`.`site` (`siteid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`uniqueidtab` (
  `uniqueid` BIGINT(16) NOT NULL,
  PRIMARY KEY (`uniqueid`)
)
ENGINE = INNODB;

CREATE TABLE `bhop_3_0`.`userpriv` (
  `userid` BIGINT(16) NOT NULL,
  `privid` VARCHAR(16) NOT NULL,
  `status` VARCHAR(1) NOT NULL,
  `privsiteid` VARCHAR(8) NULL,
  PRIMARY KEY (`userid`, `privid`),
  CONSTRAINT `fk_userpriv_privid` FOREIGN KEY `fk_userpriv_privid` (`privid`)
    REFERENCES `bhop_3_0`.`bhprivileges` (`privid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_userpriv_userid` FOREIGN KEY `fk_userpriv_userid` (`userid`)
    REFERENCES `bhop_3_0`.`bhuser` (`userid`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
)
ENGINE = INNODB;







SET FOREIGN_KEY_CHECKS = 1;


