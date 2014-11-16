/*
 Test script for the Query Browser script editor.
*/

drop table if exists test.widget;
CREATE TABLE test.widget (
  widget_id int(11),
  widget_price decimal(6,2));

INSERT INTO WIDGET VALUES (1, 253.00);
INSERT INTO WIDGET VALUES (2, 202.00);
INSERT INTO WIDGET VALUES (2, 734.40);
INSERT INTO WIDGET VALUES (4, 234.00);

DELIMITER %%

DROP PROCEDURE IF EXISTS test.setpartprice%%
CREATE PROCEDURE test.setpartprice (partid INT ,
     Quantity INT,
     price DECIMAL(6,2)
)
BEGIN
     DECLARE discount_percent DECIMAL(6,2);
     DECLARE discounted_price DECIMAL(6,2);
     DECLARE test INT;

     SET discount_percent  =  15;
     SET discounted_price = price - (discount_percent / 100 * price);

     CALL test.widgetcount(test);

     IF quantity > 2 AND test > 3 THEN
       SET discounted_price = discounted_price - 2.00;
     END IF;

     UPDATE WIDGET SET widget_price = discounted_price WHERE widget_id = partid;
     -- Select * from widget;
END%%

# call test.setpartprice (1, 15, 200);

DROP PROCEDURE IF EXISTS test.widgetcount%%
CREATE PROCEDURE test.widgetcount (OUT num INT)
BEGIN
  SELECT COUNT(*) INTO num FROM widget;
END%%

DROP PROCEDURE IF EXISTS simpleproc%%
CREATE PROCEDURE simpleproc ()
BEGIN
  DECLARE Inttest int; -- just a declaration
  Set Inttest = 3; -- and give the declaration an integer
  SELECT 'TEST' AS Col1, IntTest; -- select just string value
END%%

DROP PROCEDURE IF EXISTS test.simpleproc%%
CREATE PROCEDURE test.simpleproc ()
BEGIN
  SELECT 'TEST' AS Res1;
  SELECT 'TEST2' AS Res2;
  SELECT test.hello ('Alfredo') as Res3;
END%%

CREATE FUNCTION test.hello (s CHAR(20)) RETURNS CHAR(50)
RETURN CONCAT('Hello, ',s,'!')%%

DELIMITER ;