
Given code:
create table clusterCalls as SELECT `oid`, `phonenumber_oid`, day(starttime)=6 or day(starttime)=7 as weekend1weekday0, hour(starttime)>= 8 and hour(starttime)<= 20 as day1night0, duration, direction='Outgoing' as out1in0, direction='Missed Call' as missed1, description='Voice Call' as voice1sms0 FROM `distinctCalls`


CREATE TABLE FinalCalls AS SELECT * FROM clusterCallsNew WHERE packet1 = 0 AND MMS1 = 0 AND dataCall1 = 0 AND Duration1 = 0 AND hashNum > 0 AND myHashNum >0
 
New Codes:
CREATE TABLE clusterCalls AS SELECT * , DAY( DATETIME ) =6 OR DAY( DATETIME ) =7 AS weekend1weekday0, HOUR( DATETIME ) >=8 AND HOUR( DATETIME ) <=20 AS day1night0, direction =  'Outgoing' AS out1in0, direction =  'Missed' AS missed1, description =  'Packet Data' AS packet1, description =  'Short message' AS SMS1, description =  'MMS DIRECTION' AS MMS1, description =  'Data call' AS dataCall1, description =  'Voice call' AS VoiceCall1, direction =  'DURATION' AS Duration1
FROM  `combined_data`

update combined_date set Date = str_to_date(DateStr, '%d-%M-%Y %H:%i:%s')

CREATE TABLE groupCalls AS SELECT * 
FROM (

SELECT  `dateTime` , COUNT(  `dateTime` ) AS Count,  `event` 
FROM  `combined_data` 
WHERE  `hashNum` >0
GROUP BY  `dateTime` ,  `event` 
ORDER BY Count DESC
) AS TEST
WHERE Count >1
LIMIT 0 , 40

sum of count = 3708

SELECT Customer_Number, yr, mth, Invoice_Number, MIN( REV )
-> FROM (
-> SELECT Customer_Number, YEAR( Invoice_Date ) AS yr, MONTH( Invoice_Date ) AS mth, Invoice_Number, SUM( Price *
Quantity ) AS REV
-> FROM `Invoice4477anonSept2010`
-> GROUP BY Customer_Number, yr, mth, Invoice_Number
-> ORDER BY Customer_Number, yr, mth, REV
-> ) AS T
-> GROUP BY Customer_Number, yr, mth
-> LIMIT 93730 , 30;



 CREATE TABLE avg_duration_myhashnum AS SELECT `myHashNum` as "Customer ID", AVG(`duration`) as "Average Duration" FROM clustercalls GROUP BY `myHashNum`



Query to create new table called numbers:

CREATE TABLE numbers AS SELECT myHashNum, COUNT(*) AS 'numCalls',AVG( duration ) AS  'avgduration', 
AVG( weekend1weekday0 ) AS  'avgweekend1weekday0', AVG( day1night0 ) AS  'avgday1night0', 
AVG( out1in0 ) AS  'avgout1in0', AVG( missed1 ) AS  'avgmissed1', AVG( SMS1 ) AS  'avgSMS1', 
AVG( VoiceCall1 ) AS  'avgVoiceCall1', AVG( longDuration1 ) AS  'avglongDuration1'
FROM  `clusterCallsNew` 
GROUP BY myHashNum

Query to create new table calls92:
CREATE TABLE calls92 AS SELECT * FROM `clusterCallsNew` WHERE hashNum > 1 AND
hashNum < 106

	
