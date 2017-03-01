/*
PROC IMPORT OUT= WORK.myData 
            DATAFILE= "C:\Users\Clare\Documents\GitHub\Utilities\dsCausMed.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;

proc print;
run;

data myData;
	set myData;
	xm_int=x*m;
run;

proc print;
run;
*/

proc reg data=myData;
	model m = x / covb;
run;

proc reg data=myData;
	model y= x m xm_int/covb;
run;
