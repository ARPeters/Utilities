PROC IMPORT OUT= WORK.A1 
            DATAFILE= "C:\Users\Clare\Documents\GitHub\Utilities\dsCausM
ed.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
