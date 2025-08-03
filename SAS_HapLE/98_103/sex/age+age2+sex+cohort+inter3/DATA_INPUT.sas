proc import datafile="C:/Users/29557/Desktop/Research/Projects/HapLE_Cohort/R/space input/HapLE_98_103.csv"
    out=S.HapLE
    dbms=csv
    replace;
    getnames=yes;
run;
