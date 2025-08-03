proc import datafile="C:/Users/29557/Desktop/Research/Projects/HapLE_Cohort/R/space input/HapLE_92_97.csv"
    out=S.HapLE
    dbms=csv
    replace;
    getnames=yes;
run;
