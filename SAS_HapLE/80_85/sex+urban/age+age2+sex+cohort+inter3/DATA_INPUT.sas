proc import datafile="C:/Users/29557/Desktop/Research/Projects/HapLE_Cohort/R_HapLE/space input/HapLE_80_85.csv"
    out=S.HapLE
    dbms=csv
    replace;
    getnames=yes;
run;
