proc import datafile="C:/Users/29557/Desktop/Research/Projects/HapLE_Cohort/R_HapLE/space input/HapLE_68_73.csv"
    out=S.HapLE
    dbms=csv
    replace;
    getnames=yes;
run;
