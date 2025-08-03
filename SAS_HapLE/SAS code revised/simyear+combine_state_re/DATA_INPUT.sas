proc import datafile="C:\Users\29557\Desktop\HLE\CLHLS\HapLE_Co\SPACE_5S\input data\HLE_65_74.csv"
    out=S.HLE
    dbms=csv
    replace;
    getnames=yes;
run;
