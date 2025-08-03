%MACRO MSLT_RADxCOV_M;

  %let nCOV=0;
  %do %while (%scan(&COV, &nCOV+1, %str( )) ne %str( ));
    %let nCOV=%eval(&nCOV+1);
  %end;
  %put nCOV=&nCOV;
  %let nCOVs = &nCOV;
  
  %MACRO BOOTSTRP_RADxCOV(DATA=,BSIZE=,VAR=,NS=,COV=,NC=,STRATA=,PSU=,WGT=,LOI=,BEG=,END=, STRATCOV=, REGCOV=, 
                          TVcov=, randomSeed=, Interaction=, age_sq=);
  
    %if &nCOVs >= 1 %then %do;
      %DO MA=1 %TO &nCOVs;
        %LET COV&MA=%SCAN(&COV,&MA,' ');
        Data Datainput;
          Set &Data;
          if &VAR ne .;
          if &&COV&MA ne .;
          %if %scan(&subset, 1, %str( )) ne %str( ) %then %do;
            if &subset = 1;
          %end;
        run;
        
        PROC SORT DATA=Datainput;
          BY &&COV&MA;
        RUN;
    
        DATA TEMP&MA(KEEP=ID &&COV&MA);
          SET Datainput;
          BY &&COV&MA;
          IF FIRST.&&COV&MA;
        RUN;
    
    		DATA _NULL_;
          SET TEMP&MA END=FINAL;
          N+1;
          IF FINAL THEN CALL SYMPUT("LC&MA", N);  *** LC: LEVEL OF COVARIATES 1,2,... ***;
        RUN;
      %END;
      DATA S.COVcomboIndex0;
        %DO MA=1 %TO &nCOVs;      
          DO &&COV&MA=1 TO &&LC&MA;
        %END; 
          COVindex+1;
          OUTPUT;
        %DO MA=1 %TO &nCOVs;      
          END;
        %END;
      RUN;
      
      proc sort data = S.COVcomboIndex0;
        by COVindex; 
      run;
      data _null_;
        set S.COVcomboIndex0 END=FINAL;
        if FINAL then call symput('nCOVindex', COVindex);
      run;
    %end;
    %if &nCOVs = 0 %then %do;
      Data Datainput;
        Set &Data;
        if &VAR ne .;
        %if %scan(&subset, 1, %str( )) ne %str( ) %then %do;
          if &subset = 1;
        %end;
      run;
      %let nCOVindex = 0;
      Data S.Covcomboindex0;
        COVindex = 0;
      run;    
    %end;
    DATA S.COVcomboIndex1;
      set S.COVcomboIndex0;
    RUN;
    *** POINT ESTIMATES USING THE FULL SAMPLE ***; 
    ***%MSLT_RADxCOV_S(DATA=&DATA,S=0,VAR=&VAR,NS=&NS,COV=&COV,NC=&NC,WGT=&WGT,LOI=&LOI,BEG=&BEG,END=&END,STRATCOV=&STRATCOV, REGCOV=&REGCOV);
    %MSLT_RADxCOV_S(DATA=Datainput,S=0,VAR=&VAR,NS=&NS,COV=&COV,NC=&NC,WGT=&WGT,LOI=&LOI,BEG=&BEG,END=&END,STRATCOV=&STRATCOV, REGCOV=&REGCOV, 
                    TVcov=&TVcov, randomSeed=&randomSeed, BS_i=1234567, Interaction=&Interaction, age_sq=&age_sq);
    
    DATA S.RAD_LE;
      BS=0;
      SET BSLE;
    RUN;  
  
    *** THE BOOTSTRAP PART ***;
    PROC SORT DATA=Datainput OUT=SAMPLE3;
      BY &STRATA &PSU;
    RUN;  
  
    PROC SORT DATA=Datainput;
      BY ID ;     
    RUN;  

    DATA BS1;
      SET Datainput(KEEP=ID &STRATA &PSU);
      BY ID;
      IF FIRST.ID;
    RUN;
  
    PROC SORT NODUPKEY OUT=BS2;
      BY &STRATA &PSU;
    RUN;
  
    DATA BS3(DROP=&PSU);
      SET BS2(DROP=ID);
      BY &STRATA &PSU;
      IF FIRST.&STRATA THEN NU=0;
      NU+1;
      IF LAST.&STRATA THEN OUTPUT;
    RUN;
  
    %DO B=1 %TO &BSIZE;         
      PROC IML;
        USE BS2;
        READ ALL VAR {&PSU} INTO X;
        CLOSE BS2;
  
        USE BS3;
        READ ALL VAR {&STRATA NU} INTO Y;
        CLOSE BS3;    
   
        CUMS=J(1,1,0);
          DO I=1 TO NROW(Y); 
            CUMS=CUMS+Y[I,2];
            RS=X[CUMS-Y[I,2]+1:CUMS];
  
            IF Y[I,2]=1 THEN DO;             
              NEWSU[,1]=Y[I,1];
              NEWSU[,2]=RS;                   *** SINGLE PSU IS SELECTED WITH CERTAINTY ***;
              NEWSU[,3]=Y[I,2];    
            END; 
            ELSE DO;
              NEWSU=J(MAX(Y[I,2]-1,1),3,0);  /*** SINGLE PSU IS SELECTED WITH CERTAINTY ***/
              NEWSU[,1]=Y[I,1];   
              NEWSU[,3]=Y[I,2];   
              DO J=1 TO Y[I,2]-1; 
                NEWSU[J,2]=RS[ROUND(RANUNI(ROUND(%if &randomSeed = 1 %then %do; DATETIME() %end; %else %do; &B %end;))*Y[I,2]+0.5)]; 
              END;
            END;
            BSU=BSU//NEWSU; 
          END; 
  
        VAR={"&STRATA" "&PSU" "PS"};
        CREATE NBSU FROM BSU [COLNAME=VAR];
        APPEND FROM BSU;
        CLOSE NBSU;
      QUIT; 
  
      PROC SORT DATA=NBSU;
        BY &STRATA &PSU;
      RUN;
  
      DATA NBSU2;
        SET NBSU;
        BY &STRATA &PSU;
        IF FIRST.&PSU THEN NU=0;
        NU+1;
        IF LAST.&PSU THEN OUTPUT;
      RUN;
  
      DATA BSMPL;
        MERGE NBSU2(IN=SU2) SAMPLE3;
        BY &STRATA &PSU;
        IF SU2;
        IF PS>1 THEN &WGT=&WGT*NU*(PS/(PS-1));
      RUN;
  
      %MSLT_RADxCOV_S(DATA=BSMPL,S=1,VAR=&VAR,NS=&NS,COV=&COV,NC=&NC,WGT=&WGT,LOI=&LOI,BEG=&BEG,END=&END,STRATCOV=&STRATCOV, REGCOV=&REGCOV, 
                      TVcov=&TVcov, randomSeed=&randomSeed, BS_i=&B, Interaction=&Interaction, age_sq=&age_sq);
  
      DATA LE;
        BS=&B;
        SET BSLE;
      RUN;
  
      DATA S.RAD_LE;
        SET S.RAD_LE LE;
        IF BS NE . ;
      RUN; 
    %END;
  %MEND;
  
  %BOOTSTRP_RADxCOV(DATA=S.&Datainput,BSIZE=&BSIZE,VAR=&depVAR,NS=&nHealthState,COV=&COV,NC=&nCOVs,STRATA=&STRATA,PSU=&PSU,WGT=&WGT,LOI=&LOI,BEG=&BEG,END=&END_LT,STRATCOV=&STRATCOV, REGCOV=&REGCOV, 
                    TVcov=&TVcov, randomSeed=&randomSeed, interaction=&InteractionTerm, age_sq=&age_sq);
  
  %Macro AfterBOOTSTRP_RADxCOV();
  %put "AfterBOOTSTRP_RADxCOV()";
    DATA S.RAD_LE;
      SET S.RAD_LE;
      drop COVindex;
      ALEp = ALE/TLE;
      DLEp = DLE/TLE;
      
    run;
    %if %eval(&BSIZE) > 0 %then %do;
      PROC MEANS DATA=S.RAD_LE STD NOPRINT;
        VAR TLE ALE DLE ALEp DLEp;
        CLASS AGE %if &nCOVs >= 1 %then %do; &COV %end; STATE;
        WHERE BS>0;
        OUTPUT OUT=TLEMEAN STD=TLE_STD ALE_STD DLE_STD ALEp_STD DLEp_STD;
      RUN;
      
      DATA BOOT_TLE;
        SET TLEMEAN(WHERE=(_TYPE_=%eval(2**(&nCOVs+2)-1)));
        DROP _TYPE_ _FREQ_;
      RUN;
      proc sort data=S.RAD_LE;
        by BS %if &nCOVs >= 1 %then %do; &COV %end; age state;
      run;
      proc sort data=BOOT_TLE;
        by %if &nCOVs >= 1 %then %do; &COV %end; age state;
      run;
      DATA S.RAD_LESTD;
        MERGE S.RAD_LE(WHERE=(BS=0)) BOOT_TLE;
        DROP BS;
        by %if &nCOVs >= 1 %then %do; &COV %end; age state;
      RUN;
    %end;
    %else %if %eval(&BSIZE) = 0 %then %do;
      DATA S.RAD_LESTD;
        set S.RAD_LE;
        if BS=0;
        DROP BS ;
      run;
    %end;
    proc sort data=S.RAD_LESTD;
      by %if &nCOVs >= 1 %then %do; &COV %end; age state;
    run;
    Data S.RAD_LESTD&BEG;
      set S.RAD_LESTD;
      if AGE = &BEG;
    run;

    %if &LOI ne 1 %then %do;
      data S.RAD_LESTD_LOI;
        set S.RAD_LESTD;
          AGE     = AGE     * &LOI;
          TLE     = TLE     * &LOI;
          ALE     = ALE     * &LOI;
          DLE     = DLE     * &LOI;
          TLE_STD = TLE_STD * &LOI;
          ALE_STD = ALE_STD * &LOI;
          DLE_STD = DLE_STD * &LOI;
      run;
      Data S.RAD_LESTD&BEG._LOI;
        set S.RAD_LESTD_LOI;
        if AGE = &BEG;
      run;
    %end;
    
    %if &TXT_output=1 %then %do;
      options nosource;
      PROC EXPORT DATA=S.RAD_LESTD 
                  OUTFILE= "&workpath/RAD_A&BEG._&END_LT.b&BSIZE..txt" 
                  DBMS=TAB REPLACE;
      RUN; 
      PROC EXPORT DATA=S.RAD_LESTD&BEG 
                  OUTFILE= "&workpath/RAD_A&BEG.b&BSIZE..txt" 
                  DBMS=TAB REPLACE;
      RUN; 
      %if &LOI ne 1 %then %do;
        PROC EXPORT DATA=S.RAD_LESTD_LOI 
                    OUTFILE= "&workpath/RAD_A&BEG._&END_LT.b&BSIZE._LOI.txt" 
                    DBMS=TAB REPLACE;
        RUN; 
        PROC EXPORT DATA=S.RAD_LESTD&BEG._LOI 
                    OUTFILE= "&workpath/RAD_A&BEG.b&BSIZE._LOI.txt" 
                    DBMS=TAB REPLACE;
        RUN; 
      %end;
      options source;
    %end;
  %mend;
  %AfterBOOTSTRP_RADxCOV();

    proc datasets library=S;
      delete COVcomboIndex:   ;
    run;
%mend;