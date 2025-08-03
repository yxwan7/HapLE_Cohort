%MACRO MSLT_SIMxCOV_M_DX;
  ***********************************************;        
  %put *****SPACE_start*****; 
  DATA timenow; starting=datetime(); current=datetime(); 
  format starting datetime18.1; 
  format current  datetime18.1; 
  Total_Sim=%Eval(&SIMSIZE);
  Total_BS=%Eval(&BSIZE);
  BSnow=0;
  put current; 
  RUN;
    %if &TXT_output=1 %then %do;
      options nosource;
        PROC EXPORT DATA= WORK.Timenow 
                    OUTFILE= "&workpath/timenowBS,s&SIMSIZE.bx_&BSIZE..txt" 
                    DBMS=TAB REPLACE;
        RUN;
      options source;
    %end;
  ***********************************************;        
  %let nCOV=0;
  %do %while (%scan(&COV, &nCOV+1, %str( )) ne %str( ));
    %let nCOV=%eval(&nCOV+1);
  %end;
  %put nCOV=&nCOV;
  
  %if &nCOV >= 1 %then %do;
    %DO MA=1 %TO &nCOV;
      %LET COV&MA=%SCAN(&COV,&MA,' ');
      Data Datainput;
        Set S.&Datainput;
        if &depVAR ne .;
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
      %DO MA=1 %TO &nCOV;      
        DO &&COV&MA=1 TO &&LC&MA;
      %END; 
        COVindex+1;
        OUTPUT;
      %DO MA=1 %TO &nCOV;      
        END;
      %END;
    RUN;
    
  %end;
  %if &nCOV = 0 %then %do;
    Data Datainput;
      Set S.&Datainput;
      if &depVAR ne .;
      %if %scan(&subset, 1, %str( )) ne %str( ) %then %do;
        if &subset = 1;
      %end;
    run;
    %let nCOVindex = 0;
    Data S.Covcomboindex0;
      COVindex = 0;
    run;    
  %end;
  %if &nSession > 1 %then %do;
    %DO S=1 %TO &nSession;
      DATA S.COVcomboIndex&S;
        set S.COVcomboIndex0;
      RUN;
    %end;
  %end;
  %else %do;
    DATA S.COVcomboIndex1;
      set S.COVcomboIndex0;
    RUN;
  %end;
  proc sort data = S.COVcomboIndex0;
    by COVindex; 
  run;
  data _null_;
    set S.COVcomboIndex0 END=FINAL;
    if FINAL then call symput('nCOVindex', COVindex);
  run;
  *** POINT ESTIMATES USING THE FULL SAMPLE ***; 

  %MSLT_SIMxCOV_S(DATA=Datainput,S=0,VAR=&depVAR,NS=&nHealthState,COV=&COV,NC=&nCOV,
        WGT=&WGT,LOI=&LOI,BEG=&BEG,END=&END_SIM,SIMSIZE=&SIMSIZE,STRATCOV=&STRATCOV, REGCOV=&REGCOV, logitHazardModel=&logitHazard, 
        MaxYrSIM=&MaxYr_SIM, SIMSIZE_one=&SIMSIZE_one, SIMSIZE_part=&SIMSIZE_part, d_BEG=&d_BEG, d_AGE=&d_AGE, 
        CtrlCovModel=&CtrlCovModel, CtrlCovConti=&CtrlCovConti, CtrlCovCateg=&CtrlCovCateg, SimLifeLine=&SimLifeLine,
        TVcov=&TVcov, NoTrans=&NoTrans, nAbsorbState=&nAbsorbState, randomSeed=&randomSeed, BS_i=1234567,
        Interaction=&InteractionTerm, AGE_sq=&AGE_sq, AgeLowerLimit=&AgeLowerLimit);
  %MACRO BOOTSTRP_SIM1DX(DATA=,VAR=,NS=,COV=,NC=,STRATA=,PSU=,WGT=,STRATCOV=, REGCOV=, logitHazardModel=, 
                         MaxYrSIM=, SIMSIZE_one=, SIMSIZE_part=, d_BEG=, d_AGE=, 
                         CtrlCovModel=, CtrlCovConti=, CtrlCovCateg=, SimLifeLine=,
                         TVcov=, NoTrans=, nAbsorbState=, randomSeed=, Interaction=, AGE_sq=, AgeLowerLimit=);
  
    DATA S.SIM_LE;
      BS=0;         
      SET S.BSLE0;
    RUN;  
    **** To get simulated life lines -  begin ****;
    %if &SimLifeLine = 1 %then %do;
      DATA S.Sim_LifeLines;
        BS=0;         
        SET S.Sim_LifeLine0;
      RUN;  
      DATA S.Sim_SumLifeLines;
        BS=0;         
        SET S.Sim_SumLifeLine0;
      RUN;  
    %end;
    **** To get simulated life lines -  end ****;
  
    *** THE BOOTSTRAP PART ***;
    PROC SORT DATA=&DATA OUT=SAMPLE3;
      BY &STRATA &PSU;     
    RUN;  
  
    PROC SORT DATA=&DATA;
      BY ID ;     
    RUN;  
  
    DATA BS1;
      SET &DATA(KEEP=ID &STRATA &PSU);
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

    %DO Ibs=1+&d_BSIZE %TO (&BSIZE+&d_BSIZE) %BY &nSession;
      %DO Jbs=&Ibs %TO &Ibs+(&nSession-1);
  ***********************************************;        
        %put *******BSIZE*******;
        %put BSIZE now = &Ibs;
        %put J now = &Jbs;
        %put *****current*****; 
        DATA timenow; set timenow;  
          current=datetime();
          BSnow=%EVAL(&Jbs);
          put current; 
        RUN;
        %if &TXT_output=1 %then %do;
          options nosource;
            PROC EXPORT DATA= WORK.Timenow 
                        OUTFILE= "&workpath/timenowBS,s&SIMSIZE.bx_&BSIZE..txt" 
                        DBMS=TAB REPLACE;
            RUN;
          options source;
        %end;
  ***********************************************;        
  
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
                NEWSU[J,2]=RS[ROUND(RANUNI(ROUND(%if &randomSeed = 1 %then %do; DATETIME() %end; %else %do; &Jbs %end;))*Y[I,2]+0.5)];
                                                            
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
          BY &STRATA &PSU ;
        RUN;
  
        DATA NBSU2;
          SET NBSU;
          BY &STRATA &PSU;
            IF FIRST.&PSU THEN NU=0;
          NU+1;
          IF LAST.&PSU THEN OUTPUT;
        RUN;
  
        DATA S.BSMPL%EVAL(&Jbs-(&Ibs-1)); 
          MERGE NBSU2(IN=SU2) SAMPLE3;
          BY &STRATA &PSU;
          IF SU2;
          IF PS>1 THEN &WGT=&WGT*NU*(PS/(PS-1));  
        RUN;
      %END;

      %if &nSession > 1 %then %do;
        %DO SS=1 %TO &nSession;
          signon TASK%EVAL(&SS);
          %syslput BSMSsub=&SS;
          %syslput rpath            =&workpath;
          %syslput rnHealthState    =&nHealthState;
          %syslput rVAR             =&VAR;
          %syslput rCOV             =&COV;
          %syslput rNC              =&NC;
          %syslput rWGT             =&WGT;
          %syslput rLOI             =&LOI;
          %syslput rBEG             =&BEG;
          %syslput rEND             =&END_SIM;
          %syslput rSIMSIZE         =&SIMSIZE;
          %syslput rSTRATCOV        =&STRATCOV;
          %syslput rREGCOV          =&REGCOV;
          %syslput rlogitHazard     =&logitHazard;
          %syslput rMaxYrSIM        =&MaxYr_SIM;
          %syslput rSIMSIZE_one     =&SIMSIZE_one;
          %syslput rSIMSIZE_part    =&SIMSIZE_part;
          %syslput rd_BEG           =&d_BEG;
          %syslput rd_AGE           =&d_AGE;
          %syslput rCtrlCovModel    =&CtrlCovModel;
          %syslput rCtrlCovConti    =&CtrlCovConti;
          %syslput rCtrlCovCateg    =&CtrlCovCateg;
          %syslput rSimLifeLine     =&SimLifeLine;
          %syslput rTVcov           =&TVcov;
          %syslput rNoTrans         =&NoTrans;
          %syslput rnAbsorbState    =&nAbsorbState;
          %syslput rrandomSeed      =&randomSeed;
          %syslput rBS_i            =%sysevalf(&Ibs + &SS, int);
          %syslput rInteraction     =&Interaction;
          %syslput rAGE_sq          =&AGE_sq;
          %syslput rAgeLowerLimit   =&AgeLowerLimit;

          RSUBMIT TASK%EVAL(&SS) WAIT=NO;
            %INCLUDE "&rpath/SPACE_macro.sas";
            %INCLUDE "&rpath/MSLT_SIMxCOV_S.sas";
            PROC PRINTTO NEW LOG="&rpath/BSMSlog&BSMSsub..LOG";
            libname S "&rpath";
            %MSLT_SIMxCOV_S(DATA=S.BSMPL&BSMSsub,S=&BSMSsub,VAR=&rVAR,NS=&rnHealthState,COV=&rCOV,NC=&rNC,
                  WGT=&rWGT,LOI=&rLOI,BEG=&rBEG,END=&rEND,SIMSIZE=&rSIMSIZE,STRATCOV=&rSTRATCOV, REGCOV=&rREGCOV, logitHazardModel=&rlogitHazard, 
                  MaxYrSIM=&rMaxYrSIM, SIMSIZE_one=&rSIMSIZE_one, SIMSIZE_part=&rSIMSIZE_part, d_BEG=&rd_BEG, d_AGE=&rd_AGE,
                  CtrlCovModel=&rCtrlCovModel, CtrlCovConti=&rCtrlCovConti, CtrlCovCateg=&rCtrlCovCateg, SimLifeLine=&rSimLifeLine,
                  TVcov=&rTVcov, NoTrans=&rNoTrans, nAbsorbState=&rnAbsorbState, randomSeed=&rrandomSeed, BS_i=&rBS_i,
                  Interaction=&rInteraction, AGE_sq=&rAGE_sq, AgeLowerLimit=&rAgeLowerLimit);
          ENDRSUBMIT;
        %END;
    
        WAITFOR _ALL_ %DO SS=1 %TO &nSession; TASK%EVAL(&SS) %END;;
        %DO SS=1 %TO &nSession;
          RGET TASK%EVAL(&SS);
          SIGNOFF TASK%EVAL(&SS);
        %END;
  
      %END;
  
      %if &nSession = 1 %then %do;
        %MSLT_SIMxCOV_S(DATA=S.BSMPL1,S=1,VAR=&depVAR,NS=&nHealthState,COV=&COV,NC=&nCOV,
              WGT=&WGT,LOI=&LOI,BEG=&BEG,END=&END_SIM,SIMSIZE=&SIMSIZE,STRATCOV=&STRATCOV, REGCOV=&REGCOV, logitHazardModel=&logitHazard, 
              MaxYrSIM=&MaxYrSIM, SIMSIZE_one=&SIMSIZE_one, SIMSIZE_part=&SIMSIZE_part, d_BEG=&d_BEG, d_AGE=&d_AGE,
              CtrlCovModel=&CtrlCovModel, CtrlCovConti=&CtrlCovConti, CtrlCovCateg=&CtrlCovCateg, SimLifeLine=&SimLifeLine,
              TVcov=&TVcov, NoTrans=&NoTrans, nAbsorbState=&nAbsorbState, randomSeed=&randomSeed, BS_i=&Ibs,
              Interaction=&Interaction, AGE_sq=&AGE_sq, AgeLowerLimit=&AgeLowerLimit);
      %END;
  
      %if &nSession > 1 %then %do;
        %DO S=0 %TO (&nSession-1);    
          DATA LE%EVAL(&Ibs+&S);        
            BS=%EVAL(&Ibs+&S);          
            SET S.BSLE%EVAL(&S+1);    
          RUN;
          %put *******XXXX*******;
          %put I now = &Ibs;
          %put S now = &S;
          %put BS now = %EVAL(&Ibs+&S);
          %put *******XXXX*******;

          **** To get simulated life lines -  begin ****;
          %if &SimLifeLine = 1 %then %do;
            DATA Sim_LifeLine%EVAL(&Ibs+&S);
              BS=%EVAL(&Ibs+&S);          
              SET S.Sim_LifeLine%EVAL(&S+1);
            RUN;
            DATA Sim_SumLifeLine%EVAL(&Ibs+&S);
              BS=%EVAL(&Ibs+&S);          
              SET S.Sim_SumLifeLine%EVAL(&S+1);
            RUN;
          %end;
          **** To get simulated life lines -  end ****;

        %END;
      %end;
      %if &nSession = 1 %then %do;
          DATA LE%EVAL(&Ibs);        
            BS=%EVAL(&Ibs);          
            SET S.BSLE1;    
          RUN;
          %put *******XXXX*******;
          %put I now = &Ibs;
          %put BS now = %EVAL(&Ibs);
          %put *******XXXX*******;
          **** To get simulated life lines -  begin ****;
          %if &SimLifeLine = 1 %then %do;
            DATA Sim_LifeLine%EVAL(&Ibs);
              BS=%EVAL(&Ibs);          
              SET S.Sim_LifeLine1;
            RUN;
            DATA Sim_SumLifeLine%EVAL(&Ibs);
              BS=%EVAL(&Ibs);          
              SET S.Sim_SumLifeLine1;
            RUN;
          %end;
          **** To get simulated life lines -  end ****;
      %end;
  
      DATA BSLE;   
        SET 
          %DO J=&Ibs %TO &Ibs+(&nSession-1); 
            LE%EVAL(&J) 
          %END;;
      RUN;
    
      DATA S.SIM_LE;
        SET S.SIM_LE BSLE;
        IF BS NE . & BS<=&BSIZE+&d_BSIZE;
      RUN;
      **** To get simulated life lines -  begin ****;
      %if &SimLifeLine = 1 %then %do;
        DATA Sim_LifeLineAll;   
          SET 
            %DO J=&Ibs %TO &Ibs+(&nSession-1); 
              Sim_LifeLine%EVAL(&J) 
            %END;;
        RUN;
        DATA S.Sim_LifeLines;
          SET S.Sim_LifeLines Sim_LifeLineAll;
          IF BS NE . & BS<=&BSIZE+&d_BSIZE;
          drop COVindex;
        RUN;
        DATA Sim_SumLifeLineAll;   
          SET 
            %DO J=&Ibs %TO &Ibs+(&nSession-1); 
              Sim_SumLifeLine%EVAL(&J) 
            %END;;
        RUN;
        DATA S.Sim_SumLifeLines;
          SET S.Sim_SumLifeLines Sim_SumLifeLineAll;
          IF BS NE . & BS<=&BSIZE+&d_BSIZE;
        RUN;

      %end;
      **** To get simulated life lines -  end ****;

    %END;
    **** To get simulated life lines -  begin ****;
    %if &SimLifeLine = 1 %then %do;
      DATA  S.Sim_SumLifeLines;
        SET S.Sim_SumLifeLines;
        rename BH=STATE;
        drop COVindex BEGST ENDST BAGE EAGE ;
      RUN;
    %end;
    **** To get simulated life lines -  end ****;
  %MEND;
  
  %BOOTSTRP_SIM1DX(DATA=Datainput,VAR=&depVAR,NS=&nHealthState,COV=&COV,NC=&nCOV,
                  STRATA=&STRATA,PSU=&PSU,WGT=&WGT,STRATCOV=&STRATCOV, REGCOV=&REGCOV, logitHazardModel=&logitHazard, 
                  MaxYrSIM=&MaxYr_SIM, SIMSIZE_one=&SIMSIZE_one, SIMSIZE_part=&SIMSIZE_part, d_BEG=&d_BEG, d_AGE=&d_AGE,
                  CtrlCovModel=&CtrlCovModel, CtrlCovConti=&CtrlCovConti, CtrlCovCateg=&CtrlCovCateg, SimLifeLine=&SimLifeLine,
                  TVcov=&TVcov, NoTrans=&NoTrans, nAbsorbState=&nAbsorbState, randomSeed=&randomSeed,
                  Interaction=&InteractionTerm, AGE_sq=&AGE_sq, AgeLowerLimit=&AgeLowerLimit);
  
  %Macro AfterBOOTSTRP_SIM1DX;
  
  data tttt;
    set S.Covcomboindex0;
    do Iage = &BEG to (&BEG+&d_BEG) by &d_AGE;
      do age = &BEG to &END_SIM;
        output;
      end; 
    end; 
  run;
    *** PCT LE & STD ERR ***;
    %if %eval(&BSIZE) > 0 %then %do;
      PROC MEANS DATA=S.SIM_LE STD NOPRINT;
        %if &nHealthState=2 %then %do;
          VAR TLE ;
          CLASS IAGE %if &nCOV >= 1 %then %do; &COV %end; BH;
          WHERE BS>0;
          OUTPUT OUT=TLEMEAN STD=TLE_STD ;
        %end;
        %if &nHealthState >= 3 %then %do;
          VAR TLE   %DO U=1 %TO &nHealthState -1; LEs&U     %END;
                    %DO U=1 %TO &nHealthState -1; LEp&U     %END;
					/* <<<<<<<<<<< ??????? VAR ?? >>>>>>>>>>> */
        %IF %eval(&nHealthState) >= 5 %THEN %DO;
          %let num_living_states = %eval(&nHealthState - &nAbsorbState);
          %DO i = 1 %TO %eval(&num_living_states - 1);
            %DO j = %eval(&i + 1) %TO &num_living_states;
               LEs&i.&j LEp&i.&j
            %END;
          %END;
        %END;
        /* <<<<<<<<<<< ? VAR ???? >>>>>>>>>>> */
              ;
          CLASS IAGE %if &nCOV >= 1 %then %do; &COV %end; BH;
          WHERE (BS>0);
          OUTPUT OUT=TLEMEAN STD= TLE_STD   %DO U=1 %TO &nHealthState -1; LEs&U._STD    %END;
                                            %DO U=1 %TO &nHealthState -1; LEp&U._STD    %END;
											 /* <<<<<<<<<<< ???????? STD ???? >>>>>>>>>>> */
        %IF %eval(&nHealthState) >= 5 %THEN %DO;
          %let num_living_states = %eval(&nHealthState - &nAbsorbState);
          %DO i = 1 %TO %eval(&num_living_states - 1);
            %DO j = %eval(&i + 1) %TO &num_living_states;
               LEs&i.&j._STD LEp&i.&j._STD
            %END;
          %END;
        %END;
        /* <<<<<<<<<<< ? STD ?????? >>>>>>>>>>> */
                                  ;
        %end;
      run;
      DATA BOOT_TLE;
        SET TLEMEAN(WHERE=(_TYPE_=%eval(2**(&nCOV+2)-1)));
        DROP _TYPE_ _FREQ_;
      RUN;
      
      DATA S.SIM_LESTD;
        MERGE S.SIM_LE(WHERE=(BS=0)) BOOT_TLE;
        DROP BS _TYPE_ _FREQ_;
      RUN;
    %end;
    %else %if %eval(&BSIZE) = 0 %then %do;
      DATA S.SIM_LESTD;
        set S.SIM_LE(WHERE=(BS=0));
        DROP BS ;
      RUN;
    %end;
      
    PROC SORT DATA=S.SIM_LESTD;
        BY IAGE %if &nCOV >= 1 %then %do; &COV %end; BH;
    RUN;  
    DATA S.SIM_LESTD (rename=(BH=State IAGE=Age));
        set S.SIM_LESTD;
        drop _TYPE_;
    RUN;  
    DATA S.SIM_LE (rename=(BH=State IAGE=Age));
        set S.SIM_LE;
        drop _TYPE_;
    RUN;  

    %if &LOI ne 1 %then %do;
      data S.SIM_LESTD_LOI;
        set S.SIM_LESTD;
          AGE      = AGE      * &LOI;
          TLE       = TLE       * &LOI;
          %DO U=1 %TO &nHealthState-1; 
            LEs&U         = LEs&U         * &LOI;
			LEs&U._STD    = LEs&U._STD    * &LOI; /* <-- ??/?? */
          %END;
            /* <<<<<<<<<<< ??????? LEs_ij ??? >>>>>>>>>>> */
      %IF %eval(&nHealthState) >= 5 %THEN %DO;
        %let num_living_states = %eval(&nHealthState - &nAbsorbState);
        %DO i = 1 %TO %eval(&num_living_states - 1);
          %DO j = %eval(&i + 1) %TO &num_living_states;
             LEs&i.&j      = LEs&i.&j      * &LOI;
			   LEs&I._&J._STD  = LEs&I._&J._STD  * &LOI; /* <-- ??/?? */
             /* LEp&i.&j ?? */
          %END;
        %END;
      %END;
      /* <<<<<<<<<<< ? LEs_ij ???? >>>>>>>>>>> */
      run;
    %end;
    %if &TXT_output=1 %then %do;
      options nosource;
      PROC EXPORT DATA=S.SIM_LESTD 
                  OUTFILE= "&workpath/SIM_A&BEG.s&SIMSIZE.b&BSIZE..txt" 
                  DBMS=TAB REPLACE;
      RUN; 
      %if &LOI ne 1 %then %do;
        PROC EXPORT DATA=S.SIM_LESTD_LOI 
                    OUTFILE= "&workpath/SIM_A&BEG.s&SIMSIZE.b&BSIZE._LOI.txt" 
                    DBMS=TAB REPLACE;
        RUN; 
      %end;
      options source;

    %end;
    proc datasets library=S;
      delete BSLE: bsmpl:
              %do i = 0 %to &nSession; 
                %if &SimLifeLine = 1 %then %do;
                  Sim_SumLifeLine&i sim_lifeline&i 
                %end; 
                TransProbOrRateAll&i
              %end; 
        ;
    run;

  %Mend;
  %AfterBOOTSTRP_SIM1DX;
    proc datasets library=S;
      delete COVcomboIndex:   ;
    run;

  ***********************************************;        
        DATA timenow; set timenow;
          current=datetime();
          BSnow=%eval(&BSIZE+&d_BSIZE);
          put current; 
        RUN;
    %if &TXT_output=1 %then %do;
      options nosource;
        PROC EXPORT DATA= WORK.Timenow 
                    OUTFILE= "&workpath/timenowBS,s&SIMSIZE.bx_&BSIZE..txt" 
                    DBMS=TAB REPLACE;
        RUN;
      options source;
    %end;
  ***********************************************;        
%mend;
