%MACRO MSLT_SIMxCOV_S(DATA=,S=,VAR=,NS=,COV=,NC=,WGT=,LOI=,BEG=,END=,SIMSIZE=,STRATCOV=,REGCOV=,logitHazardModel=,
                      MaxYrSIM=,SIMSIZE_one=,SIMSIZE_part=,d_BEG=,d_AGE=,CtrlCovModel=,CtrlCovConti=,CtrlCovCateg=,SimLifeLine=,
                      TVcov=,NoTrans=,nAbsorbState=,randomSeed=,BS_i=,Interaction=,AGE_sq=,AgeLowerLimit=);  

%put MSLT_SIMxCOV_S starts;
%put DATA              = &DATA             ;
%put S                 = &S                ;
%put VAR               = &VAR              ;
%put NS                = &NS               ;
%put COV               = &COV              ;
%put NC                = &NC               ;
%put WGT               = &WGT              ;
%put LOI               = &LOI              ;
%put BEG               = &BEG              ;
%put END               = &END              ;
%put SIMSIZE           = &SIMSIZE          ;
%put STRATCOV          = &STRATCOV         ;
%put REGCOV            = &REGCOV           ;
%put logitHazardModel  = &logitHazardModel ;
%put CtrlCovModel      = &CtrlCovModel     ;
%put CtrlCovConti      = &CtrlCovConti     ;
%put CtrlCovCateg      = &CtrlCovCateg     ;
%put TVcov             = &TVcov            ;
%put randomSeed        = &randomSeed       ;
%put BS_i              = &BS_i             ;
%put AGE_sq            = &AGE_sq           ;
%put AgeLowerLimit     = &AgeLowerLimit    ;




  Data empty;
    done=1;
  run;

  %if &NC >= 1 %then %do;
    %DO MA=1 %TO &NC;
      %LET COV&MA=%SCAN(&COV,&MA,' ');
  
      PROC SORT DATA=S.COVcomboIndex&S;
        BY &&COV&MA;
      RUN;
  
      DATA TEMP&MA(KEEP=COVindex &&COV&MA);
        SET S.COVcomboIndex&S;
        BY &&COV&MA;
        IF FIRST.&&COV&MA;
      RUN;
  
      DATA _NULL_;
        SET TEMP&MA END=FINAL;
        N+1;
        IF FINAL THEN CALL SYMPUT("LC&MA", N);  *** LC: LEVEL OF COVARIATES 1,2,... ***;
      RUN;
    %END;
  %end;
  %if &NC = 0 %then %do;
  %end;

  %let nREGCOV=0;
  %let nSTRATCOV=0;
  %if &NC >= 1 %then %do;
    %do MA=1 %to &NC;
      %put COV&MA=&&COV&MA ;
    %end;
    %do %while (%scan(&REGCOV, &nREGCOV+1, %str( )) ne %str( ));
      %let nREGCOV=%eval(&nREGCOV+1);
    %end;
    %DO j0=1 %TO &nREGCOV;
      %LET REGCOV&j0=%SCAN(&REGCOV,&j0,' ');
      %put REGCOV&j0=&&REGCOV&j0;
    %end;

    %do %while (%scan(&STRATCOV, &nSTRATCOV+1, %str( )) ne %str( ));
      %let nSTRATCOV=%eval(&nSTRATCOV+1);
    %end;
    %DO j0=1 %TO &nSTRATCOV;
      %LET STRATCOV&j0=%SCAN(&STRATCOV,&j0,' ');
      %put STRATCOV&j0=&&STRATCOV&j0;
    %end;
  %end;
  %put NC=&NC;
  %put nREGCOV=&nREGCOV;
  %put nSTRATCOV=&nSTRATCOV;

  /***Code CtrlVar - begin *************************;*/        
  %let nCTRLCOVconti=0;
  %let nCTRLCOVcateg=0;
    %do %while (%scan(&CTRLCOVconti, &nCTRLCOVconti+1, %str( )) ne %str( ));
      %let nCTRLCOVconti=%eval(&nCTRLCOVconti+1);
    %end;
    %do j0=1 %TO &nCTRLCOVconti;
      %LET CTRLCOVconti&j0=%SCAN(&CTRLCOVconti,&j0,' ');
      %put CTRLCOVconti&j0=&&CTRLCOVconti&j0;
    %end;
  
    %do %while (%scan(&CTRLCOVcateg, &nCTRLCOVcateg+1, %str( )) ne %str( ));
      %let nCTRLCOVcateg=%eval(&nCTRLCOVcateg+1);
    %end;
    %do j0=1 %TO &nCTRLCOVcateg;
      %LET CTRLCOVcateg&j0=%SCAN(&CTRLCOVcateg,&j0,' ');
      %put CTRLCOVcateg&j0=&&CTRLCOVcateg&j0;
    %end;
  %let nCTRLCOV = %eval(&nCTRLCOVconti + &nCTRLCOVcateg);
  %put nCTRLCOVconti=&nCTRLCOVconti;
  %put nCTRLCOVcateg=&nCTRLCOVcateg;
  %put nCTRLCOV     =&nCTRLCOV;
  /*%if &nCTRLCOVconti >= 1 %then %do; %do j0=1 %TO &nCTRLCOVconti; %put CtrlCOVconti&j0 = &&CtrlCOVconti&j0 ; %end; %end;
  %if &nCTRLCOVcateg >= 1 %then %do; %do j0=1 %TO &nCTRLCOVcateg; %put CtrlCOVcateg&j0 = &&CtrlCOVcateg&j0 ; %end; %end;*/
  
  Data COVcomboIndex00;
    set S.COVcomboIndex&S;
  run;
  /***Code CtrlVar - end *************************;*/        
  /***Code TVcov - begin *************************;*/        
  %let nTVcov=0;
  %do %while (%scan(&TVcov, &nTVcov+1, %str( )) ne %str( ));
    %let nTVcov=%eval(&nTVcov+1);
  %end;
  %put nTVcov = &nTVcov;
  %if &nTVcov >= 1 %then %do;
    %DO j0=1 %TO &nTVcov;
      %LET TVcov&j0=%SCAN(&TVcov,&j0,' ');
      %put TVcov&j0=&&TVcov&j0;
    %end;
  %end; 
  /*%if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; %put TVcov&j0 = &&TVcov&j0 ; %end; %end;*/
  /***Code TVcov - end *************************;*/        

  /***Code No Transitons - begin *************************;*/        
  %macro No_Transition(NS=, NaS=, NoTrans= );
    %let nNoTrans=0;
    %do %while (%scan(&NoTrans, &nNoTrans+1, %str( )) ne %str( ));
      %let nNoTrans=%eval(&nNoTrans+1);
    %end;
    %put Inputted numbers of NoTrans = &nNoTrans;
    
    Data Trans;
      %do beg_i=1 %to %eval(&NS-&NaS);
        %do end_i =1 %to %eval(&NS);
          Begst = &beg_i;
          Endst = &end_i;
          %if &end_i <= %eval(&NS - &NaS) %then %do;
            Trans = 1;
          %end;
          %else %if &end_i > %eval(&NS - &NaS) %then %do;
            Trans = 3;
          %end;
          output;
        %end;
      %end;
    run;
    
    %if &nNoTrans >= 1 %then %do;
      %let nNoTrans_chk = 0;
      %do i =1 %to &nNoTrans;
        %LET NoTrans_&i=%SCAN(&NoTrans,&i,' ');
        %put NoTrans_&i=&&NoTrans_&i;
        %let NoTrans_&i.b_index= %index(&&NoTrans_&i,B);
        %let NoTrans_&i.e_index= %index(&&NoTrans_&i,E);
        /*%put location = (&&NoTrans_&i.b_index, &&NoTrans_&i.e_index)  ;*/
        %if (%eval(&&NoTrans_&i.b_index) + 1) < &&NoTrans_&i.e_index %then %do;  /* Bi and Ej locations correct */
          %let NoTrans_&i.b = %substr(&&NoTrans_&i,&&NoTrans_&i.b_index+1, &&NoTrans_&i.e_index - &&NoTrans_&i.b_index-1);
          %let NoTrans_&i.e = %substr(&&NoTrans_&i,&&NoTrans_&i.e_index+1, %length(&&NoTrans_&i) - &&NoTrans_&i.e_index);
          %if ~(%datatyp(&&NoTrans_&i.b)=NUMERIC and %datatyp(&&NoTrans_&i.e)=NUMERIC) %then %do;  /* Bi or Ej is not numeric */
            %put Plase assign NUMERICAL number for transition: &&NoTrans_&i... Ressign 0 and not consider this transition.;
            %let NoTrans_&i.b = 0;
            %let NoTrans_&i.e = 0;
          %end; 
          %else %if (%datatyp(&&NoTrans_&i.b)=NUMERIC and %datatyp(&&NoTrans_&i.e)=NUMERIC) and   /* Bi and Ej are numeric, but Bi or Ej is >= NS */
              (%substr(&&NoTrans_&i,&&NoTrans_&i.b_index+1, &&NoTrans_&i.e_index - &&NoTrans_&i.b_index-1) > &NS - &NaS or 
               %substr(&&NoTrans_&i,&&NoTrans_&i.e_index+1, %length(&&NoTrans_&i) - &&NoTrans_&i.e_index)  > &NS - &NaS   ) 
              %then %do;
            %put Plase assign correct number for transition: &&NoTrans_&i... Ressign 0 and not consider this transition.;
            %let NoTrans_&i.b = 0;
            %let NoTrans_&i.e = 0;
          %end; 
          %else %if (%datatyp(&&NoTrans_&i.b)=NUMERIC and %datatyp(&&NoTrans_&i.e)=NUMERIC) and   /* Bi and Ej are numeric, and Bi and Ej are < NS --> the correct situation */
              (%substr(&&NoTrans_&i,&&NoTrans_&i.b_index+1, &&NoTrans_&i.e_index - &&NoTrans_&i.b_index-1) <= &NS - &NaS and 
               %substr(&&NoTrans_&i,&&NoTrans_&i.e_index+1, %length(&&NoTrans_&i) - &&NoTrans_&i.e_index)  <= &NS - &NaS    ) 
              %then %do;
            %let nNoTrans_chk = %eval(&nNoTrans_chk + 1);
          %end; 
        %end; 
        %else %do;                                                                                /* other wrong situations */
          %put Plase assign NUMERICAL number for transition: &&NoTrans_&i... Ressign 0 and not consider this transition.;
          %let NoTrans_&i.b = 0;
          %let NoTrans_&i.e = 0;
        %end; 
        %put ----- NoTrans_&i = (&&NoTrans_&i.b, &&NoTrans_&i.e)  ;
      %end;
      %put ----- nNoTrans_chk = &nNoTrans_chk  ;
    
      %if &nNoTrans_chk >= 1 %then %do;
        %let nNoTrans_valid_chk = 0;
        Data NoTrans;
          %do i =1 %to &nNoTrans_chk;
            %if &&NoTrans_&i.b > 0 and &&NoTrans_&i.e > 0 %then %do;
              Begst = &&NoTrans_&i.b;
              Endst = &&NoTrans_&i.e;
              NoTrans = 1;
              output;
              %let nNoTrans_valid_chk = %eval(&nNoTrans_valid_chk + 1);
            %end; 
          %end;
        run;
    
        %if &nNoTrans_valid_chk >= 1 %then %do;
          proc sort data=NoTrans OUT=NoTrans_unique NODUPKEY;
            by Begst Endst; 
          run;
    
          Data NoTrans_unique;
            set NoTrans_unique;
            by NoTrans;
            if first.NoTrans then count = 0;
            count +1 ;
          run;
    
          data _NULL_;
          	set NoTrans_unique nobs=n;
          	call symputx('nNoTrans_valid',n);
          	stop;
          run;
          %put nNoTrans_valid = &nNoTrans_valid;  
    
          %if &nNoTrans_valid > 0 %then %do;
            %do i =1 %to &nNoTrans_valid;
              Data _NULL_;
                set NoTrans_unique;
                if count = &i;
                    call symputx(cats('NoTrans_',&i,'b_valid'),Begst);
                    call symputx(cats('NoTrans_',&i,'e_valid'),Endst);
              run;
              %put ---------------- NoTrans_&i = (&&NoTrans_&i.b_valid, &&NoTrans_&i.e_valid)  ;
            %end;
    
            Data Transitions;
              merge Trans NoTrans_unique;
              by Begst Endst; 
              TransitionType = Trans;
              if NoTrans = 1 then TransitionType = 2;
              if TransitionType = 3 then Trans_label = "Absorbing";
              if TransitionType = 1 then Trans_label = "Trans Yes";
              if TransitionType = 2 then Trans_label = "Trans NO";
            run;
            Data Transitions;
              set Transitions;
              drop Trans NoTrans count;
            run;
          %end; 
        %end;
      %end; 
      %else %if &nNoTrans_chk = 0 %then %do;
        Data Transitions;
          set Trans ;
          TransitionType = Trans;
          if TransitionType = 3 then Trans_label = "Absorbing";
          if TransitionType = 1 then Trans_label = "Trans Yes";
        run;
        Data Transitions;
          set Transitions;
          drop Trans ;
        run;
      %end; 
    %end;
    %else %if &nNoTrans = 0 %then %do;
      Data Transitions;
        set Trans ;
        TransitionType = Trans;
        if TransitionType = 3 then Trans_label = "Absorbing";
        if TransitionType = 1 then Trans_label = "Trans Yes";
      run;
      Data Transitions;
        set Transitions;
        drop Trans ;
      run;
    
    %end;
  
  %mend;
  
  %No_Transition(NS=&NS, NaS=&nAbsorbState, NoTrans=&NoTrans );
  /***Code No Transitons - end *************************;*/        
  
  PROC SORT DATA=S.COVcomboIndex&S;
    BY COVindex;
  RUN;

  DATA SEM0;
    *SET &DATA(WHERE=(AGE>=&BEG & &WGT > 0 ));
    SET &DATA(WHERE=(AGE>=&AgeLowerLimit & &WGT > 0 ));
    if &VAR ne .;
      %if &NC >= 1 %then %do;
        %DO MA=1 %TO &NC;      
          if &&COV&MA ne .;
        %END;    
      %end;
  run;
  PROC SORT DATA=SEM0;
    BY ID AGE &VAR;
  RUN;
  DATA SEM;
    SET SEM0;
    BY ID AGE;
    IF FIRST.ID THEN DO;
      PREVAGE=.;
      PREVST=.;
      PREVWGT=.;
      %if &nTVcov >= 1 %then %do; %DO j0=1 %TO &nTVcov; &&TVcov&j0.._PREV = .; %end; %end; 
    END;
    OUTPUT;
    PREVAGE=AGE;
    PREVST=&VAR;
    PREVWGT=&WGT;
    %if &nTVcov >= 1 %then %do; %DO j0=1 %TO &nTVcov; &&TVcov&j0.._PREV = &&TVcov&j0; %end; %end; 
    RETAIN PREVAGE PREVST PREVWGT
           %if &nTVcov >= 1 %then %do; %DO j0=1 %TO &nTVcov; &&TVcov&j0.._PREV %end; %end; 
          ;
  RUN;

  DATA SEM1(DROP=MIDAGE AGE &VAR &WGT J PREVAGE PREVST PREVWGT 
                 %if &nTVcov >= 1 %then %do; %DO j0=1 %TO &nTVcov; &&TVcov&j0 &&TVcov&j0.._PREV %end; %end; 
            RENAME=(NAGE=AGE HS=&VAR NWGT=&WGT
                    %if &nTVcov >= 1 %then %do; %DO j0=1 %TO &nTVcov; &&TVcov&j0.._New = &&TVcov&j0 %end; %end; 
                    )
            ) ;
    SET SEM;
    BY ID;
    IF FIRST.ID THEN DO;
      HS=&VAR;
      NAGE=AGE;
      NWGT=&WGT;
      %if &nTVcov >= 1 %then %do;
        %DO j0=1 %TO &nTVcov;
          &&TVcov&j0.._New = &&TVcov&j0;
        %end;
      %end; 
      OUTPUT;
    END;
    ELSE DO;
      IF &VAR NE &NS THEN DO;
        if PREVAGE+1 < AGE then do;
          MIDAGE=PREVAGE+CEIL((AGE-PREVAGE)*RANUNI(ROUND(%if &randomSeed = 1 %then %do; DATETIME() %end; %else %do; &BS_i %end;)))-1;

          DO J=PREVAGE+1 TO AGE;
            IF J<=MIDAGE THEN DO;
              HS=PREVST; 
              NWGT=PREVWGT+ROUND((&WGT-PREVWGT)/(AGE-PREVAGE));
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0.._PREV ; %end; %end;
            END;
            ELSE DO;
              HS=&VAR;
              NWGT=&WGT;
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
            END;
            NAGE=J;
            PREVWGT=NWGT;
            OUTPUT;
          END;
        end;
        ELSE IF PREVAGE+1=AGE THEN DO;
          HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        END;
        else if PREVAGE <= AGE < PREVAGE+1 then do; 
          HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        end;
      END;
      ELSE DO;
        IF PREVAGE+1<AGE THEN DO;
          DO J=PREVAGE+1 TO AGE;
            IF J NE AGE THEN DO;
              HS=PREVST; 
              NWGT=PREVWGT+ROUND((&WGT-PREVWGT)/(AGE-PREVAGE));
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0.._PREV ; %end; %end;
            END;
            ELSE DO;
              HS=&VAR;
              NWGT=&WGT;
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
            END;
            NAGE=J;
            PREVWGT=NWGT;
            OUTPUT;
          END;
        END;
        ELSE IF PREVAGE+1=AGE THEN DO;
          HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        END;
        else if PREVAGE <= AGE < PREVAGE+1 then do; 
          HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        end;
      END;
    END;
  RUN;        
%put after SEM1;

  %if &NS=2 %then %do;
    DATA PRSNYR(RENAME=(&VAR=ENDST));
      SET SEM1;
      BY ID;
      BEGST=1;
    RUN;
  %end;
  %else %if &NS>=3 %then %do;
    DATA PRSNYR(RENAME=(&VAR=ENDST));  
      SET SEM1;
      BY ID;
      BEGST=LAG(&VAR);
      &WGT=LAG(&WGT);
      IF FIRST.ID THEN DO; 
        **AGE=0;
        BEGST=0;
        DELETE;   
      END;
    RUN;
  %end;

  Data SEM0; 
    set empty;
  run;
  Data SEM; 
    set empty;
  run;
  Data SEM1; 
    set empty;
  run;

%put after PRSNYR;

  %if &NC >= 1 %then %do;
    data _null_;
      set S.COVcomboIndex&S END=FINAL;
      if FINAL then call symput('nCOVindex', COVindex);
    run;
  %end;
  %if &NC = 0 %then %do;
    %let nCOVindex = 0;
    run;
  %end;

  DATA PS2;
    %if &NC >= 1 %then %do;
      %DO MA=1 %TO &NC;      
        DO &&COV&MA=1 TO &&LC&MA;
      %END;
          DO AGE=&BEG TO &END;
            CONTROL=1;
            OUTPUT;
          END;
      %DO MA=1 %TO &NC;      
        END;
      %END;
    %end;
    %if &NC = 0 %then %do;
      DO AGE=&BEG TO &END;
        CONTROL=1;
        OUTPUT;
      END;
    %end;
  RUN;

  /***Code CtrlVar - begin *************************;*/        
  DATA CtrlCOVcomboIndexPrev;
    %if &NC >= 1 %then %do;
      %DO MA=1 %TO &NC;      
        DO &&COV&MA=1 TO &&LC&MA;
      %END; 
          DO AGE=&BEG TO &END;
            CtrlCOVindexPrev+1;
            OUTPUT;
          END;
      %DO MA=1 %TO &NC;      
        END;
      %END;
    %end;
    %if &NC = 0 %then %do;
          DO AGE=&BEG TO &END;
            CtrlCOVindexPrev+1;
            OUTPUT;
          END;
    %end;
  RUN;
  data _null_;
    set CtrlCOVcomboIndexPrev END=FINAL;
    if FINAL then call symput('nCtrlCOVindexPrev', CtrlCOVindexPrev);
  run;
%put nCtrlCOVindexPrev = &nCtrlCOVindexPrev ;

  %if &nREGCOV >= 1 %then %do;
    %DO MA=1 %TO &nREGCOV;
      %LET REGCOV&MA=%SCAN(&REGCOV,&MA,' ');
  
      PROC SORT DATA=S.COVcomboIndex&S;
        BY &&REGCOV&MA;
      RUN;
  
      DATA TEMP&MA(KEEP=&&REGCOV&MA);
        SET S.COVcomboIndex&S;
        BY &&REGCOV&MA;
        IF FIRST.&&REGCOV&MA;
      RUN;
  
      DATA _NULL_;
        SET TEMP&MA END=FINAL;
        N+1;
        IF FINAL THEN CALL SYMPUT("LCREGCOV&MA", N);  *** LCREGCOV: LEVEL OF REGCOV 1,2,... ***;
      RUN;
    %END;
  %end;

  DATA CtrlCOVcomboIndexTrans;
    %if &nREGCOV >= 1 %then %do;
      %DO MA=1 %TO &nREGCOV;      
        DO &&REGCOV&MA=1 TO &&LCREGCOV&MA;
      %END; 
          DO AGE=&BEG TO &END;
            CtrlCOVindexTrans+1;
            OUTPUT;
          END;
      %DO MA=1 %TO &nREGCOV;      
        END;
      %END;
    %end;
    %if &nREGCOV = 0 %then %do;
          DO AGE=&BEG TO &END;
            CtrlCOVindexTrans+1;
            OUTPUT;
          END;
    %end;
  RUN;
  data _null_;
    set CtrlCOVcomboIndexTrans END=FINAL;
    if FINAL then call symput('nCtrlCOVindexTrans', CtrlCOVindexTrans);
  run;
%put nCtrlCOVindexTrans = &nCtrlCOVindexTrans ; 

  /***Code CtrlVar - end *************************;*/        

  %if &NS=2 %then %do;
    DATA PREV4;
      set PS2;
        P1=1.0;                         /*because only have the state of "alive" for non-dead*/
      drop control;
    run;
  %end;
  %else %if &NS>=3 %then %do;
  /***Code CtrlVar - begin *************************;*/        
    %if ~(&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;  /* control variables: NO*/
  /***Code CtrlVar - end *************************;*/        
      DATA PSBACK000;
        SET PS2 &DATA(WHERE=(AGE>=&BEG & &WGT > 0 ));
        IF &VAR=&NS THEN DELETE;  
        IF CONTROL=. THEN CONTROL=0;
      RUN;
    
      PROC SORT;
        BY AGE %if &NC >= 1 %then %do; &COV %end;;
      RUN;
  
      PROC LOGISTIC DATA=PSBACK000 DESCENDING NOPRINT;;
        %if &NC >= 1 %then %do;
          CLASS &COV;
          MODEL &VAR= AGE %if &Age_sq = 1 %then %do; AGE*AGE %end; &COV &Interaction / L=GLOGIT; 
        %end;
        %if &NC = 0 %then %do;
          MODEL &VAR= AGE %if &Age_sq = 1 %then %do; AGE*AGE %end;                    / L=GLOGIT; 
        %end;
        WEIGHT &WGT;
        OUTPUT OUT=PREV PREDPROBS=I;
      RUN;
      
      DATA PREV2;
        SET PREV(WHERE=(CONTROL=1));
        ARRAY P{&NS};   
        ARRAY IP_{&NS};
        DO I=1 TO &NS-1;
          P{I}=IP_{I};
          IF IP_{I}=. THEN P{I}=0;
        END;
        KEEP %if &NC >= 1 %then %do; &COV %end; AGE P1-P%EVAL(&NS-1);
      RUN;
    
      PROC SORT;
        BY %if &NC >= 1 %then %do; &COV %end; AGE;
      RUN;
  
  /***Code CtrlVar - begin *************************;*/
    %end;
    %else %if (&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;   /*control variables: YES*/
      %do CtrlCOVindexPrev_i=1 %to &nCtrlCOVindexPrev;      
        data CtrlCOVdataPrev_i;
          set CtrlCOVcomboIndexPrev;
          if CtrlCOVindexPrev = &CtrlCOVindexPrev_i;
          CONTROL = &CtrlCOVindexPrev_i;
          %if &NC >= 1 %then %do;
            %do MA=1 %to &NC;
              rename &&COV&MA = Ctrl_&&COV&MA ;
            %end;
          %end;
          rename age = Ctrl_age ;
        run;
        Data input_ctrlPrev;
          set &DATA(WHERE=(AGE>=&BEG & &WGT > 0 ) 
                    KEEP= AGE &VAR &WGT id
                          %if &NC >= 1 %then %do; &COV %end; 
                          %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
                          %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
                   );
          CONTROL = &CtrlCOVindexPrev_i;
          IF &VAR=&NS THEN DELETE;  
        run;
        DATA input_ctrlPrev2;
          merge CtrlCOVdataPrev_i input_ctrlPrev;
          by CONTROL;
          %if &NC >= 1 %then %do;
            %do MA=1 %to &NC;
              &&COV&MA = Ctrl_&&COV&MA;
            %end;
          %end;
          age = Ctrl_age;
          Ctrl_&WGT = &WGT;
          keep AGE &VAR control id Ctrl_&WGT
               %if &NC >= 1 %then %do; &COV %end; 
               %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
               %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
               ;
        RUN;
        DATA PSBACK_CtrlCov;
          set input_ctrlPrev2 &DATA(WHERE=(AGE>=&BEG & &WGT > 0 ));
          IF &VAR=&NS THEN DELETE;  
          IF CONTROL=. THEN CONTROL=0;
        RUN;
        
        PROC SORT;
          BY AGE 
             %if &NC >= 1 %then %do; &COV %end; 
             %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
             %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end;
             ; 
        RUN;

        PROC LOGISTIC DATA=PSBACK_CtrlCov DESCENDING NOPRINT;;
          CLASS %if &NC >= 1 %then %do; &COV %end; 
                %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end; 
                ;
          MODEL &VAR= AGE %if &Age_sq = 1 %then %do; AGE*AGE %end;  &Interaction
                      %if &NC >= 1 %then %do; &COV %end;  
                      %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end; 
                      %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
                      / L=GLOGIT; 
          WEIGHT &WGT;
          OUTPUT OUT=PREV_CtrlCov PREDPROBS=I;
        RUN;

        DATA PREV_CtrlCov1;
          SET PREV_CtrlCov(WHERE=(CONTROL ne 0));
          ARRAY P{&NS};   
          ARRAY IP_{&NS};
          DO I=1 TO &NS-1;
            P{I}=IP_{I};
            IF IP_{I}=. THEN P{I}=0;
          END;
          %do MA=1 %to &NC;
            if &&COV&MA >= 1;
          %end;
          %do MA=1 %to &nCTRLCOVconti;
            if &&CtrlCOVconti&MA ne .;
          %end;
          %do MA=1 %to &nCTRLCOVcateg;
            if &&CtrlCOVcateg&MA ne .;
          %end;
          keep AGE P1-P%EVAL(&NS-1) Ctrl_&WGT
               %if &NC >= 1 %then %do; &COV %end; 
               %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
               %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
               ;
        RUN; 
        proc means data=PREV_CtrlCov1 MEAN noprint ;
          var %DO U=1 %TO &NS-1; P&U %END;;
          weight Ctrl_&WGT;
          output out=PREV_Ctrl_avg_i MEAN= %DO U=1 %TO &NS-1; P&U %END; ;
        run;
        %if &CtrlCOVindexPrev_i = 1 %then %do;
          Data PREV_Ctrl_avg;
            merge CtrlCOVdataPrev_i PREV_Ctrl_avg_i;
          run;
        %end; 
        %else %if &CtrlCOVindexPrev_i > 1 %then %do;
          Data PREV_Ctrl_avg_i;
            merge CtrlCOVdataPrev_i PREV_Ctrl_avg_i;
          run;
          Data PREV_Ctrl_avg;
            set PREV_Ctrl_avg PREV_Ctrl_avg_i;
          run;
        %end; 
      %end;
      Data PREV_Ctrl_avg;
        set PREV_Ctrl_avg ;
        %if &NC >= 1 %then %do;
          %do MA=1 %to &NC;
            rename Ctrl_&&COV&MA = &&COV&MA;
          %end;
        %end;
        rename Ctrl_age = Age;
        drop CtrlCOVindexPrev CONTROL _TYPE_ _FREQ_ ;
      run;

    %end;

  /***Code CtrlVar - end *************************;*/  

  /***Code CtrlVar - begin *************************;*/        
    %if ~(&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;   /*control variables: NO*/
  /***Code CtrlVar - end *************************;*/  
      PROC SORT DATA=S.COVcomboIndex&S;
        BY COVindex;
      RUN;
      PROC SORT DATA=PREV2;
        BY %if &NC >= 1 %then %do; &COV %end; AGE;
      RUN;
      Data PREV4;
        %if &NC >= 1 %then %do;
          merge PREV2 S.COVcomboIndex&S;
          by &COV;
        %end;
        %if &NC = 0 %then %do;
          COVindex = 0;
          set PREV2 ;
        %end;
      run;
  /***Code CtrlVar - begin *************************;*/        
    %end;
    %else %if (&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;   /*control variables: YES*/
      PROC SORT DATA=S.COVcomboIndex&S;
        BY COVindex;
      RUN;
      PROC SORT DATA=PREV_Ctrl_avg;
        BY %if &NC >= 1 %then %do; &COV %end; AGE;
      RUN;
      Data PREV4;
        %if &NC >= 1 %then %do;
          merge PREV_Ctrl_avg S.COVcomboIndex&S;
          by &COV;
        %end;
        %if &NC = 0 %then %do;
          COVindex = 0;
          set PREV_Ctrl_avg ;
        %end;
      run;
    %end;
  %end;
  /***Code CtrlVar - end *************************;*/

  %MACRO MODEL_logit; 
  /***Code CtrlVar - begin *************************;*/        
    %if ~(&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;  /* control variables: NO*/
  /***Code CtrlVar - end *************************;*/        
      DATA SEST;
        %if &NC >= 1 %then %do;
          %DO MA=1 %TO &NC;      
            DO &&COV&MA=1 TO &&LC&MA;
          %END; 
        %end;
          DO BEGST=1 TO &NS-1;
              DO AGE=&BEG TO &END;
                CONTROL=1;
                OUTPUT;
              END;
          END;
        %if &NC >= 1 %then %do;
          %DO MA=1 %TO &NC;      
            END;
          %END;
        %end;
      RUN;
      DATA MODEL;
        SET SEST PRSNYR;  
        IF CONTROL=. THEN CONTROL=0;
        KEEP ID %if &NC >= 1 %then %do; &COV %end; AGE BEGST ENDST &WGT CONTROL ;
      RUN;
  /***Code No Transitons - begin *************************;*/        
      proc sort data=MODEL;
        by Begst Endst; 
      run;
      proc sort data=Transitions;
        by Begst Endst; 
      run;
      DATA MODEL;
        merge MODEL Transitions;
        by Begst Endst; 
        if transitionType ne 2 ;
      RUN;
  /***Code No Transitons - end *************************;*/        
    
      proc sort data=MODEL;
        BY BEGST %if &nSTRATCOV > 0 %then %do; &STRATCOV %end; %if &nREGCOV > 0 %then %do; &REGCOV %end; ENDST;
      RUN;
      Data probs;
        state=.;
      run;
      
      %if &S = 0 %then %do;
        PROC PRINTTO NEW PRINT="&workpath/SIM_Logistic_trans_&depVAR..lst";   run;
      %end;
      %do beg_i=1 %to &NS-1;
        PROC LOGISTIC DATA=MODEL DESCENDING %if &S ne 0 %then NOPRINT;;
          where BEGST=&beg_i ;
          %if &NC >= 1 %then %do;
            %if %scan(&STRATCOV, 1, %str( )) ne %str( ) %then %do;  
              by &STRATCOV;
            %end;
            %if &nREGCOV > 0 %then %do;
              CLASS %do j0 = 1 %to &nREGCOV; &&REGCOV&j0 (ref="1") %end;;
              MODEL ENDST(ref="%eval(&beg_i)")=AGE %if &Age_sq = 1 %then %do; AGE*AGE %end; &REGCOV &Interaction / L=GLOGIT; 
            %end;
            %if &nREGCOV = 0 %then %do;
              MODEL ENDST(ref="%eval(&beg_i)")=AGE %if &Age_sq = 1 %then %do; AGE*AGE %end;                      / L=GLOGIT; 
            %end;
          %end;
          %if &NC = 0 %then %do;   
            MODEL ENDST(ref="%eval(&beg_i)")  =AGE %if &Age_sq = 1 %then %do; AGE*AGE %end;                      / L=GLOGIT; 
          %end;
          WEIGHT &WGT;
          OUTPUT OUT=PROBS&beg_i PREDPROBS=I;   
          TITLE "BEGST=&beg_i.. Estimation for whole sample.";
        RUN;  
        Data PROBS;
          set PROBS PROBS&beg_i;
        run;
        Data PROBS&beg_i;
          set empty;
        run;
      %end;
      PROC PRINTTO NEW PRINT=PRINT; run;
   
      DATA TRANPR;
        SET PROBS(WHERE=(CONTROL=1));
        %DO U=1 %TO &NS;
          P&U=IP_&U;
  /***Code No Transitons - begin *************************;*/        
          if P&U=. then P&U=0;
  /***Code No Transitons - end *************************;*/        
        %END;
        KEEP %if &NC >= 1 %then %do; &COV %end; AGE BEGST P1-P&NS;
      RUN; 
    
      PROC SORT;
        BY %if &NC >= 1 %then %do; &COV %end; AGE BEGST;
      RUN;  
  
      %if &NC >= 1 %then %do;
        proc sort DATA = TRANPR;
          by &COV;
        run;
      %end;
      
  /***Code CtrlVar - begin *************************;*/
    %end;
    %else %if (&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;  /* control variables: YES*/
      %do CtrlCOVindexTrans_i=1 %to &nCtrlCOVindexTrans;      
        data CtrlCOVdataTrans_i;
          set CtrlCOVcomboIndexTrans;
          if CtrlCOVindexTrans = &CtrlCOVindexTrans_i;
          CONTROL = &CtrlCOVindexTrans_i;
          %if &nREGCOV >= 1 %then %do;
            %do MA=1 %to &nREGCOV;
              rename &&REGCOV&MA = Ctrl_&&REGCOV&MA ;   /*Only consider COV in the regression in each stratification by STRACOV.*/
            %end;
          %end;
          rename age = Ctrl_age ;
        run;

%put CtrlCOVindexTrans_i = &CtrlCOVindexTrans_i;

        Data input_ctrlTrans;
          set PRSNYR(WHERE=(AGE>=&BEG & &WGT > 0 ) 
                    KEEP= AGE BEGST ENDST &WGT id
                          %if &NC >= 1 %then %do; &COV %end; 
                          %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
                          %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
                   );
          CONTROL = &CtrlCOVindexTrans_i;
        run;
        DATA input_ctrlTrans2;
          merge CtrlCOVdataTrans_i input_ctrlTrans;
          by CONTROL;
          %if &nREGCOV >= 1 %then %do;
            %do MA=1 %to &nREGCOV;
              &&REGCOV&MA = Ctrl_&&REGCOV&MA;
            %end;
          %end;
          age = Ctrl_age;
          Ctrl_&WGT = &WGT;
          keep id AGE BEGST ENDST control Ctrl_&WGT
               %if &NC >= 1 %then %do; &COV %end; 
               %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
               %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
               ;
        RUN;
        
        DATA Model_CtrlCov;
          set input_ctrlTrans2 PRSNYR;
          IF CONTROL=. THEN CONTROL=0;
          KEEP ID AGE BEGST ENDST CONTROL &WGT Ctrl_&WGT
               %if &NC >= 1 %then %do; &COV %end; 
               %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
               %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end; 
                ;
        RUN;
  /***Code No Transitons - begin *************************;*/        
        proc sort data=Model_CtrlCov;
          by Begst Endst; 
        run;
        proc sort data=Transitions;
          by Begst Endst; 
        run;
        DATA Model_CtrlCov;
          merge Model_CtrlCov Transitions;
          by Begst Endst; 
          if transitionType ne 2 ;
        RUN;
  /***Code No Transitons - end *************************;*/        
        PROC SORT data = Model_CtrlCov;
          BY BEGST 
             %if &nSTRATCOV > 0 %then %do; &STRATCOV %end; 
             %if &nREGCOV > 0 %then %do; &REGCOV %end;
             %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
             %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end;
             ENDST
             ; 
        RUN;
        Data Probs_Ctrl_i;
          state=.;
        run;

        %if &S = 0 and &CtrlCOVindexTrans_i=1 %then %do;
          PROC PRINTTO NEW PRINT="&workpath/SIM_Logistic_trans_Ctrl_&depVAR..lst";   run;
        %end;
        %do beg_i=1 %to &NS-1;
          PROC LOGISTIC DATA=Model_CtrlCov DESCENDING %if ~(&S = 0 and &CtrlCOVindexTrans_i=1) %then %do; NOPRINT %end;;
            where BEGST=&beg_i;
              by %if &nSTRATCOV >= 1 %then %do; &STRATCOV %end;;
              CLASS %if &nREGCOV       >= 1 %then %do; %do j0 = 1 %to &nREGCOV; &&REGCOV&j0 (ref="1") %end;  %end; 
                    %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
                    ;   
                MODEL ENDST(ref="%eval(&beg_i)")=AGE %if &Age_sq = 1 %then %do; AGE*AGE %end; &Interaction
                      %if &nREGCOV       >= 1 %then %do; &REGCOV       %end; 
                      %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
                      %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end;
                      / L=GLOGIT; 
            WEIGHT &WGT;
            OUTPUT OUT=Probs_Ctrl_i&beg_i PREDPROBS=I;   
            TITLE "BEGST=&beg_i.. Estimation for whole sample.(Control Variables)";
          RUN;  
          Data Probs_Ctrl_i;
            set Probs_Ctrl_i Probs_Ctrl_i&beg_i;
          run;
          Data Probs_Ctrl_i&beg_i;
            set empty;
          run;
        %end;
        PROC PRINTTO NEW PRINT=PRINT; run;
        DATA Probs_Ctrl_i;
          SET Probs_Ctrl_i(WHERE=(CONTROL>=1));
          %DO U=1 %TO &NS;
            P&U=IP_&U;
  /***Code No Transitons - begin *************************;*/        
            if P&U=. then P&U=0;
  /***Code No Transitons - end *************************;*/        
          %END;
          %do MA=1 %to &NC;
            if &&COV&MA >= 1;
          %end;
          %do MA=1 %to &nCTRLCOVconti;
            if &&CtrlCOVconti&MA ne .;
          %end;
          %do MA=1 %to &nCTRLCOVcateg;
            if &&CtrlCOVcateg&MA ne .;
          %end;
          KEEP AGE BEGST P1-P&NS ctrl_&wgt
               %if &NC >= 1 %then %do; &COV %end; 
               %if &nCTRLCOVcateg >= 1 %then %do; &CTRLCOVcateg %end;
               %if &nCTRLCOVconti >= 1 %then %do; &CTRLCOVconti %end;
               ;
        RUN; 
        proc means data=Probs_Ctrl_i MEAN noprint ;
          var %DO U=1 %TO &NS; P&U %END;;
          class BEGST 
                %if &nSTRATCOV >= 1 %then %do; &STRATCOV %end;   
                ;
                /*(1) Don't need to consider all CtrlCov since they are "control variables". 
                  (2) each Age and REGCOV conmbination is considered in CtrlCOVindexTrans_i loop. So don't need to consider here.
                  (3) only need to consider each BEGST and STRATCOV stratification.*/
          weight Ctrl_&WGT;
          output out=Probs_Ctrl_avg_i MEAN= %DO U=1 %TO &NS; P&U %END; ;
        run;
        Data Probs_Ctrl_avg_i ;
          set Probs_Ctrl_avg_i (WHERE=(_TYPE_=%eval(2**(&nSTRATCOV + 1)-1)));
          control = &CtrlCOVindexTrans_i;
        run;
        %if &CtrlCOVindexTrans_i = 1 %then %do;
          Data Probs_Ctrl_avg;
            merge CtrlCOVdataTrans_i Probs_Ctrl_avg_i ;
            by control;
          run;
        %end; 
        %else %if &CtrlCOVindexTrans_i > 1 %then %do;
          Data Probs_Ctrl_avg_i;
            merge CtrlCOVdataTrans_i Probs_Ctrl_avg_i;
            by control;
          run;
          Data Probs_Ctrl_avg;
            set Probs_Ctrl_avg Probs_Ctrl_avg_i;
          run;
        %end; 
      %end;
        
      Data Probs_Ctrl_avg;
        set Probs_Ctrl_avg;
        %if &nREGCOV >= 1 %then %do;
          %do MA=1 %to &nREGCOV;
            rename Ctrl_&&REGCOV&MA = &&REGCOV&MA;
          %end;
        %end;
        rename Ctrl_age = Age;
        drop CtrlCOVindexTrans CONTROL _TYPE_ _FREQ_ ;
      run;
      DATA TRANPR_ctrl;
        SET Probs_Ctrl_avg;
        KEEP AGE BEGST P1-P&NS 
             %if &nSTRATCOV > 0 %then %do; &STRATCOV %end; 
             %if &nREGCOV > 0 %then %do; &REGCOV %end;
             ;
      RUN; 
      PROC SORT;
        BY %if &nSTRATCOV > 0 %then %do; &STRATCOV %end; 
           %if &nREGCOV > 0 %then %do; &REGCOV %end;
           AGE BEGST
           ;
      RUN;  
      
    %end;
  /***Code CtrlVar - end *************************;*/
  /***Code CtrlVar - begin *************************;*/        
    %if ~(&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;  /* control variables: NO*/
  /***Code CtrlVar - end *************************;*/
      DATA TRANPRcjt002;
        %if &NC >= 1 %then %do;
          merge S.COVcomboIndex&S TRANPR;
          by &COV;
        %end;
        %if &NC = 0 %then %do;
          set TRANPR;
        %end;
      run;
      proc sort DATA = TRANPRcjt002;
        by %if &NC >= 1 %then %do; &COV %end; AGE BEGST;
      run;
   /***Code CtrlVar - begin *************************;*/        
    %end;
    %else %if (&CtrlCOVmodel=1 and &nCtrlCOV>=1) %then %do;  /* control variables: YES*/
      DATA TRANPRcjt002;
        %if &NC >= 1 %then %do;
          merge S.COVcomboIndex&S TRANPR_ctrl;
          by &COV;
        %end;
        %if &NC = 0 %then %do;
          set TRANPR_ctrl;
        %end;
      run;
      proc sort DATA = TRANPRcjt002;
        by %if &NC >= 1 %then %do; &COV %end; AGE BEGST;
      run;
    %end;
  /***Code CtrlVar - end *************************;*/
    Data Model;
      set empty;
    run;
    Data Model_CtrlCov;
      set empty;
    run;
  %MEND;

  %MACRO MODEL_hazard; 
    *** CALCULATE TRANSITION RATES ***;
    DATA SURVIVE;
      %if &NC >= 1 %then %do;
        %DO MA=1 %TO &NC;      
          DO &&COV&MA=1 TO &&LC&MA;
        %END;    
      %end;
          DO AGE=&BEG TO &END;
            OUTPUT;
          END;
      %if &NC >= 1 %then %do;
        %DO MA=1 %TO &NC;      
          END;
        %END;
      %end;
    RUN;

    PROC SORT;
      BY %if &NC >= 1 %then %do; &COV %end; AGE;
    RUN;

    %DO I=1 %TO &NS-1;
      DATA DTH; 
        SET PRSNYR(WHERE=(BEGST=&I));
        IF BEGST=ENDST THEN EXPOS=1; ELSE EXPOS=1/2;    ** "1" means the whole length of interval, where "1/2" means half of length of interval;
        IF AGE>=%eval(&END) THEN DELETE;
        %DO A=1 %TO &NS;
          IF ENDST=&A THEN EV&A=1; ELSE EV&A=0;
        %END;
      RUN;

      DATA COV;
        %if &NC >= 1 %then %do;
          %DO MA=1 %TO &NC;      
            DO &&COV&MA=1 TO &&LC&MA;
          %END;    
        %end;
          DO ENDST=1 TO &NS;
            DO AGE=&BEG TO &END;
              CNTL=1;
              EXPOS=.;
              OUTPUT;
            END;
          END;
        %if &NC >= 1 %then %do;
          %DO MA=1 %TO &NC;      
            END;
          %END;
        %end;
      RUN;

      %DO J=1 %TO &NS;
        %IF &I NE &J %THEN %DO;
          DATA DTH2;
            SET COV DTH; 
            IF CNTL=. THEN CNTL=0;
          RUN;  

          proc sort DATA= DTH2;
            by  %if &NC >= 1 %then %do; &COV %end; AGE ENDST;
          run;
          %if &S = 0 %then %do;
            PROC PRINTTO NEW PRINT="&workpath/SIM_Lifereg_&depVAR._B&I.E&J..lst";   run;
          %end;
          PROC LIFEREG DATA=DTH2 order=freq %if &S ne 0 %then NOPRINT;;
            %if &NC >= 1 %then %do;
              %if %scan(&STRATCOV, 1, %str( )) ne %str( ) %then %do; 
                by &STRATCOV;
              %end;
              CLASS &REGCOV;
              MODEL EXPOS*EV&J(0) = AGE  %if &Age_sq = 1 %then %do; AGE*AGE %end; &REGCOV  &Interaction / DIST=EXPONENTIAL;
            %end;
            %if &NC = 0 %then %do;
              MODEL EXPOS*EV&J(0) = AGE  %if &Age_sq = 1 %then %do; AGE*AGE %end;                       / DIST=EXPONENTIAL;
            %end;
            WEIGHT &WGT;
            OUTPUT OUT=EVTW&I&J XBETA=LP&I&J CONTROL=CNTL; 
            TITLE "BEGST=&I and ENDST=&J : Estimation for whole sample";
          RUN;
          PROC PRINTTO NEW PRINT=PRINT; run;

          PROC SORT DATA=EVTW&I&J;
            BY %if &NC >= 1 %then %do; &COV %end; AGE;
          RUN;

          DATA SURVIVE; 
            UPDATE SURVIVE EVTW&I&J(KEEP= %if &NC >= 1 %then %do; &COV %end; AGE LP&I&J);
            BY %if &NC >= 1 %then %do; &COV %end; AGE;
            H&I&J=EXP(-LP&I&J);  *** HAZARD ESTIMATE (ALLISON P.82) ***;
            DROP LP&I&J;     
          RUN; 
        %END;
      %END;
    %END;
    PROC PRINTTO NEW PRINT=PRINT; run;
    Data survive000;
      %if &NC >= 1 %then %do;
        merge survive S.COVcomboIndex&S;
        by &COV;
      %end;
      %if &NC = 0 %then %do;
        COVindex = 0;
        set survive ;
      %end;
    run;
    DATA TPTW;  
      SET survive000;
      %DO I=1 %TO &NS-1;
        H&I&I=0;
        %DO J=1 %TO &NS;
          IF H&I&J=. THEN H&I&J=0;
          %IF &I NE &J %THEN %DO;
            H&I&I=H&I&I+H&I&J;
            H&I&J=-H&I&J;
          %END;
        %END;
      %END;
    RUN;
  %MEND;
  
  DATA S.BSLE&S;
    IAGE=.;
  RUN;
  **** To get simulated life lines -  begin ****;
  %if &SimLifeLine = 1 %then %do;
    DATA S.Sim_LifeLine&S;
      COVindex=.;
    RUN;
    DATA S.Sim_SumLifeLine&S;
      COVindex=.;
    RUN;
  %end;
  **** To get simulated life lines -  end ****;

  DATA PLY4;
    COL1=.;
  RUN;  

  %if &logitHazardModel=1 %then %do ;
    %MODEL_logit;     
    %put after MODEL_logit;
  %end;
  %else %if &logitHazardModel=2 %then %do ;
    %MODEL_hazard;
    %put after MODEL_hazard;
  %end;

  %MACRO LE_2(COVindex=, Iage=);
    PROC IML;
        /*** IF USE %MODEL_LOGIT TO GET THE TRANSITION PROBABILITIES ***/
        %if &logitHazardModel=1 %then %do ;
          USE TRANPRcjt002;
          %if &NC >= 1 %then %do;
            READ ALL VAR {%DO U=1 %TO &NS; P&U %END;} INTO SRVP&COVindex WHERE (COVindex=&COVindex);
          %end;
          %if &NC = 0 %then %do;
            READ ALL VAR {%DO U=1 %TO &NS; P&U %END;} INTO SRVP&COVindex ;
          %end;
        %end;
        /*** IF USE %MODEL_HAZARD TO GET THE TRANSITION RATES ***/
        %else %if &logitHazardModel=2 %then  %do;
          USE TPTW(WHERE=(COVindex=&COVindex)); 
          READ ALL VAR {AGE} INTO AGE;
          READ ALL VAR {%DO Y=1 %TO &NS-1; %DO Z=1 %TO &NS; H&Y&Z %END; %END;} INTO MRATES;   
          NA=NROW(MRATES);
          CALL SYMPUT('NA',LEFT(CHAR(NA[1,1]))); 
    
          STATES=&NS;   
          I=I(&NS);     
        
          /*** Reshape the Observed Exposure Rates into M(x,n) Matrix for Each Age-Interval ***;*/
          %DO X=1 %TO &NA;
            M&X=J(&NS,&NS,0);
            DO J=1 TO &NS-1; 
              DO K=1 TO &NS;
                M&X[J,K]=MRATES[&X,(((J-1)*&NS)+K)];
              END; 
            END;
          %END;   
    
          /*** Calculate the Transition Probabilities Matrix PI_x Based on the Linear Method ***
          *** Px = [ I - 1/2 n M(x,n)] [I + 1/2 n M(x,n)]^-1                                ***;*/
          %DO X=1 %TO &NA-1;
            P&X=J(&NS,&NS,.);
            P&X=INV(I(&NS)+M&X/2)*(I(&NS)-M&X/2); 
            DO J=1 TO &NS-1; 
              DO K=1 TO &NS;
                IF M&X[J,K]=0 THEN P&X[J,K]=0;
              END; 
            END;    
    
            DO V=1 TO &NS-1;
              DO W=1 TO &NS;
                IF P&X[V,W]<0 THEN P&X[V,W]=0;
              END;
              P&X[V,]=P&X[V,]/P&X[V,+]; 
            END;   
            SRVP&COVindex=SRVP&COVindex//P&X[1:&NS-1,];
          %END;  
          P&NA=J(&NS,&NS,0);         /*** Everyone Dies at the Last Age Interval ***/
          DO J= 1 TO &NS; 
            P&NA[J,&NS]=1; 
          END;    
          SRVP&COVindex=SRVP&COVindex//P&NA[1:&NS-1,]; 
        %end;

        Create SimSRVP&COVindex from SRVP&COVindex;    
        APPEND FROM SRVP&COVindex;
        CLOSE SimSRVP&COVindex;
    QUIT;    /*quit IML*/
    
    Data SimSRVP&COVindex;
      set SimSRVP&COVindex;
        %DO i=1 %TO &NS; 
          rename COL&i = P&i;
        %END;
    run;


    %DO jj=1 %to &SIMSIZE_part;
      PROC IML;
        USE SimSRVP&COVindex;
        READ ALL VAR {%DO U=1 %TO &NS; P&U %END;} INTO SRVP&COVindex;
        %if &NS>=3 %then %do;
            USE PREV4(WHERE=(COVindex=&COVindex & age=&Iage)); 
          %if &NC >= 1 %then %do;
          %end;
          %if &NC = 0 %then %do;
            USE PREV4(WHERE=(age=&Iage)); 
          %end;
          READ ALL VAR {%DO U=1 %TO &NS-1; P&U %END;} INTO PREV&Iage; 
          %DO U=1 %TO &NS-1;
            Z&U=PREV&Iage[,&U];     
          %END;
        %end;
        %if %eval( %eval(&SIMSIZE) - %eval(&SIMSIZE_part) * %eval(&SIMSIZE_one) ) = 0 %then %do;       /*no remainder, i.e., size of last remaining simulation is = SIMSIZE_one*/
          iiend = %eval(&SIMSIZE_one);
        %end;
        %else %if %eval( %eval(&SIMSIZE) - %eval(&SIMSIZE_part) * %eval(&SIMSIZE_one) ) < 0 %then %do;       /*has remainder, size of last remaining simulation is < SIMSIZE_one*/
          
          %if %eval( %eval(&SIMSIZE) - %eval(&jj) * %eval(&SIMSIZE_one) ) >= 0 %then %do; 
            iiend = %eval(&SIMSIZE_one);
          %end;
          %if %eval( %eval(&SIMSIZE) - %eval(&jj) * %eval(&SIMSIZE_one) ) <  0 %then %do;                  /*size of last remaining simulation is < SIMSIZE_one**/
            iiend = %eval( %eval(&SIMSIZE) - (%eval(&jj) -1) * %eval(&SIMSIZE_one) );
          %end;
        %end;
        call symput('iiend',left(char(iiend)));   /*creating and using macro variables in SAS/IML*/
       
        %DO ii=1 %TO &iiend;
          if      &jj <> &SIMSIZE_part then T = %eval( %eval(&jj -1) * %eval(&SIMSIZE_one) + %eval(&ii) );
          else if &jj =  &SIMSIZE_part then T = %eval( %eval(&jj -1) * %eval(&SIMSIZE_one) + %eval(&ii) );
          call symput('T',left(char(T))); 
          if &ii = 1 then PTALE&Iage=J(&SIMSIZE_one,&MaxYrSIM,0); 
          PTALE&Iage[&ii,1]=&Iage*100000000000+&COVindex*100000000+&T;
          PTALE&Iage[&ii,2]=&COVindex;
          PTALE&Iage[&ii,3]=&Iage;
         %if &NS=2 %then %do;  
            IHS=1;
          %end;
          %if &NS>=3 %then %do;
            IHS=RANTBL(ROUND(%if &randomSeed = 1 %then %do; DATETIME() %end; %else %do; &BS_i %end;)%DO U=1 %TO &NS-1;,Z&U %END;); 
            /*RANTBL(seed, p_1,...,p_n,x)=> to generate
            1: with prob of p_1
            2: with prob of p_2
            ...
            n: with prob of p_n
            n+1: with prob of 1-sum(p_i)
            */
          %end;

          PTALE&Iage[&ii,4]=IHS;
          Age_pos=&Iage;
          DO WHILE (IHS^=&NS);   
            %DO U=1 %TO &NS;
              P&U=SRVP&COVindex[(Age_pos-&BEG)*(&NS-1)+IHS,&U];            
            %END;
            HS=RANTBL(ROUND(%if &randomSeed = 1 %then %do; DATETIME() %end; %else %do; &BS_i %end;)%DO U=1 %TO &NS;,P&U %END;);
            free %DO U=1 %TO &NS; P&U %END;;
            IF Age_pos-%eval(&Iage)+5 <= %eval(&MaxYrSIM) THEN PTALE&Iage[&ii,Age_pos-&Iage+5]=HS;  

            Age_pos=Age_pos+1;
            IF Age_pos <= &END THEN IHS=HS;   
            ELSE IHS=&NS;
          END;
          %if (%sysevalf( %eval(&T) - %sysevalf(&T / &SIMSIZE_one, int) * %eval(&SIMSIZE_one), int) = 0) or (&T = &SIMSIZE) %then %do;
            Create PHS&Iage._&COVindex.P%sysevalf((&T-1) / &SIMSIZE_one, ceil) from PTALE&Iage;    
            APPEND FROM PTALE&Iage;
            CLOSE PHS&Iage._&COVindex.P%sysevalf((&T-1) / &SIMSIZE_one, ceil);  
            FREE   PTALE&Iage ;
            %if (&T < &SIMSIZE) %then %do;
              PTALE&Iage=J(&SIMSIZE_one,&MaxYrSIM,0);
            %end;
          %end;
        %END;              
      QUIT;    /*quit IML*/
    %END;              
    
    %if %eval(&SIMSIZE_part) = 1 %then %do;
        Data PHS&Iage._&COVindex.P1;
          set PHS&Iage._&COVindex.P1;
            if COL1 > 0;
            rename COL1=ID;
            rename COL2=covIndex;
        run;
        Data PHS&Iage._&COVindex;
          set PHS&Iage._&COVindex.P1;
          if ID > 0;
        run;
        Data PHS&Iage._&COVindex.P1;
          set empty;
        run;
    %end;
    %else %if %eval(&SIMSIZE_part) > 1 %then %do;
      %do ii = 1 %to &SIMSIZE_part;
        Data PHS&Iage._&COVindex.P&ii;
          set PHS&Iage._&COVindex.P&ii;
            rename COL1=ID;
            rename COL2=covIndex;
        run;
        %if &ii = 1 %then %do;
          Data PHS&Iage._&COVindex;
            set PHS&Iage._&COVindex.P&ii;
            if ID > 0;
          run;
        %end;
        %else %if &ii > 1 %then %do;
          Data PHS&Iage._&COVindex;
            set PHS&Iage._&COVindex PHS&Iage._&COVindex.P&ii;
            if ID > 0;
          run;
        %end;
        Data PHS&Iage._&COVindex.P&ii;
          set empty;
        run;
      %end;
    %end;
    Data PHS&Iage._&COVindex;
      %if &NC >= 1 %then %do;
        merge PHS&Iage._&COVindex S.COVcomboIndex&S(where =(COVindex=&COVindex));
        by COVindex;
      %end;
      %if &NC = 0 %then %do;
        set PHS&Iage._&COVindex ;
      %end;
    run;
    Data PHSB&Iage._&COVindex(KEEP=ID covIndex %if &NC >= 1 %then %do; &COV %end; IAGE AGE &VAR); 
      set PHS&Iage._&COVindex;
        ARRAY COL{%eval(&MaxYrSIM)}; 
        IAGE=COL3;                  /*initial age*/
        DO i=4 TO &MaxYrSIM; 
          AGE=COL3+(i-4); 
          &VAR=COL{i};   
          IF &VAR NE 0 THEN OUTPUT; 
        END;
    run;
/* about to delete PHS&Iage._&COVindex here */
  **** To get simulated life lines -  begin ****;
    %if &SimLifeLine = 1 %then %do;
      %if &NC >= 1 %then %do; 
        %if &Iage = &BEG and &COVindex = 1 %then %do;
          DATA Sim_LifeLine;  
            SET  PHS&Iage._&COVindex;
          RUN;
        %end;
        %else %do;
          DATA Sim_LifeLine;  
            SET  Sim_LifeLine PHS&Iage._&COVindex;
          RUN;
        %end; 
      %end; 
      %if &NC = 0 %then %do;
        %if &Iage = &BEG %then %do;
          DATA Sim_LifeLine;  
            SET PHS&Iage._0;
          RUN;
        %end;
        %else %do;
          DATA Sim_LifeLine;  
            SET  Sim_LifeLine PHS&Iage._&COVindex;
          RUN;
        %end; 
      %end; 
    %end; 
  **** To get simulated life lines -  end ****;
    data PHS&Iage._&COVindex;
      set empty;
    run;

    PROC SORT DATA=PHSB&Iage._&COVindex OUT=COMAREC;
      BY %if &NC >= 1 %then %do; &COV %end; iage;
    RUN;
    PROC SORT DATA=COMAREC;
      BY IAGE ID AGE;
    RUN;

    DATA MSP&VAR.2;      
      SET COMAREC;
      BY IAGE ID;
      RENAME &VAR=ENDST; 
      BEGST=LAG(&VAR);
      BAGE=LAG(AGE);
      IF FIRST.ID THEN DO;
        BEGST=0;
        BAGE=0;
        DELETE;
      END;
    RUN;       
    data PHSB&Iage._&COVindex;
      set empty;
    run;
    data COMAREC;
      set empty;
    run;


    DATA PHS(KEEP=ID IAGE BH DAGE); 
      SET MSP&VAR.2;
      BY IAGE ID;

      RETAIN BH;
      IF FIRST.ID THEN BH=BEGST;    

      IF LAST.ID THEN DO;        
        DAGE=AGE;
        OUTPUT;
      END;
    RUN;   
     
    /*compute LE for each simulated person*/
    DATA MSP&Iage._&COVindex&VAR.3;
      MERGE MSP&VAR.2(RENAME=(AGE=EAGE)) PHS;
      BY IAGE ID;
      PYdeduct=0.5;         
      
      %if &NS=2 %then %do;
        IF FIRST.ID THEN DO;
          YRLVED=0;        /*year of lived*/
        END;
        YRLVED+1;   
        IF LAST.ID THEN DO;    
          YRLVED=YRLVED-PYdeduct;
          OUTPUT;
        END; 
      %end;
      
      %else %if &NS >= 3 %then %do;
        if first.id then do;
          YRLVED=0;        /*year of lived*/
          %DO U=1 %TO &NS; 
            PYs&U = 0;
          %END;
        end;
        %DO U=1 %TO &NS-1; 
          %DO V=1 %TO &NS; 
            if begst = &U & endst = &V then do;
              if begst = endst then do;
                PYs&U = 1;
              end;
              else if begst ne endst then do;   **transition happens;
                if endst ne &NS then do;                **not dead;
                  PYs&U = 1 - PYdeduct;
                  PYs&V = PYdeduct;
                end;
                else if endst = &NS then do;            **dead;
                  %if &logitHazardModel=1 %then %do ;
                    PYs&U = 1 - PYdeduct;
                  %end;
                  %else %if &logitHazardModel=2 %then  %do ;   
                    PYs&U = 1 - PYdeduct;
                  %end;
                end;
              end;
            end;
          %END;
        %END;
        IF FIRST.ID THEN DO;
          %DO U=1 %TO &NS; 
            sumPYs&U = 0;
          %END;
        End;
        %DO U=1 %TO &NS; 
          sumPYs&U + PYs&U;
        %END;
        YRLVED+1;   
        IF LAST.ID THEN DO;
        if ENDST = &NS or ENDST = 0 then YRLVED = YRLVED - PYdeduct; /*Last Id*/
          OUTPUT;
        END; 
      %end;
      drop %if &NS >= 3 %then %do; %DO U=1 %TO &NS; PYs&U %END; sumPYs&NS %end;  PYdeduct;

    RUN;

    PROC MEANS DATA=MSP&Iage._&COVindex&VAR.3 MEAN NOPRINT;
      %if &NS=2 %then %do;
        CLASS IAGE %if &NC >= 1 %then %do; &COV %end; BH;
        VAR YRLVED;                         
        OUTPUT OUT=MEDLE&Iage MEAN=TLE ;
      %end;
      %else %if &NS >= 3 %then %do;
        CLASS IAGE %if &NC >= 1 %then %do; &COV %end; BH;
        VAR YRLVED %DO U=1 %TO &NS-1; sumPYs&U %END;;
        OUTPUT OUT=MEDLE&Iage MEAN=TLE %DO U=1 %TO &NS-1; LEs&U %END;
                                        ;
      %end;
    RUN;          


    DATA MEDLE&Iage._&COVindex;
      SET MEDLE&Iage(WHERE=(_TYPE_ in (%eval(%eval(2**(&NC+2)-1)-1), %eval(2**(&NC+2)-1))));
      IF BH=. THEN BH=0;
      
      %DO U=1 %TO &NS-1; LEp&U = LEs&U / TLE;  %END;
      
    RUN;

  %mend;

  %DO Iage=&BEG %TO %eval(&BEG+&d_BEG) %BY &d_AGE;
    %if &NC >= 1 %then %do;
      %DO COVindex=1 %TO &nCOVindex;    
        %LE_2(COVindex=&COVindex, Iage=&Iage);
      %END;
    %end;
    %if &NC = 0 %then %do;
      %LE_2(COVindex=0, Iage=&Iage);
    %end;
  %END;
      
  DATA MEDLE;  
    SET
      %DO Iage=&BEG %TO %eval(&BEG+&d_BEG) %BY &d_AGE;
        %if &NC >= 1 %then %do;
          %DO COVindex=1 %TO &nCOVindex;    
            MEDLE&Iage._&COVindex
          %END;
        %end;
        %if &NC = 0 %then %do;
          MEDLE&Iage._0
        %end;
      %END;;          
  RUN;

  proc sort data=medle;
    by %if &NC >= 1 %then %do; &COV %end; iage BH;
  run;

  DATA S.BSLE&S;
    SET S.BSLE&S medle;
    IF IAGE NE .;
  RUN;
  PROC SORT DATA=S.BSLE&S;
    BY IAGE %if &NC >= 1 %then %do; &COV %end;;
  RUN;
    
  **** To get simulated life lines -  begin ****;
  %if &SimLifeLine = 1 %then %do;
    DATA Sim_SumLifeLine;  
      SET
        %DO Iage=&BEG %TO %eval(&BEG+&d_BEG) %BY &d_AGE;
          %if &NC >= 1 %then %do;
            %DO COVindex=1 %TO &nCOVindex;    
              MSP&Iage._&COVindex&VAR.3
            %END;
          %end;
          %if &NC = 0 %then %do;
            MSP&Iage.0&VAR.3
          %end;
        %END;;          
    RUN;
  
    DATA S.Sim_LifeLine&S;
        retain COVindex ID 
               %if &NC >= 1 %then %do; &COV %end;
               IAGE
              ;
        SET S.Sim_LifeLine&S Sim_LifeLine;
        IF COVindex NE .;
    RUN;
    Data Sim_LifeLine;
      set empty;
    run;
    DATA S.Sim_SumLifeLine&S;
        retain COVindex ID 
               %if &NC >= 1 %then %do; &COV %end;
               IAGE
              ;
        SET S.Sim_SumLifeLine&S Sim_SumLifeLine;
        IF COVindex NE .;
    RUN;
  %end;
  **** To get simulated life lines -  end ****;

  Data PRSNYR; set empty;  run;
  Data Prev; set empty;  run;
  Data PREV_CtrlCov; set empty;  run;
  Data Psback000; set empty;  run;

%MEND;



