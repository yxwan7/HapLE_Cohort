%MACRO MSLT_RADxCOV_S(DATA=,S=,VAR=,NS=,COV=,NC=,WGT=,LOI=,BEG=,END=,STRATCOV=, REGCOV=, TVcov=, randomSeed=, BS_i=, Interaction=, age_sq=);
  %let nREGCOV=0;
  %let nSTRATCOV=0;
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
    
    %do MA=1 %to &NC;
      %put COV&MA=&&COV&MA ;
    %end;
    %let nREGCOV=0;
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
    %put nSTRATCOV=&nSTRATCOV;
    %DO j0=1 %TO &nSTRATCOV;
      %LET STRATCOV&j0=%SCAN(&STRATCOV,&j0,' ');
      %put STRATCOV&j0=&&STRATCOV&j0;
    %end;
  %end;
%put NC=&NC;
%put nREGCOV=&nREGCOV;
%put nSTRATCOV=&nSTRATCOV;
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

  PROC SORT DATA=&DATA;
    BY ID AGE;
  RUN;
  PROC SORT DATA=S.COVcomboIndex&S;
    BY COVindex;
  RUN;
    
  DATA SEM0;
    SET &DATA(WHERE=(AGE>=&BEG & &WGT > 0 ));
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
            RENAME=(NAGE=AGE &VAR._HS=&VAR NWGT=&WGT
                    %if &nTVcov >= 1 %then %do; %DO j0=1 %TO &nTVcov; &&TVcov&j0.._New = &&TVcov&j0 %end; %end; 
                    )
            ) ;
    SET SEM;
    BY ID;
    IF FIRST.ID THEN DO;
      &VAR._HS=&VAR;
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
              &VAR._HS=PREVST; 
              NWGT=PREVWGT+ROUND((&WGT-PREVWGT)/(AGE-PREVAGE));
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0.._PREV ; %end; %end;
            END;
            ELSE DO;
              &VAR._HS=&VAR;
              NWGT=&WGT;
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
            END;
            NAGE=J;
            PREVWGT=NWGT;
            OUTPUT;
          END;
        end;
        ELSE IF PREVAGE+1=AGE THEN DO;
          &VAR._HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        END;
        else if PREVAGE <= AGE < PREVAGE+1 then do; 
          &VAR._HS=&VAR;
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
              &VAR._HS=PREVST; 
              NWGT=PREVWGT+ROUND((&WGT-PREVWGT)/(AGE-PREVAGE));
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0.._PREV ; %end; %end;
            END;
            ELSE DO;
              &VAR._HS=&VAR;
              NWGT=&WGT;
              %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
            END;
            NAGE=J;
            PREVWGT=NWGT;
            OUTPUT;
          END;
        END;
        ELSE IF PREVAGE+1=AGE THEN DO;
          &VAR._HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        END;
        else if PREVAGE <= AGE < PREVAGE+1 then do; 
          &VAR._HS=&VAR;
          NAGE=AGE;
          NWGT=&WGT;
          %if &nTVcov >= 1 %then %do; %do j0=1 %TO &nTVcov; &&TVcov&j0.._New =&&TVcov&j0        ; %end; %end;
          OUTPUT;
        end;
      END;
    END;
  RUN;        
%put after SEM1;

  DATA PRSNYR(RENAME=(&VAR=ENDST));   
    SET SEM1;
    BY ID;

    BEGST=LAG(&VAR);
    &WGT=LAG(&WGT);
    IF FIRST.ID THEN DO; 
      BEGST=0;
      DELETE;
    END;
  RUN;
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

  DATA PSBACK;
    SET PS2 &DATA(WHERE=(AGE>=&BEG & &WGT > 0 ));
    IF &VAR=&NS THEN DELETE;  
    IF &VAR=&NS THEN DELETE;
    IF CONTROL=. THEN CONTROL=0;
  RUN;

  PROC SORT;
    BY AGE %if &NC >= 1 %then %do; &COV %end;;
  RUN;

  PROC LOGISTIC DATA=PSBACK DESCENDING NOPRINT;
    %if &NC >= 1 %then %do;
      CLASS &COV;
      MODEL &VAR= AGE &COV %if &Age_sq = 1 %then %do; AGE*AGE %end; &Interaction / L=GLOGIT;	
    %end;
    %if &NC = 0 %then %do;
      MODEL &VAR= AGE      %if &Age_sq = 1 %then %do; AGE*AGE %end; / L=GLOGIT;	
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

			DATA COVa;
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

      data COV;
        set COVa;
      run;

      %DO J=1 %TO &NS;
        %IF &I NE &J %THEN %DO;
          DATA DTH2;
            SET COV DTH ; 
            IF CNTL=. THEN CNTL=0;
          RUN;  

				  proc sort DATA= DTH2;
				    by  %if &NC >= 1 %then %do; &COV %end; AGE ENDST;
				  run;
          %if &S = 0 %then %do;
            PROC PRINTTO NEW PRINT="&workpath/RAD_Lifereg_&depVAR._B&I.E&J..lst";   run;
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

  %MODEL_hazard;

  DATA BSLE;
/*    %if &NC >= 1 %then %do;
      %DO MA=1 %TO &NC; 
        &&COV&MA=.;
      %END;
    %end;*/
    AGE=.;
    STATE=.;
    TLE=.;
    ALE=.;
    DLE=.;
  RUN;

  %MACRO LE_2(COVindex=);
    PROC IML; 
      USE PREV4(WHERE=(COVindex=&COVindex));
      READ ALL VAR {%DO U=1 %TO &NS-1; P&U %END;} INTO PREV; 

      *** IF USE %MODEL_HAZARD TO GET THE TRANSITION RATES ***;
      USE TPTW(WHERE=(COVindex=&COVindex)); 
      READ ALL VAR {AGE} INTO AGE;
      READ ALL VAR {%DO Y=1 %TO &NS-1; %DO Z=1 %TO &NS; H&Y&Z %END; %END;} INTO MRATES;   
      NA=NROW(MRATES);
      CALL SYMPUT('NA',LEFT(CHAR(NA[1,1])));         *** AGEGROUP=# of Age-Intervals ***;

      STATES=&NS;   *** # of Living States Plus Absorbing State ***;
      I=I(&NS);     *** Identity Matrix ***;
  
      *** Reshape the Observed Exposure Rates into M(x,n) Matrix for Each Age-Interval ***;
      %DO X=1 %TO &NA;
        M&X=J(&NS,&NS,0);
        DO J=1 TO &NS-1; 
          DO K=1 TO &NS;
            M&X[J,K]=MRATES[&X,(((J-1)*&NS)+K)];
          END; 
        END;
      %END;   
      
      * THE FOLLOWING CALCULATES POPULTATION- AND STATUS-BASED LE AT &BEG-&END USING *
      * THE RADIX POPULATION. THE ALGORITHM IS BASED ON MARK HAYWARD PROGRAM.    *
      *                                                               - 07/26/10 *;

      START SBLT;
        Lx=J(&NA,STATES,0);
        * Set the Radix Value *;
        Lx[Radix_X,Radix_J]=100000;
        nLx=J(&NA,STATES,0);
        %DO X=1 %TO &NA-1;
          IF &X>=RADIX_X THEN DO;
            Lx[&X+1,]=Lx[&X,]*(I-M&X/2)*INV(I+M&X/2);
            nLx[&X,]=(Lx[&X,]+Lx[&X+1,])/2;
          END;
        %END;
        L_H=Lx[&NA,1] %DO J=2 %TO &NS-1; || Lx[&NA,&J] %END;;  
        *nLx_H = L_H*INVM_H;
        nLx_H=L_H*INV(M&NA[1:&NS-1,1:&NS-1]);
        DO K=1 TO STATES-1; 
          nLx[&NA,K]=nLx_H[K]; 
        END;
        QnLx=nLx[,1] %DO J=2 %TO &NS-1; || nLx[,&J] %END;;
        QTx=J(&NA,STATES-1,0);
        QTx[&NA,]=QnLx[&NA,];
        DO X=2 TO &NA-Radix_X+1; 
          DO J=1 TO STATES-1;
            QTx[&NA+1-X,J]= QTx[&NA+1-X+1,J]+QnLx[&NA+1-X,J];
          END; 
        END;
        QnLx=QnLx[,+]||QnLx;
        QTx=QTx[,+]||QTx;
        QEx=QTx[Radix_X,]/100000;
      FINISH SBLT;

      %DO Radix_X=1 %TO &NA; 
        QR=J(1,STATES,0);
        %DO Radix_J=1 %TO &NS-1;
          Radix_X=&Radix_x; 
          Radix_J=&Radix_J;   

          RUN SBLT;

          /* Retain the Information for &Radix_X. Age Interval *;*/
          QE = QE//(AGE[Radix_X,]||Radix_J ||QEx); *** QE HOLDS STATUS-BASED LE FOR ALL AGES ***;
          QW=J(1,&NS,0);
          QW=QEx#PREV[Radix_X,Radix_J];             *** QW WEIGHTS STATUS-BASED LE BY PREVALENCE ***;
          QR=QR+QW;                        *** QR SUMS OVER STATUS AT AGE X TO GET POP-BASED LE ***;
            
          FREE Lx QnLx QTx QEx;
        %END; 
        QQ=QQ//QR;                         *** QQ HOLDS POP-BSED LE FOR ALL AGES ***;
      %END;  
      QS=AGE||J(&NA,1,0)||QQ;        *** QS ADDS AGE & STATUS CODE (0) FOR THE QQ MATRIX ***;
        
      QA=QS//QE;                           *** QA HOLDS POP- & STATUS-BASED LE FOR ALL AGES ***;
      CREATE PSLE FROM QA; 
      APPEND FROM QA; 
      CLOSE PSLE;      
    QUIT;

    DATA PSLE2;
      COVindex=&COVindex;    
      SET PSLE(RENAME=(COL1=AGE COL2=STATE COL3=TLE COL4=ALE COL5=DLE));
    RUN;

    DATA BSLE;
      SET BSLE PSLE2;
      IF AGE=. THEN DELETE;
    RUN;

  %MEND;

  %if &NC >= 1 %then %do;
    %DO COVindex=1 %TO &nCOVindex;    
      %LE_2(COVindex=&COVindex);
    %END;
  %end;
  %if &NC = 0 %then %do;
    %LE_2(COVindex=0);
  %end;

  PROC SORT DATA=BSLE;
    by COVindex;
  run;
  Data BSLE;
    merge S.COVcomboIndex&S BSLE;
    by COVindex; 
  run; 
  Data BSLE;
    retain Age &COV;
    set BSLE;
  Run;

%MEND;


