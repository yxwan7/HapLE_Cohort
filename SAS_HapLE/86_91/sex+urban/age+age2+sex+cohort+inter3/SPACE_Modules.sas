%macro SPACE_module;

%put --------------SPACE Starts---------------;
OPTIONS AUTOSIGNON=NO CONNECTWAIT=NO SASCMD="!SASCMD";
options fullstimer ;
options nocenter linesize=180 pagesize=67;
options nofmterr; 
ods noresults;
dm 'log;clear;output;clear;';
PROC PRINTTO NEW LOG="&workpath/SPACE.LOG";   run;
/*
  proc datasets lib=S;
    modify &Datainput;
    format _all_;
    informat _all_;
  run;
  quit;
  
  proc sort DATA=S.&Datainput;    
    by ID;
  run;
*/  

/*---------- Don't need to modify: begin ----------*/
  /*** (1) All programs */
  %LET LOI=1;                                                         /*** Length of interval in years; Default = 1 (year);                                      */
                                                                      /*** For example, 0.5 or 1/2 means half year, 2 means 2 year.                              */
                                                                      /*** This will affect the Age variable being running in the background by SPACE.           */
                                                                      /*** If LOI=1, Age is age in year. If LOI=1/12, Age is age in month.                       */
  %LET BEG_age=%sysevalf(&BEG / %sysevalf(&LOI), int);                /*** First age in life table;                                                              */
  %LET BEG=%sysevalf(&BEG_age / %sysevalf(&LOI), int);                /*** First age in life table;                                                              */
  %LET COV =  &STRATCOV &REGCOV ;                                     /*** List of all covariates;                                                               */

  /***(2) RAD program (module 1) only */
  %LET END_LT_age=%sysevalf(&END_LT / %sysevalf(&LOI), int);          /*** Last age in life table;                                                               */
  %LET END_LT=%sysevalf(&END_LT_age / %sysevalf(&LOI), int);          /*** Last age in life table;                                                               */

  /***(3) SIM (module 2) program only */
  %LET d_BEG_age=%sysevalf(&d_BEG / %sysevalf(&LOI), int);            /*** Range of age in the output. If only need AGE=BEG in the output, put 0 here;           */
  %LET d_BEG=%sysevalf(&d_BEG_age / %sysevalf(&LOI), int);            /*** Range of age in the output. If only need AGE=BEG in the output, put 0 here;           */
  %LET d_AGE_age=%sysevalf(&d_AGE / %sysevalf(&LOI), int);            /*** length of age interval for the output. Can NOT be 0. At least 1;                      */
  %LET d_AGE=%sysevalf(&d_AGE_age / %sysevalf(&LOI), int);            /*** length of age interval for the output. Can NOT be 0. At least 1;                      */
  %LET END_SIM=%sysevalf(&BEG+&SIMYEAR+5 / %sysevalf(&LOI), int);                 /*** Simulated lifes must die at this age in simulation                                    */
  %LET MaxYr_SIM=%sysevalf( 4+&SIMYEAR / %sysevalf(&LOI), int); /*** the maximum length of simulated lifes in simulation                                   */
  %LET d_BSIZE=0;                                                     /*** to change the index value of bootstrap sample, the final value will be BSIZE+d_BSIZE  */
  %LET SIMSIZE_one=13000;                                            /*** the size of each subset of simulation; in order to avoid computer RAM upper limit     */
  %LET SIMSIZE_part=%sysevalf( &SIMSIZE / &SIMSIZE_one,ceil);         /*** the number of simulation subsets                                                      */
  %LET nAbsorbState= 1 ;          /*** # of absorbing health states. At least 1 (default). Has to be 1 so far.   */
                                  /*** Always the last 1 or few states out of all states (nHealthState).         */ 
                                  /*** Has to be less than nHealthState.                                         */ 
 
  %if &SIMSIZE_one <= 10000 %then %do;
    %let SIMSIZE_one=10000; 
    %LET SIMSIZE_part=%sysevalf( &SIMSIZE / &SIMSIZE_one,ceil);
  %end; 

  %if &BSIZE <= 1 %then %let BSIZE=0;                                                                             %put BSIZE              =&BSIZE;
  %if &BEG   <= 0 %then %let BEG=%sysevalf(0 / %sysevalf(&LOI), int);                                             %put BEG                =&BEG;
  %if &d_BEG <= 0 %then %let d_BEG=%sysevalf(0 / %sysevalf(&LOI), int);                                           %put d_BEG              =&d_BEG;
  %if &d_AGE <= 0 %then %let d_AGE=%sysevalf(1 / %sysevalf(&LOI), int);                                           %put d_AGE              =&d_AGE;
  %if &BSIZE <= 1 %then %let BSIZE=%sysevalf(0 / %sysevalf(&LOI), int);                                           %put BSIZE              =&BSIZE;
  %if ~(&TXT_output = 1 | &TXT_output = 0) %then %let TXT_output = 1;                                             %put TXT_output         =&TXT_output;
  %if (&END_LT < &BEG | &END_LT < 0) %then %let END_LT=%sysevalf((&BEG + 10) / %sysevalf(&LOI), int);             %put END_LT             =&END_LT;
  %if (&AgeLowerLimit > &BEG | &AgeLowerLimit < 0) %then %do;
    %let AgeLowerLimit=%sysevalf(&BEG / %sysevalf(&LOI), int);             
  %end;            
                                                                                                                  %put AgeLowerLimit      =&AgeLowerLimit;
  %if &SIMSIZE <= 100 %then %let SIMSIZE=100;                                                                     %put SIMSIZE            =&SIMSIZE;
  %if ~(&logitHazard = 1 | &logitHazard = 2) %then %let logitHazard=1;                                            %put logitHazard        =&logitHazard;
  %if &nSession < 1 %then %let nSession=1;                                                                        %put nSession           =&nSession;
  %if &SPACEmodule = 1 %then %do;                                                                                                         
    %if &nHealthState ne 3 %then %let nHealthState=3;                                                             %put nHealthState       =&nHealthState;
  %end;                                                                                                                                   
  %else %if &SPACEmodule = 2 %then %do;                                                                                                   
    %if &nHealthState <= 2 %then %let nHealthState=2;                                                             %put nHealthState       =&nHealthState;
  %end;                                                                                                                                   
  %else %if &SPACEmodule = 4 %then %do;                                                                                                   
    %if &nHealthState <= 2 %then %let nHealthState=2;                                                             %put nHealthState       =&nHealthState;
  %end;                                                                                                                                   
  %if ~(&randomSeed = 1 | &randomSeed = 0) %then %let randomSeed=0;                                               %put randomSeed         =&randomSeed;

  %if &SPACEmodule = 1 %then %do;
    %include "&workpath/MSLT_RADxCOV_M.sas";
    %include "&workpath/MSLT_RADxCOV_S.sas";
    %MSLT_RADxCOV_M;
    ***Deterministic approach;
  %end;
  %else %if &SPACEmodule = 2 %then %do;
    %include "&workpath/MSLT_SIMxCOV_M.sas";
    %include "&workpath/MSLT_SIMxCOV_S.sas";
    %MSLT_SIMxCOV_M_DX;
    ***Microsimulation approach (first-order Markov);
  %end;
/*---------- Don't need to modify: end ----------*/


%mend;
