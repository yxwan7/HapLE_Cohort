/*---------- Need to modify below ----------*/
/***(1) All programs */
  %LET BSIZE=100;                 /*** # of bootstrap samples. At least 0. It has to be a positive integer >= 2.*/
  %LET BEG=92;                  /*** First age of the life table.*/
  %LET AgeLowerLimit=89;                  /*** The lowest age from the input data. Range from 0 to BEG*/
  %LET Datainput= haple ;  /*** The name of SAS input data file. */
  %LET depVAR= H ;            /*** Dependent variable (Health state variable). Can NOT use "HS".*/
  %LET STRATCOV=  ;           /*** Covariate(s) used to stratify the analysis.*/
  %LET REGCOV= cohort edu;               /*** Covariate(s) in regression models. */
  %LET TVcov=    ;         /*** Time-varying covariate(s) <categorical>, should be part of REGCOV (or REGCOVmx in M4)    */
  %LET STRATA= strata ;         /*** STRATA variable */
  %LET PSU= psu ;               /*** PSU variable    */
  %LET WGT= weight ;               /*** Sample weight */
  %LET TXT_output= 1;           /*** 1 (default): generate the tab separated TXT output files that EXCEL can open. 0: NO.                       */
                                /*** if put TXT_output=1, you may have to use -noterminal option under Unix-like platform.                      */
  %LET nHealthState= 3;          /*** # of health states (including the absorbing state). Only SIM program (2+) can you use values other than 3. */
  %let subset =   ;             /*** the dichotomized variable (0/1) for subset of the input data set. Default is blank.                        */
  %LET randomSeed = 0 ;         /*** Random number seed: 1 = DateTime(); 0 = BS ***/
  %LET InteractionTerm = age*sex age*cohort cohort*sex cohort*edu edu*sex age*edu  ;   /*** User defined interaction terms. Default is blank.              ***/
                                                            /*** For example:                                                   ***/
                                                            /***              Age*Sex                                           ***/
                                                            /***              Age*Sex Sex*Education                             ***/
  %LET AGE_sq = 1;              /*** 1 = having an age squared term; 0, otherwise. Default = 0*/

/***(2) RAD program (module 1) only */
  %LET END_LT= 85 ;            /*** Last age (open interval) the life table.*/

/***(3) SIM program (module 2) only */
  %LET SIMSIZE= 20000 ;         /*** Size of simulation cohort.*/
  %LET SIMYEAR= 6 ;         /*** Year length of simulation.*/
  %LET d_BEG=  0 ;           /*** Range of age in the output. If only needing AGE=BEG in the output, put 0 here.*/
  %LET d_AGE=  1 ;           /*** The increment of first age across life tables. Can NOT be 0. At least 1.      */
  %LET logitHazard= 1 ;      /*** 1=Logit model (Default), 2=Hazard model.                                      */
  %LET nSession= 10 ;         /*** # of sessions for parallel computation under a multi-CPU computer.            */
                             /*** Without SAS/CONNECT, it has to be 1.                                          */
                             /*** Please make sure your computer has SAS/CONNECT.                               */

  %LET NoTrans=  ;
                              /*** Specify transitions that are not allowed. Default is blank.                */
                              /*** Eg., B3E1 B3E2.                                                             */
                              /*** B: beginning state; E: ending state                                         */
                              /*** B3E1: transition from state 3 to state 1                                    */
                              /*** Transitions separated by blank spaces                                       */
                              /*** Has to be numeric and less than nHealthState - nAbsorbState                 */
  %LET SimLifeLine=  0 ;            /*** Whether to save simulation outputs: 0=No (Default). 1=Yes.                       */
  %LET CtrlCOVmodel= 1   ;            /*** Whether to use control variable(s) in regression models: 0=No (Default). 1=Yes.  */
  %LET CtrlCOVcateg= sex ;            /*** Control variable(s) <categorical> in the regression model.                       */
  %LET CtrlCOVconti=   ;            /*** Control variable(s) <continuous> in the regression model.                        */

/*------------------------------------------------------*/

/* NOTE for SIM program (module 2):
  Transition "rate" (logitHazard=2) can be estimated up to nHealthState = 9, after 9, it will have problems.
  However, Logit model (logitHazard=1) does not have this issue.
*/
