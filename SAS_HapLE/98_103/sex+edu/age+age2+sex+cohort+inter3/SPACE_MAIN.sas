%let workpath = C:/Users/29557/Desktop/Research/Projects/HapLE_Cohort/SAS_HapLE/98_103/sex+edu/age+age2+sex+cohort+inter3; *** the path you put SPACE and data set. Need to modify;
LIBNAME S "&workpath";
%include "&workpath/SPACE_macro.sas";
%include "&workpath/SPACE_Modules.sas";

%LET SPACEmodule= 2 ;

/*---------------------------------------------------
*** please specify the SPACE module (see below) you would like to use.;
SPACEmodule = 1 : Deterministic approach, 0+ COV, nHealthState=3
SPACEmodule = 2 : Microsimulation approach, 0+ COV, nHealthState=2+, can run either non-parallel (with nSession=1) or parallel (with nSession>=2) computation
                                          When running parallel computation (with nSession>=2), SAS/CONNECT is required!
----------------------------------------------------*/
/*---------------------------------------------------
Output files:
For example, if BEG=65, END_LT=100, BSIZE=250, SIMSIZE=100000, nHealthState=3, depVAR=HSQ
(1) Health Expectancy (TXT files) (if TXT_output=1 in the SPACE_macro.sas)
  SPACEmodule = 1 :
    RAD_A65_100b250.txt
    RAD_A65b250.txt
  SPACEmodule = 2 :
    SIM_A65s100000b250.txt

(2) Health Expectancy (SAS data files):
  SPACEmodule = 1 :
    RAD_LE.sas7bdat        (all bootstrap results)
    RAD_LESTD.sas7bdat     (= RAD_A65_100b250.txt)
    RAD_LESTD_A65.sas7bdat (= RAD_A65b250.txt)
  SPACEmodule = 2 :
    SIM_LE.sas7bdat        (all bootstrap results)
    SIM_LESTD.sas7bdat     (= SIM_A65s100000b250.txt)

(3) Coefficients (lst files):
  SPACEmodule = 1 :
    RAD_Lifereg_HSQ_B1E2.lst  (BiEj: the transition from i to j)
    RAD_Lifereg_HSQ_B1E3.lst
    RAD_Lifereg_HSQ_B2E1.lst
    RAD_Lifereg_HSQ_B2E3.lst
  SPACEmodule = 2 :
    if logitHazard=1  (transition probability)
      SIM_Logistic_trans_HSQ.lst
    if logitHazard=2  (transition rate)
      SIM_Lifereg_HSQ_B1E2.lst  (BiEj: the transition from i to j)
      SIM_Lifereg_HSQ_B1E3.lst
      SIM_Lifereg_HSQ_B2E1.lst
      SIM_Lifereg_HSQ_B2E3.lst

(4) Simulated life lines (if SimLifeLine=1 in the SPACE_macro.sas)
  SPACEmodule = 2 :
      Sim_LifeLines.sas7bdat     (all detailed simulated life lines)
      Sim_SumLifeLines.sas7bdat  (all summary simulated life lines)

(5) Log file:
  SPACE.log
----------------------------------------------------*/

%SPACE_module;

PROC PRINTTO NEW LOG=LOG;   run;
%put --------------DONE---------------;
