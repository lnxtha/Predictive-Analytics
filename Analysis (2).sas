/* *********************************************************************************** */
/* Which area (PRCDDA) in Ontario is suitable for opening a new cannabis store? */
/* *********************************************************************************** */
/* ************************************** - 2 - ********************************************* */
/* importing dev file */

proc import file="/home/u63728454/Predictive Analytics (BAN 210)/Final/ban210dev1.csv" out=dev_data 
		dbms=csv Replace;
	delimiter=',';
	***	TAB - delimiter	;
	GUESSINGROWS=20;
	getnames=YES;
run;

filename data '/home/u63728454/Predictive Analytics (BAN 210)/Final/EAexercise2_Reduced Metadata_Feb7 (1).xlsx';
proc import datafile=data out=meta_data dbms=xlsx replace;
	sheet='Sheet1';
run;


/* proc contents data=dev_data;run; */

proc print data=dev_data(obs=5);
run;


/*****************************************************************************************/
/*****************************************************************************************/
/*2a. Basic stats of objective function or target variable*/

title "Basic Stats of Target Variable";
footnote 
	"DEPVAR7=Spent [Pst Mth] - Cannabis - Consumption ($/mth) Per Person Aged 19+";
ods proctitle off;

proc means data=dev_data N nmiss Mean Median mode stddev Min Max;
	var DEPVAR7;
run;

title;
footnote;
title "Basic Stats of 5 Independent Variables";
footnote1"1. ECYEDUUD=University Degree ";
footnote2"2.ECYPOWNFIX=No Fixed Workplace Address";
footnote3"3.HSTA001S=Spent on - Tobacco products and alcoholic beverages";
footnote4"4.HSRE001=Spent on - Recreation";
footnote5"5.HSHC001=Spent on - Health care";
ods proctitle off;

proc means data=dev_data N nmiss Mean Median mode stddev Min Max;
	var ECYEDUUD ECYPOWNFIX HSTA001S HSRE001 HSHC001;
run;

title;
footnote1;
footnote2;
footnote3;
footnote4;
footnote5;
title "Meta Data of Target Variable and Independent Variable";

proc print data=meta_data;
	where variable in ('DEPVAR7', 'ECYEDUUD','ECYPOWNFIX', 'HSTA001S', 'HSRE001','HSHC001');
	var variable description;
run;





/*****************************************************************************************/
/*****************************************************************************************/
/*3. Random sample 10 records*/

data analytical_file;
	set dev_data(drop=LS SG PRCDDA DensityClusterCode5_lbl DensityClusterCode5 
		DensityClusterCode15 DensityClusterCode15_2 PRIZM5DA SG_U2);
run;

title "Random 10 Sample Of Analytical File";

proc surveyselect data=analytical_file method=srs n=10 out=random_sample;
run;
proc print data=random_sample;
run;

title;


/*****************************************************************************************/
/*****************************************************************************************/
/* ************************************************* - 4 - ********************************* */
/* a. correlation of all variables on analytical file. */


title "Correlation between Target variables and Independent Variables";

PROC CORR DATA=analytical_file RANK OUT=corr_data  /*noprint*/;
	with DEPVAR7;
run;

title;

proc transpose data=corr_data out=tran_corr;
run;

data corr_final;
	*ooo;/* (keep=_name_ dow1 DEPVAR7); */
	set tran_corr;
	ABS_DEPVAR7=abs(DEPVAR7);
	drop ABS_DEPVAR7;
run;

proc sort data=corr_final;
	by descending ABS_DEPVAR7;
run;

title "Correlation of Key Model Variables On Target Variables:";

proc print data=corr_final(rename=(_NAME_=Variables depvar7=corr_coeff_DEPVAR7 
/* 		abs_depvar7=corr_coeff_ABS_DEPVAR7 */
		));
		
run;
title;





/* out of all the variables only 152 variables are correlated to our target variable DEPVAR7 */
/* Now using these 152 variables we will determine the most important variables using stepwise regression */
/* ******************************************** - 5 - *************************************** */
/* -------------------------------------------- EDA ----------------------------------------- */
/* ****************************************************************************************** */
*now do EDA's;




data corr_final;
	set corr_final;
	__N=left(_N_);
	call symput('a'!!__N, _NAME_);
run;

%MACRO CONTIN(DATASET=, GR=, VAR=);
	data tfile4;
		set &DATASET;
		COUNT=1;
		keep &var DEPVAR7;
	run;

	PROC RANK DATA=tfile4 GROUP=&GR OUT=TFILE2;
		VAR &VAR;
		RANKS __rank;
	run;

	PROC summary DATA=TFILE2 NOPRINT;
		class __rank;
		VAR &VAR  DEPVAR7;
		OUTPUT OUT=TFILE3 MIN=MINVAR DU1 MAX=MAXVAR DU4 MEAN=DU7 defect2 N=Count du2;
	run;

	DATA TFILE4;
		INFORMAT MMINVAR $8. MMAXVAR $8.;
		length _VAR $20. range $20.;
		SET TFILE3;
		_VAR="&VAR";
		*_LAB = "&LAB" ;
		MMINVAR=put(MINVAR, 8.2);
		MMAXVAR=put(MAXVAR, 8.2);
		RANGE=MMINVAR||' to '||MMAXVAR;

		if __rank=0 then
			RANGE=' <= '|| MMAXVAR;

		if __rank=&GR-1 then
			Range=MMINVAR ||'+';

		if MINVAR=MAXVAR then
			Range=MMINVAR;

		if _N_=1 then
			Range='Average';
		pcdef=defect2;
		LABEL RANGE='Range of Variable' COUNT='# OF CONSUMPTION' 
			pcdef='AVERAGE OF VALUE INDEX';
run;

	PROC PRINT SPLIT=' ';
		VAR _var Range pcdef COUNT;
		***         format pcdef percent8.2;
		*TITLE2 "&LAB";
	RUN;

%MEND;

options missing=' ';

%macro eda;
	*getting top 50 correlated variables with response and creating 5 bins with equal number of observations;

	%do i=1 %to 8;
		%CONTIN(DATASET=analytical_file, GR=5, VAR=&&a&i);
		run;
	%END;
%mend eda;

%eda;




/* *************************************** - 6 - ********************************************* */
/* 1.Modelling Technique - Stepwise Regression */
/* 2. Final Model Solution. */
/* 3. Final Model Report. */
/* ******************************************************************************************** */
title 'First stepwise regression';
proc stepwise data=analytical_file;
	model DEPVAR7=
HSRE011
HSRE040
HSCM001F
HSED006
WSD2AR
HSSH014
TOT__SPENT7
HSRE061
HSRE001S
SV00041
ECYMARSING
HSCM001D
SV00044
ECYHTA6569
ECYHTA6064
ECYMTN2534
ECYHTA3034
ECYHTA2529
SV00066
ECYHTA7074
HSCS007
ECYTENOWN
HSSH037B
HSTA002A
HSTR050
ECYTIMSA
WSWORTHV
SV00030
HSTA005
SV00093
ECYSTYSING
ECYMARM
HSRO002
HSCS013
HSHC007
ECYHTA5559
WSIN100_P
ECYMTN3544
ECYHOMPANJ
HSCS008
ECYSTYAPT
SV00043
HSRE021
CNBBAS1934
ECYCFSLP
SV00061
SV00086
ECYHSZ2PER
HSHC003
SV00058
ECYTRAWALK
HSRM014
ECYMARCL
ECYACTINLF
HSTA006
HSTA002B
ECYHOMCHIN
ECYOCCSCND
SV00038
ECYRELCHR
SV00028
SV00011
SV00021
ECYACTUR
ECYSTYAPU5
ECYMARWID
HSHC004B
HSMG008
HSTA001S
ECYRELCATH
ECYHSZ1PER
ECYBASHPOP
ECYPOWHOME
CNBBAS19P
HSRV001B
SV00025
ECYCHAKIDS
SV00023
SV00079
ECYINDADMN
ECYTRABIKE
ECYEDUHSCE
SV00037
ECYOCCMGMT
ECYEDUUD
HSTR058
HSTR034
ECYMARDIV
SV00002
ECYINDMANU
ECYTRAPUBL
SV00036
HSED005
HSFD991
ECYHOMUKRA
ECYSTYSEMI
ECYHOMFREN
SV00077
ECYINDPROF
HSRE042
HSRE063
ECYMTN4554
ECYTIMSAM
ECYINDREAL
SV00005
CNBBAS35P
SV00091
SV00035
SV00070
SV00012
ECYPOWNFIX
ECYINDFINA
HSHC004A
HSFD990
HSRE001
HSSH011
HSCL001
HSRO001
	/sls=.05 sle=.05;
run;

*end of stepwise;

/* out of 152 variables we shortlisted less than 96 important variables using stepwise regression */
/* b. Determination of key model variables */


/* Now we will confirm the final most important variables again using stepwise regression */
/* title "Key Model Variables"; */
/* final model report */



/* Finally we are able to shorlist from 240 variables to 55 variables which are most  */
/* important to determine our target variable */
/* title "Key Model Variables"; */

/* final model report */

/* ----------------------------- ALTERNATIVE SOLUTION --------------------------------- */
/* title 'Second stepwise regression'; */
/* proc stepwise data=analytical_file; */
/* 	model DEPVAR7=HSRE011 ECYHOMCHIN HSED006 HSTA001S HSRO002 HSFD991 HSGC001S  */
/* 		HSRE040 HSHC007 HSRE042 HSRE052 HSSH037A HSED005 ECYMARSING HSHC003 HSRE001S  */
/* 		ECYHTA5559 ECYHTA2529 HSRV001B SV00005 SV00064 HSTA002B HSRE061 HSTR058  */
/* 		HSCM001F HSSH011 HSRM014 WSIN100_P SV00041 ECYMTN2534 SV00038 SV00030 SV00036  */
/* 		HSTA005 HSTA002A HSHC002 HSTR050 WSCARDSB SV00035 HSRE063 SV00043 HSCS013  */
/* 		TOT__SPENT7 SV00058 ECYHOMPANJ HSCL001 ECYTENOWN ECYINDMGMT HSMG008 WSWORTHV  */
/* 		HSRE021 HSHC001S SV00002 ECYHTA6064 ECYCHAKIDS SV00028 SV00091 SV00079  */
/* 		SV00023 SV00070 SV00061 HSHC004B ECYHSZ1PER HSTR034 ECYHOMFREN WSD2AR SV00012  */
/* 		SV00066 HSRO001 SV00044 HSHC001 HSSH037B ECYHTA7074 ECYMTN4554 CNBBAS35P  */
/* 		ECYTRAPUBL ECYEDUUD ECYSTYAPT HSCS008 /sls=.05 sle=.05; */
/* run; */

/* title; */


title 'Third stepwise regression';
proc stepwise data=analytical_file;
model DEPVAR7=
HSRE011
HSTA001S
HSRE040
HSED005
HSHC007
HSSH037A
HSRO002
HSRE052
HSRE042
ECYMARSING
HSTA002B
HSHC003
HSRM014
HSTA005
SV00058
/sls=.05 sle=.05;
run;
title;

/* ************************************************************************************** */
*run final model in multiple regression and create decile report on validation sample;
*creates algorithm;


title "6b. Run final model in multiple regression";
 proc reg data=analytical_file outest=regout1;
oxyhat: model  depvar7=
HSRE011
HSTA001S
HSRE040
HSED005
HSHC007
HSSH037A
HSRO002
HSRE052
HSRE042
ECYMARSING
HSTA002B
HSHC003
HSRM014
HSTA005
SV00058;
run;





*runs algorithm against validation dataset and creates output scores on validation dataset;

proc import file="/home/u63728454/Predictive Analytics (BAN 210)/Final/ban210val1.csv" out=val_data 
		dbms=csv Replace;
	delimiter=',';
	***	TAB - delimiter	;
	GUESSINGROWS=20;
	getnames=YES;
run;

title "7. Validation model";
proc print data=Val (obs=20);
run;

PROC SCORE DATA =val_data
SCORE=REGOUT1 OUT=RSCOREP
TYPE=PARMS;
VAR 
HSRE011 HSTA001S HSRE040 HSED005 HSHC007 HSSH037A 
HSRO002 HSRE052 HSRE042 ECYMARSING HSTA002B HSHC003 
HSRM014 HSTA005 SV00058
;
RUN;

DATA file2;
 	SET RSCOREP;
	ARRAY RAW _NUMERIC_;
	DO OVER RAW;
	IF RAW= . THEN RAW=0;
END; 
run;


*sort validation file by model score in descending order and put into 10 deciles;
  PROC SORT DATA=file2;BY DESCENDING oxyhat;
    	     DATA file3;
          SET file2 nobs=NO;
          account=1;
          IF _N_=1 THEN  PERCENT=0;
          IF CEIL((_N_/NO)*10) GT PERCENT THEN  PERCENT+1;
          RETAIN PERCENT;
		  run;
		  
		  title "Sample Validation Score";
		  proc print data=file3;
		  where ranuni(0) le 0.01;
		  var oxyhat percent;
		  run;
		  title;
		  		  
		  
*now summarize observed or actual behaviour of target variable(response) against these 10 bins and create decile or gains chart table;
		  PROC SUMMARY data=file3;
          CLASS PERCENT;
          VAR  OXYHAT depvar7 account;
          OUTPUT OUT=TEST3 SUM=DU1 TOTRESP  ACCOUNT
							mean=DU2 RESPRATE du3
           min=SCORE du4 DU5;
           
           run;
           
	      DATA GAINS1;
          SET TEST3;
          DROP DU1-DU5 _TYPE_ _FREQ_;
          IF _N_=1 THEN DO;
            TOTALRES=totRESP;
			TOTRS=RESPRATE;
			cummail=0;
                END;
          ELSE DO;
            PCTOTRES=ROUND((TOTRESP/TOTALRES)*100,.01);
            INRSRATE=ROUND((RESPrate/TOTRS)*100,.01);
                      END;
			      RETAIN TOTALRES TOTRS;

          DATA GAINS2;
          SET GAINS1;
          IF _N_=1 THEN DELETE;
                LABEL SCORE='MINIMUM* SCORE IN RANGE'
                      PCTOTRES='% OF TOTAL* CONSUMPTION IN INTERVAL'
                      RESPRATE='AVG. CNSUMPTION. RATE* WITHIN INTERVAL'
                      INRSRATE='INTERval LIFT IN CONSUMPTION.RATE'
                      percent='% OF PROSPECTS* CONSUMED IN INTERVAL';

           PROC FORMAT;
           value  ABC 1='0-5%'
                  2='5%-10%'
                  3='10%-15%'
                  4='15%-20%'
                  5='20%-25%'
                  6='25%-30%'
                  7='30%-35%'
                  8='35%-40%'
                  9='40%-45%'
                  10='45%-50%'
                  11='50%-55%'
                  12='55%-60%'
                  13='60%-65%'
                  14='65%-70%'
                  15='70%-75%'
                  16='75%-80%'
                  17='80%-85%'
                  18='85%-90%'
                  19='90%-95%'
                  20='95%-100%';
				
          PROC PRINT DATA=GAINS2 SPLIT='*';
          ID PERCENT;
          VAR score pctotRES RESPrate ACCOUNT;
          *FORMAT PERCENT ABC.;
		  title "Decile Chart";
          RUN;
quit;

/*end of gains chart/decile chart*/





PROC HPSPLIT DATA=dev_data nodes plots=all maxdepth=5 maxbranch=3;
MODEL depvar7= 
HSRE011
HSTA001S
HSRE040
HSED005
HSHC007
HSSH037A
HSRO002
HSRE052
HSRE042
ECYMARSING
HSTA002B
HSHC003
HSRM014
HSTA005
SV00058
;
   *grow entropy;
    *PRUNE costcomplexity;
    PARTITION FRACTION(VALIDATE=0.1 SEED=42);
    *CODE FILE=  'C:/Users/Richard01/Documents/Richard Documents/bank_tree.sas';
    *OUTPUT OUT = SCORED;
 
run;

*ods graphics off;
ods html close;

title "Some Good Locations Based on the Model";
proc sort data=dev_data out=good_locations;
   by descending 
   	HSRE011
	HSRM014
	HSTA005
	HSRE040
	HSTA002B
	HSRE052
	HSSH037A
	HSRO002
	ECYMARSING
	HSHC003
	HSTA001S
	SV00058
	HSHC007
	HSTA005
	HSRE042
	;
run;
/* ----------------------------------------------------------------------- */

title"good Locations for opening Cannabis shop";
proc print data=good_locations (obs=10);
var prcdda
    HSRE011
	HSRM014
	HSTA005
	HSRE040
	HSTA002B
	HSRE052
	HSSH037A
	HSRO002
	ECYMARSING
	HSHC003
	HSTA001S
	SV00058
	HSHC007
	HSTA005
	HSRE042;
run;
title;