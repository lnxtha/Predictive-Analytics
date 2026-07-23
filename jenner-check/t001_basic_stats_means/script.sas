/* --- jenner-check setup: mock dev_data (substitutes the external ban210dev1.csv import) --- */
/* Fixed seed => deterministic; columns match those the analysis references. */
data dev_data;
  call streaminit(20240921);
  do _i = 1 to 60;
    PRCDDA     = 35200000 + _i;
    DEPVAR7    = round(80 + rand('normal',20,15), 0.01);
    ECYEDUUD   = round(rand('uniform')*40, 0.01);
    ECYPOWNFIX = round(rand('uniform')*10, 0.01);
    HSTA001S   = round(rand('uniform')*300, 0.01);
    HSRE001    = round(rand('uniform')*450, 0.01);
    HSHC001    = round(rand('uniform')*180, 0.01);
    HSRE011    = round(rand('uniform')*500, 0.01);
    HSRE040    = round(rand('uniform')*400, 0.01);
    HSED005    = round(rand('uniform')*100, 0.01);
    HSHC007    = round(rand('uniform')*200, 0.01);
    HSSH037A   = round(rand('uniform')*150, 0.01);
    HSRO002    = round(rand('uniform')*250, 0.01);
    HSRE052    = round(rand('uniform')*300, 0.01);
    HSRE042    = round(rand('uniform')*350, 0.01);
    ECYMARSING = round(rand('uniform')*80, 0.01);
    HSTA002B   = round(rand('uniform')*120, 0.01);
    HSHC003    = round(rand('uniform')*90, 0.01);
    HSRM014    = round(rand('uniform')*140, 0.01);
    HSTA005    = round(rand('uniform')*110, 0.01);
    SV00058    = round(rand('uniform')*60, 0.01);
    output;
  end;
  drop _i;
run;

/* From Analysis (2).sas: 2a. Basic stats of objective function / target variable
   and of the 5 independent variables, verbatim titles/footnotes and PROC MEANS
   statistics as written by the author (data source substituted with dev_data). */

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
footnote1; footnote2; footnote3; footnote4; footnote5;
