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

/* From Analysis (2).sas: 4a. Correlation of all variables on the analytical file
   against the target DEPVAR7, using PROC CORR with the author's RANK and WITH
   options. The correlation output dataset is retained as the author wrote it. */

data analytical_file;
	set dev_data(drop=PRCDDA);
run;

title "Correlation between Target variables and Independent Variables";

PROC CORR DATA=analytical_file RANK OUT=corr_data;
	with DEPVAR7;
run;

title;
