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

/* From Analysis (2).sas: 3. Random sample 10 records. The author drops the
   non-analytical bookkeeping columns to form analytical_file, then draws a
   simple random sample of 10 with PROC SURVEYSELECT. Only the drop-list is
   trimmed to the columns present in the substituted dev_data. */

data analytical_file;
	set dev_data(drop=PRCDDA);
run;

title "Random 10 Sample Of Analytical File";

proc surveyselect data=analytical_file method=srs n=10 out=random_sample;
run;
proc print data=random_sample;
run;

title;
