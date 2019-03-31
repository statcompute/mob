%macro num_woe(data = , y = , x = );
***********************************************************;
* THE SAS MACRO IS TO PERFORM UNIVARIATE IMPORTANCE RANK  *;
* ORDER AND MONOTONIC WEIGHT OF EVIDENCE TRANSFORMATION   *;
* FOR NUMERIC ATTRIBUTES IN PRE-MODELING DATA PROCESSING  *;
* (IT IS RECOMMENDED TO RUN THIS MACRO IN THE BATCH MODE) *;
* ======================================================= *;
* PAMAMETERS:                                             *;
*  DATA: INPUT SAS DATA TABLE                             *;
*  Y   : RESPONSE VARIABLE WITH 0/1 VALUE                 *;
*  X   : A LIST OF NUMERIC ATTRIBUTES                     *;
* ======================================================= *;
* OUTPUTS:                                                *;
*  MONO_WOE.WOE: A FILE OF WOE TRANSFORMATION RECODING    *;
*  MONO_WOE.FMT: A FILE OF BINNING FORMAT                 *;
*  MONO_WOE.PUT: A FILE OF PUT STATEMENTS FOR *.FMT FILE  *;
*  MONO_WOE.SUM: A FILE WITH PREDICTABILITY SUMMARY       *;
*  MONO_WOE.OUT: A FILE WITH STATISTICAL DETAILS          *;
*  MONO_WOE.IMP: A FILE OF MISSING IMPUTATION RECODING    *;
* ======================================================= *;
* CONTACT:                                                *;
*  WENSUI.LIU@53.COM                                      *;
***********************************************************;
 
options nocenter nonumber nodate mprint mlogic symbolgen
        orientation = landscape ls = 150;
 
*** DEFAULT PARAMETERS ***;
 
%local maxbin minbad miniv bignum;
 
%let maxbin = 100;
 
%let minbad = 50;
 
%let miniv  = 0.03;
 
%let bignum = 1e300;
 
***********************************************************;
***         DO NOT CHANGE CODES BELOW THIS LINE         ***;
***********************************************************;
 
*** DEFAULT OUTPUT FILES ***;
 
* WOE RECODING FILE                     *;
filename woefile "MONO_WOE.WOE";
 
* FORMAT FOR BINNING                    *;
filename fmtfile "MONO_WOE.FMT";
 
* PUT STATEMENT TO USE FORMAT           *;
filename binfile "MONO_WOE.PUT";
 
* KS SUMMARY                            *;
filename sumfile "MONO_WOE.SUM";
  
* STATISTICAL SUMMARY FOR EACH VARIABLE *;
filename outfile "MONO_WOE.OUT";
 
* IMPUTE RECODING FILE                  *;
filename impfile "MONO_WOE.IMP";
 
*** A MACRO TO DELETE FILE ***;
%macro dfile(file = );
  data _null_;
    rc = fdelete("&file");
    if rc = 0 then do;
      put @1 50 * "+";
      put "THE EXISTED OUTPUT FILE HAS BEEN DELETED.";
      put @1 50 * "+";
    end;
  run;
%mend dfile;
 
*** CLEAN UP FILES ***;
%dfile(file = woefile);
 
%dfile(file = fmtfile);
 
%dfile(file = binfile);
 
%dfile(file = sumfile);
 
%dfile(file = outfile);
 
%dfile(file = impfile);
 
*** PARSING THE STRING OF NUMERIC PREDICTORS ***;
ods listing close;
ods output position = _pos1;
proc contents data = &data varnum;
run;
 
proc sql noprint;
  select
    upcase(variable) into :x2 separated by ' '
  from
    _pos1
  where
    compress(upcase(type), ' ') = 'NUM' and
    index("%upcase(%sysfunc(compbl(&x)))", compress(upcase(variable), ' ')) > 0;
 
 
  select
    count(variable) into :xcnt
  from
    _pos1
  where
    compress(upcase(type), ' ') = 'NUM' and
    index("%upcase(%sysfunc(compbl(&x)))", compress(upcase(variable), ' ')) > 0;
quit;
 
data _tmp1;
  retain &x2 &y;
  set &data;
  where &Y in (1, 0);
  keep &x2 &y;
run;
 
ods output position = _pos2;
proc contents data = _tmp1 varnum;
run;
 
*** LOOP THROUGH EACH PREDICTOR ***;
%do i = 1 %to &xcnt;
     
  proc sql noprint;
    select
      upcase(variable) into :var
    from
      _pos2
    where
      num= &i;
 
    select
      count(distinct &var) into :xflg
    from
      _tmp1
    where
      &var ~= .;
  quit;
 
  proc summary data = _tmp1 nway;
    output out  = _med(drop = _type_ _freq_)
    median(&var) = med nmiss(&var) = mis;
  run;
   
  proc sql;
    select
      med into :median
    from
      _med;
 
    select
      mis into :nmiss
    from
      _med;
 
    select 
      case when count(&y) = sum(&y) then 1 else 0 end into :mis_flg1
    from
      _tmp1
    where
      &var = .;
 
    select
      case when sum(&y) = 0 then 1 else 0 end into :mis_flg2
    from
      _tmp1
    where
      &var = .;
  quit;
 
  %let nbin = %sysfunc(min(&maxbin, &xflg));
 
  *** CHECK IF THE NUMBER OF DISTINCT VALUES > 1 ***;
  %if &xflg > 1 %then %do;
 
    *** IMPUTE MISS VALUE WHEN WOE CANNOT BE CALCULATED ***;
    %if &mis_flg1 = 1 | &mis_flg2 = 1 %then %do;
      data _null_;
        file impfile mod;
        put " ";
        put @3 "*** MEDIAN IMPUTATION OF %TRIM(%UPCASE(&VAR)) (NMISS = %trim(&nmiss)) ***;";
        put @3 "IF %TRIM(%UPCASE(&VAR)) = . THEN %TRIM(%UPCASE(&VAR)) = &MEDIAN;";
      run;
 
      data _tmp1;
        set _tmp1;
        if &var = . then &var = &median;
      run; 
    %end;      
       
    *** LOOP THROUGH THE NUMBER OF BINS ***;
    %do j = &nbin %to 2 %by -1;
      proc rank data = _tmp1 groups = &j out = _tmp2(keep = &y &var rank);
        var &var;
        ranks rank;
      run;
 
      proc summary data = _tmp2 nway missing;
        class rank;
        output out = _tmp3(drop = _type_ rename = (_freq_ = freq))
        sum(&y)   = bad    mean(&y)  = bad_rate
        min(&var) = minx   max(&var) = maxx;
      run;
 
      *** CREATE FLAGS FOR MULTIPLE CRITERION ***;
      proc sql noprint;
        select
          case when min(bad) >= &minbad then 1 else 0 end into :badflg
        from
          _tmp3;
 
        select
          case when min(bad_rate) > 0 then 1 else 0 end into :minflg
        from
          _tmp3;
 
        select
          case when max(bad_rate) < 1 then 1 else 0 end into :maxflg
        from
          _tmp3;              
      quit;
 
      *** CHECK IF SPEARMAN CORRELATION = 1 ***;
      %if &badflg = 1 & &minflg = 1 & &maxflg = 1 %then %do;
        ods output spearmancorr = _corr(rename = (minx = cor));
        proc corr data = _tmp3 spearman;
          var minx;
          with bad_rate;
        run;
 
        proc sql noprint;
          select
            case when abs(cor) = 1 then 1 else 0 end into :cor
          from
            _corr;
        quit;
 
        *** IF SPEARMAN CORR = 1 THEN BREAK THE LOOP ***;
        %if &cor = 1 %then %goto loopout;
      %end;
      %else %if &nbin = 2 %then %goto exit;
    %end;
 
    %loopout:
     
    *** CALCULATE STATISTICAL SUMMARY ***;
    proc sql noprint;
      select 
        sum(freq) into :freq
      from
        _tmp3;
 
      select
        sum(bad) into :bad
      from
        _tmp3;
    quit;
 
    proc sort data = _tmp3 sortsize = max;
      by rank;
    run;
 
    data _tmp4;
      retain bin minx maxx bad freq pct bad_rate;
      set _tmp3 end = eof;
      by rank;
 
      if rank = . then bin = 0;
      else do;
        retain b 0;
        bin + 1;
      end;
   
      pct  = freq / &freq;
      bpct = bad / &bad;
      gpct = (freq - bad) / (&freq - &bad);
      woe  = log(bpct / gpct);
      iv   = (bpct - gpct) * woe;
 
      retain cum_bpct cum_gpct;
      cum_bpct + bpct;
      cum_gpct + gpct;
      ks = abs(cum_gpct - cum_bpct) * 100;
 
      retain iv_sum ks_max;
      iv_sum + iv;
      ks_max = max(ks_max, ks);
      if eof then do;
        call symput("bin", put(bin, 4.));
        call symput("ks", put(ks_max, 10.4));
        call symput("iv", put(iv_sum, 10.4));
      end;
 
      keep bin minx maxx bad freq pct bad_rate
           gpct bpct woe iv cum_gpct cum_bpct ks;
    run;
 
    *** REPORT STATISTICAL SUMMARY ***;
    proc printto print = outfile;
    run;
 
    title;
    ods listing;
    proc report data = _tmp4 spacing = 1 split = "*" headline nowindows;
      column(" * MONOTONIC WEIGHT OF EVIDENCE TRANSFORMATION FOR %upcase(%trim(&var))"
             bin minx maxx freq pct bad bad_rate woe iv ks);
 
      define bin      /"BIN*LEVEL"   width = 5  format = z3. order order = data;
      define minx     /"LOWER*LIMIT" width = 15 format = 14.4;
      define maxx     /"UPPER*LIMIT" width = 15 format = 14.4;
      define bad      /"#BADS*(Y=1)" width = 8  format = 7.;
      define freq     /"#FREQ"       width = 10 format = 9.;
      define pct      /"PERCENT"     width = 8  format = percent8.2;
      define bad_rate /"BAD*RATE"    width = 8  format = percent8.2;
      define woe      /"WOE"         width = 10 format = 9.4;
      define iv       /"INFO.*VALUE" width = 10 format = 9.4;
      define ks       /"KS"          width = 10 format = 9.4;
      compute after;
        line @1 110 * "-";
        line @5 "# TOTAL = %trim(&freq), # BADs(Y=1) = %trim(&bad), "
                "OVERALL BAD RATE = %trim(%sysfunc(round(&bad / &freq * 100, 0.0001)))%, "
                "MAX. KS = %trim(&ks), INFO. VALUE = %trim(&iv).";
        line @1 110 * "-";    
      endcomp;
    run;
    ods listing close;
 
    proc printto;
    run;
 
    proc sql noprint;
      select
        case when sum(iv) >= &miniv then 1 else 0 end into :ivflg
      from
        _tmp4;
    quit;
 
    *** OUTPUT RECODING FILES IF IV >= &miniv BY DEFAULT ***;
    %if &ivflg = 1 %then %do;
      data _tmp5;
        length upper $20 lower $20;
        lower = compress(put(maxx, 20.4), ' ');
 
        set _tmp4 end = eof;
        upper = compress(put(maxx, 20.4), ' ');
        if bin = 1 then lower = "-%trim(&bignum)";
        if eof then upper = "%trim(&bignum)";
        w%trim(&var) = compress(put(woe, 12.8), ' ');
      run;
 
      *** OUTPUT WOE RECODE FILE ***;
      data _null_;
        set _tmp5 end = eof;
        file woefile mod;
 
        if bin = 0 and _n_ = 1 then do;
          put " ";
          put @3 3 * "*"
                 " WOE RECODE OF %upcase(%trim(&var)) (KS = %trim(&ks), IV = %trim(&iv))"
                 + 1 3 * "*" ";";
          put @3  "if %trim(&var) = . then w%trim(&var) = " + 1 w%trim(&var) ";";
        end;
        if bin = 1 and _n_ = 1 then do;
          put " ";
          put @3 3 * "*"
                 " WOE RECODE OF %upcase(%trim(&var)) (KS = %trim(&ks), IV = %trim(&iv))"
                 + 1 3 * "*" ";";
          put @3 "if " + 1 lower " < %trim(&var) <= " upper
                 " then w%trim(&var) = " + 1 w%trim(&var) ";";
        end;
        if _n_ > 1 then do;
          put @5 "else if " + 1 lower " < %trim(&var) <= " upper
                 " then w%trim(&var) = " + 1 w%trim(&var) ";";
        end;
        if eof then do;
          put @5 "else w%trim(&var) = 0;";
        end;
      run;
 
      *** OUTPUT BINNING FORMAT FILE ***;
      data _null_;
        set _tmp5 end = eof;
        file fmtfile mod;
 
        if bin = 1 then lower = "LOW";
        if eof then upper = "HIGH";
 
        if bin = 0 and _n_ = 1 then do;
          put " ";
          put @3 3 * "*"
                 " BINNING FORMAT OF %trim(&var) (KS = %trim(&ks), IV = %trim(&IV))"
              + 1 3 * "*" ";";
          put @3 "value %trim(&var)_fmt";
          put @5 ". " @40 " = '" bin: z3.
                 ". MISSINGS'";
        end;
 
             
        if bin = 1 and _n_ = 1 then do;
          put " ";
          put @3 3 * "*"
              @5 "BINNING FORMAT OF %trim(&var) (KS = %trim(&ks), IV = %trim(&IV))"
              + 1 3 * "*" ";";
          put @3 "value %trim(&var)_fmt";
          put @5 lower @15 " - " + 1 upper  @40 " = '" bin: z3.
                 ". " + 1 lower " - " + 1 upper "'";
        end;
 
        if _n_ > 1 then do;
          put @5 lower @15 "<- " + 1 upper @40 " = '" bin: z3.
                 ". " + 1 lower "<- " + 1 upper "'";
        end;
        if eof then do;
          put @5 "OTHER" @40 " = '999 .  OTHERS';";
        end;
      run;
 
      *** OUTPUT BINNING RECODE FILE ***;
      data _null_;
        file binfile mod;
        put " ";
        put @3 "*** BINNING RECODE of %trim(&var) ***;";
        put @3 "c%trim(&var) = put(%trim(&var), %trim(&var)_fmt.);";
      run;
 
      *** SAVE SUMMARY OF EACH VARIABLE INTO A TABLE ***;
      %if %sysfunc(exist(work._result)) %then %do;
        data _result;
          format variable $32. bin 3. ks 10.4 iv 10.4;
          if _n_ = 1 then do;
            variable = "%trim(&var)";
            bin      = &bin;
            ks       = &ks;
            iv       = &iv;
            output;
          end;
          set _result;
          output;
        run;
      %end;
      %else %do;
        data _result;
          format variable $32. bin 3. ks 10.4 iv 10.4;
          variable = "%trim(&var)";
          bin      = &bin;
          ks       = &ks;
          iv       = &iv;
        run;        
      %end;
    %end;
 
    %exit:
 
    *** CLEAN UP TEMPORARY TABLES ***;
    proc datasets library = work nolist;
      delete _tmp2 - _tmp5 _corr / memtype = data;
    run;
    quit;
  %end;    
%end;
 
*** SORT VARIABLES BY KS AND OUTPUT RESULTS ***;
proc sort data = _result sortsize = max;
  by descending ks descending iv;
run;
 
data _null_;
  set _result end = eof;
  file sumfile;
 
  if _n_ = 1 then do;
    put @1 80 * "-";
    put @1  "| RANK" @10 "| VARIABLE RANKED BY KS" @45 "| # BINS"
        @55 "|  KS"  @66 "| INFO. VALUE" @80 "|";
    put @1 80 * "-";
  end;
  put @1  "| " @4  _n_ z3. @10 "| " @12 variable @45 "| " @50 bin
      @55 "| " @57 ks      @66 "| " @69 iv       @80 "|";
  if eof then do;
    put @1 80 * "-";
  end;
run;
 
proc datasets library = work nolist;
  delete _result (mt = data);
run;
quit;
 
*********************************************************;
*           END OF NUM_WOE MACRO                        *;
*********************************************************;
%mend num_woe;
 
libname data 'D:\SAS_CODE\woe';
 
%let x = 
tot_derog
tot_tr
age_oldest_tr
tot_open_tr
tot_rev_tr
tot_rev_debt
tot_rev_line
rev_util
bureau_score
ltv
tot_income
;
 
%num_woe(data = data.accepts, y = bad, x = &x);
