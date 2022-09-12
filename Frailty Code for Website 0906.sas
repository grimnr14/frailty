 options ps = 54 ls = 72 obs = max pageno = 1;
 libname new 'H:\research\A_progs\pacs_code\may2020';

 
***********************************************************************;
* Section 1: Generate Frailty Measures with input from various sources ;
***********************************************************************;

/*  
 Generate Frailty Measures Using Kim's Code.
 These codes were derived directly from Kim's Website: "https://dataverse.harvard.edu/dataverse/cfi/".
 Repeat Section 1 using information from three sources within each year: "claims", "EHR", and "claims or EHR".
 Change the default file names in the following codes so that you have three sets of files/frailty measures corresponding to each source.
 Each file should contain two variables from Kim's codes (patient_ID & frailty_index) and a new variable (year): 
   1. patient_ID
   2. Score (Make sure this frailty variable is renamed for each data source: frailty_index_clm, frailty_index_ehr or frailty_index_either) 
   3. Year (This variable represents the time frame used to calculate frialty index and needs to be created by the SAS programmer).
*/
 
**************************************;
* sample datasets:    variables:     ;
* 1.ids               patid           ;
* 2.dx09              patid, dx(icd9) ;
* 3.dx10              patid, dx(icd10);
* 4.px                patid, px(CPT4) ;
**************************************;

**************************************;
* scores data:                        ;
* 1.codes_org                         ;
* 2.disease_weight                    ;
* 3.frailty_icd10_dx                  ;
**************************************;

 data px_score
      dx_score;
    set new.codes_org;
 if set_type = 'dx' then output dx_score;
 if set_type = 'px' then output px_score;
 keep range_min range_max disease_number;
 run;

**************************************;
* ICD9 dx format for frailty disease  ;
**************************************;
 
data master;
  length label $5;
  set dx_score;
  label = left(put(disease_number, 2.));
  drop disease_number;
run;
 
data other;
  start = 'other';
  end   = 'other';
  label = 'other';
run;
 
data study_dx;
  set master(rename=(range_min = start range_max = end))
      other;
  retain fmtname '$study_dx' EEXCL 'Y';
run;
 
proc format cntlin = study_dx;
run;

 data score_dx;
     set new.dx09(keep = patid dx);
 class    = put(dx, $study_dx.);
 if class ^= 'other';
 run;


**********************************************;
* cpt4 procedures format for frailty disease  ;
**********************************************;

 data master;
   set px_score;
 label = left(disease_number);
 drop disease_number;
 run;


  data other;
     start = 'other';
     end   = 'other';
     label = 'other';
  run;

  data study_px;
      set master(rename=(range_min = start range_max = end))
          other;
  fmtname = '$study_px';
  run;

 proc format cntlin = study_px;
  run;

 data score_px;
     set new.px(keep = patid px);
 class    = put(px, $study_px.);
 if class ^= 'other';
 last_d =  substr(left(px), 5,1);
 if last_d = ' '  or last_d = '0'  or last_d = '1'  or last_d ='2' or
    last_d = '3'  or last_d = '4'  or last_d = '5'  or last_d ='6' or
    last_d = '7'  or last_d = '8'  or last_d = '9' ;
 drop last_d;
 run;

 *****************************;
 * frailty disease weight     ;
 *****************************;

 data weight;
   set new.disease_weight;
class = left(disease_number);
keep class weight;
run;

proc sort data = weight;
   by class;
run;

data score;
 set  score_px
      score_dx;
keep patid class;
run;

proc sort nodupkey data = score;
    by patid class;
run;

proc sort data = score;
    by class;
run;

data scores_09_px;
   merge score(in = in1)
         weight(in = in2);
  by class;
  if in1 and in2;
run;

*******************************************;
* link icd10/disease/weight to             ;
* prior icd10 dx                           ;
*******************************************;


proc sql;
   create table score_dx10(keep = patid dx disease_number weight) as
   select * from new.frailty_icd10_dx diag,
            new.dx10  dx10
   where diag.dx = dx10.dx;
quit;

data score_10;
     set score_dx10;
class  = left(disease_number);
keep patid class weight;
run;

proc sort nodupkey data = score_10;
    by patid class;
run;

data scores;
     set scores_09_px
         score_10;
keep patid class weight;
run;

proc sort nodupkey data = scores;
   by patid class;
run;


data count;
   set scores;
   by patid;
   if first.patid then score = 0.10288;
       score + weight;
   if last.patid then output;
keep patid score;
run;

data frailty_score;
   merge new.ids(in = in1 keep = patid)
         count(in = in2);
   by patid;
   if in1;
   if not in2 then score = 0.10288;
keep patid score ;
run;

proc means data = frailty_score;
   var score;
run;


 
****************************************************;
* Section 2: Merge files and concordance assessment ;
****************************************************;

/* 
 Set up one file of frailty representing each source (files of the same source acorss different years will be aggregated).
 Three files of frailty measures (frailty_clm, frailty_ehr & frailty_either) will be created representing three various sources: claims, EHR, claims or EHR.
 These files should contain three variables: patient_ID, frailty_index_xx (xx=clm, ehr or either) & year (in this example 2016-2019 is used).
*/

%Macro abc(tx=); Data frailty_&tx; set frailty_score_&tx_2016-frailty_score_&tx_2019; run; proc sort; by patient_id year; run; %Mend abc;
%abc(tx=clm);%abc(tx=ehr);%abc(tx=either);


/* 
 Pt file is unique at "Patient_ID + Year" level and contains eligible subjects only with the following information: 
  1. Patient_ID
  2. Year
  3. Demographicss & Comoorbidity: age_group, sex, race, comorbidity (Charlson Index, Elihauser Index, or any other morbidity measure), etc.
  4. Medical utilization: any_IP (having any hospitalization), any_ED (having any ED visit), any_NH (having any nursing home visit), etc.
  5. other variables to be evaluated. 
*/

***   Merge files of three frailty measures with patient file   ***;
Data frialty_pt; merge pt (in=a) frailty_clm frailty_ehr frailty_clmehr; by patient_id year; if a=1; run;

***   Replace missing (people without eligible diagnosis or procedure codes) with the reference score of 0.10288 and create binary frailty indicators   ***;
Data frialty_pt; set frialty_pt;
%Macro abc(tx=); if frailty_index_&tx=. then frailty_index_&tx=0.10288; length FI_&tx 3; FI_&tx=0; if frailty_index_&tx>=0.2 then FI_&tx=1; %Mend abc;
%abc(tx=clm);%abc(tx=ehr);%abc(tx=either);
run;


**  Concordance Assessment - Overall by year  **;
Data frialty_pt; set frialty_pt; length Frailty_b_agree 3; Frailty_b_agree=0; if FI_clm=FI_ehr then Frailty_b_agree=1; run;
proc sort data=frialty_pt; by year; run; 
proc corr data=frialty_pt pearson; var FI_ehr FI_clm; by year; run; /* Calculate Phi Coefficient */
proc freq data=frialty_pt; table Frailty_b_agree FI_ehr*FI_clm/agree(nullkappa=0); by year; run; /* Percent agreement and Kappa's Coefficient */
proc corr data=frialty_pt pearson spearman; var frailty_index_clm frailty_index_ehr; by year; run; /* Correlation between continuous variables */

**  Concordance Assessment - by year and group  **;
%Macro abc(tx=); 
proc sort data=frialty_pt; by year &tx; run; 
proc corr data=frialty_pt pearson; var FI_ehr FI_clm; by year &tx; run; /* Calculate Phi Coefficient */
proc freq data=frialty_pt; table Frailty_b_agree FI_ehr*FI_clm/agree(nullkappa=0); by year &tx; run; /* Percent agreement and Kappa's Coefficient */
proc corr data=frialty_pt pearson spearman; var frailty_index_clm frailty_index_ehr; by year &tx; run; /* Correlation between continuous variables */
%Mend abc;
%abc(tx=age_group);%abc(tx=sex);%abc(tx=race); /* Other groups can be added */



 
******************************;
* Section 3: Predictive Model ;                      ;
******************************;

/**  
 Create dummy variables for each categorical variable
 Users should include all cateogrical variables intended to be included in the model.
 Users should determine the reference level for each categorical variable.
**/

Data frialty_pt; set frialty_pt;
length female age_group_18_49 age_group_50_64 age_group_65plus 3;
female=0; if sex="F" then female=1;
age_group_18_49=0; if age_group="1 18-49" then age_group_18_49=1; 
age_group_50_64=0; if age_group="2 50-64" then age_group_50_64=1; 
age_group_65plus=0; if age_group in ("3 65-79" "4 80+") then age_group_65plus=1; 
%Macro ab(tx=); legnth race_&tx 3; race_&tx=0; if race="&tx" then race_&tx=1; %Mend ab; %ab(tx=Asian);%ab(tx=Black);%ab(tx=Other);%ab(tx=White);
%Macro ab(tx=); legnth year_&tx 3; year_&tx=0; if year_&tx=&tx then year_&tx=1; %Mend ab; %ab(tx=2017);%ab(tx=2018);
run;

**  Create Development(80%)/Validation(20%) Sample by year. Performance measures will be derived from the validation set to aviod overfitting  **;
proc sort data=frialty_pt; by year; run;
proc surveyselect data=frialty_pt out=a seed=621225 samprate=0.2; strata year/alloc=prop; run;
Data a; set a; length validation 3; validation=1; keep patient_id year validation; run;
proc sort; by patient_id year; run;

proc sort data=frialty_pt; by patient_id year; run;
data frialty_pt; merge frialty_pt a; by patient_id year; if validation=. then validation=0; run;
proc freq; table year*validation; run;

**  Separate development and validation files  **;
data frialty_pt_d; set frialty_pt; where validation=0; run;
data frialty_pt_v; set frialty_pt; where validation=1; run;


/** 
 GEE is used to control for correlation among observations from the same individual.
 Unstructured correlation is chosen given the least amount of restriction on the correlation structure; users can choose what they want to use.
 Repeat this model nine times (three outcomes * three frialty measures).
**/

%Macro abc(ta=,tx=); 
proc genmod data=frialty_pt_d namelen=30 plots=none; class patient_id;
model any_&ta(event="1")= FI_&tx female age_group_50_64 age_group_65plus race_asian race_black race_other year_2017 year_2018 comorbidity / dist=bin;
repeated subject=patient_id / type=un modelse;
ods output GEEEmpPEST=&ta._&tx._un;
run;

data a; set &ta._&tx._un; keep parm estimate; run; proc transpose data=a out=b; id parm; var estimate; run;
proc logistic data=frialty_pt_v inest=b;
model any_&ta(event="1")= FI_&tx female age_group_50_64 age_group_65plus race_asian race_black race_other year_2017 year_2018 comorbidity / maxiter=0 ROCCI;
ods ROCAssociation=c&ta._&tx._un; /* this file contains information of c-statistics */
output out=validation p=p_&ta._&tx;
run;
%Mend abc;
%abc(ta=ip,tx=ehr);%abc(ta=ip,tx=clm);%abc(ta=ip,tx=either);  
%abc(ta=ed,tx=ehr);%abc(ta=ed,tx=clm);%abc(ta=ed,tx=either);  
%abc(ta=nh,tx=ehr);%abc(ta=nh,tx=clm);%abc(ta=nh,tx=either);  


/** 
 Calculate sensitivity, Specificity, Positive Predictive Value and Negative Predictive Value using 2*2 table.  
 Threshold is set as top 5%; users can choose other thresholds by adjusting the code.
**/
data validation; set validation; drop _level_ _level_2-_level_9; run;
proc rank data=validation out=validation group=100;
var p_ip_ehr p_ip_clm p_ip_either p_ed_ehr p_ed_clm p_ed_either p_nh_ehr p_nh_clm p_nh_either;
ranks g_ip_ehr g_ip_clm g_ip_either g_ed_ehr g_ed_clm g_ed_either g_nh_ehr g_nh_clm g_nh_either;
run;
proc freq; table g_ip_ehr g_ip_clm g_ip_either g_ed_ehr g_ed_clm g_ed_either g_nh_ehr g_nh_clm g_nh_either; run;

data validation; set validation; 
length g_ip_ehr_top5 g_ip_clm_top5 g_ip_either_top5 g_ed_ehr_top5 g_ed_clm_top5 g_ed_either_top5 g_nh_ehr_top5 g_nh_clm_top5 g_nh_either_top5 3;
array abc(9) g_ip_ehr g_ip_clm g_ip_either g_ed_ehr g_ed_clm g_ed_either g_nh_ehr g_nh_clm g_nh_either;
array def(9) g_ip_ehr_top5 g_ip_clm_top5 g_ip_either_top5 g_ed_ehr_top5 g_ed_clm_top5 g_ed_either_top5 g_nh_ehr_top5 g_nh_clm_top5 g_nh_either_top5;
do i=1 to 9; def(i)=0; if abc(i)>=95 then def(i)=1; end; drop i;
run;
proc freq; table g_ip_ehr_top5 g_ip_clm_top5 g_ip_either_top5 g_ed_ehr_top5 g_ed_clm_top5 g_ed_either_top5 g_nh_ehr_top5 g_nh_clm_top5 g_nh_either_top5; run;

**  Agreement Sensitivity Specificity PPN NPV  **;
%Macro abc(tx=,ta=); 
proc freq Data=validation; table g_&ta._&tx._top5*any_&ta/out=a outexpect sparse; run;
proc transpose data=a out=b prefix=N_; id g_&ta._&tx._top5 any_&ta; var count; run;
Data &ta._&tx; set b; drop _label_ _name_; length label $15.; label="&ta._&tx";
agreement=(N_00+N_11)/(N_00+N_11+N_01+N_10); Sen=N_11/(N_01+N_11); Spe=N_00/(N_00+N_10); PPV=N_11/(N_10+N_11); NPV=N_00/(N_00+N_01);
run;
%Mend abc;
%abc(ta=ip,tx=ehr);%abc(ta=ip,tx=clm);%abc(ta=ip,tx=either);%abc(ta=ed,tx=ehr);%abc(ta=ed,tx=clm);%abc(ta=ed,tx=either);  
%abc(ta=nh,tx=ehr);%abc(ta=nh,tx=clm);%abc(ta=nh,tx=either);  
