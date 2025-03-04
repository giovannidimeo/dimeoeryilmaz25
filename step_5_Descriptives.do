*____________________________________________________________________________________________________*
* The Impacts of Health Shocks on Household Labor Supply and Domestic Production
* Di Meo & Eryilmaz, 2025	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

*____________________________________________________________________________________________________*
* SETUP	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
version 16 
set more off 
clear all  
drop _all  
capture clear matrix 
capture log close   
set maxvar 25000  


*----------------------------------------------------------------------------------------------------*
* IMPORTANT: RUN THIS CODE ONLY AFTER RUNNING STEP_1 + STEP_2 + STEP_3 

cd $saved
use R_soeponly.dta, clear

gen german = 1 if pgnation == 1
replace german = 0 if pgnation != 1 & pgnation > 0 

replace migback = . if migback < 0 
replace migback = 0 if migback == 1
replace migback = 1 if migback > 1

replace pgbilzeit = . if pgbilzeit < 0 

bys pid (syear): egen min_ev = max(event_time) if event_time < 0 
gen event_0 = event_time == min_ev
keep if event_0 == 1

global varlist = "age gender  l11101_ew nr_children sickdays hospital_staysnights satisfaction_health i11110 quitjob_pequiv fulltime_pequiv parttime_pequiv e11101_weekly unemployed_pequiv nonemployed_pequiv er_rente_pequiv i11101 i11102 time_childcare_weekdays time_chores_weekdays time_caring_weekdays time_leisure_weekdays hospital_staysnights " 
				  
qui distinct pid 
sca ndistinct = r(ndistinct) 
qui eststo full: estpost summarize $varlist, d
qui estadd scalar ndistinct

global varlist2 = "age l11101_ew nr_children  sickdays hospital_staysnights satisfaction_health  i11110 quitjob_pequiv fulltime_pequiv parttime_pequiv e11101_weekly unemployed_pequiv nonemployed_pequiv er_rente_pequiv i11101 i11102 time_childcare_weekdays time_chores_weekdays time_caring_weekdays time_leisure_weekdays" 

qui distinct pid if gender == 1
sca ndistinct = r(ndistinct) 
qui eststo treated_w: estpost summarize $varlist2 if gender == 1, d
qui estadd scalar ndistinct

qui distinct pid  if gender == 0
sca ndistinct = r(ndistinct) 
qui eststo treated_m: estpost summarize $varlist2 if gender == 0 , d
qui estadd scalar ndistinct

********************************************************************************
**** SOEP (Partner)
use R_soeponly_partner.dta, clear

gen event_0 = syear == shock - 1
bys pid (syear): ereplace event_0 = max(event_0) 

gen german = 1 if pgnation == 1
replace german = 0 if pgnation != 1 & pgnation > 0 

replace migback = . if migback < 0 
replace migback = 0 if migback == 1
replace migback = 1 if migback > 1

replace pgbilzeit = . if pgbilzeit < 0 

replace sickdays = 0 if sickdays < 0

gen treat = 1 if event_partner == -1 
*keep if treat == 1

qui distinct pid  if  event_0 == 1 & treat == 1
sca ndistinct = r(ndistinct) 
qui eststo partner: estpost summarize $varlist if  event_0 == 1 & treat == 1, d
qui estadd scalar ndistinct

qui distinct pid if gender == 0 & event_0 == 1 & treat == 1
sca ndistinct = r(ndistinct) 
qui eststo partner_m: estpost summarize $varlist2 if gender == 0 & event_0 == 1 & treat == 1, d
qui estadd scalar ndistinct

qui distinct pid if gender == 1 & event_0 == 1 & treat == 1
sca ndistinct = r(ndistinct) 
qui eststo partner_w: estpost summarize $varlist2 if gender == 1 & event_0 == 1 & treat == 1, d
qui estadd scalar ndistinct


********************************************************************************
* Broad population
use R_soeponly_control.dta, clear

keep if shock == 0 

drop if couple == 0

keep if age > 25 & age < 55

gen german = 1 if pgnation == 1
replace german = 0 if pgnation != 1 & pgnation > 0 

replace migback = . if migback < 0 
replace migback = 0 if migback == 1
replace migback = 1 if migback > 1

replace pgbilzeit = . if pgbilzeit < 0 

foreach var of varlist i11110 i11102{
    winsor2 `var', replace cuts(1 99) trim
}

qui distinct pid
sca ndistinct = r(ndistinct) 
qui eststo control: estpost summarize $varlist, d
qui estadd scalar ndistinct

* Visualize table 	
esttab full treated_w treated_m partner partner_w partner_m control,   ///
	cell("mean(pattern(1 1 1 1 1 1 1) fmt(2))" )  /// 
	scalars("ndistinct Unique observations")  noobs ///
	varlabels(age "Age"  gender "Women" l11101_ew "East" sickdays "Days of sick leave" hospital_staysnights "Overnight Hospital Stays" satisfaction_health "Satisfaction with Own Health"  er_rente_pequiv "Disability pension " unemployed_pequiv "Unemployment benefits" nonemployed_pequiv "Unemployment w/o benefits" quitjob_pequiv "In labor force" fulltime_pequiv "Full-time job" parttime_pequiv "Part-time job"  e11101_weekly "Weekly hrs"  time_chores_weekdays "Household Chores (hrs/day)"  time_childcare_weekdays "Childcare (hrs/day)"  i11110 "Gross labor income" i11101 "Household pre-government income" i11102 "Household post-government income" nr_children "Nr. of children" time_leisure_weekdays "Leisure (hrs/day)" time_caring_weekdays "Care (hrs/day)") ///
	mtitle( "Full sample" "Women" "Men" "Full sample" "Women" "Men" "SOEP") nogaps compress /// 
	addnotes("Note: Data refer to year before treatment. Monetary values in 2020 euro.") collabels(,none)
	
	
* Save table 
cd $results
esttab full treated_w treated_m partner partner_w partner_m control using desc_maintable, replace booktabs ///
	cell("mean(pattern(1 1 1 1 1 1) fmt(2))")  /// 
	scalars("ndistinct Unique observations")  noobs ///
	varlabels(age "Age"  gender "Women" l11101_ew "East" sickdays "Days of sick leave"  hospital_staysnights "Overnight hospital stays" satisfaction_health "Satisfaction with own health"   er_rente_pequiv "Disability pension " unemployed_pequiv "Unemployment benefits"  nonemployed_pequiv "Unemployment w/o benefits"  quitjob_pequiv "In labor force"  fulltime_pequiv "Full-time job"  parttime_pequiv "Part-time job" e11101_weekly "Weekly hrs"  time_chores_weekdays "Household chores (hrs/day)"  time_childcare_weekdays "Childcare (hrs/day)"  i11110 "Gross labor income" i11101 "Household pre-government income" i11102 "Household post-government income" nr_children "Nr. of children" time_leisure_weekdays "Leisure (hrs/day)" time_caring_weekdays "Care (hrs/day)") ///
	mtitle( "Full sample" "Women" "Men" "Full sample" "Women" "Men" "SOEP") nogaps compress /// 
	addnotes("Note: Data refer to year before treatment. Monetary values in 2020 euro.")  collabels(,none)
