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


cd $saved
use pre_R_soeponly, clear 

* Define Variable to keep
*---------------------------------------------------------------------------*
global timeuse  "pli0010 pli0036 pli0051 pli0046 pli0057 pli0055 pli0044_h pli0043_h pli0019_h pli0022_h pli0012_h pli0016_h"
global pequiv "e11101 e11102 e11103 e11104 i11101 i11102 i11103 i11107 i11110 h11101 alg2 ioldy iwidy icomp iprvp iunby iunay iguv1 icom1 iprv1 irie1 ison1 m11124 l11101_ew"

*____________________________________________________________________________________________________*
* PART 1: RECODE MISSINGS AND FOWARD VARIABLES THAT REFER TO LAST YEAR 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

* TREAT -2 LIKE ANY OTHER MISSING
*--------------------------------------------------------------------------------------*
quietly foreach var of varlist $timeuse pla0009_v2 plh0171 ple0010_h p_isco08 p_isco88 pgpartnr pgstib pglfs pgjobch plb0302 kal1a02 kal1b02 kal1d02 hlc0005_h ple0053 kal1i01 ple0040 plc0091_h e11104 {
    di "`var'"
    replace `var' = . if `var' == -2
}


* TREAT -2 AS "0"
*--------------------------------------------------------------------------------------*
quietly foreach var of varlist pld0140 pglabgro pglabnet pgtatzeit {
    di "`var'"
    replace `var' = 0 if `var' == -2
}

* RECODE 2 TO 0 IF IT MEANS "NO"
*--------------------------------------------------------------------------------------*

quietly foreach var of varlist ple0053 {
    di "`var'"
    replace `var' = 0 if `var' == 2
}

* PEQUIV
*--------------------------------------------------------------------------------------*
des $pequiv

quietly foreach var of varlist h11101 d11106 d11107 d11108 e11101 e11102 e11103 e11104 e11105_v1 e11105_v2 e11106 e11107 m11124 i11108 i11109 {
    di "`var'"
    replace `var' = . if `var' == -1
}

quietly foreach var of varlist e11106 e11107 i11101 i11103 e11105_v1 e11105_v2 i11107 i11110 i11104 i11106 i11117 ioldy iwidy iunby iunay iguv1 icom1 ison1 m11124 i11102 i11108 i11109 irie1 icomp iprvp iprv1 {
    di "`var'"
    replace `var' = 0 if `var' == -2
}


foreach var in $pequiv {
	di "`var'"
	count if `var'<0
}

* HOSPITAL VISITS
*---------------------------------------------------------------------------*
* Krankenhausaufenthalte - bedingen auf ple0053: Krankenhausaufenthalt J/N

* Krankenhausaufenthalt VORJAHR Anzahl
replace ple0055 = 0 if ple0053 == 0  

* Krankenhausaufenthalt VORJAHR Naechte
replace ple0056 = 0 if ple0053 == 0  

rename ple0053 hospital_binary
rename ple0055 hospital_stays
rename ple0056 hospital_staysnights

replace hospital_stays = 0 if hospital_stays == -2 & hospital_binary != . 
replace hospital_staysnights = 0 if hospital_staysnights == -2 & hospital_binary != . 
replace hospital_staysnights = 0 if hospital_staysnights<0

* FORWARD VARIABLES
*--------------------------------------------------------------------------------------*
foreach var of varlist hospital* plb0302 pgjobch pld0152 kal1d01 kal1i01 {
	local labelname : variable label `var'
	di "`labelname'"    
    rename `var' `var'_lastyear
    bys pid (syear): gen `var' = `var'_lastyear[_n+1]
	label variable `var' "`labelname'" 

}

* Forward treatment variables
bys pid (syear): gen nosickdays = ple0044_h[_n+1] 
bys pid (syear): gen sickdays = ple0046[_n+1] 
bys pid (syear): gen sick6weeks = plb0024_h[_n+1] 

bys pid (syear): gen nosickdays_v2 = ple0044_h[_n+1] if (syear +1) == syear[_n+1]
bys pid (syear): gen sickdays_v2 = ple0046[_n+1] if (syear +1) == syear[_n+1]
bys pid (syear): gen sick6weeks_v2 = plb0024_h[_n+1] if (syear +1) == syear[_n+1]

* Identify and remove gaps in survey - only those where we'd mistakenly assign treatment to wrong year. Leave "-2" entries
gen gaps_in_survey  = sick6weeks != sick6weeks_v2 & sick6weeks == 1

* ... tolerate gaps of two years
cap drop gap_length
bys pid (syear): gen gap_length = abs((syear[_n]-syear[_n+1]))>2 if gaps_in_survey == 1

* ... do not tolerate any gaps just before the shock
bys hid (syear): gegen hh_gaps_in_survey = max(gaps_in_survey)
drop if hh_gaps_in_survey == 1
drop hh_gaps_in_survey


* 29-Aug-2023 || 12:37:10
foreach var of varlist nosickdays sickdays sick6weeks {
	replace `var' = `var'_v2
	drop `var'_v2
}

drop ple0044_h ple0046 plb0024_h gap_length /*gaps_in_survey*/


*____________________________________________________________________________________________________*
* PART 2: GENERATE TREATMENT VARIABLE	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
* Clean sick6weeks
replace sick6weeks = 0 if sick6weeks == -2 

* Clean sickdays
replace sickdays = 0 if nosickdays == 1 
replace sickdays = . if sickdays == 450
replace sickdays = . if syear == 2010 & sickdays == -2
replace sickdays = 0 if sickdays == -2

* COMPLEMENT SICK6WEEKS IF SICKDAYS IS INFORMATIVE
*---------------------------------------------------------------------------*

* Flag observations if they have less than 30 sickdays and report more than six weeks sick leave
replace sick6weeks = 99 if sick6weeks == 1 & sickdays<30


* NOTE: This will correct cases (a): 54 sickdays in 2006, 29 in 2007 --> 2007 will be corrected but will not be consequential and 
* (b) where we may have 27 sickdays in 2010 and 22 in 2011 AND sick6weeks == 1 in 2011 etc. --> assume that sickness crosses the year.

* sick6weeks may stretch over two years - in that cases take into account sickdays in PREVIOUS year too
bys pid (syear): replace sick6weeks = 77 if sick6weeks == 99 & (sickdays+sickdays[_n-1]>=30 | sickdays+sickdays[_n-1]>=28 & sickdays == -2) & !mi(sickdays[_n-1])

* ... then shock actually started a year earlier
bys pid (syear): replace sick6weeks = 1 if sick6weeks != 1 & sick6weeks[_n+1] == 77 & pid[_n] == pid[_n+1]

* Also allow sick6weeks to be valid if sickdays has not been answered this year but there are sickdays in the previous year
bys pid (syear): replace sick6weeks = 55 if sick6weeks == 99 & (sickdays[_n-1]>0 & !mi(sickdays[_n-1])) & sickdays == -2 & !mi(sickdays[_n-1])

* ... then shock actually started a year earlier
bys pid (syear): replace sick6weeks = 1 if sick6weeks != 1 & sick6weeks[_n+1] == 55 & pid[_n] == pid[_n+1]


* KEEP FIRST SHOCK OR SHOCKS IN CONSECUTIVE YEARS (SAME SHOCK BUT OVER 2 SURVEY YEARS)	 	 	 	 	 	 	 	 	 	   
*--------------------------------------------------------------------------------------*        
* Mark first occurrence of sick6weeks == 1
gen AUF_30 = sick6weeks == 1
gsort pid syear
egen first_AUF_30= tag(pid) if sick6weeks== 1

* Mark last occurence of sick6weeks == 1
gen max_first_auf = syear if AUF_30 == 1
bys pid (syear): ereplace max_first_auf = max(max_first_auf)

bysort pid (syear): egen min_year = min(syear)

* Generate a running variable (event_time)
gen first_year = syear if first_AUF_30 == 1
bys pid (syear): ereplace first_year = max(first_year)
gen event_time = syear-first_year 

* Count # of times with sick6weeks == 1
bys pid (syear) : gegen nr_shocks = total(AUF_30)

* DEFINE NOT-YET-TREATED CONTROL GROUP
replace first_year = 0 if mi(first_year)
rename first_year shock /* TREATMENT VARIBLE DEFINED */

* Recode first_AUF_30
replace first_AUF_30 = 1 if syear >=shock  


* REMOVE THOSE WITH MULTIPLE SHOCKS - EXCEPTION: SAME SHOCK CROSSING INTO NEXT YEAR
*---------------------------------------------------------------------------*
gen crossingyears = max_first_auf if nr_shocks == 2 
replace crossingyears = 0 if mi(crossingyears)
drop max_first_auf

gen gap_shocks = crossingyears - shock if crossingyears != 0 
replace gap_shocks = 0 if mi(gap_shocks)
la var gap_shocks "How many years between 1st and 2nd shock"
keep if nr_shocks <= 1 | gap_shocks == 1 // Hier fallen die 325 raus partner ruas. 

* Drop always-treated
drop if shock == min_year 

* Count unique pids
unique pid if shock != 0 & !mi(shock) //  6,566

drop crossingyears gap_shocks nr_shocks nosickdays sick6weeks min_year


*____________________________________________________________________________________________________*
* PART 3: GENERATE NEW VARIABLES	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

* GENDER
*--------------------------------------------------------------------------------------*
* Generate gender variable - assume gender does not change
* 1 = Maennlich, 2 = Weiblich
cap drop gender*
bys pid (syear): gegen gender = max(pla0009_v2)   
replace gender = gender - 1   
drop pla0009_v2 


* SELF-EMPLOYED AND CIVIL SERVANTS
*--------------------------------------------------------------------------------------*
rename plb0057_h1 self_employed
replace self_employed = 0 if self_employed < 0
replace self_employed = 1 if self_employed > 0 & self_employed != 4
replace self_employed = 2 if self_employed == 4 /* Mithelfende Familienangehoerige */

rename plb0065 civil_servant 
replace civil_servant = 0 if civil_servant < 0
replace civil_servant = 1 if civil_servant > 0 

gen self_employed_flag = 1 if self_employed == 1 & syear == shock
bys pid (syear): ereplace self_employed_flag = max(self_employed_flag)

gen civil_servant_flag = 1 if civil_servant == 1 & syear == shock
bys pid (syear): ereplace civil_servant_flag = max(civil_servant_flag)


* BLUE-COLLAR
*--------------------------------------------------------------------------------------*
* Define blue-collar variable
gen bluecollar = 1 if ((p_isco88 >= 5000 & !mi(p_isco88)) | (p_isco08 >= 5000 & !mi(p_isco08))) 
replace bluecollar = 0 if (p_isco88 >= 0 &  p_isco88 < 5000) | (p_isco08 >= 0 &  p_isco08 < 5000)

drop p_isco88 p_isco08 e11105_v1 e11105_v2


* EMPLOYMENT STATUS
*--------------------------------------------------------------------------------------*
* Unemployment 
rename plb0021 unemployed
replace unemployed = 0 if unemployed == 2

* Occupational change comapred to last year
gen occupchange = 1 if pgjobch == 4 |pgjobch == 5 
replace occupchange = 0 if pgjobch == 1 | pgjobch == 2 | pgjobch == 3 


* EAST
*---------------------------------------------------------------------------*
replace l11101_ew = l11101_ew - 21

* AGE
*---------------------------------------------------------------------------*
bys pid (syear): ereplace ple0010_h = max(ple0010_h)
drop if mi(ple0010_h)
gen age = syear-ple0010_h
gen age_sq = age*age


* (NUMBER OF) CHILDREN & ADULTS
*---------------------------------------------------------------------------*
rename d11107 nr_children 

gen adults = hhgr-nr_children
replace adults = 1 if adults == 0


* 2001 REFORM
*---------------------------------------------------------------------------*
gen reform = 0 if shock < 2001
replace reform = 0 if ple0010_h < 1961 & shock >= 2001
replace reform = 1 if shock >= 2001 &  ple0010_h >= 1961


* CONDITION ON EXISTANCE OF PERIOD -1 (-2)
*---------------------------------------------------------------------------*
* Check if -1 exists
bys pid (syear): gegen last_pretreatment_period = max(event) if event <0

* Check if +1 or +2 exist 
bys pid (syear): gegen first_postreatment_period = min(event) if event >0

* Some have -3, -4, -17 --> Drop
bys pid (syear): gegen flag = max(last_pretreatment_period)
drop if flag <=-3
drop flag 

* Some have 3, 4, 5... --> Drop
bys pid (syear): gegen flag = max(first_postreatment_period)
drop if flag >=3 & !mi(flag)
drop flag 


* COUPLE STATUS AND PARTNERSHIP
*---------------------------------------------------------------------------*
* DEFINE NEW PARTNER VARIABLE BASED ON --HGTYP2HH--
gen couple = 1 if hgtyp2hh > 40 & hgtyp2hh < 70 & (event == -1 | (event == -2 & last_pretreatment_period == -2))
replace couple = 1 if hgtyp2hh == 21 & (event == -1 | (event == -2 & last_pretreatment_period == -2))
replace couple = 0 if hgtyp2hh != 21 &  hgtyp2hh < 40 & (event == -1 | (event == -2 & last_pretreatment_period == -2))

* Remove HHs that cannot be classified into couple or single
replace couple = . if (event == -1 | (event == -2 & last_pretreatment_period == -2)) &  inlist(hgtyp2hh, 71, 72, 73, 81)

* Set to missing when hgtyp2hh and pgpartz are conflictual
replace couple = .  if couple == 1 & !inlist(pgpartz, 1, 2, 3, 4)
replace couple = .  if couple == 0 &  pgpartz != 0   

* Ensure that singles are living alone and are not children of the main person 
tab stell_h if couple == 0 
tab stell_h if couple == 0 & age > 25
replace couple = . if couple == 0 & stell_h != 0

*Ensure that partnered are not children, siblings etc
tab stell_h if couple == 1
tab stell_h if couple == 1 & age > 25

replace couple = . if couple == 1  & stell_h > 13 

bys pid (syear): ereplace couple = max(couple)


* define variable for partnership status in every period
gen couple_cont = 1 if hgtyp2hh > 40 & hgtyp2hh < 70 
replace couple_cont = 1 if hgtyp2hh == 21 
replace couple_cont = 0 if hgtyp2hh != 21 &  hgtyp2hh < 40

* Remove HHs that cannot be classified into couple or single
replace couple_cont = . if inlist(hgtyp2hh, 71, 72, 73, 81)

* Set to missing when hgtyp2hh and pgpartz are conflictual
replace couple_cont = .  if couple_cont == 1 & !inlist(pgpartz, 1, 2, 3, 4)
replace couple_cont = .  if couple_cont == 0 &  pgpartz != 0   

* Ensure that singles are living alone and are not children of the main person 
replace couple_cont = . if couple_cont == 0 & stell_h != 0

*Ensure that partnered are not children, siblings etc
tab stell_h if couple_cont == 1
tab stell_h if couple_cont == 1 & age > 25

replace couple_cont = . if couple_cont == 1  & stell_h > 13 

* TIME-USE VARIABLES
*--------------------------------------------------------------------------------------*

* - HOBBIES/LEISURE
*  When it is not a weekday & weekend-entries are valid, replace weekday by zero and vice versa
replace pli0051 = 0 if pli0051 == -2 & (pli0010 > 0 | pli0036 > 0)
replace pli0010 = 0 if pli0010 == -2 & pli0051>0
replace pli0036 = 0 if pli0036 == -2 & pli0051>0

* Generate 7-day variable
gen time_leisure = pli0051 + pli0010 + pli0036

* Keep weekday variable
rename pli0051 time_leisure_weekdays
replace time_leisure_weekdays = 0 if time_leisure_weekdays<0

drop pli0010 pli0036

* - CARING FOR PERSON
* When it is not a weekday & weekend-entries are valid, replace weekday by zero and vice versa
replace pli0046 = 0 if pli0046 == -2 & (pli0057 > 0 | pli0055 > 0)
replace pli0057 = 0 if pli0057 == -2 & pli0046>0
replace pli0055 = 0 if pli0055 == -2 & pli0046>0

* Replace remaining negative values with missing values
replace pli0046 = . if pli0046 < 0
replace pli0057 = . if pli0057 < 0
replace pli0055 = . if pli0055 < 0

* Generate 7-day variable
gen time_caring = pli0046 + pli0057 + pli0055

* Keep weekday variable
rename pli0046 time_caring_weekdays

drop pli0057 pli0055

* - HOUSEHOLD CHORES
* When it is not a weekday & weekend-entries are valid, replace weekday by zero and vice versa
replace pli0043_h = 0 if pli0043_h == -2 & (pli0012_h > 0 | pli0016_h > 0)
replace pli0012_h = 0 if pli0012_h == -2 & pli0043_h>0
replace pli0016_h = 0 if pli0016_h == -2 & pli0043_h>0

* Replace remaining negative values with missing values
replace pli0043_h = . if pli0043_h < 0
replace pli0012_h = . if pli0012_h < 0
replace pli0016_h = . if pli0016_h < 0

* Generate 7-day variable
gen time_chores = pli0043_h + pli0012_h + pli0016_h

* Keep weekday variable
rename pli0043_h time_chores_weekdays

drop pli0012_h pli0016_h

* - CHILDCARE 
* When it is not a weekday & weekend-entries are valid, replace weekday by zero and vice versa
replace pli0044_h = 0 if pli0044_h == -2 & (pli0019_h > 0 | pli0022_h > 0)
replace pli0019_h = 0 if pli0019_h == -2 & pli0044_h>0
replace pli0022_h = 0 if pli0022_h == -2 & pli0044_h>0

* Replace remaining negative values with missing values
replace pli0044_h = 0 if pli0044_h < 0
replace pli0019_h = 0 if pli0019_h < 0
replace pli0022_h = 0 if pli0022_h < 0

* Generate 7-day variable
gen time_childcare = pli0044_h + pli0019_h + pli0022_h
replace time_childcare = . if time_childcare>72

* Keep weekday variable
rename pli0044_h time_childcare_weekdays

drop pli0019_h pli0022_h

* - FULL TIME HOUSEWORK
replace kal1i01 = 0 if kal1i01 == 2
rename kal1i01 housework_fulltime

*____________________________________________________________________________________________________*
* PART 4: GENERATE VARIABLES FROM PEQUIV DATA 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

* APPLY CONVERSION RATE TO ALL INCOME VARIABLES
foreach var of varlist i11101 i11102 i11103 i11110 i11107 i11108 i11109 i11104 i11106 i11117 iunby iunay alg2 {
    replace `var' = `var'*conversion_rate
}

* Employment
gen quitjob_pequiv = 3.e11103 & !mi(e11103)
recode quitjob_pequiv (1=0) (0=1)

* Unemployment 
gen ialg = iunay + iunby
replace kal1d01 = 0 if kal1d01 == 2
replace ialg = ialg + alg2 if (!mi(alg2) & kal1d01 == 1)

gen unemployed_pequiv = ialg>0 & !mi(ialg)
replace unemployed_pequiv = . if ialg == .

* HH-Level Unemployment Benefits (alg2 only on HH-level, iunby & iunay on individual-level)
replace alg2 = iunay if syear<=2004
gen hhialg =  alg2 + iunby
gen hhialg_ss = hhialg+i11108

* Retirement 
gen retired_pequiv = (icom1 >0 & !mi(icom1) | icomp >0 & !mi(icomp) | ioldy >0 & !mi(ioldy) ///
| iprv1 >0 & !mi(iprv1) | iprvp >0 & !mi(iprvp) | irie1 >0 & !mi(irie1) | ison1 >0 & !mi(ison1) ///
| iwidy >0 & !mi(iwidy))

* Disability benefits
gen handicap_EWB_pequiv =  1.m11124
replace handicap_EWB = 0 if handicap_EWB == 2

gen er_rente_pequiv = 1 if retired_pequiv == 1 & handicap_EWB_pequiv == 1
replace er_rente_pequiv = 0 if !mi(retired_pequiv) & retired_pequiv != 1

* Unemployed without benefits 
gen nonemployed_pequiv = quitjob_pequiv
recode nonemployed_pequiv (1=0) (0=1)

replace nonemployed_pequiv = 0 if retired_pequiv == 1
replace nonemployed_pequiv = 0 if unemployed_pequiv == 1

* Full-time and part-time occupaton
gen parttime_pequiv = 2.e11103
gen fulltime_pequiv = 1.e11103

* OECD weights and weighted HH-income
gen oecd_weight = (1.0+0.7*(d11106-h11101-1)+0.5*h11101) 

gen i11101_adj = i11101/oecd_weight
gen i11102_adj = i11102/oecd_weight
drop oecd_weight

label variable i11101_adj "Equivalent HH Pre-Government Income"
label variable i11102_adj "Equivalent HH Post-Government Income"

* Weekly working hours
gen e11101_weekly = e11101/52.1429
label variable e11101_weekly "Weekly Working Hours"

*____________________________________________________________________________________________________*
* PART 5: ROBUSTNESS CHECK: FRESH MOTHERS 	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
rename ple0010_h year_birth
gen tag = shock != 0 & ((shock-year_birth)<=55) & ((shock-year_birth)>=25)  & gender == 1

* Replace missings as "NO"
rename pld0152 child_born
replace child_born = 0 if child_born == -2 & tag == 1
replace pld0152_lastyear = 0 if pld0152_lastyear == -2 & tag == 1

* Year of Birth
gen syear_child_born = syear if child_born == 1 & tag == 1
replace syear_child_born = 0 if child_born == 0 & tag == 1

* Year of Shock
gen syear_shock = syear == shock if tag == 1

* A. Correlation between child birth and shock
corr child_born syear_shock if tag == 1 // 0.0975
corr child_born syear_shock if age<=40 & tag == 1 // 0.1229, considering potential child-bearing years

* B. Correlation between child birth and shock if birth followed shock
bys pid (syear): gen child_borntminus1 = child_born[_n+1] if tag == 1

corr child_borntminus1 syear_shock if tag == 1 // 0.0397
corr child_borntminus1 syear_shock if age<=40 & tag == 1 // 0.0532 , considering potential child-bearing years

* C. Correlations between child birth and shock if shock followed birth (unlikely)
bys pid (syear): gen child_borntplus1 = child_born[_n-1] if tag == 1

corr child_borntplus1 syear_shock // -0.0408  
corr child_borntplus1 syear_shock if age<=40 & tag == 1 // -0.0645 , considering potential child-bearing years

* Child and Shock Variables refer to the same year. 

* A. Birth and shock in the same year
gen BinyearAUF = syear_child_born == shock if tag == 1
unique pid if BinyearAUF == 1 // 150 mothers

* B. Birth after the year of Shock  
gen BafterAUF = (syear_child_born +1) == shock if tag == 1
unique pid if BafterAUF == 1 // 18 mothers

* C. Birth before shock (unlikely)
gen BbeforeAUF = (syear_child_born -1) == shock if tag == 1
unique pid if BbeforeAUF == 1 // 93 mothers

* No of unique women in the sample
unique pid if tag == 1 // 2,184

gen dropmothers = .

foreach var of varlist BinyearAUF BafterAUF /*BbeforeAUF*/ {
	replace dropmothers = 1 if `var' == 1 & tag == 1
}

bys pid (syear): ereplace dropmothers = max(dropmothers) if tag == 1
unique pid if dropmothers == 1 & tag == 1 // 185 unique mothers 

rename dropmothers recent_births

* Age youngest child
gen age_youngest_child = syear-k_birthy_h

* Age oldest child
gen age_oldest_child = syear-k_birthy_h_oldest

gen ChildAgeAtShock = age_youngest_child <=10 & event_time == 0 & tag == 1
bys pid (syear): ereplace ChildAgeAtShock = max(ChildAgeAtShock) if tag == 1

drop BinyearAUF BbeforeAUF BafterAUF tag

*____________________________________________________________________________________________________*
* PART 6: SAMPLE SELECTION	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

* Drop Variables no longer needed, rename others
*--------------------------------------------------------------------------------------*
drop cid
drop *AUF*

rename pld0140 divorce
rename ple0008 health_now
rename plh0035 worry_ownhealth
rename plh0171 satisfaction_health

* Change ordering of values for worry_ownhealth and satisfaction_health
recode worry_ownhealth (1 = 3) (2 = 2) (3 = 1)

recode health_now (1 = 5) (2 = 4) (3 = 3) (4 = 2) (5 = 1) 

* Standardize values for worry_ownhealth, health_now and satisfaction_health
bys syear: ereplace worry_ownhealth = std(worry_ownhealth)
bys syear: ereplace satisfaction_health = std(satisfaction_health)
bys syear: ereplace health_now = std(health_now)

gsort pid syear

* SAMPLE SELECTION
*--------------------------------------------------------------------------------------*

* Control group
preserve

	keep if shock == 0 
	drop couple 

	gen worked = quitjob_pequiv == 1
	bys pid (syear): ereplace worked = max(worked)
	keep if worked == 1
	
	bys pid (syear): gen two_periods_employment = quitjob_pequiv + quitjob_pequiv[_n-1]
	bys pid (syear): ereplace two_periods_employment = max(two_periods_employment)
	replace two_periods_employment = 0 if two_periods_employment < 2
	replace two_periods_employment = 1 if two_periods_employment > 1
		
	cap drop flag
	gen flag = inlist(pglfs, 3, 5, 8, 9, 10, 12, 13)
	bys pid (syear): ereplace flag = max(flag)
	drop if flag == 1
	
	* Self-employed 
	drop self_employed_flag
	gen self_employed_flag = 1 if self_employed == 1
	bys pid (syear): ereplace self_employed_flag = max(self_employed_flag)

	bys pid (syear): egen couple_max = max(couple_cont)

	keep if couple_max == 1 
	drop couple_max
	
	* Trim variables
	winsor2 i11102 i11102_adj i11101 i11101_adj i11110 i11103 i11107 i11108 hhialg_ss ialg hhialg i11104 i11106 i11117, replace cuts(1 99) trim

	cd $saved 
	save control_group, replace

restore 

* Age filter
gen tag_age = 1 if event == 0 & age < 25
bys pid (syear): ereplace tag_age = max(tag_age)
drop if tag_age == 1
drop tag_age

gen tag_age = 1 if event == 0 & age > 55 & !mi(age)
bys pid (syear): ereplace tag_age = max(tag_age)
drop if tag_age == 1
drop tag_age

* Remove value labels
foreach var of varlist * {
    label values `var' 
    format `var' %9.0g
}


/* Flag people:
 - in education 
 - in military service
 - not working but occasional secondary job
 - not wortin but paid work in past 7 days 
 - not wokring but regular secondary job 
*/

cap drop flag
gen flag = inlist(pglfs, 3, 5, 8, 9, 10, 12, 13) & event_time>=-5 & event_time<0
bys pid (syear): ereplace flag = max(flag)
drop if flag == 1

* Self-employed 
drop if self_employed_flag == 1

keep pid syear shock couple i11102 i11102_adj i11110 i11108 ialg hhialg i11104 i11106 i11117 i11101 i11101_adj i11103 i11107 hhialg_ss e11101 e11101_weekly unemployed_pequiv retired_pequiv quitjob_pequiv handicap_EWB_pequiv er_rente_pequiv nonemployed_pequiv parttime_pequiv fulltime_pequiv time_leisure_weekdays time_caring_weekdays time_chores_weekdays time_childcare_weekdays housework_fulltime satisfaction_health worry_ownhealth hospital_staysnights health_now m11124 child_born flag age age_sq gender year_birth pgpartnr reform event_time sickdays sampreg nr_children pgvebzeit pgtatzeit pglfs pgnation migback pgbilzeit recent_births divorce self_employed self_employed_flag civil_servant civil_servant_flag couple_cont d11108 hhgr l11101_ew mcs pcs gs_avg ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0187 ple0023 ple0024


* SAVE TEMPFILE 
*--------------------------------------------------------------------------------------*

cd $saved 
tempfile main
save `main'
*____________________________________________________________________________________________________*
* PART 7: GENERATE PARTNER DATASET	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

preserve 

    * Reconstruct couples
    rename pid pid0
    keep pid0 syear shock

    merge 1:1 pid0 syear using pers_merged_junk3
    rename pid1 parnter_pid
    keep if (_merge == 1 | _merge == 3)
    drop _merge nr1 index mark 

    rename couple_id0 couple_id
    rename pid0 pid1 
    merge 1:1 pid1 syear using pers_merged_junk3
	drop index nr1 mark

    replace parnter_pid = pid0 if parnter_pid == . & pid0 != . 
    replace couple_id = couple_id0 if couple_id == . & couple_id0 != . 

    keep if (_merge == 1 | _merge == 3)
    drop pid0 couple_id0 _merge

    rename pid1 pid_sick
    rename parnter_pid  pid 
    rename shock shock_partner /* This is the sick person's shock, not the healthy partner's one, in case they get one too */

    keep if pid != . 
    merge 1:1 pid syear using `main', keep(3) nogen 
    order pid pid_sick couple_id syear 
    gen event_partner = syear - shock_partner

    *Drop repeated treatments WITHIN A COUPLE	
    sort pid syear
    by pid couple_id shock_partner, sort: gen nvals = _n == 1 
    bys pid couple_id: replace nvals = sum(nvals)
    by pid couple_id: replace nvals = nvals[_N] 
    drop if nvals > 1 
	drop nvals

	*Mark repeated treatments FOR A PERSON
	sort pid syear
	bys pid pid_sick, sort: gen nvals = _n == 1 if shock_partner > 0 
	sort pid syear
	bys  pid : replace nvals = sum(nvals)
	bys pid : replace nvals = nvals[_N] 
	bys pid: egen flag_multiple = max(shock_partner) if nvals > 1
	replace flag_multiple = 1 if shock_partner == flag_multiple
	replace flag_multiple = 0 if flag_multiple > 1 & !mi(flag_multiple)
	drop nvals

    * Mark if spousal shock happend after partner's shock
    gen twoshocks = shock != 0 & shock_partner != 0 & shock >= shock_partner

    * Generate a twoshocks variable for the partner data set that removes HH if there are two shocks no matter how far apart
    bys couple_id (syear): gegen twoshocks_partner = max(twoshocks)
	
	bys pid (syear): ereplace twoshocks_partner = max(twoshocks_partner)
	bys pid (syear): ereplace flag_multiple = max(flag_multiple)

    gsort pid syear
	
	* Select sample
	keep if shock_partner != 0
	keep if shock_partner - year_birth <= 55
	keep if shock_partner - year_birth >= 25
	
	unique pid 
	
	* Get oberservations from periods outside partnership
	keep pid syear twoshocks_partner flag_multiple shock_partner couple_id
	merge 1:1 pid syear using `main', keep(2 3) gen(flag_merge) 
	bys pid (syear): egen max_flag = max(flag_merge)
	keep if max_flag == 3
	bys pid (syear): egen shock_partner_max  = max(shock_partner) 
	replace shock_partner = shock_partner_max if syear > shock_partner_max
	drop shock_partner_max
	drop if shock_partner == . 
	gen event_partner = syear - shock_partner
	bys pid (syear): ereplace shock_partner = max(shock_partner)
	bys pid (syear): ereplace flag_multiple = max(flag_multiple)
	bys pid (syear): ereplace twoshocks_partner = max(twoshocks_partner)
	bys pid (syear): ereplace couple_id = max(couple_id)
	drop max_flag
	
	* Save 
    cd $saved
    save R_soeponly_partner, replace

restore 


*____________________________________________________________________________________________________*
* PART 8: EXPORT VARIABLES FROM PARTNER DATASET TO MAIN DATASET AND SELECT FINAL SAMPLE 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

use R_soeponly_partner, clear

keep pid syear couple_id flag_multiple twoshocks_partner twoshocks

merge 1:1 pid syear using `main', gen(match_couple) 

gsort pid syear

* Removes HH if there are two shocks no matter how far apart for the stable_partner data set
bys pid (syear): ereplace couple_id = max(couple_id)
bys pid (syear): ereplace twoshocks_partner = max(twoshocks_partner)
bys pid (syear): ereplace flag_multiple = max(flag_multiple)

* Identify partners that are in stable partnerships
gen stable_partner = match_couple == 3 & twoshocks_partner != 1
gen stable_partner2 = match_couple == 3 

* Save one copy of full dataset for descriptive statistics 
save R_soeponly_full, replace

* Keep people in couples
keep if couple == 1

* Keep people in households where the partner does not have a shock him/herself
drop if twoshocks_partner == 1
count

* The next filters shouldn't change anything, but better to keep them
keep if shock - year_birth <= 55 & shock - year_birth >= 25
count
keep if shock != 0 
count
drop if flag == 1 /* irregular unemployment */
count
drop if self_employed_flag == 1
count 
drop if flag_multiple == 1
count

* Winsorize variables
winsor2 i11102 i11102_adj i11101 i11101_adj i11110 i11103 i11107 i11108 hhialg_ss ialg hhialg i11104 i11106 i11117, replace cuts(1 99) trim


* SAVE DATA
cd $saved
save R_soeponly, replace


* Select final sample for partners 
cd $saved
use R_soeponly_partner, clear 

drop if twoshocks_partner == 1
count

drop if flag_multiple == 1
count

* The next filters shouldn't change anything, but better to keep them
keep if shock_partner - year_birth <= 55 & shock_partner - year_birth >= 25
count 

keep if shock_partner != 0
count

* Rename shock variables
rename shock shock_spouse
rename shock_partner shock 

* Winsorize variables
winsor2 i11102 i11102_adj i11101 i11101_adj i11110 i11103 i11107 i11108 hhialg_ss ialg hhialg i11104 i11106 i11117, replace cuts(1 99) trim
 
* SAVE DATA
cd $saved
save R_soeponly_partner, replace


*----------------------------------------------------------------------------------------------------*

* APPEND CONTROL GROUP TO MAIN DATA
cd $saved

use R_soeponly, clear

append using control_group

replace two_periods_employment = 1 if shock != 0 

save R_soeponly_control, replace


