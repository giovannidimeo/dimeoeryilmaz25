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


cd $soep

*____________________________________________________________________________________________________*
* DEFINE VARIABLES TO SELECT	 	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
global household "hid cid syear hlc0005_h hlc0002 hlc0059_h hlc0106 hlf0001_h hlc0065 hlc0039_h hlc0042_h" 

global individual "pid hid syear pnr ple0010_h pla0009_v2 plk0001_v1 plk0001_v2 plk0001_v3 plj0014_v3 plb0022_h plb0021 plb0024_h plb0031_h plb0036_h plb0037_h plb0176_h plb0186_h  plb0187_h plb0284_h plb0474_h plc0137_h plb0477_h plc0013_h plc0014_h plc0075_h plc0274_h plc0017_h plc0243_h plc0220_h plc0236_h plc0238_h plc0240_h plc0249_h plc0268_h plc0279_h pld0133 ple0008 ple0044_h ple0097 ple0099_h ple0040 ple0041 plc0046 ple0191 ple0191 ple0046 pld0140 pld0142 pli0040 pli0010 pli0036 pli0051 pli0046 pli0057 pli0055 pli0044_h pli0019_h pli0022_h pli0043_h pli0012_h pli0016_h plh0171 plh0035 ple0056 plh0175 p_isco08 p_isco88 plb0719 pld0152 plc0091_h plb0302 ple0055 ple0053 plb0057_h1 plb0065 ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0187 ple0023 ple0024"

global individual_gen "pid hid syear pgpartz pgpartnr pglabgro pglabnet pgsndjob pgemplst pgstib pglfs pgtatzeit pgvebzeit pgbilzeit pgjobch pgnation"

global ppathl "hid pid cid syear sex gebjahr rv_id sampreg migback"

*____________________________________________________________________________________________________*
* LOAD DATA FROM HOUSEHOLD DATABASE	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
use hl.dta, clear
keep $household

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}


cd $saved
compress
save hl_trimmed, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* LOAD DATA FROM INDIVIDUAL DATABASE	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $soep
use pl.dta, clear
keep $individual

* Replace split variables by unique variable

gen pnr_partner = plk0001_v1
replace pnr_partner = plk0001_v2 if pnr_partner < 0 & plk0001_v1 > 0 
replace pnr_partner = plk0001_v3 if pnr_partner < 0 & plk0001_v3 > 0 
drop plk0001_v1 plk0001_v2 plk0001_v3

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}

cd $saved
compress
save pl_trimmed, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* LOAD DATA FROM PKAL DATABASE	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $soep
use pkal.dta, clear
keep pid syear kal1i01 kal1d01

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}

cd $saved
compress
save pkal_trimmed, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* LOAD DATA FROM GENERATED INDIVIDUAL DATABASE	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $soep
use pgen.dta, clear
keep $individual_gen

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}

cd $saved
compress
save pgen_trimmed, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* MERGE INDIVIDUAL-LEVEL DATA (pgen_trimmed AND pl_trimmed) 	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $saved
use pgen_trimmed.dta, clear
merge 1:1 pid hid syear using "pl_trimmed.dta", keep(3) nogen // 45 using-only

rename 		pgpartz				partner_indicator
la var 		partner_indicator	"Partnership indicator, i.e. does the person has partner within HH (pgpartz)"

rename 		pgpartnr			partner_pid
la var 		partner_pid			"Partner's pid (pgpartnr)"

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}


* REDUCE VAR WIDTH
quietly foreach var of varlist _all {
	cap format `var' %10.0g
	continue

	else {
		cap format `var' %10s
		continue
	}	

} 


gsort pid syear
replace partner_pid = 0 if mi(partner_pid)
gsort hid pid syear

cd $saved
compress
save pers_merged.dta, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* PPATHL	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

* GET DATA ON YEARS OF FIRST/LAST INTERVIEWS AND ENTRY/EXIT
cd $soep
use ppathl, clear

keep $ppathl

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}


cd $saved
compress
save Jppathl, replace
*----------------------------------------------------------------------------------------------------* 
 
*____________________________________________________________________________________________________*
* KIDS LONG	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

cd $soep
use kidlong, clear

keep hid syear k_nrkid k_birthy_h
replace k_nrkid = . if k_nrkid<0

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}

drop if mi(k_birthy_h)

bys hid syear: gegen k_birthy_h_oldest = min(k_birthy_h)
bys hid syear: ereplace k_birthy_h = max(k_birthy_h)


duplicates drop hid syear k_nrkid, force
duplicates drop hid syear, force

cd $saved
compress
save kids_size, replace
*----------------------------------------------------------------------------------------------------* 

* GRIPSTR	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $soep
use gripstr, clear

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -2, -1)
}

egen gs_avg = rowmean(gs03 gs04 gs05 gs06)
la var gs_avg "Average grip strength"

keep pid syear gs_avg

keep if gs_avg != . 

cd $saved
compress
save gripstr, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* HH BRUTTO	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $soep
use hbrutto, clear

keep hid syear hhgr
drop if hhgr == 0

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}

cd $saved
compress
save hh_size, replace
*----------------------------------------------------------------------------------------------------*

*____________________________________________________________________________________________________*
* HEALTH	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*
cd $soep
use health, clear

keep pid syear mcs pcs

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}

cd $saved
compress
save health, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* MIHINC	 	 	 	 	 	 	 	 	 	 	 
*----------------------------------------------------------------------------------------------------*
* Imputed income								   
cd $soep
use mihinc, clear

drop if  ihinc < 1
drop cid impflag mi mj
collapse (mean) ihinc, by(hid syear)

cd $saved
compress
save mihinc_collapsed, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* PEQUIV	 	 	 	 	 	 	 	 	 	 	 
*----------------------------------------------------------------------------------------------------*
cd $soep
use pequiv2021, clear

global pequiv "alg2 d11106 d11107 d11108 e11101 e11102 e11103 e11104 e11105_v1 e11105_v2 e11106 e11107 h11101 i11101 i11102 i11103 i11104 i11106 i11107 i11108 i11109 i11110 i11117 h11101 icom1 icomp iguv1 ioldy iprv1 iprvp irie1 ison1 iunay iunby iwidy m11124 l11101_ew"

keep pid syear $pequiv

foreach var of varlist e11101 e11102 e11103 i11101 i11102 i11103 i11107 i11108 i11109 i11110 alg2 ioldy iwidy icomp iprvp iunby iunay iguv1 icom1 iprv1 irie1 ison1   {
	local labelname : variable label `var'
	di "`labelname'"
    rename `var' `var'_lastyear
    bys pid (syear): gen `var' = `var'_lastyear[_n+1]
	label variable `var' "`labelname'" 
}

drop *_lastyear
drop if syear == 2021

cd $saved
compress
save pequiv2021_junk, replace
*----------------------------------------------------------------------------------------------------*


*____________________________________________________________________________________________________*
* MERGE ALL REQUIRED DATA SETS	 	 	 	 	 	 	 	 	 	 	 	   
*----------------------------------------------------------------------------------------------------*

* Define Variable to keep
*---------------------------------------------------------------------------*

global timeuse  "pli0010 pli0036 pli0051 pli0046 pli0057 pli0055 pli0044_h pli0043_h pli0019_h pli0022_h pli0012_h pli0016_h"
global individual2 "pid hid syear pla0009_v2 ple0010_h plb0022_h plb0024_h ple0046 pld0140 plb0021 ple0008 ple0040 plh0175 ple0044_h p_isco08 p_isco88 pld0152 plc0091_h plb0302 ple0055 ple0053 plb0057_h1 plb0065 ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0187 ple0023 ple0024"
global health "plh0171 plh0035 ple0056"
*--------------------------------------------------------------------------------------*

cd $saved
use pl_trimmed.dta, clear
keep $individual2 $timeuse $health

merge 1:1 pid syear using Jppathl, keep(3) nogen
keep $individual2 $timeuse $health rv_id $ppathl

merge 1:1 pid syear using pequiv2021_junk, keep(3) nogen
keep $individual2 $timeuse $health rv_id $pequiv $ppathl

cd $saved
merge 1:1 pid syear using pkal_trimmed, keep(3) nogen
keep $individual2 $timeuse $health plh0171 plh0035 kal1i01 kal1d01 rv_id $pequiv $ppathl

cd $saved
merge m:1 hid syear using hl_trimmed, keep(3) nogen
keep cid $individual2 $timeuse $health plh0171 plh0035 kal1i01 kal1d01 hlc0005_h hlf0001_h rv_id $pequiv $ppathl

cd $saved
merge 1:1 pid syear using pgen_trimmed, keep(3) nogen
keep $individual2 $timeuse $health plh0171 plh0035 cid hlc0005_h pglabnet pglabgro pgvebzeit pgtatzeit pgpartnr pgstib pgpartz pgjobch pgbilzeit kal1i01 kal1d01 pglfs p_isco08 rv_id $pequiv $ppathl pgnation hlf0001_h

cd $soep
merge m:1 hid syear using hgen, keep(3) nogen
keep $individual2 $timeuse $health plh0171 plh0035 cid hlc0005_h pglabnet pglabgro pgvebzeit pgtatzeit pgpartnr pgstib pgpartz hgtyp2hh pgbilzeit kal1i01 kal1d01 pgjobch pglfs rv_id $pequiv hghinc $ppathl pgnation hlf0001_h

cd $soep
merge 1:1 pid syear using pbrutto, keep(3) nogen
keep $individual2 $timeuse $health plh0171 plh0035 cid hlc0005_h pglabnet pglabgro pgvebzeit pgtatzeit pgpartnr pgstib pgpartz hgtyp2hh pgbilzeit kal1i01 kal1d01 pgjobch pglfs stell_h rv_id $pequiv hghinc $ppathl pgnation hlf0001_h

cd $soep 
merge 1:1 pid syear using pkal.dta
keep $individual2 $timeuse $health plh0171 plh0035 cid hlc0005_h pglabnet pglabgro pgvebzeit pgtatzeit pgpartnr pgstib pgpartz hgtyp2hh pgbilzeit kal1i01 kal1d01 pgjobch pglfs stell_h rv_id $pequiv kal1a02 kal1b02 kal1d02 hghinc kal1d01 $ppathl pgnation hlf0001_h

cd $saved
merge m:1 hid syear using kids_size, keep(1 3) nogen

merge m:1 hid syear using hh_size,  keep(1 3) nogen

cd $saved
merge 1:1 pid syear using health, keep(1 3) nogen

cd $saved
merge 1:1 pid syear using gripstr, keep(1 3) nogen


* CONVERT INCOMES
preserve
cd $soep
import excel conversion_rates.xlsx, sheet("conversion_rates") firstrow clear
gen c2 = conversion_rate if syear == 2020
ereplace c2 = max(c2)
gen c_new = c2/conversion_rate 
drop conversion_rate
rename c_new conversion_rate
drop if syear < 1984 
drop if syear == . 
drop c2

cd $saved
save conversion_rates, replace 
restore 

cd $saved
merge m:1 syear using conversion_rates.dta, nogen
la var conversion_rate "Real effective exchange rate (2020: 1)"

* Imputed income								   
cd $saved
merge m:1 hid syear using mihinc_collapsed, keep(1 3) nogen 


keep pid hid syear pla0009_v2 plb0021 plb0022_h plb0024_h pld0140 ple0008 ple0010_h ple0040 ple0044_h ple0046 ple0056 plh0035 plh0171 pli0010 pli0012_h pli0016_h pli0019_h pli0022_h pli0036 pli0043_h pli0044_h pli0046 pli0051 pli0055 pli0057 p_isco08 p_isco88 kal1i01 kal1d01 cid hlc0005_h pgpartz pgpartnr pglabgro pglabnet pgstib pglfs pgjobch pgtatzeit hgtyp2hh stell_h kal1a02 kal1b02 kal1d02 hhgr ihinc pld0152 plc0091_h k_birthy_h* hghinc $pequiv $ppathl plb0302 ple0055 ple0053 conversion_rate pgvebzeit pgnation pgbilzeit plb0057_h1 plb0065 hlf0001_h mcs pcs gs_avg ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0187 ple0023 ple0024


* Drop if individual only has two observations
*--------------------------------------------------------------------------------------*
bys pid (syear): gen count = _N
drop if count <=2
drop count

quietly foreach var of varlist * {
	*di "`var'"
	replace `var' = . if inlist(`var', -8, -7, -6, -5, -4, -3, -1)
}


* SAVE 
cd $saved
compress
save pre_R_soeponly, replace
*----------------------------------------------------------------------------------------------------*


