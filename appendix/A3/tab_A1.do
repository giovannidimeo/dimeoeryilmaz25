cd $saved

use R_soeponly_control, clear

gen flag_illness = . 

local var = "ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0187 ple0023"
foreach v in  `var' {
	replace flag_illness = 1 if `v' == 1
	replace flag_illness = 0 if `v' == -2
}

keep if syear >= 2009 

sum flag_illness if event_time <= 0 & event_time >= -5, d /* .1520081 */
sum flag_illness if event_time >= 0 & event_time <= 5, d /*  .1796473 */

sum flag_illness if shock == 0 & age >= 25 & age <= 55, d /*  .1115279 */


local var = "ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0019 ple0020 ple0021 ple0022 ple0187 ple0023"
foreach v in  `var' {
	qui replace `v'  = 0 if `v' == -2
	di `"`:var label `v''"'
	qui sum `v' if event_time < 0 & event_time >= -5, d
	di "Pre-shock average:", r(mean)
	qui sum `v' if event_time > 0 & event_time <= 5, d
	di "After-shock average:", r(mean)
	qui sum `v' if shock ==  0 & age >= 25 & age <= 55, d 
	di "Population average:", r(mean)

}


qui distinct pid if shock >= 2009 
sca ndistinct = r(ndistinct) 
qui eststo pre: estpost sum ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0021 ple0019 ple0020  ple0022 ple0187 ple0023 if event_time < 0 & event_time >= -5, d
qui estadd scalar ndistinct

qui distinct pid if shock >= 2009 
sca ndistinct = r(ndistinct) 
qui eststo post: estpost sum ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0021 ple0019 ple0020  ple0022 ple0187 ple0023 if event_time <= 0 & event_time >= -5, d
qui estadd scalar ndistinct

qui distinct pid if shock == 0 & syear >= 2009 & age >= 25 & age <= 55 
sca ndistinct = r(ndistinct) 
qui eststo control: estpost sum ple0011 ple0012 ple0013 ple0014 ple0015 ple0016 ple0017 ple0018 ple0021 ple0019 ple0020  ple0022 ple0187 ple0023 if shock == 0 & age >= 25 & age <= 55, d
qui estadd scalar ndistinct


esttab pre post control,   ///
	cell("mean(pattern(1 1 1) fmt(2))")  /// 
	scalars("ndistinct Unique observations")  noobs ///
	varlabels(ple0011 "Sleeping issues" ple0012 "Diabetes" ple0013 "Asthma" ple0014 " Cardiopathy" ple0015 "Cancer" ple0016 "Stroke" ple0017 "Migraine" ple0018 "High Blood Presure" ple0021 "Joint Disorder" ple0019 "Depression" ple0020 "Dementia" ple0022 "Chronic Back Pain" ple0187 "Burnout" ple0023 "Other illness" ) ///
	mtitle( "Pre-treatment" "Post-treatment" "SOEP") nogaps compress  collabels(,none)
	

* Save table 
cd $results
esttab pre post control, replace booktabs ///
	cell("mean(pattern(1 1 1 1 1 1) fmt(2))")  /// 
	scalars("ndistinct Unique observations")  noobs ///
	varlabels(ple0011 "Sleeping issues" ple0012 "Diabetes" ple0013 "Asthma" ple0014 " Cardiopathy" ple0015 "Cancer" ple0016 "Stroke" ple0017 "Migraine" ple0018 "High Blood Presure" ple0021 "Joint Disorder" ple0019 "Depression" ple0020 "Dementia" ple0022 "Chronic Back Pain" ple0187 "Burnout" ple0023 "Other illness" ) ///
	mtitle( "Pre-treatment" "Post-treatment" "SOEP") nogaps compress  collabels(,none)
	