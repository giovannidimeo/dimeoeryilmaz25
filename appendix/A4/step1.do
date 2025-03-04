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
macro drop _all 
capture log close   
set maxvar 25000  


*----------------------------------------------------------------------------------------------------*
* IMPORTANT: RUN THIS CODE ONLY AFTER RUNNING STEP_1 + STEP_2 + STEP_3 
 
cd $saved

use R_soeponly, clear

* de Chaisemartin and D'Haultfoeuille (2020)
gen shock_bin = 1 if syear >= shock
replace shock_bin = 0 if syear < shock

foreach var of varlist quitjob_pequiv i11110 {
	did_multiplegt `var' pid syear shock_bin, robust_dynamic dynamic(5) placebo(5) trends_nonparam(age age_sq gender) switchers(in) 
	matrix dcdh_b_`var' = e(estimates) // storing the estimates for later
	matrix dcdh_v_`var' = e(variances)
	matrix total = dcdh_b_`var' , dcdh_v_`var'
	matrix colnames total = estimates variances
	putexcel set "did_multiplegt_`var'", sheet("total") replace
	putexcel A1=matrix(total)
}

event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
	title("de Chaisemartin and D'Haultfoeuille (2020)") xlabel(-5(1)5)) stub_lag(Effect_#) stub_lead(Placebo_#) together


* TWFE
// creating dummies for the lags 0..5, based on K = number of periods since treatment (or missing if there is a never-treated group)
cap drop L*event
cap drop F*event
forvalues l = 0/5 {
		gen L`l'event = event_time== -`l'
}
gen L6event = event_time>=6 // binning K=20 and above
        
// creating dummies for the leads 0..5
forvalues l = 0/5 { 
		gen F`l'event = event_time==`l'
}
gen F6event = event_time<=-6

foreach var of varlist quitjob_pequiv i11110  {
	reghdfe `var' o.L1event L2event-L6event F*event, a(pid syear age age_sq gender) cluster(pid)
	matrix ols_b_`var' = e(b)' // storing the estimates for later
	matrix ols_v_`var' = e(V)
	mata st_matrix("ols_v_`var'",sqrt(diagonal(st_matrix("e(V)"))))
	matrix total = ols_b_`var' , ols_v_`var'
	matrix colnames total = estimates variances
	putexcel set "ols_`var'", sheet("total") replace
	putexcel A1=matrix(total)

}

	