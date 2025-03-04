use "***\auf19.dta", clear


foreach v of varlist cases_w days_w spell_w cases_m days_m spell_m cases_tot days_tot spell_tot{
	replace `v' = 0 if mi(`v')
}

gen rel_w = cases_w/cases_tot
gen rel_m = cases_m/cases_tot


qui sum cases_tot, d
scalar sum = r(sum)
gen rel_cases = cases_tot/sum

gen auf42 = 1 if spell_tot >= 42
replace auf42 = 0 if mi(auf42)

qui sum cases_tot if auf42 == 1, d
scalar sum42 = r(sum)
di sum42 /* 2504146 */
di sum42/sum /* 0.04149255 */

bys Chapter_Desc: egen sum42_nr = sum(auf42) 
bys Chapter_Desc: egen sum42_agg = sum(cases_tot) if auf42 == 1

gen sum42_agg_rel = sum42_agg/sum42

bys Chapter_Desc: ereplace sum42_agg_rel = max(sum42_agg_rel)


foreach i in "tot" "w" "m" {
	qui sum cases_`i', d
	scalar sum = r(sum)
	gen rel_cases_`i' = cases_`i'/sum

	gen auf42_`i' = 1 if spell_`i' >= 42
	replace auf42_`i' = 0 if mi(auf42_`i')

	qui sum cases_`i' if auf42_`i' == 1, d
	scalar sum42 = r(sum)

	bys Chapter_Desc: egen sum42_nr_`i' = sum(auf42_`i') 
	bys Chapter_Desc: egen sum42_agg_`i' = sum(cases_`i') if auf42_`i' == 1

	gen sum42_agg_rel_`i' = sum42_agg_`i'/sum42

	bys Chapter_Desc: ereplace sum42_agg_rel_`i' = max(sum42_agg_rel_`i')
}



keep Chapter_Desc sum42_agg_rel_tot sum42_agg_rel_w sum42_agg_rel_m
drop if Chapter_Desc == ""
drop if sum42_agg_rel_tot == .
duplicates drop

gen desc_short = Chapter_Desc
replace desc_short = "Diseases of the blood" if Chapter_Desc == "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism (D50-D89)"
replace desc_short = "Unclassified" if Chapter_Desc == "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)"
replace desc_short = "Injury, poisoning" if Chapter_Desc == "Injury, poisoning and certain other consequences of external causes (S00-T98)"
replace desc_short = "Congenital malformations"  if Chapter_Desc == "Congenital malformations, deformations and chromosomal abnormalities (Q00-Q99)"
replace desc_short = "Infectious and parasitic diseases" if Chapter_Desc == "Certain infectious and parasitic diseases (A00-B99)"
replace desc_short = "Conditions originating in the perinatal period" if Chapter_Desc == "Certain conditions originating in the perinatal period (P00-P96)"
replace desc_short = "Diseases of the musculoskeletal system"  if Chapter_Desc == "Diseases of the musculoskeletal system and connective tissue (M00-M99)"
replace desc_short =  regexr(desc_short," \([A-Z][0-9][0-9]-[A-Z][0-9][0-9]\)", "")

replace desc_short = "Other" if desc_short != "Mental and behavioural disorders" &  desc_short != "Diseases of the musculoskeletal system" & ///
 desc_short != "Diseases of the circulatory system" & desc_short != "Injury, poisoning" &  desc_short != "Neoplasms" & desc_short != "Diseases of the nervous system"
 
drop Chapter_Desc

collapse (sum) sum42_agg_rel_tot sum42_agg_rel_w sum42_agg_rel_m, by(desc_short)

rename  desc_short desc_short_orig
encode desc_short_orig, gen(des_short)

mylabels 0(10)40, myscale(@/100) local(myla) suffix(%)

	* With colors and legend instead of x-axis value labels
	graph bar (mean) sum42_agg_rel_tot , over(desc_short, sort(1) descending) ytitle("Share") ylab(`myla') legend(position(6) order(5 "Mental and Behavioral" 2 "Musculoskeletal" 1 "Circulatory System" 4 "Injury, Poisoning" 6 "Neoplasms" 7 "Other" 3 "Nervous System")) asyvars  graphregion(fcolor(white)) 

cd $results
graph export "deseases_auf42_tot.png", as(png) name("Graph") replace

********************************************************************************
* Only musculoskeletal issues
use "***\auf19.dta", clear


foreach v of varlist cases_w days_w spell_w cases_m days_m spell_m cases_tot days_tot spell_tot{
	replace `v' = 0 if mi(`v')
}

gen rel_w = cases_w/cases_tot
gen rel_m = cases_m/cases_tot


qui sum cases_tot, d
scalar sum = r(sum)
gen rel_cases = cases_tot/sum

gen auf42 = 1 if spell_tot >= 42
replace auf42 = 0 if mi(auf42)

qui sum cases_tot if auf42 == 1, d
scalar sum42 = r(sum)

keep if Chapter_Desc == "Diseases of the musculoskeletal system and connective tissue (M00-M99)"

bys Group_Desc: egen sum42_nr = sum(auf42) 
bys Group_Desc: egen sum42_agg = sum(cases_tot) if auf42 == 1

gen sum42_agg_rel = sum42_agg/sum42

bys Group_Desc: ereplace sum42_agg_rel = max(sum42_agg_rel)

foreach i in "tot" "w" "m" {
	qui sum cases_`i', d
	scalar sum = r(sum)
	gen rel_cases_`i' = cases_`i'/sum

	gen auf42_`i' = 1 if spell_`i' >= 42
	replace auf42_`i' = 0 if mi(auf42_`i')

	qui sum cases_`i' if auf42_`i' == 1, d
	scalar sum42 = r(sum)

	bys Group_Desc: egen sum42_nr_`i' = sum(auf42_`i') 
	bys Group_Desc: egen sum42_agg_`i' = sum(cases_`i') if auf42_`i' == 1

	gen sum42_agg_rel_`i' = sum42_agg_`i'/sum42

	bys Group_Desc: ereplace sum42_agg_rel_`i' = max(sum42_agg_rel_`i')
}

keep Group_Desc sum42_agg_rel_tot sum42_agg_rel_w sum42_agg_rel_m
drop if sum42_agg_rel_tot == .
duplicates drop


gen desc_short = ""
replace desc_short = "Arthropathies" if Group_Desc == "Arthropathies: Arthrosis"
replace desc_short = "Arthropathies" if Group_Desc == "Arthropathies: Inflammatory polyarthropathies"
replace desc_short = "Arthropathies" if Group_Desc == "Arthropathies: Other joint disorders"
replace desc_short = "Dorsopathies" if Group_Desc == "Dorsopathies: Deforming dorsopathies"
replace desc_short = "Osteopathies and chondropathies" if Group_Desc == "Osteopathies and chondropathies: Chondropathies"
replace desc_short = "Osteopathies and chondropathies" if Group_Desc == "Osteopathies and chondropathies: Disorders of bone density and structure"
replace desc_short = "Osteopathies and chondropathies" if Group_Desc == "Osteopathies and chondropathies: Other osteopathies"
replace desc_short = "Other" if Group_Desc == "Soft tissue disorders: Disorders of synovium and tendon"
replace desc_short = "Other"  if desc_short == ""

collapse (sum) sum42_agg_rel_tot, by(desc_short)


mylabels 0(10)60, myscale(@/100) local(myla) suffix(%)

graph bar (mean) sum42_agg_rel_tot , over(desc_short, sort(1) descending) ytitle("Share") ylab(`myla')  asyvars  graphregion(fcolor(white)) 

cd $results
graph export "deseases_musculoskeletal.png", as(png) name("Graph") replace

********************************************************************************
* Only Mental and Behavioral issues
use "***\auf19.dta", clear

foreach v of varlist cases_w days_w spell_w cases_m days_m spell_m cases_tot days_tot spell_tot{
	replace `v' = 0 if mi(`v')
}

gen rel_w = cases_w/cases_tot
gen rel_m = cases_m/cases_tot


qui sum cases_tot, d
scalar sum = r(sum)
gen rel_cases = cases_tot/sum

gen auf42 = 1 if spell_tot >= 42
replace auf42 = 0 if mi(auf42)

qui sum cases_tot if auf42 == 1, d
scalar sum42 = r(sum)

keep if Chapter_Desc == "Mental and behavioural disorders (F00-F99)"

replace Group_Desc = "Depressive disorders" if ICD10_3_Code_Desc == "Depressive episode"
replace Group_Desc = "Depressive disorders" if ICD10_3_Code_Desc == "Recurrent depressive disorder"

bys Group_Desc: egen sum42_nr = sum(auf42) 
bys Group_Desc: egen sum42_agg = sum(cases_tot) if auf42 == 1

gen sum42_agg_rel = sum42_agg/sum42

bys Group_Desc: ereplace sum42_agg_rel = max(sum42_agg_rel)

foreach i in "tot" "w" "m" {
	qui sum cases_`i', d
	scalar sum = r(sum)
	gen rel_cases_`i' = cases_`i'/sum

	gen auf42_`i' = 1 if spell_`i' >= 42
	replace auf42_`i' = 0 if mi(auf42_`i')

	qui sum cases_`i' if auf42_`i' == 1, d
	scalar sum42 = r(sum)

	bys Group_Desc: egen sum42_nr_`i' = sum(auf42_`i') 
	bys Group_Desc: egen sum42_agg_`i' = sum(cases_`i') if auf42_`i' == 1

	gen sum42_agg_rel_`i' = sum42_agg_`i'/sum42

	bys Group_Desc: ereplace sum42_agg_rel_`i' = max(sum42_agg_rel_`i')
}


keep Group_Desc sum42_agg_rel_tot sum42_agg_rel_w sum42_agg_rel_m
drop if sum42_agg_rel_tot == .
duplicates drop


gen desc_short = ""
replace desc_short = "Depressive disorders" if Group_Desc == "Depressive disorders"
replace desc_short = "Stress-related disorders" if Group_Desc == "Neurotic, stress-related and somatoform disorders"
replace desc_short = "Schizophrenia" if Group_Desc == "Schizophrenia, schizotypal and delusional disorders"
replace desc_short = "Other"  if desc_short == ""

collapse (sum) sum42_agg_rel_tot, by(desc_short)


mylabels 0(20)80, myscale(@/100) local(myla) suffix(%)

graph bar (mean) sum42_agg_rel_tot , over(desc_short, sort(1) descending) ytitle("Share") ylab(`myla')  asyvars  graphregion(fcolor(white)) 

cd $results
graph export "deseases_mental_tot.png", as(png) name("Graph") replace


********************************************************************************
* Microcategories
use "***\auf19.dta", clear


foreach v of varlist cases_w days_w spell_w cases_m days_m spell_m cases_tot days_tot spell_tot{
	replace `v' = 0 if mi(`v')
}

gen rel_w = cases_w/cases_tot
gen rel_m = cases_m/cases_tot


qui sum cases_tot, d
scalar sum = r(sum)
gen rel_cases = cases_tot/sum

gen auf42 = 1 if spell_tot >= 42
replace auf42 = 0 if mi(auf42)

qui sum cases_tot if auf42 == 1, d
scalar sum42 = r(sum)

bys Group_Desc: egen sum42_nr = sum(auf42) 
bys Group_Desc: egen sum42_agg = sum(cases_tot) if auf42 == 1

gen sum42_agg_rel = sum42_agg/sum42

bys Group_Desc: ereplace sum42_agg_rel = max(sum42_agg_rel)

foreach i in "tot" "w" "m" {
	qui sum cases_`i', d
	scalar sum = r(sum)
	gen rel_cases_`i' = cases_`i'/sum

	gen auf42_`i' = 1 if spell_`i' >= 42
	replace auf42_`i' = 0 if mi(auf42_`i')

	qui sum cases_`i' if auf42_`i' == 1, d
	scalar sum42 = r(sum)

	bys Group_Desc: egen sum42_nr_`i' = sum(auf42_`i') 
	bys Group_Desc: egen sum42_agg_`i' = sum(cases_`i') if auf42_`i' == 1

	gen sum42_agg_rel_`i' = sum42_agg_`i'/sum42

	bys Group_Desc: ereplace sum42_agg_rel_`i' = max(sum42_agg_rel_`i')
}


keep Chapter_Desc Group_Desc sum42_agg_rel_tot sum42_agg_rel_w sum42_agg_rel_m
drop if sum42_agg_rel_tot == .
duplicates drop

sort Chapter_Desc


gen group_short = ""
order group_short, after(Group_Desc)
replace group_short = "Arthropathies" if Group_Desc == "Arthropathies: Arthrosis"
replace group_short = "Dorsopathies" if Group_Desc == "Dorsopathies: Deforming dorsopathies"
replace group_short = "Other musculoskeletal" if group_short == "" & Chapter_Desc == "Diseases of the musculoskeletal system and connective tissue (M00-M99)"
replace group_short = "Diseases of the nervous system" if Chapter_Desc == "Diseases of the nervous system (G00-G99)"
replace group_short = "Diseases of the circulatory system" if Chapter_Desc == "Diseases of the circulatory system (I00-I99)"
replace group_short = "Injury, poisoning and other external causes" if Chapter_Desc == "Injury, poisoning and certain other consequences of external causes (S00-T98)"
replace group_short = "Mood disorders" if Group_Desc == "Mood [affective] disorders"
replace group_short = "Stress-related disorders" if Group_Desc == "Neurotic, stress-related and somatoform disorders"
replace group_short = "Other mental and behavioural disorders" if group_short == "" & Chapter_Desc == "Mental and behavioural disorders (F00-F99)"
replace group_short = "Neoplasms" if group_short == "" & Chapter_Desc == "Neoplasms (C00-D48)"
replace group_short = "Other" if group_short == "" 

collapse (sum) sum42_agg_rel_tot, by(group_short)

mylabels 0(20)80, myscale(@/100) local(myla) suffix(%)

graph bar (mean) sum42_agg_rel_tot , over(group_short, sort(1) descending) ytitle("Share") ylab(`myla')  asyvars  graphregion(fcolor(white)) 


replace group_short = "Other" if group_short == "Other mental and behavioural disorders" | group_short == "Other musculoskeletal" | group_short == "Diseases of the nervous system" | group_short == "Diseases of the nervous system"

collapse (sum) sum42_agg_rel_tot, by(group_short)

mylabels 0(5)30, myscale(@/100) local(myla) suffix(%)

graph bar (mean) sum42_agg_rel_tot , over(group_short, sort(1) descending) ytitle("Share") ylab(`myla')  asyvars  graphregion(fcolor(white)) 

cd $results
graph export "deseases_microcat.png", as(png) name("Graph") replace

