// Dissertation data work ------------------------------------------------------

*************************************************************************
*net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
*net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
*net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
*net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
*net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace
*************************************************************************
*net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
*************************************************************************

// 0.0 Settings and data loading -----------------------------------------------

set more off
clear all

use "L:\99.LSE studies\99.Dissertation\01.data\01.ECSC 2020\dt_tabs1.dta"
cd "L:\99.LSE studies\99.Dissertation\01.data\99.descriptives"

svyset keyhog [pweight=fex_c], /*
*/ vce(linearized) /*
*/ singleunit(missing) /*
*/ || keyper

// -----------------------------------------------------------------------------
// 1.0 Creates variables  ------------------------------------------------------
// -----------------------------------------------------------------------------
* 1.1. Pre and post constitution dummy + re centred running variable + marings

*(dummy pre post)
gen byear = 2020 - p5785
gen pos_cons_91 = 0 // born before 1991
replace pos_cons_91 = 1 if byear > 1991 // born after 1991

* (re centered RV)
gen byear_rv = byear - 1991

// -----------------------------------------------------------------------------
* 1.2. Pre and post constitution dummy for over 18 at cut-off + re centred running variable + margins

*(dummy pre post)
gen ayear = byear + 18
gen a_pos_cons_91 = 0 // adult before 1991
replace a_pos_cons_91 = 1 if ayear > 1991 // adult after 1991

* (re centered RV)
gen ayear_rv = ayear - 1991

// -----------------------------------------------------------------------------
* 1.3. Pre and post constitution dummy for over 25 at cut-off + re centred running variable + margins

*(dummy pre post)
gen cyear = byear + 25
gen c_pos_cons_91 = 0 // adult before 1991
replace c_pos_cons_91 = 1 if ayear > 1991 // adult after 1991

* (re centered RV)
gen cyear_rv = cyear - 1991

// -----------------------------------------------------------------------------
* 1.4. Pre and post penal code for over 18 at cut-off + re centred running variable + margins

*(dummy pre post)
gen pyear = byear + 18
gen pos_penal = 0 // adult before 2000
replace pos_penal = 1 if ayear > 2000 // adult after 1991

* (re centered RV)
gen pyear_rv = pyear - 2000

// -----------------------------------------------------------------------------
* 1.5. Innaction dummy 
gen inac = 0
replace inac = 1 if p1672_laben == "Did nothing"

// -----------------------------------------------------------------------------
* 1.6. More than one need dummy 
gen lnG1 = 0
replace lnG1 = 1 if nj_1_count > 1

// -----------------------------------------------------------------------------
* 1.7 Raplace labels for prospect of taking the same path
replace p1687 = 0 if p1687 == 2

// -----------------------------------------------------------------------------
* 1.8 Civil legal problems general grouping
gen civ_prob = 0
replace civ_prob = 1 if p3013_cat_labens != "Crime"

// -----------------------------------------------------------------------------
* 1.9 Civil legal problems specific grouping
gen civ_prob_s = 0
replace civ_prob_s = 1 if (p3013_cat_labens == "Consumption" /*
*/| p3013_cat_labens == "Debts" /*
*/| p3013_cat_labens == "Family" /*
*/| p3013_cat_labens == "Health" /*
*/| p3013_cat_labens == "Home utilities" /*
*/| p3013_cat_labens == "Housing" /*
*/| p3013_cat_labens == "Public env" /*
*/| p3013_cat_labens == "State" /*
*/| p3013_cat_labens == "Work" /*
*/| p3013_cat_labens == "Discrimination" /*
*/| p3013_cat_labens == "Edu" /*
*/| p3013_cat_labens == "Env resources" /*
*/| p3013_cat_labens == "Land" )

// -----------------------------------------------------------------------------
* 1.10 Crime legal problems specific grouping
gen cri_prob_s = 0
replace cri_prob_s = 1 if (p3013_cat_labens == "Armed conflict" /*
*/| p3013_cat_labens == "Crime" )

// -----------------------------------------------------------------------------
* 1.11. Perceptions on policy and judges
gen pol_cont_per = 0          // postive perception of police contribution to security
replace pol_cont_per = 1 if  p1182s1 == 1

gen jud_cont_per = 0          // postive perception of judges contribution to security
replace jud_cont_per = 1 if  p1181s2 == 1

// -----------------------------------------------------------------------------
* 1.12. Age
gen age_sq = p5785^2

// -----------------------------------------------------------------------------
* 1.13. Hosehold head
gen hh_head = 0
replace hh_head = 1  if p5501 == 1

// -----------------------------------------------------------------------------
* 1.14. uni level education
gen uni_educ = 0
replace uni_educ = 1  if p6210 == 6 

// -----------------------------------------------------------------------------
* 1.15. EAP
gen eap = 0
replace eap = 1  if (p1365 == 1 | p1365 == 2)

// -----------------------------------------------------------------------------
* 1.16 General changes in coding 
replace p6210 = .  if p6210 == 9 // Replace not declared with missing values to garantee ordinal scales
replace p220 = 0  if p220 == 2   // Recode gender for woman from 2 to 0 - base line references male declarants
gen lp = 1 // Count for declared justiable problems


// -----------------------------------------------------------------------------
* 2. Full data set descriptives ------------------------------------------------
// ----------------------------------------------------------------------------- 

preserve

duplicates drop keyper, force

collapse (sum) fex =fex_c, by(lnG1) 
list

restore


preserve
collapse (sum) fex = fex_c, by(ayear_rv p3013_cat_labens inac) 

sort ayear_rv p3013_cat_labens

by ayear_rv p3013_cat_labens: egen yer_ln0 = sum(fex)
by ayear_rv p3013_cat_labens: egen yer_ln1 = sum(fex*inac)
by ayear_rv p3013_cat_labens: gen yer_ln2 = yer_ln1/yer_ln0

twoway scatter yer_ln2 ayear_rv, by(p3013_cat_labens,norescale) /*
*/xline(0,lcolor(red)) /*
*/msize(2pt) /*
*/mfcolor(none) /*
*/mlwidth(0.1pt)/*
*/mlcolor(colorstyle)/*
*/ytitle("Inaction rate") /*
*/xtitle("Running variable centred at 1991 (Mayority of age at 1991)")

*graph export scttr_rv_1991.svg, fontface(Arial) clipstroke(off) bgfill(off) 
*twoway (scatter yer_ln2 ayear_rv, sort), by(p3013_cat_labens,norescale)
restore


tab p3013_cat_labens, m sort

preserve
collapse (sum) fex = fex_c, by(ayear_rv mpio inac) 

sort ayear_rv mpio

by ayear_rv mpio: egen yer_ln0 = sum(fex)
by ayear_rv mpio: egen yer_ln1 = sum(fex*inac)
by ayear_rv mpio: gen yer_ln2 = yer_ln1/yer_ln0

twoway scatter yer_ln2 ayear_rv, by(mpio,norescale) /*
*/xline(0,lcolor(red)) /*
*/msize(2pt) /*
*/mfcolor(none) /*
*/mlwidth(0.1pt)/*
*/mlcolor(colorstyle)/*
*/ytitle("Inaction rate") /*
*/xtitle("Running variable centred at 1991 (Mayority of age at 1991)")

*graph export scttr_rv_1991.svg, fontface(Arial) clipstroke(off) bgfill(off) 
*twoway (scatter yer_ln2 ayear_rv, sort), by(p3013_cat_labens,norescale)
restore

// -----------------------------------------------------------------------------
* 2. drops and sub sets --------------------------------------------------------
// ----------------------------------------------------------------------------- 

*2.1 Drops records with no valid solution or valid response on retaking paths --------------------------------------------------------------------------------

tab p1685, m
drop if p1685 == 9 // effective solution

tab p1687, m
drop if missing(p1687) // retaking path

* 2.2. drop records on action path ---------------------------------------------

// Dropping outocompositive solution paths takes the data set from justiable
// probles to legal needs
* drop if p1672_laben == "Tried to reach a direct agreement"

// Categorie checks over low freuency and high variation paths
* drop if p1672_laben == "Turned to an illegal actor"
* drop if p1672_laben == "Acted violently"


* 2.3. drop tipologies --------------------------------------------------------- 

// Dropping low frequency hihgh variation problem tipologies -------------------

 drop if p3013_cat_labens == "Discrimination"
 drop if p3013_cat_labens == "Edu"
 drop if p3013_cat_labens == "Env resources"
 drop if p3013_cat_labens == "Land"
 drop if p3013_cat_labens == "Armed conflict"

// Drop on problem concurrence --------------------------------------------- 

* drop if lnG1 == 0

// Drop on city frequency concurrence ------------------------------------------

*drop if (mpio == "Barranquilla" /*
**/| mpio == "Bucaramanga" /*
**/| mpio == "Cartagena de Indias" /*
**/| mpio == "Montería" /*
**/| mpio == "San José de Cúcuta" /*
**/| mpio == "Manizales" )

drop if missing(mpio)
tab mpio, m
// 2.0 Band width selection ----------------------------------------------------
gen fsc = ayear_rv // running variable centred at 1991

global  v2101 " p220 uni_educ eap hh_head civ_prob_s lnG1 nj_impacto inac p1685"

 rdwinselect fsc $v2101 , cutoff(0) wmin(1) wstep(5) wasymmetric dropmissing   approximate plot
 
 rdrandinf inac fsc , wl(-15) wr(11) // test the sharp null hypothesis of no treatment effect using randomization inference methods

foreach y of global v2101 {
	rdrobust `y' fsc
}



rdwinselect fsc, seed(50)   wobs(5) nwindows(100) plot approximate 



rdbwselect inac fsc, all /*

*/ covs ( lnG1 civ_prob_s nj_impacto uni_educ eap hh_head p1687 )
ereturn list

rdplot inac fsc if -e(b_certwo_l) <= fsc & fsc <= e(b_certwo_r), binselect(esmvpr) /*
*/kernel(triangular) /*
*/h(`e(b_certwo_l))' `e(b_certwo_r)') /*
*/p(1) /*
*/weights(fex_c)/*
*/graph_options( graphregion(color(white)))

// manipulation test -  null hypothesis of no manipulation
preserve
rddensity  fsc,all
rddensity fsc, c(0) plot
ereturn list

// difference in cut-offs and fits
capture drop smoothdem0* smoothdem1* x0* x1*
local co 0
foreach i in 2 5 7 10 15 20 {
local co = `co' +1
lpoly inac fsc if a_pos_cons_91 == 0, nograph kernel(triangle) gen(x0`co' smoothdem0`co') ///
bwidth(`i') degree(3)
lpoly inac fsc if a_pos_cons_91 == 1, nograph kernel(triangle) gen(x1`co' smoothdem1`co') ///
bwidth(`i') degree(3)
}

line smoothdem01 x01, msize(small) color(gray) sort || line smoothdem11 x11, sort color(gray) || ///
line smoothdem02 x02, color(black) sort || line smoothdem12 x12, sort color(black) || ///
line smoothdem03 x03, color(red) sort || line smoothdem13 x13, sort color(red) || ///
line smoothdem04 x04, color(blue) sort || line smoothdem14 x14, sort color(blue) || ///
line smoothdem05 x05, color(green)sort || line smoothdem15 x15, sort color(green)|| ///
line smoothdem06 x06, color(orange) sort || line smoothdem16 x16, sort color(orange) ///
xline(0.5,lstyle(dot)) legend(off) xtitle("Democratic vote share") ytitle("ADA score") ///
title("Bandwidths: 2, 5, 7, 10, 15, 20")
graph export lee_dif_bws.png, replace

// 2.0 Descriptive statistics --------------------------------------------------

* 2.1. General centrality and disperssion meassures  
global  v2101 "p5785 p220 uni_educ eap hh_head nj_1_count civ_prob_s lnG1 nj_impacto inac pos_cons_91 p1686 p1685 p1687"

// balance on covariates

// SMD denotes the standardized mean difference between the two groups of data. The more closely this value approaches 0, the more balanced the two groups are

// The variation ratio denotes the variance difference between the two groups. Again, the more closely this value approaches 0, the more balanced the two groups are.

preserve
*keep if ayear_rv > -21 
*keep if ayear_rv < 21 
rdbalance a_pos_cons_91 $v2101, /* 
*/ s(N mean sd median min max) /*
*/ f(%9.sf) /*
*/ wt(fex_c) /*
*/ saving(output.xls, replace) excel
restore

// 2.0 Models specification ----------------------------------------------------

reg inac a_pos_cons_91  p1685  nj_impacto i.lnG1 i.civ_prob_s i.civ_prob_s##i.lnG1 [pweight = fex_c], vce(cluster mpio)

preserve
*keep if ayear_rv > -21 
*keep if ayear_rv < 21 
keep if lnG1 == 1
sort ayear_rv mpio

by ayear_rv  : egen yer_ln0 = sum(fex_c)
by ayear_rv  : egen yer_ln1 = sum(fex_c*inac)
by ayear_rv  : gen yer_ln2 = yer_ln1/yer_ln0
by ayear_rv  : egen yer_ln3 = sum(ln)
twoway scatter yer_ln2 ayear_rv, xline(0,lcolor(red)) /*
*/msize(2pt) /*
*/mfcolor(none) /*
*/mlwidth(0.1pt)/*
*/mlcolor(colorstyle)/*
*/ytitle("Inaction rate") /*
*/xtitle("Running variable centred at 1991 (Mayority of age after 1991)")


// 2.0 Regressions -------------------------------------------------------------

regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if (ayear_rv > - 11 & ayear_rv < 11), vce(cluster mpio)
estimates store domestic
quietly regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if (ayear_rv > - 10 & ayear_rv < 10), vce(cluster mpio)
estimates store foreign

coefplot (domestic, label(Domestic Cars)) (foreign, label(Foreign Cars)), drop(˙cons) xline(0)
quietly eststo w30: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 30 & ayear_rv < 30), vce(cluster mpio) 
quietly eststo w28: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 28 & ayear_rv < 28), vce(cluster mpio) 
quietly eststo w26: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 26 & ayear_rv < 26), vce(cluster mpio) 
quietly eststo w24: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 24 & ayear_rv < 24), vce(cluster mpio) 
quietly eststo w22: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 22 & ayear_rv < 22), vce(cluster mpio) 
quietly eststo w20: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 20 & ayear_rv < 20), vce(cluster mpio) 
quietly eststo w18: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 18 & ayear_rv < 18), vce(cluster mpio) 
quietly eststo w16: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 16 & ayear_rv < 16), vce(cluster mpio) 
quietly eststo w14: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 14 & ayear_rv < 14), vce(cluster mpio) 
quietly eststo w12: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 12 & ayear_rv < 12), vce(cluster mpio) 
quietly eststo w10: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 10 & ayear_rv < 10), vce(cluster mpio) 
quietly eststo w08: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 8 & ayear_rv < 8), vce(cluster mpio) 
quietly eststo w06: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 6 & ayear_rv < 6), vce(cluster mpio) 
quietly eststo w04: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 4 & ayear_rv < 4), vce(cluster mpio) 
quietly eststo w02: regress inac fsc lnG1 civ_prob_s nj_impacto [pweight = fex_c] if /* 
*/(ayear_rv > - 2 & ayear_rv < 2), vce(cluster mpio) 

coefplot w30 || w28 || w26 || w24 || w22 || w20 || w18 || w16 || w14  /*
*/ || w12 || w10 || w08 || w06|| w04 ,/*
*/ drop(_cons ) vertical bycoefs byopts(yrescale)

rdrobust inac fsc, c(0) bwselect(mserd)  covs(lnG1 civ_prob_s nj_impacto  p1687) weights(fex_c)

rdrobust score demvoteshare, c(0.5) bwselect(mserd) covs(pcturban pctblack) weights(fex_c)


