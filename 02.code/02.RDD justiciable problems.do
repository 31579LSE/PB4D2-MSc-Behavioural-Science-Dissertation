********************************************************************************
// Dissertation RDD ------------------------------------------------------------
********************************************************************************

* Inference and results

********************************************************************************
*net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
*net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
*net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
*net install rdmulti, from(https://raw.githubusercontent.com/rdpackages/rdmulti/master/stata) replace
*net install rdpower, from(https://raw.githubusercontent.com/rdpackages/rdpower/master/stata) replace

********************************************************************************
*net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace
********************************************************************************

********************************************************************************
// 0.0 Settings and data loading 
********************************************************************************

set more off
clear all

use "L:\99.LSE studies\99.Dissertation\01.data\01.ECSC 2020\dt_tabs5.dta"

cd "L:\99.LSE studies\99.Dissertation\01.data\99.results"

svyset keyhog [pweight=fex_c], /*
*/ vce(linearized) /*

*/ singleunit(missing) /*
*/ || keyper

*(dummy pre post) 12 PET
gen year12 = byear + 12
gen pos_cons12 = 0
replace pos_cons12 = 1 if year12 > 1991 // adult after 1991

gen ayear_rv12 = year12 - 1991

*(dummy pre post) 16 secundary
gen year16 = byear + 16
gen pos_cons16 = 0
replace pos_cons16 = 1 if year16 > 1991 // adult after 1991

gen ayear_rv16 = year16 - 1991

*(dummy pre post) 21
gen year25 = byear + 25
gen pos_cons25 = 0
replace pos_cons25 = 1 if byear > 1991

// adult after 1991

gen ayear_rv25 = year25 - 1991

*** placebo before 1991 (5 + 2) ------------------------------------------------

*(dummy pre post)

gen pos_86 = 0 
replace pos_86 = 1 if byear > 1986
label variable pos_86  "born after 1986"

* (re centered RV)
gen byear_rv86 = byear - 1986
label variable byear_rv86  "born after 1986 centred running variable"

*(dummy pre post)

gen pos_89 = 0 
replace pos_89 = 1 if byear > 1989
label variable pos_89  "born after 1989"

* (re centered RV)
gen byear_rv89 = byear - 1989
label variable byear_rv89  "born after 1989 centred running variable"


*** placebo after 1991 (5 + 2) ------------------------------------------------

*(dummy pre post)

gen pos_96 = 0 
replace pos_96 = 1 if byear > 1996
label variable pos_96  "born after 1996"

* (re centered RV)
gen byear_rv96 = byear - 1996
label variable byear_rv96  "born after 1996 centred running variable"

*(dummy pre post)

gen pos_93 = 0 
replace pos_93 = 1 if byear > 1993
label variable pos_93  "born after 1993"

* (re centered RV)
gen byear_rv93 = byear - 1993
label variable byear_rv93  "born after 1993 centred running variable"

********************************************************************************
* 1. Parametric approach 
********************************************************************************

// main analysis parameter (born before and after 1991)
 
gl rv  byear_rv
gl D   pos_cons_91
gl C    1991
gl hl   -10
gl hr   10
gl Y  inac
gl X  byear

gl rv2 ayear_rv
gl D2 a_pos_cons_91
gl X2 ayear

gl  v_per "pol_cont_per jud_cont_per p220  eap uni_educ p1988s1"
gl  v_pro " jp_g1 nj_impacto civ_prob_s p1685 p1687"
gl  v_all "inac nj_1_count jp_g1 nj_impacto civ_prob_s p1685 p1687 pol_cont_per jud_cont_per p220  p5785 eap uni_educ p1988s1 "
gl  v_con "nj_1_count jp_g1 nj_impacto civ_prob_s p1685 p1687 pol_cont_per jud_cont_per p220  eap uni_educ p1988s1 "

gl v_per1 "pol_cont_per jud_cont_per  p220   "

rdrobust $Y $rv , covs( $v_pro $v_per1 ) bwselect(msetwo)  level(90) kernel(triangular) vce(cluster depmuni)

gl  v_pro1 " nj_impacto p1685 p1687"


* ------------------------------------------------------------------------------
* 1.1.Balance at a 10 y cutoff  ------------------------------------------------
* ------------------------------------------------------------------------------

// balance on covariates

preserve
keep if  $rv > $hl
keep if  $rv  < $hr

rdbalance $D $v_all , /* 
*/ s(N mean sd median min max) /*
*/ wt(fex_c) /*
*/ f(%9.0g) /*
*/ saving(v11_balanceBD.xls, replace) excel
restore

preserve

keep if $rv >= $hl
keep if $rv <= $hr

file open table_1991 using "v11_balance_mdiffBD.csv", w replace
file write table_1991 /*
*/ "Trait (obs), Pre 1991, Post 1991, s1, s2, n1, n2, p value" _n

foreach var in  $v_all {
qui: ttest `var', by($D)
		local white_mean = round(r(mu_1),0.01)
		local black_mean = round(r(mu_2),0.01)
		local s1 = round(r(sd_1),0.01)
		local s2 = round(r(sd_2),0.01)
		local n1 = round(r(N_1),0.01)
		local n2 = round(r(N_2),0.01)
		local p = round(r(p),0.01)

file write table_1991 /*
*/ "`var' (`n'), `white_mean', `black_mean', `s1',`s2',`n1',`n2',`p'" _n
}

file close table_1991

restore

preserve

keep if $rv >= $hl
keep if $rv <= $hr

duplicates drop keyper, force
collapse (mean) age = p5785, by($D) 
list

restore

preserve


collapse (sum) fex = fex_c
list

restore

tab  p3013_cat_labens,sort

* ------------------------------------------------------------------------------
* 1.2.Graphical identification  ------------------------------------------------
* ------------------------------------------------------------------------------

* 1.2.1. general dsitribution --------------------------------------------------

preserve

collapse (sum) fex = fex_c, by($rv  $Y)  
sort $rv 

by $rv : egen yer_ln0 = sum(fex)
by $rv : egen yer_ln1 = sum(fex * inac)
by $rv : gen yer_ln2 = yer_ln1/yer_ln0

twoway scatter yer_ln2 $rv,/*
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

* 1.2.2. Graphical RDD ---------------------------------------------------------

preserve

gen rv = byear_rv


collapse (mean) Y = inac, by(rv) 

rdplot Y rv if rv >= $hl & rv <= $hr, binselect (qs) /*
 */kernel(triangular) /*
*/ ci(95)

restore

* ------------------------------------------------------------------------------
* 1.3. Manipulation test RDD ---------------------------------------------------
* ------------------------------------------------------------------------------

twoway (histogram $X if ($X< $C) , freq width(3) bcolor(red)) /*
       */ (histogram $X if ($X>= $C) , freq width(3) bcolor(blue) xline($C)), /*
	   */  leg(off)
	   
rddensity $X, c($C)
	   
preserve

rddensity $rv ,  plot  nohistogram fitselect(restricted) vce(plugin) all  h(10)
ereturn list
restore

* ------------------------------------------------------------------------------
* 1.4. Pre intervention covariates ---------------------------------------------
* ------------------------------------------------------------------------------

** Pre-intervention covariates and placebo outcomes

rdrobust $Y $rv, h(10) kernel(uni) vce(hc0)
rdplot $Y $rv if  $rv >= $hl & $rv <= $hr, p(2)
rdrobust $Y $rv, h(10) kernel(uni) vce(hc0)
rdplot $Y $rv, graph_options(leg(off) title((a) Inaction))


* Problem specific predeterminated covariates ----------------//

preserve
gen rv = $rv
gen Y = $Y
gen D = $D

keep if $rv >= $hl
keep if $rv <= $hr

global v "inac nj_1_count jp_g1 nj_impacto civ_prob_s p1685 p1687 pol_cont_per jud_cont_per p220  eap uni_educ p1988s1"

foreach j of varlist $v  {
reg `j' rv D [pweight = fex_c] , vce(cluster mpio) 
est store a_`j'
}


restore

* Plots - Problem and Indivual specific predeterminet covariates -------------//

foreach j of varlist $Y $v_pro $v_per  {

local lab : variable label `j'
rdrobust `j' $X, c($C) h($hr) kernel(uni)
rdplot 	 `j' $rv , graph_options(leg(off) title( `lab' ))
graph save rdd_`j', replace

}

graph combine rdd_inac.gph /*
*/rdd_jp_g1.gph /*
*/rdd_nj_impacto.gph /*
*/rdd_civ_prob_s.gph /*
*/rdd_p1685.gph /*
*/rdd_p1687.gph /*
*/rdd_pol_cont_per.gph /*
*/rdd_jud_cont_per.gph /*
*/rdd_p220.gph /*
*/rdd_eap.gph /*
*/rdd_uni_educ.gph /*
*/rdd_p1988s1.gph

* Plots - Problem and Indivual specific predeterminet covariates -------------//

foreach j of varlist $Y $v_pro $v_per  {

local lab : variable label `j' 	
rdrobust `j' $X, c($C) h($hr) kernel(uni)
rdplot 	 `j' $rv if $rv >= $hl & $rv <= $hr ,  /*
*/ graph_options(leg(off) title( `lab' )) p(2)
graph save rdd10_`j', replace

}

graph combine rdd10_inac.gph /*
*/rdd10_jp_g1.gph /*
*/rdd10_nj_impacto.gph /*
*/rdd10_civ_prob_s.gph /*
*/rdd10_p1685.gph /*
*/rdd10_p1687.gph /*
*/rdd10_pol_cont_per.gph /*
*/rdd10_jud_cont_per.gph /*
*/rdd10_p220.gph /*
*/rdd10_eap.gph /*
*/rdd10_uni_educ.gph /*
*/rdd10_p1988s1.gph

* ------------------------------------------------------------------------------
* 1.5. Sensitivity to band width -----------------------------------------------
* ------------------------------------------------------------------------------

preserve 

gen rv2  = $rv * $rv

forvalues j = 2/20  {

quietly eststo w`j' : reg $Y $rv  $D $v_pro $v_per [pweight = fex_c] if abs($rv) <= `j' , vce(cluster mpio) 

tab $D if abs($rv) <= `j'

}

coefplot w20 || w19 || w18 || w17 || w16 || w15 || w14 || w13 ||/*
*/		 w12 || w11 || w10 ||  w9 || w8  || w7  || w6  || w5  ||/*
*/       w4  || w3, /*
*/ drop(_cons $rv $v_pro $v_per) /*
*/ vertical /*
*/ bycoefs byopts(yrescale) /*
*/ yline(0) /*
*/ ciopts(recast(rcap))

graph save coef_plot2, replace 

graph combine coef_plot1.gph coef_plot2.gph 

esttab w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16 w17 w18 w19 w20 /*
*/ using "1.5.1.bw4_reg.rtf", b(%5.3f) se(%5.3f) replace

restore

* ------------------------------------------------------------------------------
* 1.6. donut hole test ---------------------------------------------------------
* ------------------------------------------------------------------------------

preserve 

keep if $rv >= $hl
keep if $rv <= $hr

 table $D


forvalues j = 1/6 {
	
 eststo w`j' : reg $Y $rv $D [pweight = fex_c] if abs($rv) >= `j' , vce(cluster mpio)

 keep if abs($rv) >= `j'
 table $D

}

coefplot w1 || w2 || w3 || w4 || w5 || w6 , /*
*/ drop(_cons $rv $v_pro $v_per) /*
*/ vertical /*
*/ bycoefs byopts(yrescale) /*
*/ yline(0) /*
*/ ciopts(recast(rcap))

esttab w1 w2 w3 w4/*

*/ using "1.6.1.donut_reg.rtf", b(%5.3f) p(%5.3f) replace


restore

* ------------------------------------------------------------------------------
* 1.7. Placebo cut-offs --------------------------------------------------------
* ------------------------------------------------------------------------------


*rigth from the cutoff

gen rva = $rv + 1 if ($rv <= $hr &  $rv >= 0)
tab  $rv rva, m

	gen rvb3 = rva - 3
		gen rvb5 = rva - 5
			gen rvb7 = rva - 7
			
	gen Db3 = 0 
	replace Db3 = 1 if rvb3 > 0		
		gen Db5 = 0 
		replace Db5 = 1 if rvb5 > 0		
			gen Db7 = 0 
			replace Db7 = 1 if rvb7 > 0		
	
tab  rvb3 Db3
tab  rvb5 Db5
tab  rvb7 Db7

tab  $rv Db3
tab  $rv Db5
tab  $rv Db7

foreach j in 3 5 7  {
quietly reg $Y rvb`j' Db`j' [pweight = fex_c]  if  ($rv >= 0 & $rv <= $hr), vce(cluster mpio)
estimates store rep_`j'
tab Db`j' if ($rv >= 0 & $rv <= $hr)
}

*left from the cutoff

gen rvc = ($rv*-1) if ($rv >= $hl &  $rv < 0)
tab  $rv rvc, m

	gen rvc3 = rvc - 3
		gen rvc5 = rvc - 6
			gen rvc7 = rvc - 9
			
	gen Dc3 = 0 
	replace Dc3 = 1 if rvc3 < 0		
		gen Dc5 = 0 
		replace Dc5 = 1 if rvc5 < 0		
			gen Dc7 = 0 
			replace Dc7 = 1 if rvc7 < 0		
	
tab  rvc3 Dc3
tab  rvc5 Dc5
tab  rvc7 Dc7

tab  $rv Dc3
tab  $rv Dc5
tab  $rv Dc7

tab  rvc Dc3
tab  rvc Dc5

foreach j in 3 5 7 {
quietly reg $Y rvc`j' Dc`j' [pweight = fex_c] if ($rv < 0 & $rv >= $hl), vce(cluster mpio)
estimates store repc_`j'
tab Dc`j' if ($rv < 0 & $rv >= $hl)
}

reg $Y $rv $D [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg151_1
tab $D if ($rv >= $hl & $rv <= $hr )

coefplot  repc_7 repc_5  reg151_1  rep_5 rep_7, /*
*/ drop(_cons rvb7 rvb5 rvb3 rvc7 rvc5 rvc3 $rv) /*
*/ vertical /*
*/ yline(0) /*
*/byopts(yrescale) /*
*/ ciopts(recast(rcap))


esttab reg151_1 rep_7 rep_5 repc_5 repc_7/*

*/ using "1.7.1.cutoff_reg.rtf", b(%5.3f) p(%5.3f) replace

* ------------------------------------------------------------------------------
* 1.8. Regression --------------------------------------------------------------
* ------------------------------------------------------------------------------


// checks on intercept difference methods 
//-----------------------------------------------------------
reg $Y $rv $D    [pweight = fex_c] if ($rv >= $hl & $rv <= $hr )

reg $Y $rv [pweight = fex_c] if $rv < 0 & $rv >= -10
 matrix coef_left = e(b)
 local intercept_left = coef_left[1, 2]
 
reg $Y $rv [pweight = fex_c] if $rv  >= 0 & $rv <= 10
 matrix coef_right = e(b)
 local intercept_right = coef_right[1, 2]
 
 display coef_right[1, 2]  - coef_left[1, 2]

//-----------------------------------------------------------

* 1.8.1. linar model -----------------------------------------------------------

reg $Y $rv $D [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg151_1

reg $Y $rv $D $v_pro [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
 */ vce(cluster mpio) 
est store reg151_2

reg $Y $rv $D $v_per  [pweight = fex_c]if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio) 
est store reg151_3

reg $Y $rv $D $v_pro $v_per  [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /* 
*/ vce(cluster mpio) 
est store reg151_4

tab $D  if ($rv >= $hl & $rv <= $hr )
esttab/*  
*/ reg151_1/*
*/ reg151_2/*
*/ reg151_3/*
*/ reg151_4/*
*/ using "1.5.1.linear_reg.rtf", b(%5.3f) se(%5.3f) replace

* 1.8.2. Polinomials -----------------------------------------------------------

preserve
gen Drv  = $rv * $D
gen rv2  = $rv * $rv
gen rv3  = $rv * rv2
gen rv4  = $rv * rv3
gen Drv2 = $D *  rv2

* interaction term
reg $Y $rv $D Drv [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_1
lincom $D + Drv

reg $Y $rv $D Drv $v_pro $v_per [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_12
lincom $D + Drv

*quadratic
reg $Y $rv rv2 $D  [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_2

reg $Y $rv rv2 $D  $v_pro $v_per [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_22

*quibic
reg $Y $rv rv3 $D  [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_3

reg $Y $rv rv3 $D  $v_pro $v_per [pweight = fex_c] if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_32

*quadratic interaction
reg $Y $rv rv2 $D Drv Drv2 [pweight = fex_c] /*
*/ if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_4

lincom $D + Drv + Drv2

reg $Y $rv rv2 $D Drv Drv2  $v_pro $v_per [pweight = fex_c] /*
*/ if ($rv >= $hl & $rv <= $hr ), /*
*/ vce(cluster mpio)
est store reg152_42

lincom $D + Drv + Drv2


esttab/*  
*/ reg152_1/*
*/ reg152_12/*
*/ reg152_2/*
*/ reg152_22/*
*/ reg152_3/*
*/ reg152_32/*
*/ reg152_4/*
*/ reg152_42/*
*/ using "1.5.2.poly_reg.rtf", b(%5.3f) se(%5.3f) replace


restore

********************************************************************************
* 2. Non Parametric approach 
******************************************************************************** 

rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) covs( $v_pro $v_per1 ) p(1)
rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1)

preserve
	collapse (sum) fex = fex_c, by($rv  $Y) 

by $rv : egen yer_ln0 = sum(fex)
by $rv : egen yer_ln1 = sum(fex*$Y)
by $rv : gen yer_ln2 = yer_ln1/yer_ln0

	rdrobust yer_ln2 $rv , kernel(uniform) vce(hc0)
	
restore

ereturn list

*-------------------------------------------------------------------------------
* 2.1. Bandwidths --------------------------------------------------------------
*-------------------------------------------------------------------------------

rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) covs( $v_pro $v_per) bwselect(mserd) level(90)


*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(mserd ) 
*est store rdrb12 level(90)

rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(msetwo) level(90)
 
*est store rdrb21
*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(msetwo  ) 
*est store rdrb22 level(90)

rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(cerrd) level(90)
est store rdrb31

*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(cerrd  ) 
*est store rdrb32 level(90)

rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) covs( $v_pro $v_per ) bwselect(certwo) level(90)
est store rdrb41

*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(certwo) level(90)
*est store rdrb42

rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) covs( $v_pro $v_per ) h(10) level(90)
est store rdrb41

esttab/*  
*/ rdrb11/*
*/ rdrb21/*
*/ rdrb31/*
*/ rdrb41/*
*/ using "2.1.rddRBci.rtf", b(%5.3f) e(pv_rb) replace

*-------------------------------------------------------------------------------
* 2.2. regresion --------------------------------------------------------------
*-------------------------------------------------------------------------------

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(mserd ) level(90)
est store rdrb11t 

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni)  bwselect(mserd ) level(90)
est store rdrb11t 

rdplot $Y $rv if  $rv >= -e(h_l)   & $rv <= e(h_r), p() graph_options(leg(off) title( mserd)) covs( $v_pro $v_per ) 
graph save rdrb11t, replace 
*rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni)   bwselect(mserd ) 
*est store rdrb12t level(90)

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(msetwo  ) level(90)
est store rdrb21t 

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro  )  bwselect(msetwo  ) level(90)
est store rdrb21t 

rdplot $Y $rv if  $rv >= -e(h_l)   & $rv <= e(h_r), p(1) graph_options(leg(off) title( msetwo))  covs( $v_pro $v_per ) 
graph save rdrb21t, replace 

*rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni)   bwselect(mserd ) 
*est store rdrb12t level(90)

*rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) bwselect(msetwo  ) 
*est store rdrb22t level(90)

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(cerrd ) level(90)
est store rdrb31t 

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni)   bwselect(cerrd ) level(90)
est store rdrb31t 

rdplot $Y $rv if  $rv >= -e(h_l)   & $rv <= e(h_r), p(2) graph_options(leg(off) title( cerrd))
graph save rdrb31t, replace 

*rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) p(1) bwselect(cerrd  ) 
*est store rdrb32t level(90)

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per ) bwselect(certwo  ) level(90) 
est store rdrb41t

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni)   bwselect(certwo ) level(90)
est store rdrb31t 

 
rdplot $Y $rv if  $rv >= -e(h_l)   & $rv <= e(h_r), p(2) graph_options(leg(off) title( certwo)) covs( $v_pro $v_per ) 
graph save rdrb41t, replace 

*rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) p(1) bwselect(certwo  ) 
*est store rdrb42t level(90)

rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per ) scaleregul(1)  h(10) level(90)
est store rdrb11t 

esttab/*  
*/ rdrb11t/*
*/ rdrb21t/*
*/ rdrb31t/*
*/ rdrb41t/*
*/ using "2.1.rddRBcit.rtf", b(%5.3f) ci(%5.3f) replace

graph combine rdrb11t.gph  rdrb31t.gph  rdrb21t.gph rdrb41t.gph 

*-------------------------------------------------------------------------------
* 2.3. balance in non parametric -----------------------------------------------
*-------------------------------------------------------------------------------

preserve

rdbwselect $Y $rv , kernel(triangular)  all vce(cluster depmuni) covs( $v_pro $v_per1 )

keep if $rv >= -e(h_msetwo_l)
keep if $rv <= e(h_msetwo_r)

file open table_1991 using "v11_balance_mdiffBD_np.csv", w replace
file write table_1991 /*
*/ "Trait (obs), Pre 1991, Post 1991, s1, s2, n1, n2, p value" _n

foreach var in  $v_all {
qui: ttest `var', by($D)
		local cmean = round(r(mu_1),0.01)
		local tmean = round(r(mu_2),0.01)
		local s1 = round(r(sd_1),0.01)
		local s2 = round(r(sd_2),0.01)
		local n1 = round(r(N_1),0.01)
		local n2 = round(r(N_2),0.01)
		local p = round(r(p),0.01)

file write table_1991 /*
*/ "`var' (`n'), `cmean', `tmean', `s1',`s2',`n1',`n2',`p'" _n
}

file close table_1991

restore

*-------------------------------------------------------------------------------
* 2.4. polinomials in non parametric -------------------------------------------
*-------------------------------------------------------------------------------

forvalues j = 1/3 {
	
eststo npreg_`j' : rdrobust $Y $rv , kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(msetwo ) level(90) p(`j') all

 rdplot $Y $rv if $rv >= -9 & $rv <= 7, p(`j') h(`bandwidth') kernel( triangular) covs $v_per $v_pro) vce(cluster depmuni) level(90)
 
 graph save npreg_`j', replace
 
}

esttab/*  
*/  npreg_1/*
*/  npreg_2/*
*/  npreg_3/*
*/ using "2.1.rddnp_msetwo.rtf", b(%5.3f) ci(%5.3f) replace


graph combine npreg_1.gph  npreg_2.gph  npreg_3.gph

*-------------------------------------------------------------------------------
* 2.5. predeterminated covarites in non paramtric ------------------------------
*-------------------------------------------------------------------------------

global v " inac nj_1_count jp_g1 nj_impacto civ_prob_s p1685 p1687 pol_cont_per jud_cont_per p220  eap uni_educ p1988s1 "

foreach y of global v  {
eststo w`y' : rdrobust `y' $rv,  vce(cluster depmuni) kernel(uniform) level(90) bwselect(msetwo)

}

rdrobust inac $rv,  vce(cluster depmuni) kernel(uniform) level(90) bwselect(msetwo)

esttab/*  
*/ wnj_1_count/*
*/ wjp_g1/*
*/ wnj_impacto/*
*/ wciv_prob_s/*
*/ wp1685/*
*/ wp1687/*
*/ wpol_cont_per/*
*/ wjud_cont_per/*
*/ wp220/*
*/ weap/*
*/ wuni_educ/*
*/ wp1988s1/*
*/ using "1.6.1.linear_reg_noparam.rtf", b(%5.3f) se(%5.3f) replace

*-------------------------------------------------------------------------------
* 2.6. placebo cut-off ---------------------------------------------------------
*-------------------------------------------------------------------------------

forvalues j = 4(1)10 {
  eststo w`j' : rdrobust $Y $rv if $rv < 0, c(-`j') covs( $v_pro $v_per ) bwselect(msetwo)  level(90) kernel(triangular) vce(cluster depmuni)
 
}

forvalues j = 4 {
  eststo l`j' :
rdrobust $Y $rv if $rv >= 0, c(5) covs( $v_pro $v_per ) bwselect(msetwo)  level(90) kernel(triangular) vce(cluster depmuni)
}

 eststo w0 :rdrobust $Y $rv , covs( $v_pro $v_per ) bwselect(msetwo)  level(90) kernel(triangular) vce(cluster depmuni) 

coefplot  w10 w9 w8 w7 w6 w5 w4 w0 l4 l5, /*
*/ drop(_cons  $rv) /*
*/ vertical /*
*/ yline(0) /*
*/ levels(90)  /*
*/ bycoefs byopts(yrescale) /*
*/ yline(0) /*
*/ ciopts(recast(rcap))

esttab/*  
*/ w10/*
*/ w9/*
*/ w8/*
*/ w7/*
*/ w6/*
*/ w5/*
*/ w4/*
*/ w0/*
*/ l4/*
*/ l5/*
*/ using "2.cuttofs_noparam.rtf", b(%5.3f) ci(%5.3f) replace

*-------------------------------------------------------------------------------
* 2.7. Placebo outcome test for non paramtric ----------------------------------
*-------------------------------------------------------------------------------

// inaction when facing a legal need -------------------------------------------

tab p1324, m
tab p1324 civ_prob_s, m


preserve

drop if missing(p1324)

 eststo w1: rdrobust $Y $rv if p3013 == 1401, covs($per) level(90) kernel(triangular)  vce(cluster depmuni) bwselect(msetwo) 

*rdplot $Y $rv if   $rv >= -e(h_l)   & $rv <= e(h_r),  covs(  $v_per ) bwselect(certwo)  level(90) kernel(triangular)  vce(cluster depmuni)  graph_options(leg(off) title( inaction)) p(1)
*graph save rdd_inacplace, replace 
restore

// efective reporting of a crime -----------------------------------------------

preserve

drop if missing(p1324)

 eststo w2: rdrobust p1324 $rv if p3013 == 1401,  covs($per)  level(90) kernel(triangular)  vce(cluster depmuni)  bwselect(msetwo)

*rdplot p1324 $rv if   $rv >= -e(h_l)   & $rv <= e(h_r),  covs( $v_per ) bwselect(certwo)  level(90) kernel(triangular)  vce(cluster depmuni) graph_options(leg(off) title( report)) p(1)
*graph save rdd_reportplace, replace 
restore

*---

esttab/*  
*/ w1/*
*/ w2/*

*/ using "8.1.placebo_report.rtf", b(%5.3f) ci(%5.3f) replace

graph combine rdd_reportplace.gph  rdd_inacplace.gph 


********************************************************************************
* 3. New runningvariable: adulthood 
********************************************************************************

*-------------------------------------------------------------------------------
* 3.1. age histogram 
*-------------------------------------------------------------------------------

preserve
*keep if ayear_rv > -21 
*keep if ayear_rv < 21 
gen adult = 0
replace adult = 1 if  p5785 >= 40

collapse (sum) fex_c =fex_c, by(p5785 civ_prob_s inac) 


sort p5785 civ_prob_s

by p5785 civ_prob_s  : egen yer_ln0 = sum(fex_c)
by p5785 civ_prob_s  : gen lnyer_ln0 = log(yer_ln0)
by p5785 civ_prob_s : egen yer_ln1 = sum(fex_c*inac)
by p5785 civ_prob_s  : gen yer_ln2 = yer_ln1/yer_ln0
twoway (scatter lnyer_ln0 p5785 , sort) (lfit lnyer_ln0 p5785 ), by(civ_prob_s) /*
*/ytitle("Inaction rate") /*
*/xtitle("Running variable centred at 1991 (Mayority of age after 1991)")

*levelsof p3013_cat_labens, local(cate)
*foreach j of local cate {

*byhist lnyer_ln0 if p3013_cat_labens == `"`j'"' , by (adult)
 
*graph save hist_age`j', replace

*}
tab lnyer_ln0, m sort


********************************************************************************
* 4. Fuzzy 
********************************************************************************

rdrobust $Y $rv , fuzzy($D) kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per) bwselect(mserd) level(90)


*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(mserd ) 
*est store rdrb12 level(90)

rdrobust $Y $rv , fuzzy($D) kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(msetwo) level(90)
 
*est store rdrb21
*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(msetwo  ) 
*est store rdrb22 level(90)

rdrobust $Y $rv , fuzzy($D) kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per )  bwselect(cerrd) level(90)
est store rdrb31

*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(cerrd  ) 
*est store rdrb32 level(90)

rdrobust $Y $rv , fuzzy($D) kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per ) bwselect(certwo) level(90)
est store rdrb41

*rdrobust $Y $rv , kernel(uniform) vce(cluster depmuni) p(1) bwselect(certwo) level(90)
*est store rdrb42

rdrobust $Y $rv , fuzzy($D) kernel(triangular) vce(cluster depmuni) covs( $v_pro $v_per ) h(10) level(90)
est store rdrb41


* placebo cut-off fuzzy ----------------------------------


global v " inac nj_1_count jp_g1 nj_impacto civ_prob_s p1685 p1687 pol_cont_per jud_cont_per p220  eap uni_educ p1988s1 "

foreach y of global v  {
eststo w`y' : 	rdrobust `y' $rv, fuzzy($D)  kernel(triangular) vce(cluster depmuni)   bwselect(msetwo) level(90)

}

esttab winac wnj_1_count wnj_impacto wciv_prob_s wp1685 wp1687 wpol_cont_per wjud_cont_per wp220 weap wuni_educ wp1988s1 /*
*/ using "4.21.bw4_reg.rtf", b(%5.3f) se(%5.3f) replace

restore
