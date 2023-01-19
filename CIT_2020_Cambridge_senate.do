*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
* A Practical Introduction to Regression Discontinuity Designs
* Authors: Matias D. Cattaneo, Nicolas Idrobo and Rocio Titiunik
* Last update: 21-AGO-2020
*-------------------------------------------------------------------------------------------------------------*
* SOFTWARE WEBSITE: https://rdpackages.github.io/
*-------------------------------------------------------------------------------------------------------------*
* TO INSTALL STATA PACKAGES:
* RDROBUST: net install rdrobust, from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
* RDDENSITY: net install rddensity, from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
* RDLOCRAND: net install rdlocrand, from(https://raw.githubusercontent.com/rdpackages/rdlocrand/master/stata) replace
* SJLATEX: type "findit sjlatex" and install from "sjlatex from http://www.stata-journal.com/production"
*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
* NOTE: If you are using RDROBUST version 2020 or newer, the option "masspoints(off) stdvars(on)" may be
* needed to replicate the results. For example:
*
*     rdrobust Y X
*
* should be replaced by:
*
*     rdrobust Y X, masspoints(off) stdvars(on)
*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
* NOTE: If you are using RDDENSITY version 2020 or newer, the option "nomasspoints" may be needed to 
* replicate the results. For example:
*
*     rddensity X
*
* should be replaced by:
*
*     rddensity X, nomasspoints
*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
clear
clear all
clear matrix
cap log close
set more off

** Loading the data
use "CIT_2018_Cambridge_senate.dta", clear

** Re-labeling the three main variables
rename demmv X
rename demvoteshfor2 Y

gen T=.
replace T=0 if X<0 & X!=.
replace T=1 if X>=0 & X!=.
label var T "Democratic Win at t"

order X Y T

*-------------------------------*
* Section 2                     *
* The Caninical Sharp RD Design *
*-------------------------------*
** Figure 2.3a
** Raw comparison of means
rdplot Y X, nbins(2500 500) p(0)

** Figure 2.3b
** Local comparison of means
rdplot Y X if abs(X)<=50, nbins(2500 500) p(4)

** Table 2.1
** Descriptive statistics
global sumstats "X Y T presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen"
matrix define R = J(11, 6, .)
local k = 1
foreach x of global sumstats {
	quietly summarize `x', detail
	local label_`k': variable label `x'
	matrix R[`k', 1] = r(mean)
	matrix R[`k', 2] = r(p50)
	matrix R[`k', 3] = r(sd)
	matrix R[`k', 4] = r(min)
	matrix R[`k', 5] = r(max)
	matrix R[`k', 6] = r(N)
	local k = `k' + 1
}

preserve
	clear
	local t = `k' - 1
	svmat R
	gen R0 = ""
	forvalues k = 1 / `t' {
		replace R0 = "`label_`k''" if _n == `k'
	}
	order R0
restore

*-----------*
* Section 3 *
* RD Plots  *
*-----------*
** Code snippet 1 (Figure 3.1)
** Scatter plot
twoway (scatter Y X, ///
	mcolor(black) xline(0, lcolor(black))), ///
	graphregion(color(white)) ytitle(Outcome) ///
	xtitle(Score)

** Table 3.1 (no output)
** Partition of Islamic margin of victory into 40 bins of equal length
preserve
	rdplot Y X, nbins(20 20) genvars support(-100 100)
	gen obs = 1
	collapse (mean) rdplot_mean_x rdplot_mean_y (sum) obs, by(rdplot_id)
	order rdplot_id
	sort rdplot_id
restore

** Code snippet 2 (Figure 3.2)
** RD plot using 40 bins of equal length
rdplot Y X, nbins(20 20) binselect(esmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

** Code snippet 3 (Figure 3.3)
** 40 Evenly-spaced bins
rdplot Y X, nbins(20 20) binselect(es) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

** Code snippet 4 (Figure 3.4)
** 40 Quantile-spaced bins
rdplot Y X, nbins(20 20) binselect(qs) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

** Code snippet 5 (Figure 3.6)
** IMSE RD plot with evenly-spaced bins
rdplot Y X, binselect(es) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

** Code snippet 6 (Figure 3.7)
** IMSE RD plot with quantile-spaced bins
rdplot Y X, binselect(qs) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

** Code snippet 7 (Figure 3.8)
** Mimicking variance RD plot with evenly-spaced bins
rdplot Y X, binselect(esmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))

** Code snippet 8 (Figure 3.9)
** Mimicking variance RD plot with quantile-spaced bins
rdplot Y X, binselect(qsmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))








*----------------------------------------------*
* Section 4                                    *
* The Continuity-Based Approach to RD Analysis *
*----------------------------------------------*
** Code snippet 1
** Using two regressions to estimate
reg Y X if X < 0 & X >= -10
matrix coef_left = e(b)
local intercept_left = coef_left[1, 2]
reg Y X if X >= 0 & X <= 10
matrix coef_right = e(b)
local intercept_right = coef_right[1, 2]
local difference = `intercept_right' - `intercept_left'
display "The RD estimator is `difference'"

** Code snippet 2
** Using one regression to estimate
gen T_X = X * T
reg Y X T T_X if X >= -10 & X <= 10

** Code snippet 3
** Generating triangular weights
gen weights = .
replace weights = (1 - abs(X / 10)) if X < 0 & X >= -10
replace weights = (1 - abs(X / 10)) if X >= 0 & X <= 10

** Code snippet 4
** Using two regressions and weights to estimate
reg Y X [aw = weights] if X < 0 & X >= -10
matrix coef_left = e(b)
local intercept_left = coef_left[1, 2]
reg Y X [aw = weights] if X >= 0 & X <= 10
matrix coef_right = e(b)
local intercept_right = coef_right[1, 2]
local difference = `intercept_right' - `intercept_left'
display "The RD estimator is `difference'"

** Code snippet 5
** Using rdrobust with uniform weights
rdrobust Y X, kernel(uniform) p(1) h(10)  

** Code snippet 6
** Using rdrobust with triangular weights
rdrobust Y X, kernel(triangular) p(1) h(10)  

** Code snippet 7
** Using rdrobust with triangular weights and p  =  2
rdrobust Y X, kernel(triangular) p(2) h(10)  

** Code snippet 8
** Using rdbwselect with mserd bandwidth
rdbwselect Y X, kernel(triangular) p(1) bwselect(mserd) 

** Code snippet 9
** Using rdbwselect with msetwo bandwidth
rdbwselect Y X, kernel(triangular) p(1) bwselect(msetwo) 

** Code snippet 10
** Using rdrobust with mserd bandwidth
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)  

** Code snippet 11
** Using rdrobust to show the objects it returns
rdrobust Y X
ereturn list

** Code snippet 12
** Using rdrobust and showing the associated rdplot
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
local bandwidth = e(h_l)
rdplot Y X if abs(X) <= `bandwidth', p(1) h(`bandwidth') kernel(triangular)

** Code snippet 13
** Using rdrobust without regularization term
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd) scaleregul(0)

** Code snippet 14
** Using rdrobust with default options
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)  

** Code snippet 15
** Using rdrobust with default options and showing all the output
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd) all

** Code snippet 16
** Using rdrobust with cerrd bandwidth
rdrobust Y X, kernel(triangular) p(1) bwselect(cerrd)

** Code snippet 17
** Using rdbwselect with all the bandwidths
rdbwselect Y X, kernel(triangular) p(1) all

** Code snippet 18
** Using rdbwselect with covariates
global covariates "presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen"
rdbwselect Y X, covs($covariates) p(1) kernel(triangular) bwselect(mserd) scaleregul(1)

** Code snippet 19
** Using rdrobust with covariates
global covariates "presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen"
rdrobust Y X, covs($covariates) p(1) kernel(triangular) bwselect(mserd) scaleregul(1)

*-----------------------------------------------*
* Section 5                                     *
* Validation and Falsification of the RD Design *
*-----------------------------------------------*
** Figure 5.1 (no output)
** Rd plots for predetermined covariates
foreach y of varlist presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen {
	rdplot `y' X, graph_options(xtitle("Score"))
}

** Code snippet 1
** Using rdrobust on demvoteshlag1
rdrobust demvoteshlag1 X

** Table 5.1
** Formal continuity-based analysis for covariates
global covariates "presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen"
matrix define R = J(8, 8, .)
local k = 1
foreach y of global covariates {
	rdrobust `y' X, all
	local label_`k': variable label `y'
	matrix R[`k', 1] = e(h_l)
	matrix R[`k', 2] = e(tau_cl)
	matrix R[`k', 3] = e(tau_bc)
	matrix R[`k', 4] = e(se_tau_rb)
	matrix R[`k', 5] = 2 * normal(-abs(R[`k', 3] / R[`k', 4]))
	matrix R[`k', 6] = R[`k', 3] - invnormal(0.975) * R[`k', 4]
	matrix R[`k', 7] = R[`k', 3] + invnormal(0.975) * R[`k', 4]
	matrix R[`k', 8] = e(N_h_l) + e(N_h_r)
	
	local k = `k' + 1
}

preserve
	clear
	local t = `k' - 1
	svmat R
	gen R0 = ""
	forvalues k = 1 / `t' {
		replace R0 = "`label_`k''" if _n == `k'
	}
	order R0
restore

** Code snippet 2
** Using rdplot to show the rdrobust effect for demvoteshlag1
rdrobust demvoteshlag1 X
local bandwidth = e(h_l)
rdplot demvoteshlag1 X if abs(X) <= `bandwidth', h(`bandwidth') p(1) kernel(triangular)

** Figure 5.2 (no output)
** Graphical illustration of local linear RD effects for predetermined covariates
foreach y of varlist presdemvoteshlag1 demvoteshlag1 demvoteshlag2 demwinprv1 demwinprv2 dmidterm dpresdem dopen {
	rdrobust `y' X
	local bandwidth = e(h_l)
	rdplot `y' X if abs(X)<=`bandwidth', p(1) kernel(triangular) ///
		graph_options(xtitle("Score"))
}

** Code snippet 3
** Using rddensity
rddensity X

** Figure 5.5a (no output)
** Histogram
rddensity X
local bandwidth_left = e(h_l)
local bandwidth_right = e(h_r)
twoway (histogram X if X >= -`bandwidth_left' & X < 0, freq width(1) color(blue)) ///
	(histogram X if X >= 0 & X <= `bandwidth_right', freq width(1) color(red)), xlabel(-20(10)30) ///
	graphregion(color(white)) xtitle(Score) ytitle(Number of Observations) legend(off)
	
** Figure 5.5b (no output)
** Estimated density
rddensity X, plot plot_range(-`bandwidth_left' `bandwidth_right')

** Code snippet 4
** Using rdrobust with the cutoff equal to 1
rdrobust Y X if X >= 0, c(1)

** Table 5.2
** Continuity-based analysis for alternative cutoffs
matrix define R = J(7,10,.)
local k = 1
forvalues x = -3(1)3 {
	if `x' > 0 {
		local condition = "if X >= 0"
	}
	else if `x' < 0 {
		local condition = "if X < 0"
	}
	else {
		local condition = ""
	}
	
	rdrobust Y X `condition', c(`x')
	matrix R[`k', 1] = `x'
	matrix R[`k', 2] = e(h_l)
	matrix R[`k', 3] = e(tau_cl)
	matrix R[`k', 4] = e(tau_bc)
	matrix R[`k', 5] = e(se_tau_rb)
	matrix R[`k', 6] = 2 * normal(-abs(R[`k', 4] / R[`k', 5]))
	matrix R[`k', 7] = R[`k', 4] - invnormal(0.975) * R[`k', 5]
	matrix R[`k', 8] = R[`k', 4] + invnormal(0.975) * R[`k', 5]
	matrix R[`k', 9] = e(N_h_l)
	matrix R[`k', 10] = e(N_h_r)
	
	local k = `k' + 1
}

preserve
	clear
	svmat R
	
	** Figure 5.5
	** RD Estimation for Placebo Cutoffs
	twoway (rcap R7 R8 R1, lcolor(navy)) /*
	*/ (scatter R3 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), /*
	*/ graphregion(color(white)) xlabel(-3(1)3) xtitle("Cutoff (x=0 true cutoff)") ytitle("RD Treatment Effect") /*
	*/ legend(off)
restore

** Code snippet 5
** Using rdrobust for the donut-hole approach
rdrobust Y X if abs(X) >= 0.3

** Table 5.3
** Continuity-based analysis for the donut-hole approach
matrix define R = J(6, 11, .)
local r = 1
forvalues k = 0(0.1)0.5 {
	rdrobust Y X if abs(X) >= `k'
	
	matrix R[`r', 1] = `k'
	matrix R[`r', 2] = e(h_l)
	matrix R[`r', 3] = e(tau_cl)
	matrix R[`r', 4] = e(tau_bc)
	matrix R[`r', 5] = e(se_tau_rb)
	matrix R[`r', 6] = 2 * normal(-abs(R[`r', 4] / R[`r', 5]))
	matrix R[`r', 7] = R[`r',4] - invnormal(0.975) * R[`r', 5]
	matrix R[`r', 8] = R[`r',4] + invnormal(0.975) * R[`r', 5]
	matrix R[`r', 9] = e(N_h_l) + e(N_h_r)
	count if abs(X) < `k' & X < 0
	matrix R[`r', 10] = r(N)
	count if abs(X) < `k' & X >= 0
	matrix R[`r', 11] = r(N)
	
	local r = `r' + 1
}

preserve
	clear
	svmat R

	** Figure 5.6
	** RD estimation for the donut-hole approach
	twoway (rcap R7 R8 R1, lcolor(navy)) /*
	*/ (scatter R3 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), /*
	*/ graphregion(color(white)) xlabel(0(0.1)0.5) xtitle("Donut Hole Radius") ytitle("RD Treatment Effect") /*
	*/ legend(off)
restore

*-------------------------------------------------------------------------------------------------------------*
clear all
