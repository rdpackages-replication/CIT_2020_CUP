*-------------------------------------------------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------------*
* A Practical Introduction to Regression Discontinuity Designs: Foundations
* Authors: Matias D. Cattaneo, Nicolás Idrobo and Rocío Titiunik
* Last update: 2023-01-23
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
* needed to replicate the results in the monograph. For example:
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
* replicate the results in the monograph. For example:
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

** A folder called "outputs" needs to be created in order to store
** all of the figures, logs and tables. If the folder already exists,
** the user will get an error message but the code will not stop.
capture noisily mkdir "outputs"

** Tables 1, 2, 4, 5, and 6 are only constructed in STATA
** Figures 20, 21, and 22 are only constructed in STATA

** Loading the data
use "CIT_2020_CUP_polecon.dta", clear

*---------------------*
* Section 2           *
* The Sharp RD Design *
*---------------------*
** Figure 3a
** Raw comparison of means
rdplot Y X, nbins(2500 500) p(0) graph_options(xtitle("Islamic Margin of Victory") ///
	ytitle("Female High School Percentage") ylabel(0(10)70))

** Figure 3b
** Local comparison of means
rdplot Y X if abs(X)<=50, nbins(2500 500) p(4) graph_options(xtitle("Islamic Margin of Victory") ///
	ytitle("Female High School Percentage") ylabel(0(10)70))

** Table 1
** Descriptive statistics for Meyersson
global sumstats "Y X T hischshr1520m vshr_islam1994 partycount lpop1994 ageshr19 ageshr60 sexr shhs merkezi merkezp subbuyuk"
matrix define R = J(14, 5, .)
local k = 1
foreach x of global sumstats {
	quietly summarize `x', detail
	local label_`k': variable label `x'
	matrix R[`k', 1] = r(mean)
	matrix R[`k', 2] = r(p50)
	matrix R[`k', 3] = r(sd)
	matrix R[`k', 4] = r(min)
	matrix R[`k', 5] = r(max)
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
	save "outputs/Vol-1-STATA_meyersson_descstats.dta", replace
restore

*-----------*
* Section 3 *
* RD Plots  *
*-----------*
** Stata Snippet 1 (Figure 5)
** Scatter plot
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_raw", replace
twoway (scatter Y X, ///
	mcolor(black) xline(0, lcolor(black))), ///
	graphregion(color(white)) ytitle(Outcome) ///
	xtitle(Score)
sjlog close, replace

** Table 2
** Partition of Islamic margin of victory into 40 bins of equal length
preserve
	rdplot Y X, nbins(20 20) genvars support(-100 100)
	gen obs = 1
	collapse (mean) rdplot_mean_x rdplot_mean_y (sum) obs, by(rdplot_id)
	order rdplot_id
	sort rdplot_id
restore

** Figure 6
** RD plot using 40 bins of equal length
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_esmv_20bins", replace
rdplot Y X, nbins(20 20) binselect(esmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

** Stata Snippet 2 (Figure 7a)
** 40 Evenly-spaced bins
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_es_20bins", replace
rdplot Y X, nbins(20 20) binselect(es) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

** Stata Snippet 3 (Figure 7b)
** 40 Quantile-spaced bins
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_qs_20bins", replace
rdplot Y X, nbins(20 20) binselect(qs) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

** Stata Snippet 4 (Figure 8)
** IMSE RD plot with evenly-spaced bins
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_es", replace
rdplot Y X, binselect(es) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

** Stata Snippet 5 (Figure 9)
** IMSE RD plot with quantile-spaced bins
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_qs", replace
rdplot Y X, binselect(qs) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

** Stata Snippet 6 (Figure 10)
** Mimicking variance RD plot with evenly-spaced bins
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_esmv", replace
rdplot Y X, binselect(esmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

** Stata Snippet 7 (Figure 11)
** Mimicking variance RD plot with quantile-spaced bins
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_qsmv", replace
rdplot Y X, binselect(qsmv) ///
	graph_options(graphregion(color(white)) ///
	xtitle(Score) ytitle(Outcome))
sjlog close, replace

*----------------------------------------------*
* Section 4                                    *
* The Continuity-Based Approach to RD Analysis *
*----------------------------------------------*
** Stata Snippet 8
** Using two regressions to estimate
sjlog using "outputs/Vol-1-STATA_meyersson_manualreg_tworegs_uniform_adhoc_p1", replace
reg Y X if X < 0 & X >= -20
matrix coef_left = e(b)
local intercept_left = coef_left[1, 2]
reg Y X if X >= 0 & X <= 20
matrix coef_right = e(b)
local intercept_right = coef_right[1, 2]
local difference = `intercept_right' - `intercept_left'
display "The RD estimator is `difference'"
sjlog close, replace

** Stata Snippet 9
** Using one regression to estimate
sjlog using "outputs/Vol-1-STATA_meyersson_manualreg_onereg_uniform_adhoc_p1", replace
gen T_X = X * T
reg Y X T T_X if X >= -20 & X <= 20
sjlog close, replace

** Stata Snippet 10
** Generating triangular weights
sjlog using "outputs/Vol-1-STATA_meyersson_manualreg_weights_triangular_adhoc_p1", replace
gen weights = .
replace weights = (1 - abs(X / 20)) if X < 0 & X >= -20
replace weights = (1 - abs(X / 20)) if X >= 0 & X <= 20
sjlog close, replace

** Stata Snippet 11
** Using two regressions and weights to estimate
sjlog using "outputs/Vol-1-STATA_meyersson_manualreg_tworegs_triangular_adhoc_p1", replace
reg Y X [aw = weights] if X < 0 & X >= -20
matrix coef_left = e(b)
local intercept_left = coef_left[1, 2]
reg Y X [aw = weights] if X >= 0 & X <= 20
matrix coef_right = e(b)
local intercept_right = coef_right[1, 2]
local difference = `intercept_right' - `intercept_left'
display "The RD estimator is `difference'"
sjlog close, replace

** Stata Snippet 12
** Using rdrobust with uniform weights
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_uniform_adhoc_p1_rho1_regterm1", replace
rdrobust Y X, kernel(uniform) p(1) h(20)
sjlog close, replace

** Stata Snippet 13
** Using rdrobust with triangular weights
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_adhoc_p1_rho1_regterm1", replace
rdrobust Y X, kernel(triangular) p(1) h(20)
sjlog close, replace

** Stata Snippet 14
** Using rdrobust with triangular weights and p  =  2
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_adhoc_p2_rho1_regterm1", replace
rdrobust Y X, kernel(triangular) p(2) h(20)
sjlog close, replace

** Stata Snippet 15
** Using rdbwselect with mserd bandwidth
sjlog using "outputs/Vol-1-STATA_meyersson_rdbwselect_triangular_mserd_p1_regterm1_all", replace
rdbwselect Y X, kernel(triangular) p(1) bwselect(mserd)
sjlog close, replace

** Stata Snippet 16
** Using rdbwselect with msetwo bandwidth
sjlog using "outputs/Vol-1-STATA_meyersson_rdbwselect_triangular_msetwo_p1_regterm1_all", replace
rdbwselect Y X, kernel(triangular) p(1) bwselect(msetwo)
sjlog close, replace

** Stata Snippet 17
** Using rdrobust with mserd bandwidth
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1", replace
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
sjlog close, replace

** Stata Snippet 18
** Using rdrobust to show the objects it returns
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_namescoefsout_all", replace
rdrobust Y X
ereturn list
sjlog close, replace

** Stata Snippet 19 (Figure 15)
** Using rdrobust and showing the associated rdplot
sjlog using "outputs/Vol-1-STATA_meyersson_rdplot_maineffect", replace
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
local bandwidth = e(h_l)
rdplot Y X if abs(X) <= `bandwidth', p(1) h(`bandwidth') kernel(triangular)
sjlog close, replace

** Stata Snippet 20
** Using rdrobust without regularization term
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm0", replace
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd) scaleregul(0)
sjlog close, replace

** Stata Snippet 21
** Using rdrobust with default options
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1", replace
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd)
sjlog close, replace

** Stata Snippet 22
** Using rdrobust with default options and showing all the output
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_rhofree_regterm1_all", replace
rdrobust Y X, kernel(triangular) p(1) bwselect(mserd) all
sjlog close, replace

** Stata Snippet 23
** Using rdrobust with cerrd bandwidth
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_cerrd_p1_rhofree_regterm1", replace
rdrobust Y X, kernel(triangular) p(1) bwselect(cerrd)
sjlog close, replace

** Stata Snippet 24
** Using rdbwselect with all the bandwidths
sjlog using "outputs/Vol-1-STATA_meyersson_rdbwselect_triangular_all_p1_regterm1", replace
rdbwselect Y X, kernel(triangular) p(1) all
sjlog close, replace

** Stata Snippet 25
** Using rdbwselect with covariates
sjlog using "outputs/Vol-1-STATA_meyersson_rdbwselect_triangular_mserd_p1_regterm1_covariates_noi89", replace
global covariates "vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"
rdbwselect Y X, covs($covariates) p(1) kernel(triangular) bwselect(mserd) scaleregul(1)
sjlog close, replace

** Stata Snippet 26
** Using rdrobust with covariates
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89", replace
global covariates "vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"
rdrobust Y X, covs($covariates) p(1) kernel(triangular) bwselect(mserd) scaleregul(1)
sjlog close, replace

** Stata Snippet 27
** Using rdrobust with clusters
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_regterm1_clusters", replace
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd) ///
	scaleregul(1) vce(nncluster prov_num)
sjlog close, replace

** Stata Snippet 28
** Using rdrobust with clusters and covariates
sjlog using "outputs/Vol-1-STATA_meyersson_rdrobust_triangular_mserd_p1_regterm1_covariates_noi89_clusters", replace
global covariates "vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"
rdrobust Y X, covs($covariates) p(1) kernel(triangular) bwselect(mserd) ///
	scaleregul(1) vce(nncluster prov_num)
sjlog close, replace

*-----------------------------------------------*
* Section 5                                     *
* Validation and Falsification of the RD Design *
*-----------------------------------------------*
** Figure 16
** Rd plots for predetermined covariates
foreach y of varlist lpop1994 partycount vshr_islam1994 i89 merkezp merkezi {
	rdplot `y' X, graph_options(xtitle("Score"))
}

** Stata Snippet 29
** Using rdrobust on lpop1994
sjlog using "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_lpop1994", replace
rdrobust lpop1994 X
sjlog close, replace

** Table 4
** Formal continuity-based analysis for covariates
global covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"
matrix define R = J(9, 8, .)
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
	save "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_allcovariates.dta", replace
restore

** Formal continuity-based analysis for covariates using CER-optimal bandiwdth (not reported in the text)
foreach y of global covariates {
	rdrobust `y' X, all bwselect(cerrd)
}

** Stata Snippet 30
** Using rdplot to show the rdrobust effect for lpop1994
sjlog using "outputs/Vol-1-STATA_meyersson_falsification_rdplot_rdrobust_lpop1994", replace
rdrobust lpop1994 X
local bandwidth = e(h_l)
rdplot lpop1994 X if abs(X) <= `bandwidth', h(`bandwidth') p(1) kernel(triangular)
sjlog close, replace

** Figure 17
** Graphical illustration of local linear RD effects for predetermined covariates
foreach y of varlist lpop1994 partycount vshr_islam1994 i89 {
	rdrobust `y' X
	local bandwidth = e(h_l)
	rdplot `y' X if abs(X)<=`bandwidth', p(1) kernel(triangular) ///
		graph_options(xtitle("Score"))
}

** Stata Snippet 31
** Binomial test
sjlog using "outputs/Vol-1-STATA_meyersson_falsification_binomial_byhand_adhoc", replace
bitesti 100 53 1/2
sjlog close, replace

** Stata Snippet 32
** Using rddensity
sjlog using "outputs/Vol-1-STATA_meyersson_falsification_rddensity", replace
rddensity X
sjlog close, replace

** Figure 19a
** Histogram
rddensity X
local bandwidth_left = e(h_l)
local bandwidth_right = e(h_r)
twoway (histogram X if X >= -`bandwidth_left' & X < 0, freq width(1) color(blue)) ///
	(histogram X if X >= 0 & X <= `bandwidth_right', freq width(1) color(red)), xlabel(-30(10)30) ///
	graphregion(color(white)) xtitle(Score) ytitle(Number of Observations) legend(off)

** Figure 19b
** Estimated density
local bandwidth_left = e(h_l)
local bandwidth_right = e(h_r)
rddensity X, plot plot_range(-`bandwidth_left' `bandwidth_right')

** Stata Snippet 33
** Using rdrobust with the cutoff equal to 1
sjlog using "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_alternative-cutoff_c1", replace
rdrobust Y X if X >= 0, c(1)
sjlog close, replace

** Table 5 (Figure 20)
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

	** RD Estimation for Placebo Cutoffs
	twoway (rcap R7 R8 R1, lcolor(navy)) ///
		(scatter R3 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), ///
		graphregion(color(white)) xlabel(-3(1)3) xtitle("Cutoff (x=0 true cutoff)") ///
		ytitle("RD Treatment Effect") legend(off)
	graph export "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_alternative-cutoffs.pdf", replace

	save "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_alternative-cutoffs.dta", replace
restore

** Stata Snippet 34
** Using rdrobust for the donut-hole approach
sjlog using "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_donuthole", replace
rdrobust Y X if abs(X) >= 0.3
sjlog close, replace

** Table 6 (Figure 21)
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

	** RD estimation for the donut-hole approach
	twoway (rcap R7 R8 R1, lcolor(navy)) ///
		(scatter R3 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), ///
		graphregion(color(white)) xlabel(0(0.1)0.5) xtitle("Donut Hole Radius") ///
		ytitle("RD Treatment Effect") legend(off)
	graph export "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_donuthole.pdf", replace

	save "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_donuthole_table.dta", replace
restore

** Figure 22
** Sensitivity to bandwidth in the continuity-based approach
matrix define R = J(4, 9, .)
global bandwidths "17.239 34.478 11.629 23.258"
local r = 1
foreach k of global bandwidths {
	rdrobust Y X, h(`k')

	matrix R[`r', 1] = `k'
	matrix R[`r', 2] = e(h_l)
	matrix R[`r', 3] = e(tau_cl)
	matrix R[`r', 4] = e(tau_bc)
	matrix R[`r', 5] = e(se_tau_rb)
	matrix R[`r', 6] = 2 * normal(-abs(R[`r', 4] / R[`r', 5]))
	matrix R[`r', 7] = R[`r', 4] - invnormal(0.975) * R[`r', 5]
	matrix R[`r', 8] = R[`r', 4] + invnormal(0.975) * R[`r', 5]
	matrix R[`r', 9] = e(N_h_l) + e(N_h_r)

	local r = `r' + 1
}

clear
svmat R

twoway (rcap R7 R8 R1, lcolor(navy)) ///
	(scatter R3 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), ///
	graphregion(color(white)) xlabel(17.239 34.478 11.629 23.258) ///
	ytitle("RD Treatment Effect") legend(off) xtitle("Bandwidth")

graph export "outputs/Vol-1-STATA_meyersson_falsification_rdrobust_sensitivity.pdf", replace

*-------------------------------------------------------------------------------------------------------------*
clear all
