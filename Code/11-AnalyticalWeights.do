********************
*** INTRODUCTION ***
********************
// This .do-file computes the error as a function of data coverage using distributional simulations with different weights.
// Set working directory
cd "C:\Users\WB514665\OneDrive - WBG\DECDG\Aggregation\RR"

********************
*** PREPARE DATA ***
********************
// Select number of distributions to create
global simulations = 1000
clear
set obs `=217*$simulations'
gen iteration = floor(_n/217-0.00001)+1
// Create normally distributed drwas
gen value_raw = rnormal()
// Create two different types of weights
gen weight1 = 1
gen weight2 = rweibull(0.5,1)
sum weight2
replace weight2=weight2/r(sum)*217*$simulations
sum weight*
// Create standardized versions of values
foreach weight in 1 2 {
gen    value_std`weight' = .
forvalues iteration=1/$simulations  {
qui sum value_raw [aw=weight`weight']                         if iteration==`iteration'
qui replace value_std`weight' = (value_raw-`r(mean)')/`r(sd)' if iteration==`iteration'
}
}
drop value_raw

****************************************************************
*** CALCULATE MEAN VALUES WHEN RANDOMLY DROPPING INFORMATION ***
****************************************************************
// Using each simulation 1000 times, where 0.1%-99.9% of the data are being kept. 
foreach weight in 1 2 {
forvalues i=1/1000 {
disp in red "`weight'_`i'"
// Randomly remove `i'% of the data
quietly gen weight`weight'_`i' = weight`weight' if runiform()<`i'/1000
// Calculate the global mean with remaining data
quietly bysort iteration: egen mean_std`weight'_`i' = wtmean(value_std`weight'), weight(weight`weight'_`i')
// Calculate the global population coverage
quietly bysort iteration: egen sumweight`weight'_`i' = sum(weight`weight'_`i')
drop weight`weight'_`i' 
}
}

**************************
*** CALCULATE COVERAGE ***
**************************
drop weight* value_std*
duplicates drop
reshape long mean_std1 mean_std2 sumweight1 sumweight2, i(iteration) j(type) string
drop iteration type
// Calculate coverage
foreach weight in 1 2 {
egen total`weight'= max(sumweight`weight')
gen share`weight' = sumweight`weight'/total`weight'
drop total`weight' sumweight`weight'
replace mean_std`weight' = abs(mean_std`weight')
// Create ventiles of shares missing
gen sharemissing`weight'     = round(1-share`weight',0.05)
// Calculate means and confidence interval by ventiles
bysort sharemissing`weight': egen mu_std`weight' = mean(mean_std`weight')
bysort sharemissing`weight': egen ub_std`weight' = pctile(mean_std`weight'), p(97.5)
}

****************************
*** PREPARE FOR PLOTTING ***
****************************
drop mean* share1 share2
duplicates drop
gen i = _n
reshape long sharemissing mu_std ub_std, i(i) j(weight) 
drop i
duplicates drop
sort weight sharemissing
// Will make the figure look nicer
replace ub_std = 1 if ub_std>1
drop if sharemissing>0.95

**************
*** FIGURE ***
**************
twoway line mu_std sharemissing if weight==1, lwidth(thick) color(gs4) || ///
       line ub_std sharemissing if weight==1, lwidth(thick) color(dkorange) || ///
       line mu_std sharemissing if weight==2, lwidth(thick) color(gs4%50) || ///
       line ub_std sharemissing if weight==2, lwidth(thick) color(dkorange%50)  ///
	   xtitle("Share of global population without data", size(medlarge)) ///
ytitle("Error (standard deviations from mean)", size(medlarge)) ///
ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) ///
xlab(0(0.25)1,labsize(medlarge) grid) /// 
legend(order(1 "Expected error (simple average)" 3 "Expected error (weighted average)" ///
             2 "97.5th pctl error (simple average)" 4 "97.5th pctl error (weighted average)") ///
       size(medlarge) rows(4) span symxsize(*0.25) region(lcolor(white))) /// 
graphregion(margin(0 3 0 2) color(white)) plotregion(margin(0 0 0 0)) xsize(10) ysize(12) 
graph export "Figures/AnalyticalResults.png", as(png) width(2000) replace