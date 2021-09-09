********************
*** INTRODUCTION ***
********************
// This .do-file contains the analysis where missingness is decided by WDI missingness
// Set working directory
cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Agr"


**************************************
*** MERGE ON MISSINGNESS INDICATOR ***
**************************************
use "Data/WDIfinal.dta",clear
drop region income indicatorname countryname
merge 1:1 countrycode indicatorcode using "Data/WDImissing.dta", keepusing(nonmissing*) keep(3) nogen

************************************
*** PREPARE DATA FOR SIMULATIONS ***
************************************
// Create standardized versions of values
rename value       value_raw
gen    value_std = .
levelsof indicatorcode
foreach ind in `r(levels)' {
qui sum value_raw [aw=pop]                            if indicatorcode=="`ind'"
qui replace value_std = (value_raw-`r(mean)')/`r(sd)' if indicatorcode=="`ind'"
}
drop value_raw

// Create a variable which uses the indicator-level missingness but breaks ties using the WDI-wide missingness
gen nonmissing = nonmissing_ind + nonmissing_wdi/100

**********************************************************************
*** CALCULATE MEAN VALUES WHEN SYSTEMATICALLY DROPPING INFORMATION ***
**********************************************************************
gsort indicatorcode -nonmissing
// Sequentially enlarging means
forvalues i=1/217 {
disp in red "`i'"
// Calculate the global mean with remaining data
quietly bysort indicatorcode: gen pop_`i' = pop if _n<=`i'
quietly bysort indicatorcode: egen mean_std_`i' = wtmean(value_std), weight(pop_`i')
// Calculate the global population coverage
quietly bysort indicatorcode: egen sumpop_`i' = sum(pop_`i')
}
drop pop_*

****************
*** COLLAPSE ***
****************
keep mean* sumpop* indicator*
duplicates drop
reshape long mean_std sumpop, i(indicator*) j(type) string
format sumpop %10.0f
// Calculate population coverage
bysort indicatorcode: egen totalpop = max(sumpop)
gen sharepop = sumpop/totalpop
drop totalpop sumpop
sort indicatorcode sharepop
// Calculate deviation from mean
bysort indicatorcode (sharepop): gen dev_std = abs(mean_std-mean_std[_N])
drop type
duplicates drop
format dev*  %2.1f
format sharepop %3.2f
drop mean*
gen sharemissing = round(1-sharepop,0.05)
drop  sharepop
drop if sharemissing>0.95 // This final group is an unrealistic scenario and make the plot quite ugly
collapse (mean) dev_std, by(sharemissing)
save "Data/Simulation_empirical.dta", replace

*************
*** CHART ***
*************
use "Data/Simulation_empirical.dta", clear
rename dev_std mean_emp
merge 1:1 sharemissing using "Data/Simulation_global.dta", nogen
merge 1:1 sharemissing using "Data/Simulation_imputed.dta", nogen

drop subgroup
format mean* %3.2f
sort sharemissing

foreach var of varlist mean* {
qui reg `var' sharemissing, nocons
mat beta = e(b)
display in red "`subgroup': `=round(beta[1,1]*100,0.01)'"
gen pred_`var' = sharemissing*beta[1,1] 
}


twoway line mean_emp sharemissing, color(black) lwidth(thick) || ///
	   line mean_imp sharemissing, color(gs8) lwidth(thick) || ///
	   lfit pred_mean_std sharemissing, lwidth(thick) lcolor(dkorange) ///
	   graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data", size(medlarge))  ///
ytitle("Error (standard deviations from mean)", size(medlarge)) ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) ///
plotregion(margin(0 0 0 0)) xlab(0(0.25)1,labsize(medlarge) grid) /// 
legend(order(3 "Baseline" 1 "With empirical missingness" 2 "With imputations")  size(medlarge) rows(2) span symxsize(*0.25) region(lcolor(white))) graphregion(margin(0 3 0 2))
graph export "Figures/Robustness.png", as(png) width(2000) replace

gen decline = mean_imp/mean_std
format decline %3.2f