********************
*** INTRODUCTION ***
********************
// This .do-file contains the main analysis of the paper 
// Set working directory
cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Agr"
use "Data/WDIfinal.dta",clear

************************************
*** PREPARE DATA FOR SIMULATIONS ***
************************************
// Decide how many simulations to run
global simulations = 100
// Create standardized versions of values
rename value       value_raw
gen    value_std = .
levelsof indicatorcode
foreach ind in `r(levels)' {
qui sum value_raw [aw=pop]                            if indicatorcode=="`ind'"
qui replace value_std = (value_raw-`r(mean)')/`r(sd)' if indicatorcode=="`ind'"
}
drop value_raw

****************************************************************
*** CALCULATE MEAN VALUES WHEN RANDOMLY DROPPING INFORMATION ***
****************************************************************
// Expand by number of simulations (such that rows can be dropped differentially for each simulation)
expand $simulations
bysort indicatorcode countrycode: gen rep = _n
// Dropping some unnecessary columns
drop country* indicatorname income region
// Using 100 simulations (for each expansion of the data), where 1%-99% of the data are being kept. 
// Takes about 1 hour to run on my computer
forvalues i=1/100 {
disp in red "`i'"
// Randomly remove `i'% of the data
quietly gen pop_`i' = pop if runiform()<`i'/100
// Calculate the global mean with remaining data
quietly bysort indicatorcode rep: egen mean_std_`i' = wtmean(value_std), weight(pop_`i')
// Calculate the global population coverage
quietly bysort indicatorcode rep: egen sumpop_`i' = sum(pop_`i')
// Calculate the global country coverage
quietly bysort indicatorcode rep: egen sumcou_`i' = count(pop_`i')
// Calculate mixed coverage
forvalues j=1/9 {
quietly bysort indicatorcode rep: egen summix`j'_`i' = sum(pop_`i'^(`j'/10))	
}
drop pop_`i' 
}

**********************************************
*** COLLAPSE TO INDICATOR-REPITITION LEVEL ***
**********************************************
drop pop value_std
duplicates drop
reshape long mean_std sumpop sumcou summix1 summix2 summix3 summix4 summix5 summix6 summix7 summix8 summix9, i(rep indicator*) j(type) string
drop rep type
format sumpop %10.0f
// Calculat coverage
bysort indicatorcode: egen totalpop = max(sumpop)
bysort indicatorcode: egen totalcou = max(sumcou)
gen sharepop = sumpop/totalpop
gen sharecou = sumcou/totalcou
forvalues j=1/9 {
bysort indicatorcode: egen totalmix`j' = max(summix`j')
gen sharemix`j' = summix`j'/totalmix`j'
}
drop total* sum*
sort sharepop
//  Drop observations with zero coverage
drop if sharepop==0 
// Calculate standard deviations from mean
bysort indicatorcode (sharepop): gen dev_std = abs(mean_std-mean_std[_N])
duplicates drop
format dev*  %2.1f
format share* %3.2f
drop mean* indicator
save "Data/Simulation_main.dta", replace

****************************
*** PREPARE FOR PLOTTING ***
****************************
use "Data/Simulation_main.dta", clear
drop *mix*

// Create ventiles of shares missing
gen sharemissing     = round(1-sharepop,0.05)

// Calculate means and confidence interval by ventiles
bysort sharemissing:               egen lb_std   = pctile(dev_std), p(2.5)
bysort sharemissing:               egen mean_std = mean(dev_std)
bysort sharemissing:               egen ub_std   = pctile(dev_std), p(97.5)

drop dev* sharepop sharecou
duplicates drop

sort sharemissing 

// Run regression
reg mean_std sharemissing if sharemissing<=0.95, nocons
mat beta = e(b)
gen pred = sharemissing*beta[1,1]

// Will make the figure look nicer
replace ub_std = 1 if ub_std>1

**************
*** FIGURE ***
**************
twoway scatter mean_std      sharemissing if sharemissing<=0.95, color(gs4) || ///
       rarea   lb_std ub_std sharemissing if sharemissing<=0.95, color(dkorange%50) lcolor(dkorange%0) || ///
       line    pred          sharemissing if sharemissing<=0.95, lwidth(thick) color(dkorange) ///
xtitle("Share of global population without data", size(medlarge)) ///
ytitle("Error (standard deviations from mean)", size(medlarge)) ///
ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) ///
xlab(0(0.25)1,labsize(medlarge) grid) /// 
legend(order(1 "Point estimate" 2 "Confidence interval" 3 "Fitted line") ///
       size(medlarge) rows(1) span symxsize(*0.25) region(lcolor(white))) /// 
graphregion(margin(0 3 0 2) color(white)) plotregion(margin(0 0 0 0)) xsize(10) ysize(10) 
graph export "Figures/Allempirical_fitted.png", as(png) width(2000) replace

**************************************
*** SAVE DATA FOR FURTHER ANALYSIS ***
**************************************
drop lb ub
gen subgroup = "Global"
drop if sharemissing>=0.95
save "Data/Simulation_global.dta", replace