********************
*** INTRODUCTION ***
********************
// This .do-file shows the simulation results for a particular indicator
// Set working directory
cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Agr"
use "Data/WDIfinal.dta",clear

************************************
*** PREPARE DATA FOR SIMULATIONS ***
************************************
// Only keeping one particular indicator; here GDP growth
keep if indicatorcode=="NY.GDP.PCAP.KD.ZG"
// Decide how many simulations to run
global simulations = 1000
// Create standardized versions of values
rename value       value_raw
gen    value_std = .
qui sum value_raw [aw=pop]                            
qui replace value_std = (value_raw-`r(mean)')/`r(sd)'

****************************************************************
*** CALCULATE MEAN VALUES WHEN RANDOMLY DROPPING INFORMATION ***
****************************************************************
// Expand by number of simulations (such that rows can be dropped differentially for each simulation)
expand $simulations
bysort countrycode: gen rep = _n
// Dropping some unnessecary columns
drop country* indicator* region income
// Using 100 simulations (for each expansion of the data), where 1%-99% of the data are being kept. 
// Takes about 1 minute to run on my computer
forvalues i=1/100 {
disp in red "`i'"
// Randmoly remove `i'% of the data
quietly gen pop_`i' = pop if runiform()<`i'/100
// Calculate the global mean with remaining data
quietly bysort rep: egen mean_raw_`i' = wtmean(value_raw), weight(pop_`i')
quietly bysort rep: egen mean_std_`i' = wtmean(value_std), weight(pop_`i')
// Calculate the global population coverage
quietly bysort rep: egen sumpop_`i' = sum(pop_`i')
drop pop_`i'
}
drop value* pop

**********************************************
*** COLLAPSE TO INDICATOR-REPITITION LEVEL ***
**********************************************
duplicates drop
reshape long mean_raw mean_std sumpop, i(rep) j(type) string
drop rep type
format sumpop %10.0f
egen totalpop = max(sumpop)
gen sharepop = sumpop/totalpop
drop total* sum*
sort sharepop
drop if sharepop==0 
sort sharepop
gen dev_raw = abs(mean_raw-mean_raw[_N])
gen dev_std = abs(mean_std-mean_std[_N])
drop mean*
format dev*  %2.1f
format share* %3.2f
save "Data/Simulation_sample.dta", replace


*********************
*** PLOT FINDINGS ***
*********************
use "Data/Simulation_sample.dta", clear

// Create ventiles of shares missing
gen sharemissing     = round(1-sharepop,0.05)

// Calculate means and confidence interval by ventiles
foreach dist in raw std {
bysort sharemissing: egen lb_`dist'   = pctile(dev_`dist'), p(2.5)
bysort sharemissing: egen mean_`dist' = mean(dev_`dist')
bysort sharemissing: egen ub_`dist'   = pctile(dev_`dist'), p(97.5)
}
drop dev* sharepop 
duplicates drop

sort sharemissing 

// Dropping highest ventile in order not to make the graph range to wide
drop if sharemissing>=0.95

*********************
*** SAMPLE CHARTS ***
*********************
twoway scatter mean_raw sharemissing, color(gs4) || ///
rarea lb_raw ub_raw sharemissing, color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data") ///
ytitle("Error (pct. points)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/GDPraw.png", as(png) width(2000) replace

twoway scatter mean_std sharemissing, color(gs4) || ///
rarea lb_std ub_std sharemissing, color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/GDPstd.png", as(png) width(2000) replace
