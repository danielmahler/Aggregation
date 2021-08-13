********************
*** INTRODUCTION ***
********************
// This .do-file contains the main analysis of the paper 
// Set working directory
cd "C:\Users\WB514665\OneDrive - WBG\DECDG\Aggregation"
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

*****************************************************************
*** FIND VALUES FOR TABLE SHOWING MEANINGNESS OF SD FROM MEAN ***
*****************************************************************
preserve
// Calculate mean values
bysort indicatorcode: egen mean_raw = wtmean(value_raw), weight(pop)
drop country* pop 
// Keep if rows about 1 std from mean
replace value_std = abs(value_std)
keep if inrange(value_std,0.98,1.02)
// Calculate difference from mean
gen dif = abs(value_raw-mean_raw)
drop value_raw
// Only keep clostest to 1 standard deviation per indicator code
// Calculate difference from 1 SD
gen dif1sd = abs(value_std-1)
bysort indicatorcode: egen mindif = min(dif1sd)
bysort indicatorname: keep if dif1sd == mindif
drop dif1sd mindif value_std
duplicates drop
// Manually chosing some intuitive options
sort indicatorcode
format mean_raw dif %4.2f
*browse
restore

****************************************************************
*** CALCULATE MEAN VALUES WHEN RANDOMLY DROPPING INFORMATION ***
****************************************************************
// Expand by number of simulations (such that rows can be drop differentially for each simulation)
expand $simulations
bysort indicatorcode countrycode: gen rep = _n
// Using 100 simulations (for each expansion of the data), where 1%-99% of the data are being kept. 
// Takes several 10 minutes to run on my computer
forvalues i=1/100 {
// Randmoly remove `i'% of the data
gen pop_`i' = pop if runiform()<`i'/100
// Calculate the mean with remaining data
bysort indicatorcode rep: egen mean_raw_`i' = wtmean(value_raw), weight(pop_`i')
bysort indicatorcode rep: egen mean_std_`i' = wtmean(value_std), weight(pop_`i')
// Calculate the population coverage
bysort indicatorcode rep: egen sumpop_`i' = sum(pop_`i')
}

**********************************************
*** COLLAPSE TO INDICATOR-REPITITION LEVEL ***
**********************************************
keep mean* sumpop* rep indicator*
duplicates drop
gen type = "i"
reshape long mean_raw mean_std sumpop, i(rep indicator*) j(type) string
drop type rep
format sumpop %10.0f
bysort indicatorcode: egen totalpop = max(sumpop)
gen sharepop = sumpop/totalpop*100
drop totalpop sumpop
sort sharepop
drop if sharepop==0 
bysort indicatorcode (sharepop): gen dev_raw = abs(mean_raw-mean_raw[_N])
bysort indicatorcode (sharepop): gen dev_std = abs(mean_std-mean_std[_N])
duplicates drop

format dev*  %2.1f
format sharepop %2.1f
drop mean*

save "Data/Simulation_main.dta", replace

*********************
*** PLOT FINDINGS ***
*********************
use "Data/Simulation_main.dta", clear

// Create ventiles of shares missing
gen sharemissing = round(100-sharepop,5)

// Calculate means and confidence interval by ventiles
foreach dist in raw std {
bysort sharemissing type:               egen lb_all_`dist'   = pctile(dev_`dist'), p(2.5)
bysort sharemissing type:               egen mean_all_`dist' = mean(dev_`dist')
bysort sharemissing type:               egen ub_all_`dist'   = pctile(dev_`dist'), p(97.5)
bysort sharemissing indicatorcode type: egen lb_ind_`dist'   = pctile(dev_`dist'), p(2.5)
bysort sharemissing indicatorcode type: egen mean_ind_`dist' = mean(dev_`dist')
bysort sharemissing indicatorcode type: egen ub_ind_`dist'   = pctile(dev_`dist'), p(97.5)
}
drop dev* sharepop
duplicates drop
drop if sharemissing>95 // This final group is an unrealistic scenario and make the plot quite ugly

*********************
*** SAMPLE CHARTS ***
*********************
twoway scatter mean_ind_raw sharemissing if indicatorcode=="NY.GDP.PCAP.KD.ZG", color(gs4) || ///
rarea lb_ind_raw ub_ind_raw sharemissing if indicatorcode=="NY.GDP.PCAP.KD.ZG", color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data (%)") ///
ytitle("Error (pct. points)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/GDPraw.png", as(png) width(2000) replace

twoway scatter mean_ind_std sharemissing if indicatorcode=="NY.GDP.PCAP.KD.ZG", color(gs4) || ///
rarea lb_ind_std ub_ind_std sharemissing if indicatorcode=="NY.GDP.PCAP.KD.ZG", color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data (%)") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/GDPstd.png", as(png) width(2000) replace

***********************
*** COMBINED CHARTS ***
***********************
drop *ind* *raw
duplicates drop


twoway scatter mean_all_std sharemissing, color(gs4) || ///
rarea lb_all_std ub_all_std sharemissing, color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data (%)") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/Allrandom.png", as(png) width(2000) replace

******************
*** REGRESSION ***
******************

reg mean_all_std sharemissing, nocons
mat beta = e(b)
gen pred = sharemissing*beta[1,1]

reg ub_all_std sharemissing if sharemissing<=80, nocons

// Will make the figure look nicer
replace ub_all_std = 1 if ub_all_std>1
twoway scatter mean_all_std sharemissing, color(gs4) || ///
       rarea lb_all_std ub_all_std sharemissing, color(dkorange%50) lcolor(dkorange%0) || ///
       line pred sharemissing , lwidth(thick) color(dkorange) ///
	   graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population without data (%)", size(medlarge)) ///
ytitle("Error (standard deviations from mean)", size(medlarge)) ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) plotregion(margin(0 0 0 0)) xlab(0(25)100,labsize(medlarge)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval" 3 "Fitted line") size(medlarge) rows(1) span symxsize(*0.25) region(lcolor(white))) graphregion(margin(0 3 0 2))
graph export "Figures/Allempirical_fitted.png", as(png) width(2000) replace
