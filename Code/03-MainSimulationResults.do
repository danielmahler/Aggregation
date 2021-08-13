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
gen pop_i`i' = pop if runiform()<`i'/100
// Calculate the mean with remaining data
bysort indicatorcode rep: egen mean_raw_i`i' = wtmean(value_raw), weight(pop_i`i')
bysort indicatorcode rep: egen mean_std_i`i' = wtmean(value_std), weight(pop_i`i')
// Calculate the population coverage
bysort indicatorcode rep: egen sumpop_i`i' = sum(pop_i`i')
}

**********************************************
*** COLLAPSE TO INDICATOR-REPITITION LEVEL ***
**********************************************
keep mean* sumpop* rep indicator*
duplicates drop
reshape long mean_raw mean_std sumpop, i(rep indicator*) j(type) string
replace type = substr(type,2,1)
drop rep
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

save "Data/BeforePlotting.dta", replace

use "Data/BeforePlotting.dta", clear

gen ventile = round(sharepop,5)
drop if sharepop==100
*drop if inrange(sharepop,97.5,99.999999)


foreach dist in raw std {
bysort ventile type:               egen lb_all_`dist'   = pctile(dev_`dist'), p(2.5)
bysort ventile type:               egen mean_all_`dist' = mean(dev_`dist')
bysort ventile type:               egen ub_all_`dist'   = pctile(dev_`dist'), p(97.5)
bysort ventile indicatorcode type: egen lb_ind_`dist'   = pctile(dev_`dist'), p(2.5)
bysort ventile indicatorcode type: egen mean_ind_`dist' = mean(dev_`dist')
bysort ventile indicatorcode type: egen ub_ind_`dist'   = pctile(dev_`dist'), p(97.5)
}

drop dev* sharepop
duplicates drop
drop if ventile<=5

*********************
*** SAMPLE CHARTS ***
*********************
twoway scatter mean_ind_raw ventile if type=="i" & indicatorcode=="NY.GDP.PCAP.KD.ZG", color(gs4) || ///
rarea lb_ind_raw ub_ind_raw ventile if type=="i" & indicatorcode=="NY.GDP.PCAP.KD.ZG", color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population with data (%)") ///
ytitle("Error (pct. points)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/GDPraw.png", as(png) width(2000) replace

twoway scatter mean_ind_std ventile if type=="i" & indicatorcode=="NY.GDP.PCAP.KD.ZG", color(gs4) || ///
rarea lb_ind_std ub_ind_std ventile if type=="i" & indicatorcode=="NY.GDP.PCAP.KD.ZG", color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population with data (%)") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/GDPstd.png", as(png) width(2000) replace

***********************
*** COMBINED CHARTS ***
***********************
drop *ind* *raw
duplicates drop

twoway scatter mean_all_std ventile if type=="i", color(gs4) || ///
rarea lb_all_std ub_all_std ventile if type=="i", color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population with data (%)") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/Allrandom.png", as(png) width(2000) replace

twoway scatter mean_all_std ventile if type=="e", color(gs4) || ///
rarea lb_all_std ub_all_std ventile if type=="e", color(dkorange%50) lcolor(dkorange%0) graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population with data (%)") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/Allempirical.png", as(png) width(2000) replace

******************
*** REGRESSION ***
******************
gen missing = 100-ventile

reg mean_all_std missing if type=="e", nocons
reg mean_all_std missing if type=="i", nocons

gen pred = (100-ventile)*0.003677

twoway scatter mean_all_std ventile if type=="e", color(gs4) || ///
rarea lb_all_std ub_all_std ventile if type=="e", color(dkorange%50) lcolor(dkorange%0) || line pred ventile if type=="e", lwidth(thick) color(dkorange) ///
graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of global population with data (%)") ///
ytitle("Error (standard deviations)") ylab(,angle(horizontal)) plotregion(margin(0 0 0 0)) /// 
legend(order(1 "Point estimate" 2 "Confidence interval") region(lcolor(white)))
graph export "Figures/Allempirical_fitted.png", as(png) width(2000) replace
