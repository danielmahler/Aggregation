********************
*** INTRODUCTION ***
********************
// This .do-file contains the main analysis of the paper 

// Set working directory
cd "C:\Users\WB514665\OneDrive - WBG\DECDG\Aggregation"
use "Data/WDIfinal.dta",clear

global simulations = 100
// Merge in vector with information on country's level of missingness
merge m:1 countrycode using "Data/WDImissing.dta", keepusing(nonmissing) nogen


rename value value_raw
gen value_std = .

// Create standardized versions of values
levelsof indicatorcode
foreach ind in `r(levels)' {
qui sum value_raw [aw=pop]                            if indicatorcode=="`ind'"
qui replace value_std = (value_raw-`r(mean)')/`r(sd)' if indicatorcode=="`ind'"
}

expand $simulations
bysort indicatorcode countrycode: gen rep = _n

// Means with identical draw probability for all countries
forvalues i=1/100 {
gen pop_i`i' = pop if runiform()<`i'/100
bysort indicatorcode rep: egen mean_raw_i`i' = wtmean(value_raw), weight(pop_i`i')
bysort indicatorcode rep: egen mean_std_i`i' = wtmean(value_std), weight(pop_i`i')
bysort indicatorcode rep: egen sumpop_i`i' = sum(pop_i`i')
}

// Means with emprical draw probability 
forvalues i=1/100 {
if `i'<=50 {
gen pop_e`i' = pop if runiform()<nonmissing/50*`i'
}
if `i'>50 {
gen pop_e`i' = pop if runiform()<1-2*(1-nonmissing)+(1-nonmissing)/50*`i'
}
bysort indicatorcode rep: egen mean_raw_e`i' = wtmean(value_raw), weight(pop_e`i')
bysort indicatorcode rep: egen mean_std_e`i' = wtmean(value_std), weight(pop_e`i')
bysort indicatorcode rep: egen sumpop_e`i' = sum(pop_e`i')
}

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
