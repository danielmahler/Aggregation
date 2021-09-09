********************
*** INTRODUCTION ***
********************
// This .do-file contains the analysis of errors by subgroup and when imputing missing values 
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

****************************************************************
*** CALCULATE MEAN VALUES WHEN RANDOMLY DROPPING INFORMATION ***
****************************************************************
// Expand by number of simulations (such that rows can be dropped differentially for each simulation)
expand $simulations
bysort indicatorcode countrycode: gen rep = _n
// Dropping some unnecessary columns
drop country* indicatorname
// Using 100 simulations (for each expansion of the data), where 1%-99% of the data are being kept. 
// Takes about 2 hours to run on my computer
forvalues i=1/100 {
disp in red "`i'"
// Randmoly remove `i'% of the data
quietly gen pop_`i' = pop if runiform()<`i'/100
// Calculate the global population coverage
quietly bysort indicatorcode rep: egen sumpop_`i' = sum(pop_`i')
// Calculate the regional/income group mean with remaining data
quietly bysort indicatorcode region rep: egen mean_std_reg_`i' = wtmean(value_std), weight(pop_`i')
quietly bysort indicatorcode income rep: egen mean_std_inc_`i' = wtmean(value_std), weight(pop_`i')
// Calculate the regional/income group population coverage
quietly bysort indicatorcode region rep: egen sumpop_reg_`i' = sum(pop_`i')
quietly bysort indicatorcode income rep: egen sumpop_inc_`i' = sum(pop_`i')
// Imputed version
qui gen     value_imp_`i' = value_std if !missing(pop_`i')
qui replace value_imp_`i' = mean_std_reg_`i' if missing(value_imp_`i')
quietly bysort indicatorcode rep: egen mean_imp_`i' = wtmean(value_imp_`i'), weight(pop)
drop pop_`i' value_imp_`i'
}

**********************************
*** COLLAPSE IMPUTED ESTIMATES ***
**********************************
preserve
keep mean* sumpop* rep indicator*
drop *reg* *inc*
duplicates drop
reshape long mean_imp sumpop, i(rep indicator) j(type) string
drop rep type
format sumpop %10.0f
// Calculate coverage
bysort indicatorcode: egen totalpop = max(sumpop)
gen sharepop = sumpop/totalpop
drop total* sum*
sort sharepop
drop if sharepop==0 
// Calculate deviation from mean
bysort indicatorcode (sharepop): gen dev_imp = abs(mean_imp-mean_imp[_N])
duplicates drop
format dev*  %2.1f
format share* %3.2f
drop mean* ind*
// Create ventiles of shares missing
gen sharemissing     = round(1-sharepop,0.05)
// Calculate means and confidence interval by ventiles
bysort sharemissing: egen mean_imp = mean(dev_imp)
drop dev* sharepop 
duplicates drop
sort sharemissing 
drop if sharemissing>=0.95
save "Data/Simulation_imputed.dta", replace
restore

*******************************************************
*** COLLAPSE TO SUBGROUP-INDICATOR-REPITITION LEVEL ***
*******************************************************
foreach subgroup in inc reg {
preserve
keep *`subgroup'* rep ind*
duplicates drop
reshape long mean_std_`subgroup'_ sumpop_`subgroup'_, i(rep `subgroup' indicator*) j(type) string
drop type rep
// Calculate coverage
format sumpop %10.0f
bysort indicatorcode `subgroup': egen totalpop = max(sumpop)
gen sharepop = sumpop/totalpop
drop totalpop sumpop
sort `subgroup' sharepop
drop if sharepop==0 
sort indicatorcode `subgroup' sharepop
// Calculate deviations from mean
bysort indicatorcode `subgroup' (sharepop): gen dev_std = abs(mean_std-mean_std[_N])
format dev*  %2.1f
format sharepop %3.2f
drop mean*
rename `subgroup' subgroup
// Create ventiles of shares missing
gen sharemissing = round(1-sharepop,0.05)
drop sharepop indicator*
// Calculate means and confidence interval by ventiles
bysort sharemissing subgroup: egen mean_std = mean(dev_std)
drop dev
duplicates drop
drop if sharemissing>0.95 // This final group is an unrealistic scenario and make the plot quite ugly
// Dropping North America as it only has three countries
drop if subgroup=="NAC"
save "Data/Simulation_`subgroup'.dta", replace
restore
}

******************************
*** REGION/INCGROUP CHARTS ***
******************************
use "Data/Simulation_global.dta", clear
append using "Data/Simulation_reg"
append using "Data/Simulation_inc"

// Plot
twoway line mean_std sharemissing if subgroup=="HIC", lcolor(gs4) ||  ///
       line mean_std sharemissing if subgroup=="UMC", lcolor(gs4) ||  ///
       line mean_std sharemissing if subgroup=="LMC", lcolor(gs4) ||  ///
       line mean_std sharemissing if subgroup=="LIC", lcolor(gs4) ||  ///
       line mean_std sharemissing if subgroup=="EAS", lcolor(gs4) ||  ///
       line mean_std sharemissing if subgroup=="ECS", lcolor(gs4) ||  ///
	   line mean_std sharemissing if subgroup=="LCN", lcolor(gs4) ||  ///
	   line mean_std sharemissing if subgroup=="MEA", lcolor(gs4) ||  ///
	   line mean_std sharemissing if subgroup=="SAS", lcolor(gs4) ||  ///
	   line mean_std sharemissing if subgroup=="SSF", lcolor(gs4) || ///
	   line pred sharemissing if subgroup=="Global", lwidth(thick) lcolor(dkorange) ///
	   graphregion(color(white)) xsize(10) ysize(10) xtitle("Share of group without data", size(medlarge)) ///
ytitle("Error (standard deviations from mean)", size(medlarge)) ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) plotregion(margin(0 0 0 0)) xlab(0(0.25)1,labsize(medlarge) grid) /// 
legend(order(11 "Global" 2 "Income groups and regions") size(medlarge) rows(1) span symxsize(*0.25) region(lcolor(white))) graphregion(margin(0 3 0 2))
graph export "Figures/Allempirical_increg.png", as(png) width(2000) replace
