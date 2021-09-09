********************
*** INTRODUCTION ***
********************
// This .do-file calculates the share of missing data in WDI and by indicators for each country
// Set working directory:
cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Agr"

******************************************************
*** ONLY KEEP COUNTRY-LEVEL DATA FROM 2000 ONWARDS ***
******************************************************
use "Data/WDIfull.dta", clear
// Only keep country-level data or data for the whole world
keep if regionname!="Aggregates" & !inlist(countrycode,"AFE","AFW")
// Dropping some irrelevant variables
drop admin* lending* income* region indicatorname
// Drop everything before 2000, won't be using it
drop yr1960-yr1999
// Dropping duplicates
duplicates drop
bysort countrycode indicatorcode: drop if _n!=1
isid countrycode indicatorcode
// Keep population counts for a figure on missingness by country
preserve
keep if indicatorcode=="SP.POP.TOTL"
keep countrycode yr2019
// Missing for Eritrea, put in value used by PovcalNet
replace yr2019 = 1.423746*10^6 if countrycode=="ERI"
rename yr2019 pop
tempfile pop
save    `pop'
restore
// Reshape long
reshape long yr, i(country* indicator*) j(year)
rename yr value

********************************************************
*** CALCULATE SHARE NON-MISSING BY INDICATOR-COUNTRY ***
********************************************************
preserve
replace value = 1/21 if !missing(value)
collapse (sum) value, by(countrycode countryname indicatorcode)
rename value nonmissing_ind
tempfile nonmissing_ind
save     `nonmissing_ind'
restore 

**********************************************
*** CALCULATE SHARE NON-MISSING BY COUNTRY ***
**********************************************
drop indicator*
collapse (percent) value, by(countrycode countryname)
rename value nonmissing_wdi
// Merge on population count
merge 1:1 countrycode using `pop', nogen
// Merge on indicatorlevel missing
merge 1:m countrycode using `nonmissing_ind', nogen
order countrycode indicatorcode nonmissing*
// Save final data
save "Data/WDImissing.dta", replace

********************
*** PLOT RESULTS ***
********************

use "Data/WDImissing.dta", clear
drop nonmissing_ind indicatorcode
duplicates drop

// Sort by most missing
sort nonmissing
gen n = _n
replace nonmissing = nonmissing *100
gen missing = 100-nonmissing
replace pop = pop/10^6
format pop %3.1f

// Label populous countries and extreme cases
gen lab = countryname if (_n==1 | _n==_N) | pop>125

// Create actual figure
twoway bar missing n, color(gs4) graphregion(color(white)) || ///
       bar missing n if pop<0.5, graphregion(color(white)) color(dkorange) || ///
	   scatter missing n, msymbol(i) mlab(lab) mlabcolor(black) mlabangle(45) ///
ylab(0(20)100, angle(horizontal)) ytitle("% missing")  ///
xtitle("Countries ordered from most missing to least missing") xlab("") ///
plotregion(margin(0 0 0 0)) graphregion(margin(0 8 0 20)) ///
legend(order(2 "Population<500,000" 1 "Population>500,000") region(lcolor(white)))
graph export "Figures/Missing.png", as(png) width(2000) replace