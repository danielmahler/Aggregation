********************
*** INTRODUCTION ***
********************
// This .do-file calculates the share of missing data in WDI by country
// Set working directory:
cd "C:\Users\WB514665\OneDrive - WBG\DECDG\Aggregation"

******************************************************
*** ONLY KEEP COUNTRY-LEVEL DATA FROM 2000 onwards ***
******************************************************
use "Data/WDIfull.dta", clear
// Only keep country-level data or data for the whole world
keep if regionname!="Aggregates" & !inlist(countrycode,"AFE","AFW")
// Dropping some irrelevant variables
drop admin* lending*
// Drop everything before 2000, won't be using it
drop yr1960-yr1999
// Dropping duplicates
duplicates drop
bysort countrycode indicatorcode: drop if _n!=1
isid countrycode indicatorcode
// Keep population counts for the figure
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

**********************************************
*** CALCULATE SHARE NON-MISSING BY COUNTRY ***
**********************************************
drop indicator*
collapse (percent) value, by(countrycode countryname regionname incomelevelname)
// Merge on population count
merge 1:1 countrycode using `pop', nogen
rename value nonmissing
// Save final data
save "Data/WDImissing.dta", replace

********************
*** PLOT RESULTS ***
********************
use "Data/WDImissing.dta", clear

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