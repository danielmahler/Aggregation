********************
*** INTRODUCTION ***
********************
// Analysis on population coverage vs country coverage
// Set working directory
cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Agr"

************************************************
*** COMPARING POPULATION AND COUNTRY WEIGHTS ***
************************************************

use "Data/Simulation_main.dta", clear
rename sharepop sharemix10
rename sharecou sharemix0
keep sharemix* dev_std
foreach var of varlist sharemix* {
	replace `var' = 1-`var'
}
gen mean_onlypop = .
gen mean_onlycou = .
gen sharemissing = .

// Calculate mean error when satisfying one criterion but not the other
forvalues share = 1/19 {
	disp in red "`share'"
qui replace sharemissing = `share' if _n==`share'
qui sum dev_std if sharemix0<`share'/20 & sharemix10>`share'/20
qui replace mean_onlycou = `r(mean)' if _n==`share'
qui sum dev_std if sharemix0>`share'/20 & sharemix10<`share'/20
qui replace mean_onlypop = `r(mean)' if _n==`share'
}
keep if !missing(sharemissing)
keep sharemissing mean*
replace sharemissing = sharemissing/20

// Plot as a function of coverage threshold
twoway connected mean_onlypop sharemissing, color(dkorange) || ///
       connected mean_onlycou sharemissing, color(gs6) ///
	   graphregion(color(white)) xsize(10) ysize(10) /// 
	   xtitle("Coverage threshold (share without data)", size(medlarge)) ///
       ytitle("Error (standard deviations from mean)", size(medlarge)) ///
	   ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) ///
	   xlab(0(0.25)1,labsize(medlarge) grid) /// 
	   plotregion(margin(0 0 0 0)) ///
	   legend(order(1 "Satisfies population but not country coverage" ///
	                2 "Satisfies country but not population coverage") ///
					size(medlarge) rows(2) span symxsize(*0.5) region(lcolor(white))) graphregion(margin(0 3 0 2))
graph export "Figures/Cou_pop_comparison.png", as(png) width(2000) replace

*******************************************************************
*** FINDING OPTIMAL WEIGHTS AS A FUNCTION OF COVERAGE THRESHOLD ***
*******************************************************************
use "Data/Simulation_main.dta", clear
rename sharepop sharemix10
rename sharecou sharemix0
keep sharemix* dev_std

foreach var of varlist sharemix* {
	replace `var' = 1-`var'
}
gen only = . 
gen not  = .
gen sharemissing = .
gen relerror = .
local row = 1
// All pairwise exponent comparisons using 0.1 increments of exponents and 0.02 increments of coverage thresholds
// i.e. it makes 10x10x50 comparisions
// Takes about 30 minutes to run
forvalues only  = 0(1)10 {
forvalues not   = 0(1)10 {
	disp in red "`only' - `not'"
forvalues share = 0.02(0.02)0.9801 {
quietly {
if `only'!=`not' {
replace only  = `only'/10 if _n==`row'
replace not   = `not'/10  if _n==`row'
replace sharemissing = `share'   if _n==`row'
cap sum dev_std if sharemix`only'<`share' & sharemix`not'>`share'
cap local meanonly = `r(mean)'	
cap sum dev_std if sharemix`only'>`share' & sharemix`not'<`share'
cap local meannot = `r(mean)'	
replace relerror = `meanonly'/`meannot' if _n==`row'
local row = `row'+1
local meanonly = 0
local meannot = 0
}
}
}	
}
}
keep only not sharemissing relerror
save "Data/OptimalWeights.dta", replace

drop if missing(only) | missing(relerror)
replace only = round(100*only,0)
replace not  = round(100*not,0)
reshape wide relerror, i(only sharemissing) j(not)

egen maxrelerror = rowmax(relerror*)
keep if maxrelerror < 1

drop relerror* max
replace only = only/100
sort sharemissing
gen one = 1

// Final plot
twoway line one sharemissing if sharemissing<=0.54, lwidth(thick) lcolor(dkorange) ||  ///
	   lfit  only sharemissing if inrange(sharemissing,0.505,0.95), lwidth(thick) lcolor(dkorange)  ///
	   graphregion(color(white)) xsize(10) ysize(10)  ///
	   xtitle("Coverage threshold (share without data)", size(medlarge)) ///
       ytitle("Optimal weighting exponent", size(medlarge)) ///
	   ylab(0(0.25)1,angle(horizontal) labsize(medlarge)) ///
	   xlab(0(0.25)1,labsize(medlarge) grid) /// 
	   plotregion(margin(0 0 0 0)) ///
	   legend(off) graphregion(margin(0 3 0 2))
graph export "Figures/OptimalWeights.png", as(png) width(2000) replace

********************************************
*** OLD VERSION OF FIGURES TO BE DELETED ***
********************************************
use "Data/Simulation_main.dta", clear
keep sharepop sharecou dev_std
foreach var of varlist sharepop sharecou dev_std {
gen round_`var' = round(`var'-0.025,0.05)+0.025
}
count
bysort round_dev_std round_sharepop: gen masspop = _N/r(N)*100
bysort round_dev_std round_sharecou: gen masscou = _N/r(N)*100

preserve
keep round_dev_std round_sharepop masspop
duplicates drop
rename round_sharepop share
rename round_dev_std dev
drop if dev>1
tempfile pop
save    `pop'
restore 

keep round_dev_std round_sharecou masscou
duplicates drop
rename round_sharecou share
rename round_dev_std dev
drop if dev>1
merge 1:1 share dev using `pop', nogen

replace masscou = 0 if missing(masscou)
replace masspop = 0 if missing(masspop)
gen massdif = masscou-masspop
format mass* %4.3f

replace share = 1-share
twoway scatter dev share if massdif>0, msymbol(S) msize(vhuge) mcolor(forest_green) || ///
       scatter dev share if massdif<0, msymbol(S) msize(vhuge) mcolor(dkorange) ///
	   xsize(10) ysize(10) graphregion(color(white)) plotregion(margin(0 0 0 0)) ///
	   legend(off)
	   
twoway scatter dev share if massdif>0.15,                 msymbol(S) msize(vlarge) mcolor(forest_green) || ///
	   scatter dev share if inrange(massdif,0.05,0.15),   msymbol(S) msize(vlarge) mcolor(forest_green%50)     || ///
	   scatter dev share if inrange(massdif,-0.05,0.05),  msymbol(S) msize(vlarge) mcolor(white) || ///
       scatter dev share if inrange(massdif,-0.15,-0.05), msymbol(S) msize(vlarge) mcolor(dkorange%50) || ///
	   scatter dev share if massdif<-0.15,                msymbol(S) msize(vlarge) mcolor(dkorange)  ///
	   xsize(10) ysize(10) graphregion(color(white)) plotregion(margin(0 0 0 0)) ///
	   legend(off)
	   
replace massdif=0.15 if massdif>0.15
replace massdif=-0.15 if massdif<-0.15
twoway contour massdif dev share, scolor(dkorange) ecolor(gs4) int(shep) crule(lin) ///
       ccuts(-0.15 -0.1 -0.05 0 0.05 0.1 0.15) legend(off) ///
   	   xsize(12) ysize(10) graphregion(color(white)) plotregion(margin(0 0 0 0)) 