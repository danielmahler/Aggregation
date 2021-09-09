********************
*** INTRODUCTION ***
********************
// This .do-file tries to interpret how severe being a standard deviation off is
// Set working directory
cd "\\wbmserccpi201\GDIM\Papers\TrendsPatterns\Do-files\ARCHIVE\Agr"
use "Data/WDIfinal.dta",clear
drop reg inc

*****************************************************************
*** FIND VALUES FOR TABLE SHOWING MEANINGNESS OF SD FROM MEAN ***
*****************************************************************
rename value       value_raw
gen    value_std = .
levelsof indicatorcode
foreach ind in `r(levels)' {
qui sum value_raw [aw=pop]                            if indicatorcode=="`ind'"
qui replace value_std = (value_raw-`r(mean)')/`r(sd)' if indicatorcode=="`ind'"
}

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

**********************************************************
*** FIND CHANGE IN STANDARD DEVATIONS FROM YEAR BEFORE ***
**********************************************************
// Get list of 165 indicators chosen
use "Data/WDIfinal.dta",clear
keep indicatorcode
duplicates drop
tempfile indicators
save `indicators'

// Find probability of missing in two adjacent years
use "Data/WDImid.dta", clear
// Missing data fron one year to next
drop nonmissing region income countryname
bysort indicatorcode countrycode (year): gen mm = missing(value)  & missing(value[_n-1])  if _n!=1
bysort indicatorcode countrycode (year): gen mp = missing(value)  & !missing(value[_n-1]) if _n!=1
bysort indicatorcode countrycode (year): gen pm = !missing(value) & missing(value[_n-1])  if _n!=1
bysort indicatorcode countrycode (year): gen pp = !missing(value) & !missing(value[_n-1]) if _n!=1
tab mm if mm==1 | pm==1
tab pp if pp==1 | mp==1

// Find indicators with near complete data in adjacent years
keep if sharenonmissing>0.99
bysort indicatorcode countrycode (year): gen     oneyearapart = year==year[_n-1]+1 | year==year[_n+1]-1
drop if oneyearapart==0
drop oneyear
merge m:1 indicatorcode using `indicators', keep(3) nogen

// Compute standard deviations from mean:
rename value value_raw
gen    value_sd   = .
gen    value_mean = .
egen group = group(indicatorcode year)
levelsof group
foreach group in `r(levels)' {
    disp in red "`group'"
qui sum value_raw [aw=pop]         if group==`group'
qui replace value_mean = `r(mean)' if group==`group'
qui replace value_sd   = `r(sd)'   if group==`group'
}
// Find change from year to year in standard deviations from the mean
keep indicator* year value_sd value_mean
duplicates drop
bysort indicatorcode (year): gen change = abs((value_mean-value_mean[_n-1])/value_sd[_n-1])

// When multiple adjacant years with near complete coverage, take the average year to year change in standard deviations from the mean
collapse change, by(indicatorcode indicatorname)

sum change,d