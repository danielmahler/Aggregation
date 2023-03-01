********************
*** INTRODUCTION ***
********************
// This .do-file downloads all data in WDI and keeps a subset of indicator-year combinations where there is data for at least 99% of the world's population
// Set working directory
cd "C:\Users\WB514665\OneDrive - WBG\DECDG\Aggregation\RR"

****************************
*** DOWNLOAD ALL WDI DATA ***
*****************************
/*
// Download all WDI data. Looping over all 21 topics of WDI. 
* ssc install wbopendata
forvalues i=1/21 {
disp in red "Topic `i'"
wbopendata, topics(`i') clear
// The next line deliberately doesn't work in the first iteration of the loop
cap append using `datasofar'
tempfile datasofar
save    `datasofar'
}
save "Data/WDIfull", replace
*/

**************************************************************
*** RETAIN INDICATOR-YEARS WITH (NEARLY) NO MISSING VALUES ***
**************************************************************
use "Data/WDIfull.dta", clear
// Only keep country-level data or data for the whole world
keep if regionname!="Aggregates" | countryname=="World"
// For some reason the Africa East and West regions do not have the regionname "Aggregates" yet
drop if inlist(countrycode,"AFE","AFW")
// There should now be 217 different economies left and the world
distinct countrycode
// Dropping some irrelevant variables
drop regionname admin* incomelevelname lending*
// Drop everything before 2000 -- we will only use data from 2000 onwards
drop yr1960-yr1999
// Dropping duplicates (indicators in more than one topic)
duplicates drop
// Some are duplicates by indicator codes but have slightly different indicator names. Removing these.
bysort countrycode indicatorcode: drop if _n!=1
// Now there is one row per country-indicatorcode
isid countrycode indicatorcode
// Drop all variables that we never aggregate to the world. Many of these are not comparable across countries, like variables in local currency units.
egen worldmissing = rowmiss(yr*) if countryname=="World"
// If worldmissing==21, we never have a world aggregate. Removing these
gsort indicatorname -worldmissing
bysort indicatorname: replace worldmissing = worldmissing[_n-1] if _n!=1
drop if worldmissing==21
drop worldmissing
// That was all we needed the world rows for
drop if countryname=="World"
compress
// Reshape long
reshape long yr, i(country* indicator*) j(year)
rename yr value
// Add population variable
gen pop = value if indicatorcode=="SP.POP.TOTL"
gsort countrycode year -pop
bysort countrycode year: replace pop=pop[_n-1] if _n!=1
drop if indicatorcode=="SP.POP.TOTL"
// Dropping the other population indicators as population data here is used to define coverage
drop if strpos(indicatorcode,"SP.POP") | inlist(indicatorcode,"SP.URB.TOTL","SP.RUR.TOTL")
// Calculate share of missing by indicator-year
gen nonmissing = !missing(value)
*ssc install _gwtmean
bysort indicatorcode year: egen sharenonmissing = wtmean(nonmissing), weight(pop)
// Saving this file as it will be useful for a later .do-file
save "Data/WDImid.dta", replace
// Only keep years with max non-missing for each per indicator
bysort indicatorcode: egen maxsharenonmissing = max(sharenonmissing)
keep if maxsharenonmissing == sharenonmissing
drop maxsharenonmissing
// Only keep if at least 99% nonmissing
keep if sharenonmissing>0.99
drop nonmissing sharenonmissing
// If multiple years with same nonmissing, only keep last
bysort indicatorcode countrycode (year): keep if _n==_N
save "Data/WDIsubset", replace

************************************************************
*** SELECT AT RANGE OF DIFFERENT AND RELEVANT INDICATORS ***
************************************************************
use "Data/WDIsubset.dta", clear

// This part of the code is a bit manual and explorative
// We start out with more than 300 indicator codes
// Many of these are not super relevant or too alike to be used for the analysis
distinct indicatorcode
// For indicators where we have both totals and per capita, only keep the latter
preserve
gen lasttwo = substr(indicatorcode,-2,.)
gen allelse = substr(indicatorcode,1,length(indicatorcode)-2)
keep indicator* lasttwo allelse
duplicates drop
bysort allelse: gen N=_N
keep if N==2
bysort allelse: egen zs = sum(lasttwo=="ZS")
bysort allelse: egen pc = sum(lasttwo=="PC")
keep if (pc==1 | zs==1) & !inlist(lasttwo,"ZS","PC")
keep indicatorcode
tempfile toremove
save    `toremove'
restore
merge m:1 indicatorcode using `toremove', nogen keep(1)

// Drop indicators with no variation
bysort indicatorcode: egen stdev = sd(value)
drop if stdev==0
// Drop variables with extremely high variation
drop if stdev>30000
// Drop indicators with extremely high variation expressed in dollar terms
drop if strpos(indicatorcode,".CD") & !strpos(indicatorname,"per") & stdev>1000
// Drop total GDP in dollar terms
drop if strpos(indicatorcode,".KD") & !strpos(indicatorname,"per") & stdev>1000
drop stdev
// Drop other variables in totals
drop if strpos(indicatorcode,"TOTL") & !strpos(indicatorname,"%")
// Drop many similar variables on merchandise imports/exports from particular regions
drop if strpos(indicatorcode,"TX.VAL") | strpos(indicatorcode,"TM.VAL")
// Drop a bunch of modelled ILO estimates
drop if (strpos(indicatorcode,"FE.ZS") | strpos(indicatorcode,"MA.ZS")) & strpos(indicatorcode,"SL.")
// Drop variables on various threatened species
drop if strpos(indicatorname,"threatened") 
// Drop various emission change variables (except for change in greenhouse gasses)
drop if strpos(indicatorname,"% change from") & indicatorcode!="EN.ATM.GHGT.ZG"
// Drop variables in thousands (i.e. totals rather than per capita)
drop if strpos(indicatorname,"thousand")
// Drop variables on number of deaths, i.e. totals instead of per capita
drop if strpos(indicatorname,"Number of")
// Dropping variables on females/males/gender shares --- the baseline variables are still there
drop if (strpos(indicatorcode,"FE") | strpos(indicatorcode,"MA")) & strpos(indicatorcode,"SH.")
// Really many health care spending variables left. Dropping some that are in USD
drop if strpos(indicatorcode,"PC.") & strpos(indicatorcode,"SH.")
// Still many mortality variables left. Dropping some that are for men/women when joint variable is also there
drop if (strpos(indicatorcode,"FE") | strpos(indicatorcode,"MA")) & strpos(indicatorcode,"SP.")

drop if missing(value) | missing(pop)

compress
save "Data/WDIfinal.dta", replace

**********************************************
*** INFORMATION ON FINAL INDICATORS CHOSEN ***
**********************************************
use "Data/WDIfinal.dta", clear
keep indicator* year
duplicates drop
order *code