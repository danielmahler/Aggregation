********************
*** INTRODUCTION ***
********************
// This .do-file calculates the poverty statistics used in the paper

// Load poverty data (requires the povcalnet.ado)
povcalnet, country(all) year(all) clear
// Only keep national estimates
keep if inlist(coveragetype,3,4) | inlist(countrycode,"ARG","SUR")
// Find the number of estimates and mean headcount by country
collapse (mean) headcount (count) mean, by(countrycode)
gen obs = mean
// Collapse into three groups
gen     cat = 1 if inrange(obs,1,5)
replace cat = 2 if inrange(obs,6,10)
replace cat = 3 if inrange(obs,11,50)
drop obs mean
// Calculate the mean poverty rate by number of observations
collapse (mean) headcount, by(cat)
list