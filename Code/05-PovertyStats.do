povcalnet, country(all) year(all) clear
keep if inlist(coveragetype,3,4) | inlist(countrycode,"ARG","SUR")
collapse (mean) headcount (count) mean, by(countrycode)
gen obs = mean
collapse headcount (count) mean, by(obs)
scatter headcount obs 
gen     cat = 1 if inrange(obs,1,5)
replace cat = 2 if inrange(obs,6,10)
replace cat = 3 if inrange(obs,11,50)
collapse headcount [aw=mean], by(cat)