**************************************
* Muni Climate Change
* Clean flood risk and textual data
**************************************
set more off
clear all

global directory "folder containing data"

cd "$directory"

/*

1. Import python textual outputs
2. Import python keyword counts
3. Create robustness measures
4. Import flood risk data and clean 
5. Combine files and export to R 
*/

*===============================================================================
**# 1. Import python textual outputs and combine 

** loop through folder to get all folder names = state abbrev
local path1 "$directory"
local state : dir "`path1'" dirs "*"
display `state'


foreach s of local state {
import excel "`s'\adapt_measures.xlsx", sheet("Sheet1") firstrow clear
label variable sentences_counts "measure_adapt"
rename sentences_counts measure_adapt
label variable number_of_groups_meet_requiremen "ngroup_adapt"
rename number_of_groups_meet_requiremen ngroup_adapt
label variable number_of_keywords_meet_requirem "keyword_adapt"
rename number_of_keywords_meet_requirem keyword_adapt

save textual_data_`s', replace

** also bring in unique counts of keywords 
import excel "`s'\\adapt_counts.xlsx", sheet("Sheet1") firstrow clear
label variable total_number_of_distinct_keyword "distinct_adapt"
rename total_number_of_distinct_keyword distinct_adapt
keep A distinct_adapt
   merge 1:1 A using textual_data_`s'
drop _merge
   quietly save textual_data_`s', replace

** also bring in adapt subcategory
    import excel "`s'\adapt_subcategory.xlsx", sheet("Sheet1") firstrow clear
	rename HardProtection_sentence_counts measure_sub_hardprotect	
	label variable measure_sub_hardprotect "measure_sub_hardprotect"
	rename SoftProtection_sentence_counts measure_sub_softprotect
	label variable measure_sub_softprotect "measure_sub_softprotect"
	keep A measure_*
    merge 1:1 A using textual_data_`s'
	drop _merge
    quietly save textual_data_`s', replace


gen state = "`s'"
save textual_data_`s', replace
}


*************** Aggregate states *******************
** loop through folder to get all folder names = state abbrev
local path1 "$directory"
local state : dir "`path1'" dirs "*"
display `state'

clear
foreach s of local state {
	append using "textual_data_`s'"
}

*************** Combine with prior data *******************
** clean up city names to match with data on flood risk
replace city_name="Hollywood city" if city_name=="Hollywood City"
replace city_name="Melbourne city" if city_name=="Melbourne City"
replace city_name = "Baytown city" if city_name == "Baytown"
replace city_name = "Boston city" if city_name == "Boston"
replace city_name = "Somerville city" if city_name == "Somerville"
replace city_name = "Attleboro city" if city_name == "Attleboro"
replace city_name = "Barnstable Town city" if city_name == "Barnstable Town"
replace city_name = "Beverly city" if city_name == "Beverly"
replace city_name = "Brockton city" if city_name == "Brockton"
replace city_name = "Cambridge city" if city_name == "Cambridge"
replace city_name = "Chicopee city" if city_name == "Chicopee City"
replace city_name = "Everett city" if city_name == "Everett"
replace city_name = "Fall River city" if city_name == "Fall River"
replace city_name = "Haverhill city" if city_name == "Haverhill"
replace city_name = "Lawrence city" if city_name == "Lawrence"
replace city_name = "Lowell city" if city_name == "Lowell"
replace city_name = "Lynn city" if city_name == "Lynn"
replace city_name = "Malden city" if city_name == "Malden"
replace city_name = "Medford city" if city_name == "Medford"
replace city_name = "Methuen Town city" if city_name == "Methuen Town"
replace city_name = "New Bedford city" if city_name == "New Bedford"
replace city_name = "Newton city" if city_name == "Newton"
replace city_name = "Peabody city" if city_name == "Peabody"
replace city_name = "Pittsfield city" if city_name == "Pittsfield City"
replace city_name = "Quincy city" if city_name == "Quincy"
replace city_name = "Revere city" if city_name == "Revere"
replace city_name = "Salem city" if city_name == "Salem"
replace city_name = "Springfield city" if city_name == "Springfield City"
replace city_name = "Taunton city" if city_name == "Taunton"
replace city_name = "Waltham city" if city_name == "Waltham"
replace city_name = "Westfield city" if city_name == "Westfield City"
replace city_name = "Weymouth Town city" if city_name == "Weymouth"
replace city_name = "Worcester city" if city_name == "Worcester City"
replace city_name = "Charlottesville city" if city_name == "CharlottesvilleCity"
replace city_name = "Charlotte city" if city_name == "Charolette City"
replace city_name = "Boise City city" if city_name == "Boise City"
replace city_name = "Las Vegas city" if city_name == "Las Vegas"
replace city_name = "North Las Vegas city" if city_name == "North Las Vegas"
replace city_name = "Santa Rosa City" if city_name == "Santa Rose City"

** split state into two parts
split state, p("_")
gen notable = !missing(state2)
drop state state2 state3

rename state1 stateabbv
replace stateabbv = upper(stateabbv)
gen city_name_lower = lower(city_name)
rename city_name city_name_old

save 2_stata/textual_adapt, replace


*===============================================================================
**# 2. Import Adaptation keyword frequency 
** loop through folder to get all folder names = state abbrev
local path1 "$directory"
local state : dir "`path1'" dirs "*"
display `state'

** loop through all states to combine hazards_flood_diff_distance_size
import excel "AL\adapt_counts.xlsx", sheet("Sheet1") firstrow clear
save adapt_words, replace
foreach s of local state {
	import excel "`s'\adapt_counts.xlsx", sheet("Sheet1") firstrow clear
	gen state = "`s'"
	append using adapt_words
	save adapt_words, replace
	sleep 500
}
drop if missing(state)
keep if strpos(state,"table")==0

save adapt_words, replace

*===============================================================================
* 3. Create robustness measures 

** robustness measure 1: create measures excluding keywords toooo common 
use adapt_words, clear

gen count1 = 0
local stormwater *stormwat stormwat* 
foreach i of varlist `stormwater' {
replace count1 = count1+`i'
}

gen count2 = 0
local drainage drain* improv_drainag  improv_road_drainag street_drainag 
foreach i of varlist `drainage' {
replace count2 = count2+`i'
}

gen count12 = count1 + count2

order `stormwater'  `drainage'

order A city_index state  count1 count2 count12 
keep A city_index state count1 count2 count12 
rename (count1 count2 count12) (count_stormwater count_drainage count_stormwater_drain)
gen stateabbv = upper(state)
drop state
save adapt_stormwater_drainage_count, replace


** robustness measure 2: max each keyword at 1,5,10 times
use adapt_words, clear

egen measure_adapt_keywords = rowtotal(beach_nourishment - flood_regul)

foreach i of varlist beach_nourishment - flood_regul {
replace `i' = 10 if `i'>10
}
egen measure_adapt_max10 = rowtotal(beach_nourishment - flood_regul)

foreach i of varlist beach_nourishment - flood_regul {
replace `i' = 5 if `i'>5
}
egen measure_adapt_max5 = rowtotal(beach_nourishment - flood_regul)

rename total_number_of_distinct_keyword measure_adapt_max1

order A city_index state measure_adapt_max10 measure_adapt_max5 measure_adapt_max1 measure_adapt_keywords
keep A city_index state measure_adapt_max10 measure_adapt_max5 measure_adapt_max1 measure_adapt_keywords
gen stateabbv = upper(state)
drop state
save adapt_words_max5_measure, replace


*===============================================================================
**# 4. Import flood risk data and clean 

**** import zip-level flood risk, combine to city-level
** source: https://assets.firststreet.org/uploads/2020/06/first_street_foundation__first_national_flood_risk_assessment.pdf 
** create local of state abbreviations 
gen state = ""
statastates, name(state)
drop if inlist(state_abbrev,"AK","HI","DC")
replace state_abbrev = "\"+state_abbrev
levelsof state_abbrev, local(states) 

clear
foreach i of local states {
 preserve
import delimited "Flood risk\2020 property flood data\data from url`i'\Zipcode_Summary.csv", varnames(1) numericcols(12) clear 
 save floodrisk_zip, replace
 restore
 append using floodrisk_zip
}
*some zip across multiple states, add the total 
gen zip  = string(zipcode,"%05.0f")
collapse (sum) totalproperties fspropertiesatrisk2020total, by(zip)
gen fspropertiesatrisk2020pct = fspropertiesatrisk2020total/totalproperties*100
drop totalproperties
save floodrisk_zip, replace

**** bring in zip to place
** source: http://proximityone.com/zcta-place.htm
import excel "Data on GEO climate risk\GEO ID mapping\zip to place.xlsx", sheet("zip to place") firstrow allstring clear
destring place population, replace
gen place_fips  = string(place,"%05.0f")
keep zip place_fips state population
merge m:1 zip using floodrisk_zip, keepusing(fspropertiesatrisk2020total fspropertiesatrisk2020pct)
keep if _merge==3
** generate weight for each zipcode: if a zip is mapped to two cities, allocate properties at risk based on population ratio 
bys zip: egen zip_pop = sum(population)
gen zip_weight = population/zip_pop
gen fspropertiesatrisk2020total_w = fspropertiesatrisk2020total*zip_weight
collapse (mean) fspropertiesatrisk2020total_w fspropertiesatrisk2020pct [aw= population ] ,by(place_fips state)
rename fspropertiesatrisk2020total_w fspropertiesatrisk2020total
rename state stateabbv
save floodrisk_place, replace

**** bring in city name 
** source: https://www2.census.gov/programs-surveys/popest/datasets/2010-2011/cities/totals/sub-est2011-ip.csv
clear
import delimited "Data on GEO climate risk\GEO ID mapping\place population\sub-est2011-ip.csv", varnames(1) encoding(ISO-8859-9) 
gen place_fips  = string(place,"%05.0f")
statastates, f(state)
destring census2010pop, replace force
** 6 cities without population data, drop
drop if missing(census2010pop)
order state_abbrev place_fips census2010pop name
keep state_abbrev place_fips census2010pop name
rename state_abbrev stateabbv 
rename name city_name
save temp, replace

**** Bring city name into flood risk data
use floodrisk_place, clear
merge 1:1 stateabbv place_fips using temp
keep if _merge==3
drop _merge
save city_sample, replace

**** bring in city obligator ID to map back to Muni Altas
**** note: this data is from Muni Atlas  
use cusip6_atlas, clear
** keep those with place id = geotype 160
keep if  geotype=="160"
gen place_fips=substr(geoid,3,5) 
** keep sector = 120 = city
keep if sector=="120"
** create a unique ID for city place (place_fips can repeat across states)
egen city_id = group(place_fips stateabbv)
save temp, replace

**** bring obligator ID into flood risk city-level data
use city_sample, clear
merge 1:m stateabbv place_fips using temp
keep if _merge==3
drop _merge
keep stateabbv- city_name obligorid
duplicates drop 
save city_sample, replace


*===============================================================================
**# 5. Combine and Export to excel for R to run regression 

use 2_stata/textual_adapt, clear
keep if notable==0 //we use the sample with tables 


**** bring in flood risk data and city obligator ID
merge m:1 stateabbv city_name_lower using city_sample
keep if _merge==3
drop _merge


**** Robust measure 1 bring into main file
merge 1:1 A city_index stateabbv using adapt_stormwater_drainage_count
keep if _merge==3
drop _merge

** create new measures of adapt for robustness
gen measure_adapt_no_stormwater = measure_adapt - count_stormwater
gen measure_adapt_no_stormw_drain = measure_adapt - count_stormwater_drain
gen measure_adapt_no_drain = measure_adapt - count_drainage
replace measure_adapt_no_stormwater=0 if measure_adapt_no_stormwater<0
replace measure_adapt_no_stormw_drain=0 if measure_adapt_no_stormw_drain<0
replace measure_adapt_no_drain=0 if measure_adapt_no_drain<0


**** Robust measure 2 bring into main file
merge 1:1 A city_index stateabbv using adapt_words_max5_measure
keep if _merge==3
drop _merge

drop city_name_old A city_index
rename keyword_adapt keyword_adapt_old
rename measure_adapt_keywords keyword_adapt
drop count_stormwater count_drainage count_stormwater_drain keyword_adapt_old

save 2_stata/textual_data_all, replace

** export to excel in previous format, except adding state
export excel using "1 Combined Data\keywords_measures.xlsx", firstrow(variables) replace




