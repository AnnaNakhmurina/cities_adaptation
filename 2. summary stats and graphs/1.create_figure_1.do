**************************************
* Local Governments and Climate Change
* Create Figure 1
**************************************
set more off
clear all

global directory "folder containing data"

cd "$directory"


*===============================================================================
* 1. Clean data for Figure 1 
use adapt_words, clear

collapse (sum) beach_nourishment - flood_regul

** xpose
xpose, clear varname
rename _varname keyword

** group the keywords 
gen keyword_group = keyword
replace keyword_group="drainage" if strpos(keyword,"drain")>0
replace keyword_group="flood" if strpos(keyword,"flood")>0
replace keyword_group="hurricane" if strpos(keyword,"hurrican")>0

replace keyword_group="buyout" if strpos(keyword,"buyout")>0
replace keyword_group="dike" if strpos(keyword,"dikes")>0
replace keyword_group="dyke" if strpos(keyword,"dykes")>0
replace keyword_group="rain garden" if strpos(keyword,"rain_gardens")>0
replace keyword_group="retention" if strpos(keyword,"retention")>0
replace keyword_group="sandbag" if strpos(keyword,"sandbags")>0
replace keyword_group="sea level rise" if strpos(keyword,"sea_level")>0
replace keyword_group="seawall" if strpos(keyword,"sea_wall")>0
replace keyword_group="seawall" if strpos(keyword,"seawal")>0
replace keyword_group="shoreline" if strpos(keyword,"shorelin")>0

replace keyword_group="spillway" if strpos(keyword,"spillways")>0
replace keyword_group="storm water" if strpos(keyword,"storm_water")>0
replace keyword_group="storm water" if strpos(keyword,"stormwat")>0
replace keyword_group="tidal valve" if strpos(keyword,"valv")>0
replace keyword_group="wind" if strpos(keyword,"wind")>0

replace keyword_group="bulkhead" if strpos(keyword,"bulkheads")>0
replace keyword_group="dike" if strpos(keyword,"dyke")>0
replace keyword_group="embankment" if strpos(keyword,"embankments")>0
replace keyword_group="dune" if strpos(keyword,"dune")>0
replace keyword_group="groyne" if strpos(keyword,"groynes")>0
replace keyword_group="groyne" if strpos(keyword,"groins")>0
replace keyword_group="jetty" if strpos(keyword,"jetties")>0
replace keyword_group="mudflat" if strpos(keyword,"mud")>0
replace keyword_group="revetment" if strpos(keyword,"revetments")>0

replace keyword_group="beach nourishment" if strpos(keyword,"beach")>0
replace keyword_group="wetland" if strpos(keyword,"wetland")>0
replace keyword_group="riverbank" if strpos(keyword,"bank_stabil")>0
replace keyword_group="oyster" if strpos(keyword,"oyster")>0

collapse (sum) v1, by(keyword_group)
graph pie v1 if v1>2000, over(keyword_group) plabel(_all percent)

** next can change keywords not in top 7 as other
replace keyword_group = "other" if v1<2000
collapse (sum) v1, by(keyword_group)
save adapt_figure1_data

*===============================================================================
* 2. Plot Figure 1 
use adapt_figure1_data, clear
graph pie v1, over(keyword_group) plabel(_all percent, gap(25) for(%9.2f))  plabel(_all name)  graphregion(margin(large)) legend(off)
graph export figure_pie_keywordsgroup.jpg, replace