
# This file merges the collected textual data on sample cities with the data from a variety of other sources. The output is saved in .fst, .csv, .dta formats.

rm(list=ls())
gc()

library(readxl)
library("writexl")
library(dplyr)
library(raster)
library(rgdal)
library(tidyverse)
library(magrittr)
library(MASS)
library(lfe)
library(lsr)
library(jtools)
library(stargazer)
library(papeR)
library(gridExtra)
library(naniar)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(corrr)
library(fst)
library(foreign)
library(dplyr)
library(haven) # read in .dta

#---------------------------------------------
# Define functions
#---------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

#-----------------------------------------------------------------
# (1) Load the data parsed with textual analysis in Python
#-----------------------------------------------------------------

text_data = read_excel( "../data/datasets/keywords_measures_withtable_230710.xlsx") %>% filter( year <= 2021 ) %>% unique()

# Remove duplicated documents and documents that were turned into the text by mistake (all of those only relate to FL):
# Tampa:
text_data = text_data %>% filter ( docs_name %!in% c("2015-tampa-budget-presentation.pdf", "2016-tampa-budget-presentation.pdf", "2017-tampa-budget-presentation.pdf",
                                                     "2018-tampa-budget-presentation.pdf", "2019-tampa-budget-presentation.pdf",
                                                     "FY2015 Budget Supplement.pdf", "FY2015 Operating and Capital Budget Part 1.pdf", 
                                                     "FY2015 Operating and Capital Budget Part 2.pdf", "FY2014 Budget Supplement.pdf",
                                                     "FY2014 Operating and Capital Budget.pdf", "FY2013 Budget Supplement.pdf",
                                                     "FY2013 Operating and Capital Budget.pdf"
                                                     ) )

# St. Petersburg:
text_data = text_data %>% filter ( docs_name %!in% c("0_Intro_pages.pdf", "A_Exec_Summary_FINAL.pdf", "B_Fiscal_Policy_FINAL.pdf",
                                                     "C_Position_Summary_FINAL.pdf", "D_Revenue_Highlights_FINAL.pdf", "E_Fund_Budget_Summary_FINAL.pdf",
                                                     "F_Debt_Service_FINAL.pdf", "Fund_Descriptions.pdf", "G_City_Development_FINAL.pdf",
                                                     "Glossary_LCG.pdf", "H_General_Government_Admin_FINAL.pdf", "I_Leisure_and_CS_FINAL.pdf",
                                                     "J_Public_Safety_FINAL.pdf", "K_Public_Works_Administration_FINAL.pdf", "L_CIP_Overview_FINAL.pdf",
                                                     "M_CIP_Summary_FINAL.pdf", "N_CIP_Housing_Gen_Fund_FINAL.pdf", "O_CIP_Penny_Funds_FINAL.pdf",
                                                     "Ordinance_for_book.pdf", "P_CIP_Enterprise_Funds_FINAL.pdf", "Penny_Project_List.pdf",
                                                     "Q_CIP_Other_Funds_FINAL.pdf",
                                                     "A_Executive_Summary_final_v2.pdf", "B_Fiscal_Policies_final_v2.pdf", "C_Position_Summary_final_v2.pdf",
                                                     "D_Revenue_Highlights_FY15_final_v2.pdf", "E_Fund_Summary_final_v2.pdf", "F_Debt_Service_Summary_final_v2.pdf",
                                                     "G_City_Development_final_v2.pdf", "H_General_Government_final_v2.pdf", "I_Leisure_Services_final_v2.pdf",
                                                     "K_Public_Safety_final_v2.pdf", "L_Public_Works_final_v2.pdf", "M_CIP_Overview_final_v2.pdf",
                                                     "N_CIP_Funds_Summary_final_v2.pdf", "O_CIP_Housing_Final_v2.pdf", "P_CIP_Penny_final_v2.pdf",
                                                     "Q_CIP_Enterprise_Funds_final_v2.pdf", "R_CIP_Other_Funds_final_v2.pdf", "S_Ordinance.pdf",
                                                     "T_Penny_3_Plan.pdf", "U_Glossary.pdf", "V_Fund_Descriptions.pdf", "_Intro_final_v2.pdf") )

# Hailea city:
text_data = text_data %>% filter ( docs_name %!in% c("1.Approved-Table of Content.pdf", "10.Approved-Fleet Maintenance.pdf", "11.Approved-General Government.pdf",
                                                     "12.Approved-Human Resources.pdf", "13.Approved-Information Technology.pdf", "14.Approved-Law.pdf",
                                                     "15.Approved-Library.pdf", "16.Approved-Office of the Mayor.pdf", "17.Approved-Office of Management and Budget.pdf",
                                                     "18.Approved-Parks and Recreation.pdf", "19.Approved-Police.pdf", "2.Approved-Budget Message.pdf",
                                                     "20.Approved-911 Communications.pdf", "21.Approved-Retirement.pdf", "22.1Approved-Special Revenue Funds.pdf",
                                                     "22.Approved-Risk Management.pdf", "23.Approved-Revenues.pdf", "24.Approved-Water and Sewer.pdf",
                                                     "25.Approved-Solid Waste.pdf", "26.Approved-Capital Project Funds.pdf", "3.Approved-Introduction.pdf",
                                                     "4.Approved-City Clerks Office.pdf", "5.Approved-Community Development.pdf", 
                                                     "6.Approved-Construction and Maintenance.pdf", "7.Approved-Education and Community Services.pdf", 
                                                     "8.Approved-Finance.pdf", "9.Approved-Fire.pdf") )

# Cape Coral
text_data = text_data %>% filter ( docs_name %!in% c("2017-cape coral-gulf_coast_village_budget.pdf", "2018-cape coral-gulf_coast_village_budget.pdf",
                                                     "2019-cape coral-gulf_coast_village_budget.pdf", "2020-cape coral-gulf_coast_village_budget.pdf") )

# Pompano Beach:
text_data = text_data %>% filter ( docs_name %!in% c("Adopted_Operating Budget FY 2014.pdf") )

#Jupiter
text_data = text_data %>% filter ( docs_name %!in% c("2018-Jupiter-annual_budget.pdf") )

#Oakland Park
text_data = text_data %>% filter ( docs_name %!in% c("2019-oakland park-annual_budget_at_a_glance.pdf", "2020-oakland park-annual_budget_at_a_glance.pdf") )

# Cutler Bay 
text_data = text_data %>% filter ( docs_name %!in% c("2019-cutler bay-annual_budget_summary.pdf" ) )

# Ocala 
text_data = text_data %>% filter ( docs_name %!in% c("2017-Ocala-annual_budget summary.pdf", "2018-Ocala-budget_summary.pdf", "2019-Ocala-budget_summary.pdf" ) )

# Melboune 
text_data = text_data %>% filter ( docs_name %!in% c("2015-Melboure-annual_budget.pdf" ) )

## Assign an obligor ID to Washington, DC:
text_data$obligorid = ifelse( text_data$city_name  == "Washington city", "AT00007005", text_data$obligorid )
text_data$city_name = ifelse( text_data$city_name  == "Washington city", "District of Columbia", text_data$city_name )

#-----------------------------------------------------------------
# (2) Create textual measures
#-----------------------------------------------------------------

# Create reporting year variable:
text_data$reporting_year = text_data$year
text_data$reporting_year = ifelse( text_data$docs_type == 'Budget', text_data$reporting_year - 1 , text_data$reporting_year )

text_data = text_data %>% filter( reporting_year >= 2013 & reporting_year <= 2020 )

# Aggregate bonds. Needed since some cities have multiple bonds per year. 
bonds_text_data = text_data %>% filter( docs_type == "Bonds" )

bonds_text_data = bonds_text_data %>%
  group_by(place_fips, city_name, obligorid, stateabbv, reporting_year, docs_type) %>%
  dplyr::summarize(
    
                    measure_adapt = sum(measure_adapt, na.rm=TRUE), 
                    measure_sub_hardprotect = sum(measure_sub_hardprotect, na.rm = TRUE),
                    measure_sub_softprotect = sum(measure_sub_softprotect, na.rm = TRUE),
                    
                    measure_adapt_no_stormwater = sum(measure_adapt_no_stormwater, na.rm = TRUE),
                    measure_adapt_no_stormw_drain  = sum(measure_adapt_no_stormw_drain, na.rm = TRUE),
                    measure_adapt_no_drain  = sum(measure_adapt_no_drain, na.rm = TRUE),
                    
                    measure_adapt_max10 = sum(measure_adapt_max10, na.rm = TRUE),
                    measure_adapt_max5 = sum(measure_adapt_max5, na.rm = TRUE),
                    measure_adapt_max1 = sum(measure_adapt_max1, na.rm = TRUE),
                    
                    ngroup_adapt = sum(ngroup_adapt, na.rm = TRUE),
                    number_of_words = sum(number_of_words, na.rm = TRUE),
                    
                    keyword_adapt = sum(keyword_adapt, na.rm = TRUE),
                    measure_safety = sum(measure_safety, na.rm=TRUE), 
                    
                    total_number_of_sentences = sum(number_of_sentences, na.rm = TRUE),
                    
                    totalproperties = unique(totalproperties),
                    femapropertiesatrisk2020total = unique(femapropertiesatrisk2020total),
                    fspropertiesatrisk2020total = unique(fspropertiesatrisk2020total),
                    femapropertiesatrisk2020pct = unique(femapropertiesatrisk2020pct),
                    fspropertiesatrisk2020pct = unique(fspropertiesatrisk2020pct))


# Agregate ACFRs and Budgets (sometimes they have multiple pdfs per doc, just sum them up)
ACFR_budget_text_data = text_data %>% filter( docs_type != "Bonds" )

ACFR_budget_text_data = ACFR_budget_text_data %>%
  group_by(place_fips, city_name, obligorid, stateabbv, reporting_year, docs_type) %>%
  dplyr::summarize(
    
    measure_adapt = sum(measure_adapt, na.rm=TRUE), 
    measure_sub_hardprotect = sum(measure_sub_hardprotect, na.rm = TRUE),
    measure_sub_softprotect = sum(measure_sub_softprotect, na.rm = TRUE),
    
    measure_adapt_no_stormwater = sum(measure_adapt_no_stormwater, na.rm = TRUE),
    measure_adapt_no_stormw_drain  = sum(measure_adapt_no_stormw_drain, na.rm = TRUE),
    measure_adapt_no_drain  = sum(measure_adapt_no_drain, na.rm = TRUE),
    
    measure_adapt_max10 = sum(measure_adapt_max10, na.rm = TRUE),
    measure_adapt_max5 = sum(measure_adapt_max5, na.rm = TRUE),
    measure_adapt_max1 = sum(measure_adapt_max1, na.rm = TRUE),
    
    ngroup_adapt = sum(ngroup_adapt, na.rm = TRUE),
    number_of_words = sum(number_of_words, na.rm = TRUE),
    
    keyword_adapt = sum(keyword_adapt, na.rm = TRUE),
    measure_safety = sum(measure_safety, na.rm=TRUE), 
    
    total_number_of_sentences = sum(number_of_sentences, na.rm = TRUE),
    
    totalproperties = unique(totalproperties),
    femapropertiesatrisk2020total = unique(femapropertiesatrisk2020total),
    fspropertiesatrisk2020total = unique(fspropertiesatrisk2020total),
    femapropertiesatrisk2020pct = unique(femapropertiesatrisk2020pct),
    fspropertiesatrisk2020pct = unique(fspropertiesatrisk2020pct))

# Put Bond data and ACFR + Budget data back together:
text_data = rbind( bonds_text_data, ACFR_budget_text_data )

# Select only those city-year-documents that have more than 10 sentences:
text_data = text_data %>% filter( total_number_of_sentences > 10 )

# Scale by n sentences
all_textual_measures = text_data %>%
  mutate(
         measure_safety_scaled = measure_safety / total_number_of_sentences * 10000,
         measure_adapt_scaled = measure_adapt / total_number_of_sentences * 10000,
         measure_sub_hardprotect_scaled = measure_sub_hardprotect / total_number_of_sentences * 10000,
         measure_sub_softprotect_scaled = measure_sub_softprotect / total_number_of_sentences * 10000,
         measure_adapt_no_stormwater_scaled = measure_adapt_no_stormwater / total_number_of_sentences * 10000,
         measure_adapt_no_stormw_drain_scaled = measure_adapt_no_stormw_drain / total_number_of_sentences * 10000,
         measure_adapt_no_drain_scaled = measure_adapt_no_drain / total_number_of_sentences * 10000,
         )

all_textual_measures = all_textual_measures %>%dplyr::select(-totalproperties,            
                                                             -femapropertiesatrisk2020total, -fspropertiesatrisk2020total, 
                                                             -femapropertiesatrisk2020pct,
                                                             -fspropertiesatrisk2020pct  ) %>% unique()

#-----------------------------------------------------------------
# (3) Load party affiliation data
#-----------------------------------------------------------------

party_affiliation = read_excel( "../data/datasets/party.xlsx" )

party_affiliation = party_affiliation %>% dplyr::select(year, obligorid, party )
party_affiliation = party_affiliation %>% filter( !is.na(obligorid) )

#-----------------------------------------------------------------
# (4) Load budget outlook data
#-----------------------------------------------------------------

capital_budget_outlook  = read_excel( "../data/datasets/capital_budget_outlook.xlsx" )
capital_budget_outlook$CB_reported_outlook_years = as.numeric( capital_budget_outlook$CB_reported_outlook_years )

# Make sure that the year is aligned with reporting years in the text data (same logic as for budget text data)
capital_budget_outlook$reporting_year = as.numeric( capital_budget_outlook$year ) - 1 
capital_budget_outlook$year = NULL

capital_budget_outlook = capital_budget_outlook %>% dplyr::select( -city_name, -stateabbv  )

#-----------------------------------------------------------------
# (5) Load Atlas population data
#-----------------------------------------------------------------

population = read_excel( "../data/datasets/population_sample_altas.xlsx" ) 
population$year = as.numeric( population$Year )

#-----------------------------------------------------------------------
# (6) Load Atlas debt outstanding data and scale it with population data
#-----------------------------------------------------------------------

total_outstanding = read_excel( "../data/datasets/atlas_debt_outstanding.xlsx" ) 
total_outstanding$year = as.numeric( total_outstanding$year )
total_outstanding$TotalOutstanding = as.numeric( total_outstanding$TotalOutstanding )

total_outstanding =  total_outstanding %>% 
                      left_join(population, by = c("ObligorId", "year")) %>% unique() %>% mutate(scaled_outstanding = TotalOutstanding / Population) 

total_outstanding = total_outstanding %>% dplyr::select ( ObligorId,   year, scaled_outstanding )

#-----------------------------------------------------------------------
# (7) Load Atlas financial data for UFB ratio
#-----------------------------------------------------------------------

all_financial = read_excel( "../data/datasets/atlas_financial.xlsx") 
# Restrict to our sample:
all_financial = all_financial %>% filter( ObligorId %in% text_data$obligorid )
# Select only the finanials pertaining to governmental activities & restrict to our sample period
all_financial =  all_financial %>% filter( ReportType == "Governmental Activities" & year >= 2013 & year <= 2020 )

# Select the correct number (the data provides both estimates that were originally reported and the corrections, select the correct number from those)
all_f = data.frame()
ids = unique( all_financial$ObligorId )
for( i in 1:length(ids) ){
  
  id = ids[i]
  sub = all_financial %>% filter ( ObligorId == id )
  
  for( y in 2013:2020 ){
    
    sub_sub = sub %>% filter ( year == y )
     
    if( nrow(sub_sub) >= 2 ){ sub_sub = sub_sub %>% filter ( IsAOR == 0 ) }
    if( nrow(sub_sub) > 1 ){ sub_sub = sub_sub %>% filter ( DocId == max(sub_sub$DocId)  ) }
    
    all_f = rbind( all_f,  sub_sub )
    
  }
}

all_f$unrestricted_fund_balance_over_total_expense = as.numeric( all_f$Unrestricted ) / as.numeric( all_f$TotalExpense )
all_f = all_f %>% dplyr::select( ObligorId, year, unrestricted_fund_balance_over_total_expense   ) 

#-----------------------------------------------------------------------
# (8) Load Atlas FUND financial data
#-----------------------------------------------------------------------

fund_financial = read_excel( "../data/datasets/atlas_fund_financial.xlsx") 

# Load Atlas reference files: 
ref_obligor = read_excel( "../data/datasets/atlas_ref_obligor.xlsx") 
ref_obligor = ref_obligor  %>% dplyr::select(-GeoId, -GeoType, -GeoComp, -Sector ) # we are not using these variables
# Restrict to the sample cities
ref_obligor = ref_obligor %>% filter( ObligorId %in% text_data$obligorid ) 

# Join fund financial data with the population data
fund_financial =  fund_financial %>% 
  left_join(ref_obligor, by = c("ObligorId")) %>%
  left_join(population, by = c("ObligorId", "reporting_year" = "year"))

fund_financial = fund_financial %>% 
  group_by( ObligorId, reporting_year) %>%
  mutate(
         TotalExpense = sum( TotalExpense, na.rm = T) , # sum over all adaptation-related related gvt funds
         scaled_expense = sum( TotalExpense, na.rm = T) / Population,
         )  %>% 
  filter( reporting_year <= 2020 ) %>% # restrict to our sample period 
  dplyr::select( ObligorId, reporting_year, scaled_expense )  %>% unique()

# Rename variables to indicate that scaled expense relates to the adaptation related funds
names( fund_financial ) = c( "ObligorId", "reporting_year", "fund_scaled_expense")

#-----------------------------------------------------------------------
# (9) Load Atlas demographic data
#-----------------------------------------------------------------------

demographic_data = read.fst( "../data/datasets/atlas_demographic.fst") 
demographic_data = demographic_data %>% filter( ObligorId %in% text_data$obligorid )
# Select only the variables we use in the tests
demographic_data = demographic_data %>% dplyr::select( year, ObligorId, PerHouseholdIncomeAvg )
demographic_data$year = as.numeric( demographic_data$year )

#-----------------------------------------------------------------------
# (10) Load Yale Climate Opinion data
#-----------------------------------------------------------------------

yale_climate_opinion  = read_csv(  "../data/datasets/YCOM_2020_Data.csv" ) 

# Select county-level data and opinions about whether local officials should do something
yale_climate_opinion = yale_climate_opinion %>% filter( GeoType == "County" ) %>% dplyr::select( GeoName, localofficials )
# Load the county-id mapping file
county_map = read_excel( '../data/datasets/yale_opinion_geoname_map_all_states.xlsx' )
# Merge the data
yale_climate_opinion = left_join(  county_map, yale_climate_opinion  ) %>% dplyr::select( -city_name, -stateabbv )

#-----------------------------------------------------------------------
# (11) Load the FEMA insurance data
#----------------------------------------------------------------------

fema_insurance = read_excel( "../data/datasets/fema_insurance.xlsx" ) 
fema_insurance$reporting_year = fema_insurance$year
fema_insurance$place_fips = as.character( fema_insurance$place_fips )
fema_insurance = fema_insurance %>% dplyr::select( stateabbv, place_fips,  crsclasscode, reporting_year ) # Restrict only to the variables that we use/merge on.

#-----------------------------------------------------------------
# (12) Load the coastal indicators 
#---------------------------------------------------------------

coastal = read_excel( "../data/datasets/coastal_indicators.xlsx") 

#-----------------------------------------------------------------
# (13) Load and merge state level grants data 
#-----------------------------------------------------------------

state_grants = read_excel( "../data/datasets/state_grants.xlsx" )
state_grants$state_grant_largest_amount = ifelse( is.na( state_grants$state_grant_largest_amount ), 0, state_grants$state_grant_largest_amount )
state_grants$state_grant_above_median = ifelse( state_grants$state_grant_largest_amount >= median (state_grants$state_grant_largest_amount ), 1, 0 )
state_grants = state_grants %>% dplyr::select( stateabbv, state_grant_above_median )

#-----------------------------------------------------------------
# (14) Load NOAA data and create post variable
#-----------------------------------------------------------------

# Load and clean NOAA data
noaa = read_excel(  "../data/datasets/noaa_state_year.xlsx" ) 
noaa = noaa %>% dplyr::select( year, stateabbv, tropicalcyclonecount, tropicalcyclonecostrange ) %>% filter( year > 2012 )

noaa = noaa %>% separate( tropicalcyclonecostrange, c("tropicalcyclonecostrange_min", "tropicalcyclonecostrange_max"), "-" )
noaa$first_hur_dummy = 0

# Create a Post variable
noaa = noaa %>%
  group_by(stateabbv) %>% 
  mutate(first_hur_dummy = replace(first_hur_dummy, which(tropicalcyclonecount != 0)[1], 1 ) )

first_years = noaa %>% filter( first_hur_dummy == 1 ) %>% dplyr::select( year, stateabbv )
names( first_years ) = c( "first_hur_year", "stateabbv" )

noaa = left_join( noaa, first_years, by =  "stateabbv"  ) %>% unique()
noaa$post = ifelse( noaa$year >= noaa$first_hur_year, 1, 0 )

noaa$post = ifelse( is.na( noaa$post ), 0, noaa$post )
noaa = noaa %>% dplyr::select( stateabbv, year, post,  first_hur_year )

#-----------------------------------------------------------------------
# (15) Merge all previously downloaded data and save it
#-----------------------------------------------------------------------

# Create the panel for all possible city-year combinations
panel <- expand.grid(reporting_year = 2013:2020, 
                     obligorid = unique(all_textual_measures$obligorid), 
                     docs_type = unique(all_textual_measures$docs_type), 
                     stringsAsFactors = FALSE) 

ids = all_textual_measures %>% dplyr::select( place_fips, city_name, obligorid, stateabbv ) %>% unique()
# Add ids to the empty panel data
panel = panel %>% left_join( ids )
all_textual_measures = panel  %>% left_join(all_textual_measures)

# Restrict flood risk data only to the percentage of properties under risk (the only variable that we use). 
# We need to do this because sometimes flood risk data is missing from some city-doc types. 
# This ensures that flood risk  values are filled and consistent across the board
flood_risk = text_data %>% dplyr::select( obligorid, fspropertiesatrisk2020pct  ) %>% unique()

# combine all the data
combine = all_textual_measures %>% 
                left_join(party_affiliation, by = c("obligorid" = "obligorid", "reporting_year" = "year")) %>% 
                  left_join(population %>% mutate(year = as.numeric(year)), by = c("obligorid" = "ObligorId", "reporting_year" = "year")) %>% 
                    left_join(total_outstanding %>% mutate(year = as.numeric(year)), by = c("obligorid" = "ObligorId", "reporting_year" = "year")) %>% 
                       left_join( all_f, by = c("obligorid" = "ObligorId", "reporting_year" = "year"))  %>%
                          left_join(capital_budget_outlook %>% mutate(reporting_year = as.numeric(reporting_year)), 
                                    by = c("obligorid" = "obligorid", "reporting_year" = "reporting_year"))%>% 
                            left_join( fund_financial, by = c("obligorid" = "ObligorId",  "reporting_year" )) %>%
                                left_join( yale_climate_opinion , by = c("obligorid" = "obligorid" ) ) %>%
                                          left_join( fema_insurance ) %>%
                                            left_join( demographic_data, by = c("obligorid" = "ObligorId", "reporting_year" = "year"))  %>%
                                              left_join(flood_risk)  %>%
                                                   left_join( coastal, by = c("stateabbv" ) ) %>%
                                                      left_join( state_grants, by = c("stateabbv" ) ) %>%
                                                          left_join( noaa, by = c("stateabbv", "reporting_year" = "year" ) ) %>%
                                                            unique()

# Replace NA with zeroes in the the post variable (referring to post-hurricanes), ensuring that the variable captures underlying construct
combine$post = ifelse( is.na(combine$post), 0, combine$post ) 


# Create Main measures by summing over budgets and annual reports
combine_ACFRs_budgets = combine %>% filter( docs_type != "Bonds" )
main = combine_ACFRs_budgets %>% group_by(obligorid, reporting_year) %>%
  mutate(
    measure_adapt = sum( measure_adapt, na.rm = T ),
    measure_adapt_scaled = sum( measure_adapt_scaled, na.rm = T ),
    measure_sub_hardprotect = sum(measure_sub_hardprotect, na.rm = T),
    measure_sub_hardprotect_scaled = sum(measure_sub_hardprotect_scaled, na.rm = T),
    measure_sub_softprotect = sum(measure_sub_softprotect, na.rm = T),
    measure_sub_softprotect_scaled = sum(measure_sub_softprotect_scaled, na.rm = T),
    measure_safety = sum( measure_safety, na.rm = T ),
    measure_safety_scaled = sum( measure_safety_scaled, na.rm = T ),
    
    measure_adapt_no_stormwater = sum( measure_adapt_no_stormwater, na.rm = T ),
    measure_adapt_no_stormw_drain = sum( measure_adapt_no_stormw_drain, na.rm = T ),
    measure_adapt_no_drain = sum( measure_adapt_no_drain, na.rm = T ),
    
    measure_adapt_max10 = sum(measure_adapt_max10, na.rm = TRUE),
    measure_adapt_max5 = sum(measure_adapt_max5, na.rm = TRUE),
    measure_adapt_max1 = sum(measure_adapt_max1, na.rm = TRUE),
    
    ngroup_adapt = sum(ngroup_adapt, na.rm = TRUE),
    number_of_words = sum(number_of_words, na.rm = TRUE),
    
    keyword_adapt = sum(keyword_adapt, na.rm = TRUE),
    
    total_number_of_sentences = sum(total_number_of_sentences, na.rm = T)

  ) %>% slice(1) %>% mutate(docs_type = "All") %>% ungroup()


# Create Main + Bonds measures by summing over three types of documents 
main_bond = combine %>% group_by(obligorid, reporting_year) %>%
  mutate(
    
    measure_adapt = sum( measure_adapt, na.rm = T ),
    measure_adapt_scaled = sum( measure_adapt_scaled, na.rm = T ),
    measure_sub_hardprotect = sum(measure_sub_hardprotect, na.rm = T),
    measure_sub_hardprotect_scaled = sum(measure_sub_hardprotect_scaled, na.rm = T),
    measure_sub_softprotect = sum(measure_sub_softprotect, na.rm = T),
    measure_sub_softprotect_scaled = sum(measure_sub_softprotect_scaled, na.rm = T),
    measure_safety = sum( measure_safety, na.rm = T ),
    measure_safety_scaled = sum( measure_safety_scaled, na.rm = T ),
    
    
    measure_adapt_no_stormwater = sum( measure_adapt_no_stormwater, na.rm = T ),
    measure_adapt_no_stormw_drain = sum( measure_adapt_no_stormw_drain, na.rm = T ),
    measure_adapt_no_drain = sum( measure_adapt_no_drain, na.rm = T ),
    measure_adapt_no_stormwater_scaled = sum( measure_adapt_no_stormwater_scaled, na.rm = T ),
    measure_adapt_no_drain_scaled = sum( measure_adapt_no_drain_scaled, na.rm = T ),
    measure_adapt_no_stormw_drain_scaled = sum( measure_adapt_no_stormw_drain_scaled, na.rm = T ),
    
    measure_adapt_max10 = sum(measure_adapt_max10, na.rm = TRUE),
    measure_adapt_max5 = sum(measure_adapt_max5, na.rm = TRUE),
    measure_adapt_max1 = sum(measure_adapt_max1, na.rm = TRUE),
    
    ngroup_adapt = sum(ngroup_adapt, na.rm = TRUE),
    number_of_words = sum(number_of_words, na.rm = TRUE),
    
    keyword_adapt = sum(keyword_adapt, na.rm = TRUE),
    
    total_number_of_sentences = sum(total_number_of_sentences, na.rm = T)
    
  ) %>% slice(1) %>% mutate(docs_type = "All") %>% ungroup()


main_bond = unique(main_bond)
main = unique(main)
combine = unique(combine)

## Drop the data with missing number of sentences: -------

combine = combine %>% filter( !is.na( total_number_of_sentences ) & total_number_of_sentences > 0 )
main_bond = main_bond %>% filter( !is.na( total_number_of_sentences ) & total_number_of_sentences > 0 )
main = main %>% filter( !is.na( total_number_of_sentences ) & total_number_of_sentences > 0 )

## Save the data in .fst (quick R format), .xlsx (universal format) ------------

write_fst( main_bond, "../data/cleaned_data/main_bond.fst" ) # R
write_xlsx( main_bond, "../data/cleaned_data/main_bond.xlsx" ) # Excel

write_fst( combine, "../data/cleaned_data/data_by_doc_type.fst" ) # R
write_xlsx( combine, "../data/cleaned_data/data_by_doc_type.xlsx" ) # Excel

write_fst( main, "../data/cleaned_data/main.fst" ) # R
write_xlsx( main, "../data/cleaned_data/main.xlsx" ) # Excel


