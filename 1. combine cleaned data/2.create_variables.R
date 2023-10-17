
library(readxl)
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
library(data.table)
library("writexl")

#---------------------------------------------
# RUN: Define functions
#----------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

winsorize_x = function(x, cut = 0.01){
  cut_point_top <- quantile(x, 1 - cut, na.rm = T)
  cut_point_bottom <- quantile(x, cut, na.rm = T)
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top
  j = which(x <= cut_point_bottom) 
  x[j] = cut_point_bottom
  return(x)
}


#-----------------------------------------------------------------
# (1) Load the merged data on sample cities
#-----------------------------------------------------------------

cc_data = read.fst( "../data/cleaned_data/main_bond.fst" )
cc_data_by_doc_type = read.fst ( "../data/cleaned_data/data_by_doc_type.fst" )
cc_data_ACFRs_budgets = read.fst ( "../data/cleaned_data/main.fst" )

#-----------------------------------------------------------------
# (2.0) Winsorize unscaled variables
#-----------------------------------------------------------------

cc_data <- cc_data %>%
  mutate_at(vars( measure_adapt, 
                  measure_sub_hardprotect, measure_sub_softprotect,
                  measure_safety,
                  measure_adapt_no_stormwater, measure_adapt_no_stormw_drain, measure_adapt_no_drain,
                  measure_adapt_max1, measure_adapt_max5, measure_adapt_max10,
                  ngroup_adapt, number_of_words, keyword_adapt, total_number_of_sentences
                  ), .funs = list(~winsorize_x(.)))

cc_data_ACFRs_budgets <- cc_data_ACFRs_budgets %>%
  mutate_at(vars(  measure_adapt, 
                   measure_sub_hardprotect, measure_sub_softprotect,
                   measure_safety,
                   measure_adapt_no_stormwater, measure_adapt_no_stormw_drain, measure_adapt_no_drain,
                   measure_adapt_max1, measure_adapt_max5, measure_adapt_max10,
                   ngroup_adapt, number_of_words, keyword_adapt, total_number_of_sentences
  ), .funs = list(~winsorize_x(.)))


# Winsorize within document type for the cc_data_by_doc_type
bonds = cc_data_by_doc_type %>% filter( docs_type == "Bonds" )
ACFRs = cc_data_by_doc_type %>% filter( docs_type == "CAFR" )
budgets = cc_data_by_doc_type %>% filter( docs_type == "Budget" )

bonds <- bonds %>%
  mutate_at(vars(  measure_adapt, 
                   measure_sub_hardprotect, measure_sub_softprotect,
                   measure_safety,
                   measure_adapt_no_stormwater, measure_adapt_no_stormw_drain, measure_adapt_no_drain,
                   measure_adapt_max1, measure_adapt_max5, measure_adapt_max10,
                   ngroup_adapt, number_of_words, keyword_adapt, total_number_of_sentences
  ), .funs = list(~winsorize_x(.)))

ACFRs <- ACFRs %>%
  mutate_at(vars(  measure_adapt, 
                   measure_sub_hardprotect, measure_sub_softprotect,
                   measure_safety,
                   measure_adapt_no_stormwater, measure_adapt_no_stormw_drain, measure_adapt_no_drain,
                   measure_adapt_max1, measure_adapt_max5, measure_adapt_max10,
                   ngroup_adapt, number_of_words, keyword_adapt, total_number_of_sentences
  ), .funs = list(~winsorize_x(.)))

budgets <- budgets %>%
  mutate_at(vars(  measure_adapt, 
                   measure_sub_hardprotect, measure_sub_softprotect,
                   measure_safety,
                   measure_adapt_no_stormwater, measure_adapt_no_stormw_drain, measure_adapt_no_drain,
                   measure_adapt_max1, measure_adapt_max5, measure_adapt_max10,
                   ngroup_adapt, number_of_words, keyword_adapt, total_number_of_sentences
  ), .funs = list(~winsorize_x(.)))

#-----------------------------------------------------------------
# (2.0) Winsorize scaled variables
#-----------------------------------------------------------------

cc_data$measure_adapt_scaled_wins = winsorize_x( cc_data$measure_adapt_scaled  )
cc_data_ACFRs_budgets$measure_adapt_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_adapt_scaled  )
bonds$measure_adapt_scaled_wins = winsorize_x( bonds$measure_adapt_scaled  )
ACFRs$measure_adapt_scaled_wins = winsorize_x( ACFRs$measure_adapt_scaled  )
budgets$measure_adapt_scaled_wins = winsorize_x( budgets$measure_adapt_scaled  )

cc_data$measure_sub_hardprotect_scaled_wins = winsorize_x( cc_data$measure_sub_hardprotect_scaled  )
cc_data_ACFRs_budgets$measure_sub_hardprotect_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_sub_hardprotect_scaled  )
bonds$measure_sub_hardprotect_scaled_wins = winsorize_x( bonds$measure_sub_hardprotect_scaled  )
ACFRs$measure_sub_hardprotect_scaled_wins = winsorize_x( ACFRs$measure_sub_hardprotect_scaled  )
budgets$measure_sub_hardprotect_scaled_wins = winsorize_x( budgets$measure_sub_hardprotect_scaled  )

cc_data$measure_sub_softprotect_scaled_wins = winsorize_x( cc_data$measure_sub_softprotect_scaled  )
cc_data_ACFRs_budgets$measure_sub_softprotect_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_sub_softprotect_scaled  )
bonds$measure_sub_softprotect_scaled_wins = winsorize_x( bonds$measure_sub_softprotect_scaled  )
ACFRs$measure_sub_softprotect_scaled_wins = winsorize_x( ACFRs$measure_sub_softprotect_scaled  )
budgets$measure_sub_softprotect_scaled_wins = winsorize_x( budgets$measure_sub_softprotect_scaled  )

cc_data$measure_safety_scaled_wins = winsorize_x( cc_data$measure_safety_scaled  )
cc_data_ACFRs_budgets$measure_safety_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_safety_scaled  )
bonds$measure_safety_scaled_wins = winsorize_x( bonds$measure_safety_scaled  )
ACFRs$measure_safety_scaled_wins = winsorize_x( ACFRs$measure_safety_scaled  )
budgets$measure_safety_scaled_wins = winsorize_x( budgets$measure_safety_scaled  )

cc_data$measure_adapt_no_drain_scaled_wins = winsorize_x( cc_data$measure_adapt_no_drain_scaled  )
cc_data_ACFRs_budgets$measure_adapt_no_drain_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_adapt_no_drain_scaled  )
bonds$measure_adapt_no_drain_scaled_wins = winsorize_x( bonds$measure_adapt_no_drain_scaled  )
ACFRs$measure_adapt_no_drain_scaled_wins = winsorize_x( ACFRs$measure_adapt_no_drain_scaled  )
budgets$measure_adapt_no_drain_scaled_wins = winsorize_x( budgets$measure_adapt_no_drain_scaled  )

cc_data$measure_adapt_no_stormwater_scaled_wins = winsorize_x( cc_data$measure_adapt_no_stormwater_scaled  )
cc_data_ACFRs_budgets$measure_adapt_no_stormwater_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_adapt_no_stormwater_scaled  )
bonds$measure_adapt_no_stormwater_scaled_wins = winsorize_x( bonds$measure_adapt_no_stormwater_scaled  )
ACFRs$measure_adapt_no_stormwater_scaled_wins = winsorize_x( ACFRs$measure_adapt_no_stormwater_scaled  )
budgets$measure_adapt_no_stormwater_scaled_wins = winsorize_x( budgets$measure_adapt_no_stormwater_scaled  )

cc_data$measure_adapt_no_stormw_drain_scaled_wins = winsorize_x( cc_data$measure_adapt_no_stormw_drain_scaled  )
cc_data_ACFRs_budgets$measure_adapt_no_stormw_drain_scaled_wins = winsorize_x( cc_data_ACFRs_budgets$measure_adapt_no_stormw_drain_scaled  )
bonds$measure_adapt_no_stormw_drain_scaled_wins = winsorize_x( bonds$measure_adapt_no_stormw_drain_scaled  )
ACFRs$measure_adapt_no_stormw_drain_scaled_wins = winsorize_x( ACFRs$measure_adapt_no_stormw_drain_scaled  )
budgets$measure_adapt_no_stormw_drain_scaled_wins = winsorize_x( budgets$measure_adapt_no_stormw_drain_scaled  )

#-----------------------------------------------------------------
# (2.1) Standardize scaled variables that were winsorized during the last step
#-----------------------------------------------------------------

cc_data$measure_adapt_scaled_wins_z = scale( cc_data$measure_adapt_scaled_wins  )
cc_data_ACFRs_budgets$measure_adapt_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_adapt_scaled_wins  )
bonds$measure_adapt_scaled_wins_z = scale( bonds$measure_adapt_scaled_wins  )
ACFRs$measure_adapt_scaled_wins_z = scale( ACFRs$measure_adapt_scaled_wins  )
budgets$measure_adapt_scaled_wins_z = scale( budgets$measure_adapt_scaled_wins  )

cc_data$measure_safety_scaled_wins_z = scale( cc_data$measure_safety_scaled_wins  )
cc_data_ACFRs_budgets$measure_safety_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_safety_scaled_wins  )
bonds$measure_safety_scaled_wins_z = scale( bonds$measure_safety_scaled_wins  )
ACFRs$measure_safety_scaled_wins_z = scale( ACFRs$measure_safety_scaled_wins  )
budgets$measure_safety_scaled_wins_z = scale( budgets$measure_safety_scaled_wins  )

cc_data$measure_sub_hardprotect_scaled_wins_z = scale( cc_data$measure_sub_hardprotect_scaled_wins  )
cc_data_ACFRs_budgets$measure_sub_hardprotect_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_sub_hardprotect_scaled_wins  )
bonds$measure_sub_hardprotect_scaled_wins_z = scale( bonds$measure_sub_hardprotect_scaled_wins  )
ACFRs$measure_sub_hardprotect_scaled_wins_z = scale( ACFRs$measure_sub_hardprotect_scaled_wins  )
budgets$measure_sub_hardprotect_scaled_wins_z = scale( budgets$measure_sub_hardprotect_scaled_wins  )

cc_data$measure_sub_softprotect_scaled_wins_z = scale( cc_data$measure_sub_softprotect_scaled_wins  )
cc_data_ACFRs_budgets$measure_sub_softprotect_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_sub_softprotect_scaled_wins  )
bonds$measure_sub_softprotect_scaled_wins_z = scale( bonds$measure_sub_softprotect_scaled_wins  )
ACFRs$measure_sub_softprotect_scaled_wins_z = scale( ACFRs$measure_sub_softprotect_scaled_wins  )
budgets$measure_sub_softprotect_scaled_wins_z = scale( budgets$measure_sub_softprotect_scaled_wins  )

cc_data$measure_adapt_no_stormwater_scaled_wins_z = scale( cc_data$measure_adapt_no_stormwater_scaled_wins  )
cc_data_ACFRs_budgets$measure_adapt_no_stormwater_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_adapt_no_stormwater_scaled_wins  )
bonds$measure_adapt_no_stormwater_scaled_wins_z = scale( bonds$measure_adapt_no_stormwater_scaled_wins  )
ACFRs$measure_adapt_no_stormwater_scaled_wins_z = scale( ACFRs$measure_adapt_no_stormwater_scaled_wins  )
budgets$measure_adapt_no_stormwater_scaled_wins_z = scale( budgets$measure_adapt_no_stormwater_scaled_wins  )

cc_data$measure_adapt_no_stormw_drain_scaled_wins_z = scale( cc_data$measure_adapt_no_stormw_drain_scaled_wins  )
cc_data_ACFRs_budgets$measure_adapt_no_stormw_drain_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_adapt_no_stormw_drain_scaled_wins  )
bonds$measure_adapt_no_stormw_drain_scaled_wins_z = scale( bonds$measure_adapt_no_stormw_drain_scaled_wins  )
ACFRs$measure_adapt_no_stormw_drain_scaled_wins_z = scale( ACFRs$measure_adapt_no_stormw_drain_scaled_wins  )
budgets$measure_adapt_no_stormw_drain_scaled_wins_z = scale( budgets$measure_adapt_no_stormw_drain_scaled_wins  )

cc_data$measure_adapt_no_drain_scaled_wins_z = scale( cc_data$measure_adapt_no_drain_scaled_wins  )
cc_data_ACFRs_budgets$measure_adapt_no_drain_scaled_wins_z = scale( cc_data_ACFRs_budgets$measure_adapt_no_drain_scaled_wins  )
bonds$measure_adapt_no_drain_scaled_wins_z = scale( bonds$measure_adapt_no_drain_scaled_wins  )
ACFRs$measure_adapt_no_drain_scaled_wins_z = scale( ACFRs$measure_adapt_no_drain_scaled_wins  )
budgets$measure_adapt_no_drain_scaled_wins_z = scale( budgets$measure_adapt_no_drain_scaled_wins  )

cc_data$fspropertiesatrisk2020pct_z = scale( cc_data$fspropertiesatrisk2020pct  )
cc_data_ACFRs_budgets$fspropertiesatrisk2020pct_z = scale( cc_data_ACFRs_budgets$fspropertiesatrisk2020pct  )
bonds$fspropertiesatrisk2020pct_z = scale( bonds$fspropertiesatrisk2020pct  )
ACFRs$fspropertiesatrisk2020pct_z = scale( ACFRs$fspropertiesatrisk2020pct  )
budgets$fspropertiesatrisk2020pct_z = scale( budgets$fspropertiesatrisk2020pct  )

cc_data_by_doc_type = rbind( bonds, ACFRs )
cc_data_by_doc_type = rbind( cc_data_by_doc_type, budgets )

#--------------------------------------------------------------------------------------------
# (3)  Create lagged variables and merge different data | Winsorize and scale within-data. 
#--------------------------------------------------------------------------------------------

ACFR_sentences = ACFRs %>% dplyr::select( obligorid, reporting_year,
                                              
                                              measure_adapt,
                                              measure_adapt_scaled_wins,
                                              measure_adapt_scaled_wins_z,
                                              measure_safety_scaled_wins,
                                              measure_safety_scaled_wins_z,
                                              
                                              measure_sub_hardprotect, 
                                              measure_sub_hardprotect_scaled_wins,
                                              measure_sub_hardprotect_scaled_wins_z,
                                              
                                              measure_sub_softprotect, 
                                              measure_sub_softprotect_scaled_wins,
                                              measure_sub_softprotect_scaled_wins_z,
                                              
                                              measure_adapt_no_stormwater,
                                              measure_adapt_no_stormwater_scaled_wins,
                                              measure_adapt_no_stormwater_scaled_wins_z,
                                              
                                              measure_adapt_no_stormw_drain,
                                              measure_adapt_no_stormw_drain_scaled_wins,
                                              measure_adapt_no_stormw_drain_scaled_wins_z,
                                              
                                              measure_adapt_no_drain,
                                              measure_adapt_no_drain_scaled_wins,
                                              measure_adapt_no_drain_scaled_wins_z
                                              
)

names(ACFR_sentences) = c("obligorid", "reporting_year",
                          "ACFR_measure_adapt",
                          "ACFR_measure_adapt_scaled_wins",
                          "ACFR_measure_adapt_scaled_wins_z",
                          "ACFR_measure_safety_scaled_wins",
                          "ACFR_measure_safety_scaled_wins_z",
                          
                          "ACFR_measure_sub_hardprotect", 
                          "ACFR_measure_sub_hardprotect_scaled_wins",
                          "ACFR_measure_sub_hardprotect_scaled_wins_z",
                          
                          "ACFR_measure_sub_softprotect", 
                          "ACFR_measure_sub_softprotect_scaled_wins",
                          "ACFR_measure_sub_softprotect_scaled_wins_z",
                          
                          "ACFR_measure_adapt_no_stormwater",
                          "ACFR_measure_adapt_no_stormwater_scaled_wins",
                          "ACFR_measure_adapt_no_stormwater_scaled_wins_z",
                          
                          "ACFR_measure_adapt_no_stormw_drain",
                          "ACFR_measure_adapt_no_stormw_drain_scaled_wins",
                          "ACFR_measure_adapt_no_stormw_drain_scaled_wins_z",
                          
                          "ACFR_measure_adapt_no_drain",
                          "ACFR_measure_adapt_no_drain_scaled_wins",
                          "ACFR_measure_adapt_no_drain_scaled_wins_z"
                          
)

budget_sentences = budgets %>% dplyr::select( obligorid, reporting_year,
                                                  measure_adapt,
                                                  measure_adapt_scaled_wins,
                                                  measure_adapt_scaled_wins_z,
                                                  measure_safety_scaled_wins,
                                                  measure_safety_scaled_wins_z,
                                                  
                                                  measure_sub_hardprotect, 
                                                  measure_sub_hardprotect_scaled_wins,
                                                  measure_sub_hardprotect_scaled_wins_z,
                                                  
                                                  measure_sub_softprotect, 
                                                  measure_sub_softprotect_scaled_wins,
                                                  measure_sub_softprotect_scaled_wins_z,
                                                  
                                                  measure_adapt_no_stormwater,
                                                  measure_adapt_no_stormwater_scaled_wins,
                                                  measure_adapt_no_stormwater_scaled_wins_z,
                                                  
                                                  measure_adapt_no_stormw_drain,
                                                  measure_adapt_no_stormw_drain_scaled_wins,
                                                  measure_adapt_no_stormw_drain_scaled_wins_z,
                                                  
                                                  measure_adapt_no_drain,
                                                  measure_adapt_no_drain_scaled_wins,
                                                  measure_adapt_no_drain_scaled_wins_z
)


names(budget_sentences) = c("obligorid", "reporting_year", 
                            "budget_measure_adapt",
                            "budget_measure_adapt_scaled_wins",
                            "budget_measure_adapt_scaled_wins_z",
                            "budget_measure_safety_scaled_wins",
                            "budget_measure_safety_scaled_wins_z",
                            
                            "budget_measure_sub_hardprotect", 
                            "budget_measure_sub_hardprotect_scaled_wins",
                            "budget_measure_sub_hardprotect_scaled_wins_z",
                            
                            "budget_measure_sub_softprotect", 
                            "budget_measure_sub_softprotect_scaled_wins",
                            "budget_measure_sub_softprotect_scaled_wins_z",
                            
                            "budget_measure_adapt_no_stormwater",
                            "budget_measure_adapt_no_stormwater_scaled_wins",
                            "budget_measure_adapt_no_stormwater_scaled_wins_z",
                            
                            "budget_measure_adapt_no_stormw_drain",
                            "budget_measure_adapt_no_stormw_drain_scaled_wins",
                            "budget_measure_adapt_no_stormw_drain_scaled_wins_z",
                            
                            "budget_measure_adapt_no_drain",
                            "budget_measure_adapt_no_drain_scaled_wins",
                            "budget_measure_adapt_no_drain_scaled_wins_z"
)

bond_sentences = bonds %>% dplyr::select( obligorid, reporting_year,
                                              measure_adapt,
                                              measure_adapt_scaled_wins,
                                              measure_adapt_scaled_wins_z,
                                              measure_safety_scaled_wins,
                                              measure_safety_scaled_wins_z,
                                              
                                              measure_sub_hardprotect, 
                                              measure_sub_hardprotect_scaled_wins,
                                              measure_sub_hardprotect_scaled_wins_z,
                                              
                                              measure_sub_softprotect, 
                                              measure_sub_softprotect_scaled_wins,
                                              measure_sub_softprotect_scaled_wins_z,
                                              
                                              measure_adapt_no_stormwater,
                                              measure_adapt_no_stormwater_scaled_wins,
                                              measure_adapt_no_stormwater_scaled_wins_z,
                                              
                                              measure_adapt_no_stormw_drain,
                                              measure_adapt_no_stormw_drain_scaled_wins,
                                              measure_adapt_no_stormw_drain_scaled_wins_z,
                                              
                                              measure_adapt_no_drain,
                                              measure_adapt_no_drain_scaled_wins,
                                              measure_adapt_no_drain_scaled_wins_z
)

names(bond_sentences) = c("obligorid", "reporting_year", 
                          "bond_measure_adapt",
                          "bond_measure_adapt_scaled_wins",
                          "bond_measure_adapt_scaled_wins_z",
                          "bond_measure_safety_scaled_wins",
                          "bond_measure_safety_scaled_wins_z",
                          
                          "bond_measure_sub_hardprotect", 
                          "bond_measure_sub_hardprotect_scaled_wins",
                          "bond_measure_sub_hardprotect_scaled_wins_z",
                          
                          "bond_measure_sub_softprotect", 
                          "bond_measure_sub_softprotect_scaled_wins",
                          "bond_measure_sub_softprotect_scaled_wins_z",
                          
                          "bond_measure_adapt_no_stormwater",
                          "bond_measure_adapt_no_stormwater_scaled_wins",
                          "bond_measure_adapt_no_stormwater_scaled_wins_z",
                          
                          "bond_measure_adapt_no_stormw_drain",
                          "bond_measure_adapt_no_stormw_drain_scaled_wins",
                          "bond_measure_adapt_no_stormw_drain_scaled_wins_z",
                          
                          "bond_measure_adapt_no_drain",
                          "bond_measure_adapt_no_drain_scaled_wins",
                          "bond_measure_adapt_no_drain_scaled_wins_z"
                          
                          
)
ACFR_sentences = unique(ACFR_sentences)
budget_sentences = unique(budget_sentences)
bond_sentences = unique(bond_sentences)

cc_data = cc_data %>% 
  left_join(ACFR_sentences, by = c( "obligorid", "reporting_year")) 
cc_data_ACFRs_budgets = cc_data_ACFRs_budgets %>% 
  left_join(ACFR_sentences, by = c( "obligorid", "reporting_year")) 

cc_data = cc_data %>% 
  left_join(budget_sentences, by = c( "obligorid", "reporting_year")) %>% 
  left_join(bond_sentences, by = c( "obligorid", "reporting_year"))
cc_data_ACFRs_budgets = cc_data_ACFRs_budgets %>% 
  left_join(budget_sentences, by = c( "obligorid", "reporting_year")) %>% 
  left_join(bond_sentences, by = c( "obligorid", "reporting_year"))

cc_data = unique(cc_data)
cc_data_ACFRs_budgets = unique(cc_data_ACFRs_budgets)

#-----------------------------------------------------------------
# (2.0) Create flood risk indicators
#-----------------------------------------------------------------

states = unique( cc_data$stateabbv )

cc_data$high_flood_risk = 0
cc_data_ACFRs_budgets$high_flood_risk = 0
cc_data_by_doc_type$high_flood_risk = 0

cc_data$high_flood_risk_quartile = 0
cc_data_ACFRs_budgets$high_flood_risk_quartile = 0
cc_data_by_doc_type$high_flood_risk_quartile = 0

count = cc_data %>% count( obligorid ) 
for( i in 1:length(states) ){
  
  state = states[i]
  state_data =  cc_data %>% filter( stateabbv == state ) 
  count_n = state_data %>% count( obligorid ) %>% filter( n > 2 )
  
  state_data = state_data %>% filter( obligorid %in% count_n$obligorid ) %>%
    dplyr::select( fspropertiesatrisk2020pct ) %>% unique()
  
  state_median = median( state_data$fspropertiesatrisk2020pct )
  
  cc_data$high_flood_risk = ifelse( (cc_data$stateabbv == state & cc_data$fspropertiesatrisk2020pct >= state_median),
                                    1, cc_data$high_flood_risk )
  cc_data_ACFRs_budgets$high_flood_risk = ifelse( cc_data_ACFRs_budgets$stateabbv == state & cc_data_ACFRs_budgets$fspropertiesatrisk2020pct >= state_median,
                                                  1, cc_data_ACFRs_budgets$high_flood_risk )
  cc_data_by_doc_type$high_flood_risk = ifelse( cc_data_by_doc_type$stateabbv == state & cc_data_by_doc_type$fspropertiesatrisk2020pct >= state_median,
                                                  1, cc_data_by_doc_type$high_flood_risk )
  
  
  state_top_quartile = quantile( state_data$fspropertiesatrisk2020pct, 0.75, na.rm = T )
  cc_data$high_flood_risk_quartile = ifelse( (cc_data$stateabbv == state & cc_data$fspropertiesatrisk2020pct >= state_top_quartile),
                                             1, cc_data$high_flood_risk_quartile )
  cc_data_ACFRs_budgets$high_flood_risk_quartile = ifelse( (cc_data_ACFRs_budgets$stateabbv == state & cc_data_ACFRs_budgets$fspropertiesatrisk2020pct >= state_top_quartile),
                                                           1, cc_data_ACFRs_budgets$high_flood_risk_quartile )
  cc_data_by_doc_type$high_flood_risk_quartile = ifelse( (cc_data_by_doc_type$stateabbv == state & cc_data_by_doc_type$fspropertiesatrisk2020pct >= state_top_quartile),
                                                           1, cc_data_by_doc_type$high_flood_risk_quartile )

  
}

#-----------------------------------------------------------------
# (2.2) Create variables and data splits for multivariate regs
#-----------------------------------------------------------------

cc_data = as.data.table( cc_data )
cc_data = cc_data[ !is.na(ACFR_measure_adapt) & !is.na(budget_measure_adapt) & !is.na( bond_measure_adapt ) ]

cc_data_ACFRs_budgets = as.data.table(cc_data_ACFRs_budgets)
cc_data_ACFRs_budgets = cc_data_ACFRs_budgets[ !is.na(ACFR_measure_adapt) & !is.na(budget_measure_adapt)  ]

cc_data$county = cc_data$GeoName
cc_data_ACFRs_budgets$county = cc_data_ACFRs_budgets$GeoName

cc_data$republican = ifelse( cc_data$party == "Republican", 1, 0 )
cc_data_ACFRs_budgets$republican = ifelse( cc_data_ACFRs_budgets$party == "Republican", 1, 0 )

cc_data_ACFRs_budgets$high_cb_outlook = ifelse(  cc_data_ACFRs_budgets$CB_reported_outlook_years >= median(cc_data_ACFRs_budgets$CB_reported_outlook_years, na.rm = T), 1, 0 )

cc_data_ACFRs_budgets$high_localofficials = ifelse( cc_data_ACFRs_budgets$localofficials >= median(cc_data_ACFRs_budgets$localofficials, na.rm = T), 1, 0 )
cc_data_ACFRs_budgets$PerHouseholdIncomeAvg = as.numeric(cc_data_ACFRs_budgets$PerHouseholdIncomeAvg)
cc_data_ACFRs_budgets$high_per_household_income = ifelse( cc_data_ACFRs_budgets$PerHouseholdIncomeAvg >= median(cc_data_ACFRs_budgets$PerHouseholdIncomeAvg, na.rm = T), 1, 0 )

cc_data$high_debt = ifelse( cc_data$scaled_outstanding >= median(cc_data$scaled_outstanding, na.rm = T), 1, 0 )
cc_data_ACFRs_budgets$high_debt = ifelse( cc_data_ACFRs_budgets$scaled_outstanding >= median(cc_data_ACFRs_budgets$scaled_outstanding, na.rm = T), 1, 0 )

cc_data$high_ufb = ifelse( cc_data$unrestricted_fund_balance_over_total_expense >= median(cc_data$unrestricted_fund_balance_over_total_expense, na.rm = T), 1, 0 )
cc_data_ACFRs_budgets$high_ufb = ifelse( cc_data_ACFRs_budgets$unrestricted_fund_balance_over_total_expense >= median(cc_data_ACFRs_budgets$unrestricted_fund_balance_over_total_expense, na.rm = T), 1, 0 )

cc_data$unrestricted_fund_balance_over_total_expense = winsorize_x( cc_data$unrestricted_fund_balance_over_total_expense )
cc_data_ACFRs_budgets$unrestricted_fund_balance_over_total_expense = winsorize_x( cc_data_ACFRs_budgets$unrestricted_fund_balance_over_total_expense )

cc_data$fund_scaled_expense = winsorize_x( cc_data$fund_scaled_expense )
cc_data_ACFRs_budgets$fund_scaled_expense = winsorize_x( cc_data_ACFRs_budgets$fund_scaled_expense )

cc_data$Population = winsorize_x( cc_data$Population )
cc_data_ACFRs_budgets$Population = winsorize_x( cc_data_ACFRs_budgets$Population )

cc_data$scaled_outstanding = winsorize_x( cc_data$scaled_outstanding )
cc_data_ACFRs_budgets$scaled_outstanding = winsorize_x( cc_data_ACFRs_budgets$scaled_outstanding )

#-----------------------------------------------------------------
# (4) Save the data
#-----------------------------------------------------------------

write_fst( cc_data, "../data/cleaned_data/cc_data3.fst" ) # R
write_xlsx( cc_data, "../data/cleaned_data/cc_data3.xlsx" ) # Excel

write_fst( cc_data_ACFRs_budgets, "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )  # Excel
write_xlsx( cc_data_ACFRs_budgets, "../data/cleaned_data/cc_data_ACFRs_budgets.xlsx" ) # Excel

write_fst( cc_data_by_doc_type, "../data/cleaned_data/cc_data_by_doc_type.fst" )  # Excel
write_xlsx( cc_data_by_doc_type, "../data/cleaned_data/cc_data_by_doc_type.xlsx" ) # Excel
