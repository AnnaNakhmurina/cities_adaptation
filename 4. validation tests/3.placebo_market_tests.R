
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
library(fixest)

#-----------------------------------------------------------------
# (1) Load the data with text
#-----------------------------------------------------------------

# For these bond trading tests, we include the data with bond measures.
cc_data = read.fst( "../data/cleaned_data/cc_data3.fst" )

#-----------------------------------------------------------------
# (2) Load the primary trading data and create variables
#-----------------------------------------------------------------

primary_issuance = readRDS( '../data/datasets/mergent_primary_nov22.RDS')
primary_data = left_join( primary_issuance, cc_data, by = c("ObligorId" = "obligorid", "year" = "reporting_year") )

primary_data$mma_spread = 100*(primary_data$offering_yield_f - primary_data$mma_linear_interp)
primary_data$fr_measure_safety_scaled_wins_z = primary_data$fspropertiesatrisk2020pct_z*primary_data$measure_safety_scaled_wins_z

# Create variables and restrict the data following Painter (2020)
primary_data$county = primary_data$GeoName
primary_data$state_year = paste0( primary_data$stateabbv, primary_data$year )
primary_data = primary_data[ issue_size >= 1000000  ]

primary_data = primary_data %>% filter( year < 2020 ) # restrict data to pre-2020 to avoid pandemic effects

#-----------------------------------------------------------------
# (3) Load the secondary trading data and create variables
#-----------------------------------------------------------------

secondary_trades = readRDS( file =  paste0( '../data/datasets/monthly_return_vol_data_nov22.RDS') )
secondary_trades$year = as.numeric( substr( secondary_trades$trade_month, 1, 4 ) )

secondary_data = left_join( secondary_trades, cc_data, by = c("ObligorId" = "obligorid", "year" = "reporting_year") )
secondary_data$mma_spread = 100*(secondary_data$yield - secondary_data$mma_linear_interp)

secondary_data$volume_to_amount_outstanding = secondary_data$volume / secondary_data$total_maturity_offering_amt_f
secondary_data$fr_measure_safety_scaled_wins_z = secondary_data$fspropertiesatrisk2020pct_z*secondary_data$measure_safety_scaled_wins_z

secondary_data$county = secondary_data$GeoName
secondary_data$county_year_month = paste0( secondary_data$GeoName, secondary_data$trade_month )

secondary_data$log_time_to_maturity = log( secondary_data$time_to_maturity )

secondary_data = secondary_data %>% filter( coastal_GP_etal == 1 & tax_code_c == "EXMP") # Follow the RFS paper to restrict to coastal states & tax-exempt bonds

secondary_data = secondary_data %>% filter( year < 2020 ) # restrict data to pre-2020 to avoid pandemic effects

#-----------------------------------------------------------------
# (4) Run robustness regressions
#-----------------------------------------------------------------

secondary_test_safety =  felm( mma_spread  ~ fr_measure_safety_scaled_wins_z
                                 + log_time_to_maturity:year
                                 + insured:year
                                 + callable:year
                                 + PerHouseholdIncomeAvg
                                 + time_from_issuance
                                 + volume_to_amount_outstanding
                                 + sd_price
                                 + go_bond
                                 |  county_year_month + ObligorId | 0 | county + trade_month,
                                 data = secondary_data
)
summary(secondary_test_safety)
secondary_test_safety$N

primary_safety =  felm( mma_spread  ~   fr_measure_safety_scaled_wins_z
                                  + log( issue_size ) + log( max_maturity ) 
                                  + agg_rating_min
                                  + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                  + log( n_cusips_in_issue + 1 )
                                  + log( n_underwriter_deals )
                                  + federal_tax_exempt + state_tax_exempt 
                                  |  state_year + ObligorId | 0 | county ,
                                  data = primary_data 
)

summary(primary_safety)
primary_safety$N

vars.order = c(  "fr_measure_safety_scaled_wins_z")

sm1 = secondary_test_safety
pm1 = primary_safety

did_reg = capture.output(stargazer( 
                                   pm1, sm1,
                                  type="latex",  
                                  column.labels   = c("Offering Spread", "Secondary Spread"),
                                  column.separate = c(1, 1 ),
                                  dep.var.labels   = "",
                                  font.size= "small" ,
                                  float.env="table",
                                  header=FALSE, report="vc*t",
                                  digits=2,
                                  column.sep.width = "1pt",
                                  omit.stat=c("f", "ser", "rsq"),
                                  covariate.labels =  c(
                                                        "Safety $\\times$  Flood Risk" ),
                                  keep = vars.order,
                                  add.lines = list(
                                    c( "Controls ",   "Yes", "Yes"),
                                    c( "City FE ", "Yes", "Yes"),
                                    c( "County-Year-Month FE ", "No", "Yes" ),
                                    c( "State-Year FE ",   "Yes", "No"),
                                    c( "Clustered s.e. ",  "County", "County + YM" )
                                    
                                  ),
                                  table.placement = "H"
                                  ,order=paste0("^", vars.order , "$")
                                ) )

note.latex <- ""
did_reg[grepl("Note",did_reg)] <- note.latex
did_reg = gsub('t ?= ?([$-.0-9]*)', '(\\1)', did_reg)
writeLines( did_reg, paste0( "../results/tables/market_tests_placebo.tex" ) )
