
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

secondary_data$county = secondary_data$GeoName
secondary_data$county_year_month = paste0( secondary_data$GeoName, secondary_data$trade_month )

secondary_data$log_time_to_maturity = log( secondary_data$time_to_maturity )

secondary_data = secondary_data %>% filter( coastal_GP_etal == 1 & tax_code_c == "EXMP") # Follow the RFS paper to restrict to coastal states & tax-exempt bonds

secondary_data = secondary_data %>% filter( year < 2020 ) # restrict data to pre-2020 to avoid pandemic effects

#-----------------------------------------------------------------
# (4) Primary market regressions
#-----------------------------------------------------------------

# Run regressions without our data 
primary_long_spread_wo_our_data =  felm( mma_spread  ~  fspropertiesatrisk2020pct_z
                                         + log( issue_size ) 
                                         + log( max_maturity )
                                         + agg_rating_min
                                         + callable + insured + sinkable + go_bond + pre_funded + competitive
                                         + log( n_cusips_in_issue + 1 )
                                         + log( n_underwriter_deals )
                                         + federal_tax_exempt + state_tax_exempt
                                         |  state_year | 0 | county ,
                                         data = primary_data
)

summary(primary_long_spread_wo_our_data)
primary_long_spread_wo_our_data$N

# Include interaction with the main measure
primary_long_spread =  felm( mma_spread  ~  fspropertiesatrisk2020pct_z * measure_adapt_scaled_wins_z
                             + log( issue_size ) + log( max_maturity ) 
                             + agg_rating_min
                             + callable + insured + sinkable + go_bond + pre_funded + competitive 
                             + log( n_cusips_in_issue + 1 )
                             + log( n_underwriter_deals )
                             + federal_tax_exempt + state_tax_exempt
                             |  state_year  | 0 | county ,
                             data = primary_data
)

summary(primary_long_spread)
primary_long_spread$N

# Include interactions with main measure and city (uniquely identified by obligor id) fixed effects
primary_long_spread_cityfe =  felm( mma_spread  ~  fspropertiesatrisk2020pct_z * measure_adapt_scaled_wins_z
                                    + log( issue_size ) + log( max_maturity ) 
                                    + agg_rating_min
                                    + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                    + log( n_cusips_in_issue + 1 )
                                    + log( n_underwriter_deals )
                                    + federal_tax_exempt + state_tax_exempt 
                                    |  state_year + ObligorId | 0 | county ,
                                    data = primary_data
)

summary(primary_long_spread_cityfe)
primary_long_spread_cityfe$N

# Include interactions with hard measure
primary_long_spread_hard =  felm( mma_spread  ~   fspropertiesatrisk2020pct_z * measure_sub_hardprotect_scaled_wins_z
                                  + log( issue_size ) + log( max_maturity ) 
                                  + agg_rating_min
                                  + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                  + log( n_cusips_in_issue + 1 )
                                  + log( n_underwriter_deals )
                                  + federal_tax_exempt + state_tax_exempt 
                                  |  state_year + ObligorId | 0 | county ,
                                  data = primary_data 
)

summary(primary_long_spread_hard)
primary_long_spread_hard$N

# Include interactions with soft measure
primary_long_spread_soft =  felm( mma_spread  ~  fspropertiesatrisk2020pct_z * measure_sub_softprotect_scaled_wins_z
                                  + log( issue_size ) + log( max_maturity ) 
                                  + agg_rating_min
                                  + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                  + log( n_cusips_in_issue + 1 )
                                  + log( n_underwriter_deals )
                                  + federal_tax_exempt + state_tax_exempt
                                  |  state_year + ObligorId| 0 | county ,
                                  data = primary_data
)

summary(primary_long_spread_soft)
primary_long_spread_soft$N

#-----------------------------------------------------------------
# (5) Secondary market regressions
#-----------------------------------------------------------------

# Run regressions without our data 
secondary_test_wo_our_data =  felm( mma_spread  ~ fspropertiesatrisk2020pct_z
                                    + log_time_to_maturity:year
                                    + insured:year
                                    + callable:year
                                    + PerHouseholdIncomeAvg 
                                    + time_from_issuance
                                    + volume_to_amount_outstanding
                                    + sd_price
                                    + go_bond
                                    |  county_year_month  | 0 | county + trade_month,
                                    data = secondary_data )

summary(secondary_test_wo_our_data)
secondary_test_wo_our_data$N

# Include interactions with main measure
secondary_test =  felm( mma_spread  ~ fspropertiesatrisk2020pct_z * measure_adapt_scaled_wins_z
                        + log_time_to_maturity:year
                        + insured:year
                        + callable:year
                        + PerHouseholdIncomeAvg
                        + time_from_issuance
                        + volume_to_amount_outstanding
                        + sd_price
                        + go_bond
                        |  county_year_month  | 0 | county + trade_month,
                        data = secondary_data
                        )
summary(secondary_test)
secondary_test$N

# Include interactions with main measure and city (uniquely identified by obligor id) fixed effects
secondary_test_city_fe =  felm( mma_spread  ~ fspropertiesatrisk2020pct_z * measure_adapt_scaled_wins_z
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
summary(secondary_test_city_fe)
secondary_test_city_fe$N

# Include interactions with hard measure
secondary_test_hard =  felm( mma_spread  ~ fspropertiesatrisk2020pct_z * measure_sub_hardprotect_scaled_wins_z 
                        + log_time_to_maturity:year
                        + insured:year
                        + callable:year
                        + PerHouseholdIncomeAvg 
                        + time_from_issuance
                        + volume_to_amount_outstanding
                        + sd_price
                        + go_bond
                        |  county_year_month  + ObligorId| 0 | county + trade_month,
                        data = secondary_data
)

summary(secondary_test_hard)
secondary_test_hard$N

# Include interactions with soft measure
secondary_test_soft =  felm( mma_spread  ~ fspropertiesatrisk2020pct_z*measure_sub_softprotect_scaled_wins_z 
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

summary(secondary_test_soft)
secondary_test_soft$N


#-----------------------------------------------------------------
# (6) Put main primary and secondary regressions together
#-----------------------------------------------------------------

pm1 = primary_long_spread_wo_our_data
pm2 = primary_long_spread
pm2_1 = primary_long_spread_cityfe
pm3 = primary_long_spread_hard
pm4 = primary_long_spread_soft

sm1 = secondary_test_wo_our_data
sm2 = secondary_test
sm2_1 = secondary_test_city_fe
sm3 = secondary_test_hard
sm4 = secondary_test_soft

# Create variables order so the regressions are displayed correctly
vars.order = c(  "fspropertiesatrisk2020pct_z",
                  "fspropertiesatrisk2020pct_z:measure_adapt_scaled_wins_z", "measure_adapt_scaled_wins_z",
                "fspropertiesatrisk2020pct_z:measure_sub_hardprotect_scaled_wins_z",  "measure_sub_hardprotect_scaled_wins_z", 
                 "fspropertiesatrisk2020pct_z:measure_sub_softprotect_scaled_wins_z", "measure_sub_softprotect_scaled_wins_z"
)


did_reg = capture.output(stargazer(  
                                    pm1, pm2, pm2_1, pm3, pm4,
                                    sm1, sm2, sm2_1, sm3, sm4, 
                                    type="latex",  
                                    column.labels   =  c( "Offering Spread", "Secondary Spread"),
                                    column.separate = c(5, 5),
                                    dep.var.labels   = "",
                                    font.size= "small" ,
                                    float.env="table",
                                    header=FALSE, report="vc*t",
                                    digits=2,
                                    column.sep.width = "1pt",
                                    omit.stat=c("f", "ser", "rsq"),
                                    covariate.labels =  c("Flood Risk", 
                                                          "Adaptation $\\times$  Flood Risk",  "Adaptation",
                                                          "Hard Adaptation $\\times$  Flood Risk", "Hard Adaptation", 
                                                          "Soft Adaptation $\\times$  Flood Risk", "Soft Adaptation"
                                    ),
                                    keep = vars.order,
                                    add.lines = list(
                                      c( "Controls ",  "Yes", "Yes", "Yes", "Yes" ,  "Yes", "Yes", "Yes", "Yes" , "Yes", "Yes" ),
                                      c( "City FE ", "No", "No", "Yes", "Yes", "Yes", "No", "No", "Yes", "Yes", "Yes"),
                                      c( "County-Year-Month FE ",  "Yes" ,  "Yes", "Yes", "Yes", "Yes" ,  "No", "No", "No", "No", "No" ),
                                      c( "State-Year FE ",   "Yes" ,  "Yes", "Yes", "Yes", "Yes", "No", "No", "No", "No", "No" ),
                                      c( "Clustered s.e. ", "County + YM", "County + YM", "County + YM", "County + YM", "County + YM", "County", "County", "County", "County", "County")
                                      
                                    ),
                                    table.placement = "H"
                                    ,order=paste0("^", vars.order , "$")
                                  ) )

note.latex <- ""
did_reg[grepl("Note",did_reg)] <- note.latex
did_reg = gsub('t ?= ?([$-.0-9]*)', '(\\1)', did_reg)
writeLines( did_reg, paste0( "../results/tables/market_tests.tex" ) )



#-----------------------------------------------------------------
# (3) Robustness to excluding storm- and drain-containing sentences
#-----------------------------------------------------------------

# Run primary regressions

# Create interaction variables so it is easier to put the regression tables together
primary_data$fr_measure_adapt_no_drain_scaled_wins_z = primary_data$fspropertiesatrisk2020pct_z *primary_data$measure_adapt_no_drain_scaled_wins_z
primary_data$fr_measure_adapt_no_stormwater_scaled_wins_z = primary_data$fspropertiesatrisk2020pct_z *primary_data$measure_adapt_no_stormwater_scaled_wins_z
primary_data$fr_measure_adapt_no_stormw_drain_scaled_wins_z = primary_data$fspropertiesatrisk2020pct_z *primary_data$measure_adapt_no_stormw_drain_scaled_wins_z

primary_long_spread_no_drain =  felm( mma_spread  ~  fr_measure_adapt_no_drain_scaled_wins_z
                                      + log( issue_size ) + log( max_maturity ) 
                                      + agg_rating_min
                                      + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                      + log( n_cusips_in_issue + 1 )
                                      + log( n_underwriter_deals )
                                      + federal_tax_exempt + state_tax_exempt
                                      |  state_year + ObligorId| 0 | county ,
                                      data = primary_data
)

summary(primary_long_spread_no_drain)
primary_long_spread_no_drain$N

primary_long_spread_no_storm =  felm( mma_spread  ~  fr_measure_adapt_no_stormwater_scaled_wins_z
                                      + log( issue_size ) + log( max_maturity ) 
                                      + agg_rating_min
                                      + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                      + log( n_cusips_in_issue + 1 )
                                      + log( n_underwriter_deals )
                                      + federal_tax_exempt + state_tax_exempt
                                      |  state_year + ObligorId| 0 | county ,
                                      data = primary_data
)

summary(primary_long_spread_no_storm)
primary_long_spread_no_storm$N


primary_long_spread_no_drain_storm =  felm( mma_spread  ~  fr_measure_adapt_no_stormw_drain_scaled_wins_z
                                            + log( issue_size ) + log( max_maturity ) 
                                            + agg_rating_min
                                            + callable + insured + sinkable + go_bond + pre_funded + competitive 
                                            + log( n_cusips_in_issue + 1 )
                                            + log( n_underwriter_deals )
                                            + federal_tax_exempt + state_tax_exempt 
                                            |  state_year + ObligorId| 0 | county ,
                                            data = primary_data
)

summary(primary_long_spread_no_drain_storm)
primary_long_spread_no_drain_storm$N

# Run secondary regressions

# Create interaction variables so it is easier to put the regression tables together
secondary_data$fr_measure_adapt_no_drain_scaled_wins_z = secondary_data$fspropertiesatrisk2020pct_z*secondary_data$measure_adapt_no_drain_scaled_wins_z
secondary_data$fr_measure_adapt_no_stormwater_scaled_wins_z = secondary_data$fspropertiesatrisk2020pct_z*secondary_data$measure_adapt_no_stormwater_scaled_wins_z
secondary_data$fr_measure_adapt_no_stormw_drain_scaled_wins_z = secondary_data$fspropertiesatrisk2020pct_z*secondary_data$measure_adapt_no_stormw_drain_scaled_wins_z

secondary_test_no_drain =  felm( mma_spread  ~ fr_measure_adapt_no_drain_scaled_wins_z 
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
summary(secondary_test_no_drain)
secondary_test_no_drain$N

secondary_test_no_storm =  felm( mma_spread  ~ fr_measure_adapt_no_stormwater_scaled_wins_z
                                 + log_time_to_maturity:year
                                 + insured:year
                                 + callable:year
                                 + PerHouseholdIncomeAvg 
                                 + time_from_issuance
                                 + volume_to_amount_outstanding
                                 + sd_price
                                 + go_bond
                                 |  county_year_month +ObligorId | 0 | county + trade_month,
                                 data = secondary_data
)

summary(secondary_test_no_storm)
secondary_test_no_storm$N

secondary_test_no_drainstorm =  felm( mma_spread  ~ fr_measure_adapt_no_stormw_drain_scaled_wins_z 
                                      + log_time_to_maturity:year
                                      + insured:year
                                      + callable:year
                                      + PerHouseholdIncomeAvg 
                                      + time_from_issuance
                                      + volume_to_amount_outstanding
                                      + sd_price
                                      + go_bond
                                      |  county_year_month + ObligorId  | 0 | county + trade_month,
                                      data =secondary_data
)

summary(secondary_test_no_drainstorm)
secondary_test_no_drainstorm$N

sm5 = secondary_test_no_drain
sm6 = secondary_test_no_storm
sm7 = secondary_test_no_drainstorm

pm5= primary_long_spread_no_drain
pm6 = primary_long_spread_no_storm
pm7 = primary_long_spread_no_drain_storm

vars.order = c(  "fr_measure_adapt_no_drain_scaled_wins_z", "fr_measure_adapt_no_stormwater_scaled_wins_z", "fr_measure_adapt_no_stormw_drain_scaled_wins_z" )

did_reg = capture.output(stargazer(   
                                     pm5, pm6, pm7,
                                     sm5, sm6, sm7, 
                                    type="latex",  
                                    column.labels   =  c( "Offering Spread","Secondary Spread"),
                                    column.separate = c(4, 4),
                                    dep.var.labels   = "",
                                    font.size= "small" ,
                                    float.env="table",
                                    header=FALSE, report="vc*t",
                                    digits=2,
                                    column.sep.width = "1pt",
                                    omit.stat=c("f", "ser", "rsq"),
                                    covariate.labels =  c(
                                                          "Adapt - Drain $\\times$  Flood Risk",
                                                          "Adapt - Storm $\\times$  Flood Risk",
                                                          "Adapt - Drain - Storm $\\times$  Flood Risk"
                                    ),
                                    keep = vars.order,
                                    add.lines = list(
                                      c( "Controls ",   "Yes", "Yes", "Yes" ,  "Yes", "Yes", "Yes" ),
                                      c( "City FE ", "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes" ),
                                      c( "County-Year-Month FE ", "No", "No", "No", "Yes", "Yes", "Yes"  ),
                                      c( "State-Year FE ",   "Yes" ,  "Yes", "Yes" , "No", "No", "No" ),
                                      c( "Clustered s.e. ",   "County", "County", "County",  "County + YM", "County + YM", "County + YM")
                                      
                                    ),
                                    table.placement = "H"
                                    ,order=paste0("^", vars.order , "$")
                                  ) )

note.latex <- ""
did_reg[grepl("Note",did_reg)] <- note.latex
did_reg = gsub('t ?= ?([$-.0-9]*)', '(\\1)', did_reg)
writeLines( did_reg, paste0( "../results/tables/market_tests_robust.tex" ) )


