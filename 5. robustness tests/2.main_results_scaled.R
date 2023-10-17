
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
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggstance)

#-----------------------------------------------------------------
# (1) Load the cleaned data with variables
#-----------------------------------------------------------------

# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )

# Main + bond data
cc_data3 = read.fst( "../data/cleaned_data/cc_data3.fst" )

## ----- Main regressions (gap = 0) here  ------------

gap = 0

adaptation_all = felm( 
                        measure_adapt_scaled_wins
                        ~ fspropertiesatrisk2020pct
                        + log(Population)
                        |   stateabbv + reporting_year   | 0 | stateabbv , data = cc_data  )
summary( adaptation_all )
adaptation_all$N

cc_data$resid_adapt = residuals( adaptation_all )
cc_data$adapt_gap = ifelse( cc_data$resid_adapt < gap, 1, 0 )

adaptation =  felm(  
                      adapt_gap
                      ~  republican
                      + unrestricted_fund_balance_over_total_expense
                      + log( scaled_outstanding )
                      + CB_reported_outlook_years
                      |   stateabbv + reporting_year  | 0 | stateabbv , 
                      data = cc_data   )
summary( adaptation )
adaptation$N

adaptation_hard = felm(
                        measure_sub_hardprotect_scaled_wins
                        ~ fspropertiesatrisk2020pct
                        + log(Population) 
                        |   stateabbv + reporting_year   | 0 | stateabbv , data = cc_data  )
summary( adaptation_hard )
adaptation_hard$N

cc_data$resid_adapt_hard = residuals( adaptation_hard )
cc_data$hard_adapt_gap = ifelse( cc_data$resid_adapt_hard < gap, 1, 0 )

adaptation_h =  felm(  
                        hard_adapt_gap
                        ~  republican
                        + unrestricted_fund_balance_over_total_expense
                        + log( scaled_outstanding )
                        + CB_reported_outlook_years
                        |   stateabbv + reporting_year  | 0 | stateabbv , 
                        data = cc_data   )
summary( adaptation_h )
adaptation_h$N

adaptation_soft = felm( 
                          measure_sub_softprotect_scaled_wins
                          ~ fspropertiesatrisk2020pct
                          + log(Population) 
                          |    stateabbv + reporting_year   | 0 | stateabbv , 
                          data = cc_data
                        )
summary( adaptation_soft )
adaptation_soft$N

cc_data$resid_adapt_soft = residuals( adaptation_soft )
cc_data$soft_adapt_gap = ifelse( cc_data$resid_adapt_soft < gap, 1, 0 )

adaptation_s =  felm(  
                      soft_adapt_gap
                      ~  republican
                      + unrestricted_fund_balance_over_total_expense
                      + log( scaled_outstanding )
                      + CB_reported_outlook_years
                      |   stateabbv + reporting_year  | 0 | stateabbv , 
                      data = cc_data   )
summary( adaptation_s )
adaptation_s$N

adaptation_all3 = felm(
                        measure_adapt_scaled_wins
                        ~ fspropertiesatrisk2020pct
                        + log(Population)
                        |   stateabbv + reporting_year   | 0 | stateabbv , data = cc_data3  )
summary( adaptation_all3 )
adaptation_all3$N

cc_data3$resid_adapt_all = residuals( adaptation_all3 )
cc_data3$adapt_all_gap = ifelse( cc_data3$resid_adapt_all < gap, 1, 0 )

adaptation_a =  felm(  
                      adapt_all_gap
                      ~  republican
                      + unrestricted_fund_balance_over_total_expense
                      + log( scaled_outstanding )
                      + CB_reported_outlook_years
                      |   stateabbv + reporting_year  | 0 | stateabbv , 
                      data = cc_data3   )
summary( adaptation_a )
adaptation_a$N

m1 = adaptation
m2 = adaptation_h
m3 = adaptation_s
m4 = adaptation_a

determinant_flood_risk1 = capture.output(stargazer( m1,  m2, m3, m4,
                                                    type="latex",  
                                                    dep.var.labels   = c("Adapt Gap", "Hard Adapt Gap", "Soft Adapt Gap", "Adapt Gap"),
                                                    font.size= "small" ,
                                                    float.env="table",
                                                    header=FALSE, report="vc*t",
                                                    digits=2,
                                                    column.sep.width = "1pt",
                                                    omit.stat=c("f", "ser", "rsq"),
                                                    covariate.labels =  c(    "Republican",
                                                    "UFB/Total Expense",
                                                    "Log(Total Debt per Capita)",
                                                    "Capital Budget Outlook"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds"  ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","State", "State", "State","State")
                                                    ),
                                                    table.placement = "H"
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/adaptation_gap_scaled.tex" ) )
