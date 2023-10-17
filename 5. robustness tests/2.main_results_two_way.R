
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
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv + reporting_year , data = cc_data  )
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
                      |   stateabbv + reporting_year  | 0 | stateabbv + reporting_year , 
                      data = cc_data   )
summary( adaptation )
adaptation$N

adaptation_hard = felm(
                          log( measure_sub_hardprotect + 1 ) ~
                          + fspropertiesatrisk2020pct
                          + log(Population) 
                          + log( total_number_of_sentences )
                          |   stateabbv + reporting_year   | 0 | stateabbv + reporting_year , data = cc_data  )
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
                      |   stateabbv + reporting_year  | 0 | stateabbv + reporting_year , 
                      data = cc_data   )
summary( adaptation_h )
adaptation_h$N


adaptation_soft = felm(
                        log(  measure_sub_softprotect  + 1 )~ 
                        + fspropertiesatrisk2020pct
                        + log(Population) 
                        + log( total_number_of_sentences )
                        |    stateabbv + reporting_year   | 0 | stateabbv+ reporting_year , 
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
                        |   stateabbv + reporting_year  | 0 | stateabbv+ reporting_year , 
                        data = cc_data   )
summary( adaptation_s )
adaptation_s$N

adaptation_all3 = felm(
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv+ reporting_year ,
                        data = cc_data3  )
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
                      |   stateabbv + reporting_year  | 0 | stateabbv+ reporting_year , 
                      data = cc_data3   )
summary( adaptation_a )
adaptation_a$N

m1 = adaptation
m2 = adaptation_h
m3 = adaptation_s
m4 = adaptation_a

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4, 
                                                    title = "\\textbf{Determinants: Adaptation gap. Standard errors clustered by state and year.} \\\\ 
                                                    This table presents the relationship between city-level characteristics and \\textit{Adapt Gap}, 
                                                    an indicator that equals to one if the residuals from the Table SN 7.1 regressions are negative. 
                                                    The column specifications correspond to adaptation gaps estimated using \\textit{main adaptation} (column (1)), 
                                                    \\textit{hard adaptation} (column (2)), and \\textit{soft adaptation} (column (3)). 
                                                    Column (4) presents the same specification as column (1) but uses the adaptation gap derived from a 
                                                    measure that incorporates bond prospectus data. \\textit{Republican} is an indicator variable equal to one
                                                    if the city has a Republican mayor. \\textit{UFB/Total Expense} is unrestricted fund balance scaled by total expenses.  
                                                    \\textit{Total Debt per Capita} is total debt outstanding scaled by the population of the city. 
                                                    \\textit{Capital Budget Outlook} is the reported number of years in the capital budget. 
                                                    We also include state and year fixed effects. Standard errors are clustered by state and year level. 
                                                    *, **, and *** denote p-values less than 0.10, 0.05, and 0.01, respectively.",
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
                                                      c( "Clustered s.e. ","State \\& Year", "State \\& Year", "State \\& Year","State \\& Year" )
                                                    ),
                                                    table.placement = "H"
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/adaptation_gap_two_way_main.tex" ) )
