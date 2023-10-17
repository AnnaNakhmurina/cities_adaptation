
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


#-----------------------------------------------------------------
# (1) Load the merged data on sample cities
#-----------------------------------------------------------------

# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )

# Main + bond data
cc_data3 = read.fst( "../data/cleaned_data/cc_data3.fst" )

#-----------------------------------------------------------------
# (2) Run fund expense validating regressions
#-----------------------------------------------------------------

all_docs = cc_data  %>% filter( reporting_year > 2016 ) # The Atlas fund data starts in 2017
all_docs3 = cc_data3  %>% filter( reporting_year > 2016 ) # The Atlas fund data starts in 2017

adaptation = felm(    
                    log( measure_adapt + 1)
                    ~ log( fund_scaled_expense + 1 ) 
                    + log(Population)
                    + log( total_number_of_sentences )
                    |  obligorid + reporting_year  | 0 | obligorid, 
                    data = all_docs 
                    )
summary( adaptation )
adaptation$N

adaptation3 = felm( 
                      log( measure_adapt + 1)
                      ~ log( fund_scaled_expense + 1 ) 
                      + log(Population)
                      + log( total_number_of_sentences )
                      |  obligorid + reporting_year  | 0 | obligorid, 
                      data = all_docs3 
)
summary( adaptation3 )
adaptation3$N

adaptation_hard = felm(  
                         log( measure_sub_hardprotect + 1 )
                        ~ log( fund_scaled_expense + 1 ) 
                        + log(Population)
                        + log( total_number_of_sentences )
                        |  obligorid + reporting_year  | 0 | obligorid, 
                        data = all_docs )
summary( adaptation_hard )
adaptation_hard$N

adaptation_soft = felm(  
                         log( measure_sub_softprotect + 1 )
                         ~    log( fund_scaled_expense + 1 ) 
                         + log(Population)
                         + log( total_number_of_sentences )
                         |  obligorid + reporting_year  | 0 | obligorid, 
                         data = all_docs 
                         )
summary( adaptation_soft )
adaptation_soft$N

vars.order = c( "log( fund_scaled_expense )", "log(Population)", "log(total_number_of_sentences)" )

m1 = adaptation
m2 = adaptation_hard
m3 = adaptation_soft
m4 = adaptation3


did_reg = capture.output(stargazer( m1, m2, m3, m4,
                                    title = " ",
                                    type="latex",  
                                    dep.var.labels   = c("Adapt", "Hard Adapt", "Soft Adapt", "Adapt"),
                                    font.size= "small" ,
                                    float.env="table",
                                    header=FALSE, report="vc*t",
                                    digits=2,
                                    column.sep.width = "1pt",
                                    omit.stat=c("f", "ser", "rsq"),
                                    covariate.labels =  c("Log(Fund Expense per Capita)", "Log(Population)",
                                                          "Log(N Sentences)"
                                    ),
                                    add.lines = list(
                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                      c( "City FE ",  "Yes", "Yes", "Yes", "Yes" ),
                                      c( "Year FE ",  "Yes", "Yes", "Yes", "Yes"  ),
                                      c( "Clustered s.e. ","City", "City", "City", "City")
                                      
                                    ),
                                    table.placement = "H",
                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
did_reg[grepl("Note",did_reg)] <- note.latex
did_reg = gsub('t ?= ?([$-.0-9]*)', '(\\1)', did_reg)
writeLines( did_reg, paste0( "../results/tables/validation_fund_expense.tex" ) )

#-----------------------------------------------------------------
# (3) Robustness to excluding storm- and drain-containing sentences
#-----------------------------------------------------------------

adaptation_no_drain = felm(
                              log( measure_adapt_no_drain + 1 )
                              ~ log( fund_scaled_expense + 1 ) 
                              + log(Population)
                              + log( total_number_of_sentences )
                              |  obligorid + reporting_year  | 0 | obligorid, 
                              data = all_docs )
summary( adaptation_no_drain )
adaptation_no_drain$N

adaptation_no_stormwater = felm(
                                  log( measure_adapt_no_stormwater + 1 )
                                  ~ log( fund_scaled_expense + 1 ) 
                                  + log(Population)
                                  + log( total_number_of_sentences )
                                  |  obligorid + reporting_year  | 0 | obligorid, 
                                  data = all_docs )
summary( adaptation_no_stormwater )
adaptation_no_stormwater$N

adaptation_no_stormw_no_drain = felm( 
                                      log( measure_adapt_no_stormw_drain + 1 )
                                      ~ log( fund_scaled_expense + 1 ) 
                                      + log(Population)
                                      + log( total_number_of_sentences )
                                      |  obligorid + reporting_year  | 0 | obligorid, 
                                      data = all_docs )
summary( adaptation_no_stormw_no_drain )
adaptation_no_stormw_no_drain$N

vars.order = c( "log( fund_scaled_expense )", "log(Population)", "log(total_number_of_sentences)" )

m1 = adaptation_no_drain
m2 = adaptation_no_stormwater
m3 = adaptation_no_stormw_no_drain

did_reg = capture.output(stargazer( m1,  m2, m3, 
                                    title = " ",
                                    type="latex",  
                                    dep.var.labels   = c("Adapt-Drain", "Adapt-Stormwater", "Adapt-Drain-Stormwater"),
                                    font.size= "small" ,
                                    float.env="table",
                                    header=FALSE, report="vc*t",
                                    digits=2,
                                    column.sep.width = "1pt",
                                    omit.stat=c("f", "ser", "rsq"),
                                    covariate.labels =  c("Log(Fund Expense per Capita)", "Log(Population)",
                                                          "Log(N Sentences)"
                                    ),
                                    add.lines = list(
                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                      c( "City FE ",  "Yes", "Yes", "Yes", "Yes" ),
                                      c( "Year FE ",  "Yes", "Yes", "Yes", "Yes"  ),
                                      c( "Clustered s.e. ","City", "City", "City", "City")
                                      
                                    ),
                                    table.placement = "H",
                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
did_reg[grepl("Note",did_reg)] <- note.latex
did_reg = gsub('t ?= ?([$-.0-9]*)', '(\\1)', did_reg)
writeLines( did_reg, paste0( "../results/tables/validation_fund_expense_robust.tex" ) )
