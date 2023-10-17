
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
# (2) Run placebo regressions
#-----------------------------------------------------------------

all_docs = cc_data  %>% filter( reporting_year > 2016 ) # The Atlas fund data starts in 2017
all_docs3 = cc_data3  %>% filter( reporting_year > 2016 ) # The Atlas fund data starts in 2017

safety = felm(
                log( measure_safety + 1)
                ~ log( fund_scaled_expense + 1 ) 
                + log(Population)
                + log( total_number_of_sentences )
                |  obligorid + reporting_year  | 0 | obligorid, 
                data = all_docs 
              )
summary( safety )
safety$N

safety3 = felm( 
                  log( measure_safety + 1)
                  ~ log( fund_scaled_expense + 1 ) 
                  + log(Population)
                  + log( total_number_of_sentences )
                  |  obligorid + reporting_year  | 0 | obligorid, 
                  data = all_docs3 
                )
summary( safety3 )
safety3$N

vars.order = c( "log( fund_scaled_expense + 1 )",
                "log(Population)",  "log(total_number_of_sentences)" )

m1 = safety
m2 = safety3

did_reg = capture.output(stargazer( m1, m2,
                                    type="latex",  
                                    dep.var.labels   = c("Safety/Police"),
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
                                      c( "Measure",  "Main", "Main+Bonds" ),
                                      c( "City FE ",  "Yes", "Yes"  ),
                                      c( "Year FE ",  "Yes", "Yes" ),
                                      c( "Clustered s.e. ","City", "City" )
                                      
                                    ),
                                    table.placement = "H",
                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
did_reg[grepl("Note",did_reg)] <- note.latex
did_reg = gsub('t ?= ?([$-.0-9]*)', '(\\1)', did_reg)
writeLines( did_reg, paste0( "../results/tables/validation_fund_expense_placebo.tex" ) )
