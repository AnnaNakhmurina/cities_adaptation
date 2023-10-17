
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
cc_data = read.fst( "../data/cleaned_data/cc_data_cafrs_budgets.fst" )

# Main + bond data
cc_data3 = read.fst( "../data/cleaned_data/cc_data3.fst" )

#-----------------------------------------------------------------
# (2) Run placebo regressions
#-----------------------------------------------------------------

safety = felm( log( measure_safety + 1 ) ~ crsclasscode
                + log(Population)
                + log( total_number_of_sentences )
                | stateabbv + reporting_year   | 0 | obligorid, 
                data = cc_data
)
summary( safety )
safety$N

safety3 = felm( log( measure_safety + 1 ) ~ crsclasscode
                       + log(Population)
                       + log( total_number_of_sentences )
                       | stateabbv + reporting_year   | 0 | obligorid, 
                       data = cc_data3
)
summary( safety3 )
safety3$N

vars.order = c( "crsclasscode", "log(Population)",  "log( total_number_of_sentences )")

m1 = safety
m2 = safety3


determinant_flood_risk1 = capture.output(stargazer( m1,  m2,
                                                    type="latex",  
                                                    dep.var.labels   = c( "Safety/Police" ),
                                                    font.size= "small" ,
                                                    float.env="table",
                                                    header=FALSE, report="vc*t",
                                                    digits=2,
                                                    column.sep.width = "1pt",
                                                    omit.stat=c("f", "ser", "rsq"),
                                                    covariate.labels =  c( "CRS Class Code", 
                                                                          "Log(Population)", "Log(N Sentences)"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main",  "Main+Bonds" ),
                                                      c( "State FE ",  "Yes", "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes" ),
                                                      c( "Clustered s.e. ","City", "City" )
                                                      
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/insurance_validation_placebo.tex" ) )
