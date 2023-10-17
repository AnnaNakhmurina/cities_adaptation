
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
# Load insurance data and textual data
#---------------------------------------------

# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )

# Main + bond data
cc_data3 = read.fst( "../data/cleaned_data/cc_data3.fst" )

#---------------------------------------------
# Load insurance data and textual data
#---------------------------------------------

adaptation_all = felm( log( measure_adapt + 1 ) ~ crsclasscode
                                                   + log(Population)
                                                   + log( total_number_of_sentences )
                                                   | stateabbv + reporting_year   | 0 | obligorid, 
                                                   data = cc_data 
                       )
summary( adaptation_all )
adaptation_all$N

adaptation_all3 = felm( log( measure_adapt + 1 ) ~ crsclasscode
                       + log(Population)
                       + log( total_number_of_sentences )
                       | stateabbv + reporting_year   | 0 | obligorid, 
                       data = cc_data3 
)
summary( adaptation_all3 )
adaptation_all3$N

adaptation_hard = felm( log( measure_sub_hardprotect + 1 ) ~ crsclasscode
                        + log(Population)
                        + log( total_number_of_sentences )
                         | stateabbv + reporting_year   | 0 | obligorid,
                        data = cc_data
                        )
summary( adaptation_hard )
adaptation_hard$N

adaptation_soft = felm(   log( measure_sub_softprotect + 1)  ~  crsclasscode
                          + log(Population)
                          + log( total_number_of_sentences )
                         | stateabbv + reporting_year   | 0 | obligorid, 
                          data = cc_data 
                        )
summary( adaptation_soft )
adaptation_soft$N

vars.order = c( "crsclasscode", "log(Population)", "log( total_number_of_sentences )")

m1 = adaptation_all
m2 = adaptation_hard
m3 = adaptation_soft
m4 = adaptation_all3

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4, 
                                                    type="latex",  
                                                    dep.var.labels   = c("Adapt", "Hard Adapt", "Soft Adapt", "Adapt"),
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
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes"),
                                                      c( "Clustered s.e. ","City", "City", "City","City")
                                                      
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, "../results/tables/insurance_validation.tex" ) 


#---------------------------------------------
# Robust - Storm-Drain
#---------------------------------------------

adaptation_no_drain = felm(   log( measure_adapt_no_drain +1 )  ~  crsclasscode
                              + log(Population)
                              + log( total_number_of_sentences )
                              | stateabbv + reporting_year   | 0 | obligorid, 
                              data = cc_data
)
summary( adaptation_no_drain )
adaptation_no_drain$N

adaptation_no_stormwater = felm(   log( measure_adapt_no_stormwater + 1)  ~  crsclasscode
                                   + log(Population)
                                   + log( total_number_of_sentences )
                                   | stateabbv + reporting_year   | 0 | obligorid, 
                                   data = cc_data
)
summary( adaptation_no_stormwater )
adaptation_no_stormwater$N

adaptation_no_stormwater_drain = felm(   log( measure_adapt_no_stormw_drain + 1)  ~  crsclasscode
                                         + log(Population)
                                         + log( total_number_of_sentences )
                                         | stateabbv + reporting_year   | 0 | obligorid, 
                                         data = cc_data
)
summary( adaptation_no_stormwater_drain )
adaptation_no_stormwater_drain$N

vars.order = c( "crsclasscode", "log(Population)", "log( total_number_of_sentences )")

m1 = adaptation_no_drain
m2 = adaptation_no_stormwater
m3 = adaptation_no_stormwater_drain

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3,
                                                    type="latex",  
                                                    dep.var.labels   = c(
                                                                         "Adapt-Drain", "Adapt-Stormwater", "Adapt-Drain-Stormwater"),
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
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","City", "City", "City","City")
                                                      
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, "../results/tables/insurance_validation_robust.tex" ) 

