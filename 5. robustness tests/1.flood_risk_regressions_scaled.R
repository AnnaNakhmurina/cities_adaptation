
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
# (2) Run regressions with scaled measures
#-----------------------------------------------------------------

adaptation_all = felm(  
                          measure_adapt_scaled_wins
                        ~ fspropertiesatrisk2020pct
                        + log(Population)
                        |   stateabbv + reporting_year   | 0 | stateabbv , data = cc_data  )
summary( adaptation_all )
adaptation_all$N

adaptation_all3 = felm( 
                          measure_adapt_scaled_wins
                          ~ fspropertiesatrisk2020pct
                          + log(Population)
                          |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data3  )
summary( adaptation_all3 )
adaptation_all3$N

adaptation_hard = felm(  
                          measure_sub_hardprotect_scaled_wins
                         ~ fspropertiesatrisk2020pct
                         + log(Population) 
                         |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_hard )
adaptation_hard$N

adaptation_soft = felm( 
                          measure_sub_softprotect_scaled_wins
                          ~ fspropertiesatrisk2020pct
                         + log(Population) 
                         |    stateabbv + reporting_year   | 0 | stateabbv, 
                         data = cc_data 
                         )
summary( adaptation_soft )
adaptation_soft$N

vars.order = c("fspropertiesatrisk2020pct", "log(Population)" )

m1 = adaptation_all
m2 = adaptation_hard
m3 = adaptation_soft
m4 = adaptation_all3

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3,  m4,
                                                    type="latex",  
                                                    dep.var.labels   = c("Adapt", "Hard Adapt", "Soft Adapt", "Adapt"),
                                                    font.size= "small" ,
                                                    float.env="table",
                                                    header=FALSE, report="vc*t",
                                                    digits=2,
                                                    column.sep.width = "1pt",
                                                    omit.stat=c("f", "ser", "rsq"),
                                                    covariate.labels =  c("Flood Risk",
                                                                          "Log(Population)", "Log(N Sentences)"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds"  ),
                                                       c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","State", "State", "State","State")
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/determinant_flood_risk_all_scaled.tex" ) )
