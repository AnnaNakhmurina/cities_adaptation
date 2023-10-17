
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
# (2) Run flood risk regressions (& cluster two way)
#-----------------------------------------------------------------

adaptation_all = felm(
                        log( measure_adapt + 1 ) ~
                        fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv + reporting_year, data = cc_data  )
summary( adaptation_all )
adaptation_all$N

adaptation_all3 = felm(
                        log( measure_adapt + 1 ) ~
                        fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv+ reporting_year, data = cc_data3  )
summary( adaptation_all3 )
adaptation_all3$N

adaptation_hard = felm( log( measure_sub_hardprotect + 1 ) ~
                         fspropertiesatrisk2020pct
                         + log(Population) 
                         + log( total_number_of_sentences )
                         |   stateabbv + reporting_year   | 0 | stateabbv+ reporting_year, data = cc_data  )
summary( adaptation_hard )
adaptation_hard$N

adaptation_soft = felm(  log(  measure_sub_softprotect  + 1 )~ 
                         fspropertiesatrisk2020pct
                         + log(Population) 
                         + log( total_number_of_sentences )
                         |    stateabbv + reporting_year   | 0 | stateabbv+ reporting_year, 
                         data = cc_data
                         )
summary( adaptation_soft )
adaptation_soft$N


vars.order = c("fspropertiesatrisk2020pct", "log(Population) ", "log(total_number_of_sentences)")

m1 = adaptation_all
m2 = adaptation_hard
m3 = adaptation_soft
m4 = adaptation_all3

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4, 
                                                    title = "\\textbf{ Determinants: Flood risk. Standard errors clustered by state and year. } \\\\
                                                    This table presents the association between flood risk and \\textit{adaptation}. 
                                                    The dependent variables are \\textit{main adaptation} (column (1)).
                                                    In Column (2), \\textit{main adaptation} is calculated by limiting the appearance of a single adaptation term at 1.
                                                    In Column (3), \\textit{main adaptation} is calculated by limiting the appearance of a single adaptation term at 5. 
                                                    In Column (4), \\textit{main adaptation} is calculated by limiting the appearance of a single adaptation term at 10.
                                                    \\textit{Flood Risk} is the percentage of properties at risk in a city. 
                                                    \\textit{Log(Population)} is a natural logarithm of population. 
                                                    \\textit{Log(N Sentences)} is the natural logarithm of the total number of sentences in city-level documents. 
                                                    We also include state and year fixed effects. 
                                                    Standard errors are clustered by state and year. *, **, and *** denote p-values less than 0.10, 0.05, and 0.01, 
                                                    respectively.",
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
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","State \\& Year", "State \\& Year", "State \\& Year","State \\& Year")
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/determinant_flood_risk_all_two_way_main.tex" ) )
