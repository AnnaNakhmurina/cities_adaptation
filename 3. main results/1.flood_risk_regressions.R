
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
# RUN: Define functions
#----------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

#-----------------------------------------------------------------
# (1) Load the merged data on sample cities
#-----------------------------------------------------------------

# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )

# Main + bond data
cc_data3 = read.fst( "../data/cleaned_data/cc_data3.fst" )

#-----------------------------------------------------------------
# (2) Regress adaptation measures on flood risk 
#-----------------------------------------------------------------

adaptation_all = felm(  
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all )
adaptation_all$N

adaptation_all3 = felm(
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data3  )
summary( adaptation_all3 )
adaptation_all3$N

adaptation_hard = felm( 
                          log( measure_sub_hardprotect + 1 ) ~
                           + fspropertiesatrisk2020pct
                           + log(Population) 
                           + log( total_number_of_sentences )
                         |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_hard )
adaptation_hard$N

adaptation_soft = felm(  
                          log(  measure_sub_softprotect  + 1 ) ~ 
                           + fspropertiesatrisk2020pct
                         + log(Population) 
                         + log( total_number_of_sentences )
                         |    stateabbv + reporting_year   | 0 | stateabbv, 
                         data = cc_data  
                         )
summary( adaptation_soft )
adaptation_soft$N


vars.order = c("fspropertiesatrisk2020pct", "log(Population)", "log(total_number_of_sentences)")

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
                                                    covariate.labels =  c("Flood Risk",
                                                                          "Log(Population)", "Log(N Sentences)"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds"  ),
                                                       c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","State", "State", "State","State" )
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/determinant_flood_risk_all.tex" ) )

#-----------------------------------------------------------------
# (3) DID regressions (post hurricanes)
#-----------------------------------------------------------------

# Create a state-year variable
cc_data$state_year = paste0( cc_data$stateabbv, "_", cc_data$reporting_year )
cc_data3$state_year = paste0( cc_data3$stateabbv, "_", cc_data3$reporting_year )

adaptation_did = felm(  
                              log( measure_adapt + 1) ~  
                               high_flood_risk_quartile:post
                             + high_flood_risk_quartile
                             + log(Population)
                             + log( total_number_of_sentences )
                             |    state_year  | 0 | stateabbv,  data = cc_data )
summary( adaptation_did )
adaptation_did$N

adaptation3_did = felm( 
                            log( measure_adapt + 1) ~
                              high_flood_risk_quartile:post
                            + high_flood_risk_quartile
                            + log(Population)
                            + log( total_number_of_sentences )
                            |    state_year  | 0 | stateabbv,  data = cc_data3 )
summary( adaptation3_did )
adaptation3_did$N

adaptation_hard_did = felm(  
                                 log( measure_sub_hardprotect + 1) ~
                                   high_flood_risk_quartile:post
                                  + high_flood_risk_quartile
                                  + log(Population) 
                                  + log( total_number_of_sentences )
                                 |   state_year  | 0 | stateabbv,  data = cc_data )
summary( adaptation_hard_did )
adaptation_hard_did$N

adaptation_soft_did = felm(
                                 log( measure_sub_softprotect + 1) ~ 
                                  high_flood_risk_quartile:post
                                  + high_flood_risk_quartile
                                  + log(Population) 
                                  + log( total_number_of_sentences )
                                 |   state_year  | 0 | stateabbv,
                                  data = cc_data
                                  )
summary( adaptation_soft_did )
adaptation_soft_did$N

vars.order = c( 
                "high_flood_risk_quartile:post", "high_flood_risk_quartile",
                "log(Population)", "log( total_number_of_sentences )")

m1 = adaptation_did
m2 = adaptation_hard_did
m3 = adaptation_soft_did
m4 = adaptation3_did

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4,
                                                    type="latex",  
                                                    dep.var.labels   = c("Adapt", "Hard Adapt", "Soft Adapt", "Adapt"),
                                                    font.size= "small" ,
                                                    float.env="table",
                                                    header=FALSE, report="vc*t",
                                                    digits=2,
                                                    column.sep.width = "1pt",
                                                    omit.stat=c("f", "ser", "rsq"),
                                                    covariate.labels =  c("High Flood Risk $\\times$ Post",  "High Flood Risk", 
                                                                          "Log(Population)", "Log(N Sentences)"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                                      c( "State-Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","State", "State", "State","State")
                                                      
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/determinant_flood_risk_did.tex" ) )


#-----------------------------------------------------------------
# (4) Robustness to DID; drop cities affected by hurricanes
#-----------------------------------------------------------------

hurracaine_cities  = read_excel( "../data/datasets/cities_list_affected_first_hur.xlsx" ) 
hurracaine_cities = hurracaine_cities %>% filter( affected_first_hur == 1 )

cc_data_no_affected = cc_data %>% filter( cc_data$obligorid %!in% hurracaine_cities$obligorid )
cc_data3_no_affected = cc_data3 %>% filter( cc_data3$obligorid %!in% hurracaine_cities$obligorid )

adaptation_did_no_hur_cities = felm( 
                            log( measure_adapt + 1) ~ 
                              high_flood_risk_quartile:post
                            + high_flood_risk_quartile
                            + log(Population)
                            + log( total_number_of_sentences )
                            |    state_year  | 0 | stateabbv,  data = cc_data_no_affected )
summary( adaptation_did_no_hur_cities )
adaptation_did_no_hur_cities$N

adaptation3_did_no_hur_cities = felm( 
                                      log( measure_adapt + 1) ~
                                      high_flood_risk_quartile:post
                                      + high_flood_risk_quartile
                                      + log(Population)
                                      + log( total_number_of_sentences )
                                      |    state_year  | 0 | stateabbv,  data = cc_data3_no_affected )
summary( adaptation3_did_no_hur_cities )
adaptation3_did_no_hur_cities$N

adaptation_hard_did_no_hur_cities = felm( 
                                  log( measure_sub_hardprotect + 1) ~
                                    high_flood_risk_quartile:post
                                  + high_flood_risk_quartile
                                  + log(Population) 
                                  + log( total_number_of_sentences )
                                  |   state_year  | 0 | stateabbv,  data = cc_data_no_affected )
summary( adaptation_hard_did_no_hur_cities )
adaptation_hard_did_no_hur_cities$N

adaptation_soft_did_no_hur_cities = felm(
                                  log( measure_sub_softprotect + 1)
                                  ~ high_flood_risk_quartile:post
                                  + high_flood_risk_quartile
                                  + log(Population) 
                                  + log( total_number_of_sentences )
                                  |   state_year  | 0 | stateabbv,
                                  data = cc_data_no_affected 
)
summary( adaptation_soft_did_no_hur_cities )
adaptation_soft_did_no_hur_cities$N

vars.order = c(
  "high_flood_risk_quartile:post", "high_flood_risk_quartile",
  "log(Population)", "log( total_number_of_sentences )")

m1 = adaptation_did_no_hur_cities
m2 = adaptation_hard_did_no_hur_cities
m3 = adaptation_soft_did_no_hur_cities
m4 = adaptation3_did_no_hur_cities

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4,
                                                     title = "\\textbf{Determinants: Flood risk.  Excluding cities that were affected by hurracaines.} \\\\ 
                                                    This table presents difference-in-differences regressions relative to extreme hurricanes.  
                                                    The dependent variables are \\textit{main adaptation} (column (1)), \\textit{hard adaptation} (column (2)), 
                                                    and \\textit{soft adaptation} (column (3)). 
                                                    Column (4) presents the same specification as column (1) but uses the \\textit{main+bond andaptation} 
                                                    that incorporates bond prospectus data. \\textit{High Flood Risk} cities have top-quartile percentage 
                                                    properties at risk.  \\textit{Log(Population)} is a natural logarithm of population. 
                                                    \\textit{Post} is an indicator variable equal to one after an extreme weather event. 
                                                    \\textit{Log(N Sentences)} is the natural logarithm of the total number of sentences in city-level documents. 
                                                    We also include state-year fixed effects. 
                                                    Standard errors are clustered at the state level. 
                                                    *, **, and *** denote p-values less than 0.10, 0.05, and 0.01, respectively. ",
                                                    type="latex",  
                                                    dep.var.labels   = c("Adapt", "Hard Adapt", "Soft Adapt", "Adapt"),
                                                    font.size= "small" ,
                                                    float.env="table",
                                                    header=FALSE, report="vc*t",
                                                    digits=2,
                                                    column.sep.width = "1pt",
                                                    omit.stat=c("f", "ser", "rsq"),
                                                    covariate.labels =  c("High Flood Risk $\\times$ Post",  "High Flood Risk", 
                                                                          "Log(Population)", "Log(N Sentences)"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                                      c( "State-Year FE ",  "Yes", "Yes", "Yes",  "Yes"),
                                                      c( "Clustered s.e. ","State", "State", "State","State")
                                                      
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/determinant_flood_risk_did_no_hur_affected.tex" ) )



#-----------------------------------------------------------------
# (5) Robustness against capping N sentences at 1,5, 10, N groups and N words
#-----------------------------------------------------------------

adaptation_all = felm(  
                      log( measure_adapt + 1 ) ~
                      + fspropertiesatrisk2020pct
                      + log(Population)
                      + log( total_number_of_sentences )
                      |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all )
adaptation_all$N

adaptation_all_1 = felm( 
                          log( measure_adapt_max1 + 1 ) ~
                          + fspropertiesatrisk2020pct
                          + log(Population)
                          + log( total_number_of_sentences )
                          |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all_1 )
adaptation_all_1$N

adaptation_all_5 = felm(
                          log( measure_adapt_max5 + 1 ) ~
                          + fspropertiesatrisk2020pct
                          + log(Population)
                          + log( total_number_of_sentences )
                          |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all_5 )
adaptation_all_5$N

adaptation_all_10 = felm( 
                          log( measure_adapt_max10 + 1 ) ~
                          + fspropertiesatrisk2020pct
                          + log(Population)
                          + log( total_number_of_sentences )
                          |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all_10 )
adaptation_all_10$N

adaptation_all_n_groups = felm( 
                                log( ngroup_adapt + 1 ) ~
                                + fspropertiesatrisk2020pct
                                + log(Population)
                                + log( total_number_of_sentences )
                                |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all_n_groups )
adaptation_all_n_groups$N

adaptation_all_nwords = felm( 
                              log( keyword_adapt + 1 ) ~
                              + fspropertiesatrisk2020pct
                              + log(Population)
                              + log( total_number_of_sentences )
                              |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all_nwords )
adaptation_all_nwords$N


m1 = adaptation_all
m2 = adaptation_all_1
m3 = adaptation_all_5
m4 = adaptation_all_10
m5 = adaptation_all_n_groups
m6 = adaptation_all_nwords

vars.order = c(
                "fspropertiesatrisk2020pct",
                "log(Population1)", "log( total_number_of_sentences )")


determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4,  m6, m5,
                                                    title = "\\textbf{ Adaptation and Flood Risk} \\\\ ",
                                                    type="latex",  
                                                    dep.var.labels   = c("Main","Max 1", "Max 5", "Max 10", "N words",  "N groups"),
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
                                                      c( "Measure",  "Main", "Main", "Main", "Main", "Main", "Main" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes"),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes"),
                                                      c( "Clustered s.e. ","State", "State", "State","State", "State", "State")
                                                    ),
                                                    table.placement = "H",
                                                    order=paste0("^", vars.order , "$") 
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/determinant_flood_risk_robust_max_measure.tex" ) )

