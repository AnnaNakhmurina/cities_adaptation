
# This code runs main regressions, and plots the coefficients out

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

#--------------------------------------------------------------------
# (2)  Add splitting variables
#-------------------------------------------------------------------
## ----- Main regressions (gap = 0) here  ------------

gap = 0

adaptation_all = felm(  log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
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
                  |   stateabbv + reporting_year  | 0 | stateabbv, 
                  data = cc_data   )
summary( adaptation )
adaptation$N

adaptation_hard = felm(
                        log( measure_sub_hardprotect + 1 ) ~
                          + fspropertiesatrisk2020pct
                        + log(Population) 
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
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
                      |   stateabbv + reporting_year  | 0 | stateabbv,  data = cc_data   )
summary( adaptation_h )
adaptation_h$N


adaptation_soft = felm( 
                        log(  measure_sub_softprotect  + 1 ) ~ 
                        + fspropertiesatrisk2020pct
                        + log(Population) 
                        + log( total_number_of_sentences )
                        |    stateabbv + reporting_year   | 0 | stateabbv, 
                         data = cc_data )
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
                      |   stateabbv + reporting_year  | 0 | stateabbv, 
                      data = cc_data   )
summary( adaptation_s )
adaptation_s$N

adaptation_all3 = felm(
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data3  )
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
                        |   stateabbv + reporting_year  | 0 | stateabbv, data = cc_data3   )
summary( adaptation_a )
adaptation_a$N

m1 = adaptation
m2 = adaptation_h
m3 = adaptation_s
m4 = adaptation_a

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, m4, 
                                                    type="latex",  
                                                    dep.var.labels   = c("Adapt Gap", "Hard Adapt Gap", "Soft Adapt Gap", "Adapt Gap"),
                                                    font.size= "small" ,
                                                    float.env="table",
                                                    header=FALSE, report="vc*t",
                                                    digits=2,
                                                    column.sep.width = "1pt",
                                                    omit.stat=c("f", "ser", "rsq"),
                                                    covariate.labels =  c( "Republican", 
                                                                            "UFB/Total Expense",
                                                                            "Log(Total Debt per Capita)",
                                                                            "Capital Budget Outlook"
                                                    ),
                                                    add.lines = list(
                                                      c( "Measure",  "Main", "Main", "Main", "Main+Bonds" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes" ),
                                                      c( "Clustered s.e. ","State", "State", "State", "State" )
                                                    ),
                                                    table.placement = "H"
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/adaptation_gap.tex" ) )

#--------------------------------------------------------------------
# (3)  Run splits 
#------------------------------------------------------------------
### --------------- Splits by flood risk ------------

adaptation_hfr =  felm(  
                        adapt_gap
                        ~  republican
                        + unrestricted_fund_balance_over_total_expense
                        + log( scaled_outstanding )
                        + CB_reported_outlook_years
                        |   stateabbv + reporting_year  | 0 | stateabbv, 
                        data = cc_data  %>% filter( high_flood_risk == 1 )  )
summary( adaptation_hfr )
adaptation_hfr$N

adaptation_lfr =  felm(  
                          adapt_gap
                          ~  republican
                          + unrestricted_fund_balance_over_total_expense
                          + log( scaled_outstanding )
                          + CB_reported_outlook_years
                          |   stateabbv + reporting_year  | 0 | stateabbv, 
                          data = cc_data  %>% filter( high_flood_risk == 0 )  )
summary( adaptation_lfr )
adaptation_lfr$N

###---------- Splits by local opinion ---------

adaptation_hw =  felm(  
                      adapt_gap
                      ~  republican
                      + unrestricted_fund_balance_over_total_expense
                      + log( scaled_outstanding )
                      + CB_reported_outlook_years
                      |   stateabbv + reporting_year  | 0 | stateabbv, 
                      data = cc_data  %>% filter( high_localofficials == 1 )  )
summary( adaptation_hw )
adaptation_hw$N

adaptation_lw =  felm(  
                        adapt_gap
                        ~  republican
                        + unrestricted_fund_balance_over_total_expense
                        + log( scaled_outstanding )
                        + CB_reported_outlook_years
                        |   stateabbv + reporting_year  | 0 | stateabbv, 
                        data = cc_data  %>% filter( high_localofficials == 0 )  )
summary( adaptation_lw )
adaptation_lw$N

###---------- Splits by state grants ---------

adaptation_sg  =  felm(  
                        adapt_gap
                        ~  republican
                        + unrestricted_fund_balance_over_total_expense
                        + log( scaled_outstanding )
                        + CB_reported_outlook_years
                        |   stateabbv + reporting_year  | 0 | stateabbv, 
                        data = cc_data  %>% filter( state_grant_above_median == 1 )  )
summary( adaptation_sg )
adaptation_sg$N

adaptation_no_sg =  felm(  
                          adapt_gap
                          ~  republican
                          + unrestricted_fund_balance_over_total_expense
                          + log( scaled_outstanding )
                          + CB_reported_outlook_years
                          |   stateabbv + reporting_year  | 0 | stateabbv, 
                          data = cc_data  %>% filter( state_grant_above_median == 0 )  )
summary( adaptation_no_sg )
adaptation_no_sg$N

###---------- Splits by Income per household ---------

adaptation_high_per_household  =  felm(  
                                      adapt_gap
                                      ~  republican
                                      + unrestricted_fund_balance_over_total_expense
                                      + log( scaled_outstanding )
                                      + CB_reported_outlook_years
                                      |   stateabbv + reporting_year  | 0 | stateabbv, 
                                      data = cc_data  %>% filter( high_per_household_income == 1 )  )
summary( adaptation_high_per_household )
adaptation_high_per_household$N

adaptation_low_per_household  =  felm(  
                                      adapt_gap
                                      ~  republican
                                      + unrestricted_fund_balance_over_total_expense
                                      + log( scaled_outstanding )
                                      + CB_reported_outlook_years
                                      |   stateabbv + reporting_year  | 0 | stateabbv, 
                                      data = cc_data  %>% filter( high_per_household_income == 0 )  )
summary( adaptation_low_per_household )
adaptation_low_per_household$N

###---------- Stargaze split regressions ---------

m1 = adaptation_hfr
m2 = adaptation_lfr
m3 = adaptation_hw
m4 = adaptation_lw
m5 = adaptation_sg
m6 = adaptation_no_sg
m7 = adaptation_high_per_household
m8 = adaptation_low_per_household

determinant_flood_risk1 = capture.output(stargazer( m1,  m2, m3, m4, m5, m6, m7, m8, 
                                                    title = "\\textbf{ Adaptation Gap } \\\\ ",
                                                    type="latex",  
                                                      column.labels   =  c("High Flood Risk", "Low Flood Risk", 
                                                                           "High Local Off.", "Low Local Off.",
                                                                           "Large Grant", "Small Grant",
                                                                           "High H/H Income", "Low H/H Income"),
                                                    dep.var.labels   = c("Adapt Gap"),
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
                                                      c( "Measure",  "Main", "Main", "Main", "Main", "Main", "Main", "Main","Main", "Main", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"  ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
                                                      c( "Clustered s.e. ","State", "State", "State","State", "State", "State", "State", "State")
                                                    ),
                                                    table.placement = "H"
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( "../results/tables/adaptation_gap_splits.tex" ) )

#-----------------------------------------------------------------
# (4) Plot the main specification for Figure 4
#-----------------------------------------------------------------

#Create data for the plot
a_palette<-c('#2A363B','#E84A5F','#025196', '#99B898')
adapt_plot = plot_summs(adaptation, adaptation_h, adaptation_s, adaptation_a,
                        ci_level =0.9,  inner_ci_level = .9,
                        model.names = c("Main", "Hard", "Soft", "Main+Bond"),
                        legend.title = "",
                        coefs = c("Republican"= "republican" 
                                  , "UFB/Total Expense"="unrestricted_fund_balance_over_total_expense",
                                  "Debt Outstanding" = "log(scaled_outstanding)",
                                  "CB Outlook" ="CB_reported_outlook_years"
                        ),
                        colors = a_palette
)

figure4_data = adapt_plot$data
# Save the data for publication
write_xlsx( figure4_data, "../data for figures/figure 4/figure4.xlsx" ) # Excel

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica'),
        legend.title=element_blank(), 
        axis.text=element_text(size=30),
        axis.title=element_text(size=30),
        legend.text = element_text(size = 30))

adapt_plot = adapt_plot  + xlim(-0.2, 0.2) +  labs(x = "\n Adaptation Gap \n ", y = NULL)
adapt_plot

ggsave(paste0( '../results/figures/'
               ,  'adaptation_gap_determinants.pdf', sep = '' ), adapt_plot 
       ,width = 8.65, height = 6.69, dpi = 300, units = "in"
       
)
#-----------------------------------------------------------------
# (5) Plot the splits for Figure 5
#-----------------------------------------------------------------
# Set theme
apatheme=theme_bw()+
  theme(
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(),
    legend.position = "bottom",
  )

### --------------- Plot splits by flood risk---------------
adapt_plot = plot_summs(adaptation_hfr, adaptation_lfr,
                        ci_level =0.9,  inner_ci_level = .9,
                        model.names = c("High Flood Risk", "Low Flood Risk"),
                        legend.title = "",
                        coefs = c("Republican"= "republican" 
                                  , "UFB/Total Expense"="unrestricted_fund_balance_over_total_expense",
                                  "Debt Outstanding" = "log(scaled_outstanding)",
                                  "CB Outlook" ="CB_reported_outlook_years"
                        ), scale = T
                        
)
figure5a_data = adapt_plot$data
# Save the data for publication
write_xlsx( figure5a_data, "../data for figures/figure 5/figure5_panelA.xlsx" ) # Excel

adapt_plot =  adapt_plot  + labs(x = "\n Adaptation Gap \n ", y = NULL) + theme(legend.position = "bottom")

ggsave(paste0( '../results/figures/'
               ,  'adaptation_gap_by_fr.pdf', sep = '' ), adapt_plot 
       ,width = 4.5, height = 6.88, dpi = 300, units = "in")

### --------------- Plot splits by opinions---------------

adapt_plot = plot_summs(adaptation_hw, adaptation_lw,
                        ci_level =0.9,  inner_ci_level = .9,
                        model.names = c("High Local Off.", "Low Local Off."),
                        legend.title = "",
                        coefs = c("Republican"= "republican" 
                                  , "UFB/Total Expense"="unrestricted_fund_balance_over_total_expense",
                                  "Debt Outstanding" = "log(scaled_outstanding)",
                                  "CB Outlook" ="CB_reported_outlook_years"
                        ), scale = T
)

figure5b_data = adapt_plot$data
# Save the data for publication
write_xlsx( figure5b_data, "../data for figures/figure 5/figure5_panelB.xlsx" ) # Excel

adapt_plot =  adapt_plot  + labs(x = "\n Adaptation Gap \n ", y = NULL) + theme(legend.position = "bottom")

ggsave(paste0( '../results/figures/'
               ,  'adaptation_gap_by_opinion.pdf', sep = '' ), adapt_plot 
       ,width = 4.5, height = 6.88, dpi = 300, units = "in")

### --------------- Plot splits by state grants---------------

adapt_plot = plot_summs(adaptation_sg, adaptation_no_sg,
                        ci_level =0.9,  inner_ci_level = .9,
                        model.names = c("Large Grant", "Small Grant"),
                        legend.title = "",
                        coefs = c("Republican"= "republican" 
                                  , "UFB/Total Expense"="unrestricted_fund_balance_over_total_expense",
                                  "Debt Outstanding" = "log(scaled_outstanding)",
                                  "CB Outlook" ="CB_reported_outlook_years"
                        ), scale = T
)
figure5c_data = adapt_plot$data
# Save the data for publication
write_xlsx( figure5c_data, "../data for figures/figure 5/figure5_panelC.xlsx" ) # Excel

adapt_plot =  adapt_plot  + labs(x = "\n Adaptation Gap \n ", y = NULL) + theme(legend.position = "bottom")

ggsave(paste0('../results/figures/'
               ,  'adaptation_gap_by_grant.pdf', sep = '' ), adapt_plot
       ,width = 4.5, height = 6.88, dpi = 300, units = "in")

### --------------- Plot splits by income per household---------------

adapt_plot = plot_summs(adaptation_high_per_household, adaptation_low_per_household,
                        ci_level =0.9,  inner_ci_level = .9,
                        model.names = c("High H/H Income", "Low H/H Income"),
                        legend.title = "",
                        coefs = c("Republican"= "republican" 
                                  , "UFB/Total Expense"="unrestricted_fund_balance_over_total_expense",
                                  "Debt Outstanding" = "log(scaled_outstanding)",
                                  "CB Outlook" ="CB_reported_outlook_years"
                        ), scale = T
)
figure5d_data = adapt_plot$data
# Save the data for publication
write_xlsx( figure5d_data, "../data for figures/figure 5/figure5_panelD.xlsx" ) # Excel


adapt_plot =  adapt_plot  + labs(x = "\n Adaptation Gap \n ", y = NULL) + theme(legend.position = "bottom")

ggsave(paste0( '../results/figures/'
               ,  'adaptation_gap_by_per_hh_income.pdf', sep = '' ), adapt_plot  
       ,width = 4.5, height = 6.88, dpi = 300, units = "in")


#--------------------------------------------------------------------
# (6)  Robustness: alternative definitions of adaptation gap 
#------------------------------------------------------------------

gap = 0

adaptation_all = felm(
                      log( measure_adapt + 1 ) ~
                      + fspropertiesatrisk2020pct
                      + log(Population)
                      + log( total_number_of_sentences )
                      |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_all )
adaptation_all$N

cc_data$resid_adapt = residuals( adaptation_all )
cc_data$adapt_gap_0 = ifelse( cc_data$resid_adapt < gap, 1, 0 )
cc_data$adapt_gap_half = ifelse( cc_data$resid_adapt < -0.5, 1, 0 )
cc_data$adapt_gap_one = ifelse( cc_data$resid_adapt < -1, 1, 0 )

adaptation =  felm(  
                      adapt_gap
                      ~  republican
                      + unrestricted_fund_balance_over_total_expense
                      + log( scaled_outstanding )
                      + CB_reported_outlook_years
                      |   stateabbv + reporting_year  | 0 | stateabbv, 
                      data = cc_data   )
summary( adaptation )
adaptation$N

adaptation_half =  felm(  
                          adapt_gap_half
                          ~  republican
                          + unrestricted_fund_balance_over_total_expense
                          + log( scaled_outstanding )
                          + CB_reported_outlook_years
                          |   stateabbv + reporting_year  | 0 | stateabbv, 
                          data = cc_data   )
summary( adaptation_half )
adaptation_half$N

adaptation_one =  felm(  
                        adapt_gap_one
                        ~  republican
                        + unrestricted_fund_balance_over_total_expense
                        + log( scaled_outstanding )
                        + CB_reported_outlook_years
                        |   stateabbv + reporting_year  | 0 | stateabbv, 
                        data = cc_data   )
summary( adaptation_one )
adaptation_one$N

m1 = adaptation
m2 = adaptation_half
m3 = adaptation_one

determinant_flood_risk1 = capture.output(stargazer( m1, m2, m3, 
                                                    title = "\\textbf{Robustness: Determinants of the Adaptation Gap.} \\\\
                                                    		    This table reproduces Supplementary Table 5 Panel A with the alternative definitions of \\textit{Adapt Gap}.
		                                                        Column (1) defines \\textit{Adapt Gap} as an indicator that equals to one if the residuals from the Supplementary Table 4 Panel A regressions are negative.
                                                    		    Column (2) defines \\textit{Adapt Gap} as an indicator that equals to one if the residuals from the Supplementary Table 4 Panel A regressions are less than --0.5.
                                                    		     Column (3) defines \\textit{Adapt Gap} as an indicator that equals to one if the residuals from the Supplementary Table 4 Panel A regressions are less than --1.
                                                    		    \\textit{Republican} is an indicator variable equal to one if the city has a Republican mayor.
                                                    		    \\textit{UFB/Total Expense} is unrestricted fund balance scaled by total expenses.
                                                            \\textit{Total Debt per Capita} is total debt outstanding scaled by the population of the city.
                                                            \\textit{Capital Budget Outlook} is the reported number of years in the capital budget.
                                                            We also include state and year fixed effects. Standard errors are clustered at the state level.
                                                            *, **, and *** denote p-values less than 0.10, 0.05, and 0.01, respectively.",
                                                    type="latex",  
                                                      column.labels   =  c("Adapt Gap"),
                                                     column.separate = c(3, 3 ),
                                                    dep.var.labels   = c("I(Res < 0)", "I(Res < -0.5)", "I(Res < -1)"),
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
                                                      c( "Measure",  "Main", "Main", "Main" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes"),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes"),
                                                      c( "Clustered s.e. ","State", "State", "State")
                                                    ),
                                                    table.placement = "H"
) )

note.latex <- ""
determinant_flood_risk1[grepl("Note",determinant_flood_risk1)] <- note.latex
determinant_flood_risk1 = gsub('t ?= ?([$-.0-9]*)', '(\\1)', determinant_flood_risk1)
writeLines( determinant_flood_risk1, paste0( '../results/tables/adaptation_gap_robust.tex' ) )

#-----------------------------------------------------------------
# (7) Robustness: alternative adaptation measures
#-----------------------------------------------------------------

gap = 0

adaptation_max1 = felm(
                        log( measure_adapt_max1 + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_max1 )
adaptation_max1$N

cc_data$resid_adapt = residuals( adaptation_max1 )
cc_data$adapt_gap1 = ifelse( cc_data$resid_adapt < gap, 1, 0 )

adaptation_gap_max1 =  felm(  
                              adapt_gap1
                              ~  republican
                              + unrestricted_fund_balance_over_total_expense
                              + log( scaled_outstanding )
                              + CB_reported_outlook_years
                              |   stateabbv + reporting_year  | 0 | stateabbv, 
                              data = cc_data   )
summary( adaptation_gap_max1 )
adaptation_gap_max1$N

adaptation_max5 = felm( 
                          log( measure_adapt_max5 + 1 ) ~
                          + fspropertiesatrisk2020pct
                          + log(Population)
                          + log( total_number_of_sentences )
                          |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_max5 )
adaptation_max5$N

cc_data$resid_adapt = residuals( adaptation_max5 )
cc_data$adapt_gap5 = ifelse( cc_data$resid_adapt < gap, 1, 0 )

adaptation_gap_max5 =  felm(  
                              adapt_gap5
                              ~  republican
                              + unrestricted_fund_balance_over_total_expense
                              + log( scaled_outstanding )
                              + CB_reported_outlook_years
                              |   stateabbv + reporting_year  | 0 | stateabbv, 
                              data = cc_data   )
summary( adaptation_gap_max5 )
adaptation_gap_max5$N

adaptation_max10 = felm( 
                        log( measure_adapt_max10 + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_max10 )
adaptation_max10$N

cc_data$resid_adapt = residuals( adaptation_max10 )
cc_data$adapt_gap10 = ifelse( cc_data$resid_adapt < gap, 1, 0 )

adaptation_gap_max10 =  felm(  
                              adapt_gap10
                              ~  republican
                              + unrestricted_fund_balance_over_total_expense
                              + log( scaled_outstanding )
                              + CB_reported_outlook_years
                              |   stateabbv + reporting_year  | 0 | stateabbv, 
                              data = cc_data   )
summary( adaptation_gap_max10 )
adaptation_gap_max10$N

adaptation_ngroup = felm(
                          log( ngroup_adapt + 1 ) ~
                          + fspropertiesatrisk2020pct
                          + log(Population)
                          + log( total_number_of_sentences )
                          |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_ngroup )
adaptation_ngroup$N

cc_data$resid_adapt = residuals( adaptation_ngroup )
cc_data$adapt_gap_ngroup = ifelse( cc_data$resid_adapt < gap, 1, 0 )

adaptation_gap_ngroup =  felm(  
                                adapt_gap_ngroup
                                ~  republican
                                + unrestricted_fund_balance_over_total_expense
                                + log( scaled_outstanding )
                                + CB_reported_outlook_years
                                |   stateabbv + reporting_year  | 0 | stateabbv, 
                                data = cc_data   )
summary( adaptation_gap_ngroup )
adaptation_gap_ngroup$N

adaptation_nwords = felm( 
                            log( keyword_adapt + 1 ) ~
                            + fspropertiesatrisk2020pct
                            + log(Population)
                            + log( total_number_of_sentences )
                            |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( adaptation_nwords )
adaptation_nwords$N

cc_data$resid_adapt = residuals( adaptation_nwords )
cc_data$adapt_gap_nwords = ifelse( cc_data$resid_adapt < gap, 1, 0 )

adaptation_gap_nwords =  felm(  
                                adapt_gap_nwords
                                ~  republican
                                + unrestricted_fund_balance_over_total_expense
                                + log( scaled_outstanding )
                                + CB_reported_outlook_years
                                |   stateabbv + reporting_year  | 0 | stateabbv, 
                                data = cc_data   )
summary( adaptation_gap_nwords )
adaptation_gap_nwords$N

m0 = adaptation
m1 = adaptation_gap_max1
m2 = adaptation_gap_max5
m3 = adaptation_gap_max10
m5 = adaptation_gap_ngroup
m6 = adaptation_gap_nwords

robust_max = capture.output(stargazer( m0, m1, m2, m3, m6,  m5,
                                                    title = "\\textbf{Determinants of the Adaptation Gap. Robustness to capping adaptation terms.} \\\\
                                                    		    This table reproduces Supplementary Table 5 Panel A with the alternative definitions of \\textit{Adapt Gap}.
                                                            Column (1) defines \\textit{Adapt Gap} as an indicator that equals to one if the residuals from the Table SN 6.1 Panel A regressions are negative.
                                                    		    Column (2) defines \\textit{Adapt Gap} as an indicator that equals to one if the residuals from the Table SN 6.1  Panel A regressions are negative,
                                                    		    using adaptation measure that caps the appearance of a single adaptation term at 1.
                                                    		    Column (3) defines \\textit{Adapt Gap}  as an indicator that equals to one if the residuals from the Table SN 6.1  Panel A regressions are negative,
                                                    		    using adaptation measure that caps the appearance of a single adaptation term at 5.
                                                    		     Column (4) defines \\textit{Adapt Gap}  as an indicator that equals to one if the residuals from the Table SN 6.1  Panel A regressions are negative,
                                                    		    using adaptation measure that caps the appearance of a single adaptation term at 10.
                                                    		    Column (5) defines \\textit{Adapt Gap}  as an indicator that equals to one if the residuals from the Table SN 6.1 Panel A regressions are negative,
                                                    		    using adaptation measure that is defined as the number of keywords.
                                                    		    Column (6) defines \\textit{Adapt Gap}  as an indicator that equals to one if the residuals from the Table SN 6.1 Panel A regressions are negative,
                                                    		    using adaptation measure that is defined as the number of groups.
                                                    		    \\textit{Republican} is an indicator variable equal to one if the city has a Republican mayor.
                                                    		    \\textit{UFB/Total Expense} is unrestricted fund balance scaled by total expenses.
                                                            \\textit{Total Debt per Capita} is total debt outstanding scaled by the population of the city.
                                                            \\textit{Capital Budget Outlook} is the reported number of years in the capital budget.
                                                            We also include state and year fixed effects. Standard errors are clustered at the state level.
                                                            *, **, and *** denote p-values less than 0.10, 0.05, and 0.01, respectively.",
                                                    type="latex",  
                                                    dep.var.labels   = c("Main","Max 1", "Max 5", "Max 10",  "N words", "N groups"),
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
                                                      c( "Measure",  "Main", "Main", "Main", "Main", "Main", "Main" ),
                                                      c( "State FE ",  "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes"),
                                                      c( "Year FE ",  "Yes", "Yes", "Yes",  "Yes", "Yes", "Yes"),
                                                      c( "Clustered s.e. ","State", "State", "State","State", "State", "State")
                                                    ),
                                                    table.placement = "H"
) )

note.latex <- ""
robust_max[grepl("Note",robust_max)] <- note.latex
robust_max = gsub('t ?= ?([$-.0-9]*)', '(\\1)', robust_max)
writeLines( robust_max, paste0( "../results/tables/adaptation_gap_robust_max_measure.tex" ) )


