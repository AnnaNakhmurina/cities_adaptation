
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
library(hrbrthemes)
library(awtools)
library(extrafont)
library("writexl")

#-----------------------------------------------------------------
# (1) Load the merged data on collected cities
#-----------------------------------------------------------------

# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )

#-----------------------------------------------------------------
# (2) Create the adaptation gap
#-----------------------------------------------------------------

gap = 0

adaptation_all = felm(  log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv + reporting_year, data = cc_data  )
summary( adaptation_all )
adaptation_all$N

cc_data$resid_adapt = residuals( adaptation_all )
cc_data$adapt_gap = ifelse( cc_data$resid_adapt < gap, 1, 0 )

#-----------------------------------------------------------------
# (3) Plot by party
#-----------------------------------------------------------------

cc_data$rep = ifelse( cc_data$republican == 1, "Republican", "Not Republican" )

# Create the data for graphs
overall_time_trend = cc_data %>% group_by(reporting_year, rep) %>% 
  summarise_each( funs(mean(., na.rm = TRUE)), adapt_gap ) %>% 
  dplyr::select( reporting_year, rep, adapt_gap)
# Save the data
write_xlsx( overall_time_trend, "../data for figures/figure 3/figure3_panelA.xlsx" ) # Excel

graph = ggplot(overall_time_trend, aes(x = as.integer(reporting_year),
                                       y = adapt_gap,
                                       color = rep )) +
  geom_point( size=12, pch=20 ) + geom_line( size = 1.7) + 
  ggtitle("") + 
  labs( y = "Adaptation gap", x = "", color = "Party\n" ) +
  scale_linetype( name = "Party\n", labels = c("High", "Low") ) +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  scale_color_manual(name="Party\n", values=c(  "#025196", '#E84A5F' )) +
  theme_ipsum() + theme( 
    legend.position="right",
    strip.text.x = element_text( size = 30),
    axis.title.y = element_text( size = 50 )
    ,legend.text = element_text( size = 45 )
    ,legend.title = element_text( size = 45 )
    ,axis.text.x = element_text( size = 35 )
    ,axis.text.y = element_text( size = 35 )
  ) 
graph

ggsave(paste0( '../results/figures/'
               ,'all_sentences_by_party_flood_risk_gap_adaptation.pdf', sep = '' ), graph,  
                width = 17, height = 10, dpi = 300, units = "in") 

#-----------------------------------------------------------------
# (4) Plot by debt per capita
#-----------------------------------------------------------------

overall_time_trend = cc_data %>% group_by(reporting_year, high_debt) %>%  filter( high_debt ==  0 | high_debt == 1 ) %>%
  summarise_each( funs(mean(., na.rm = TRUE)), adapt_gap ) %>% 
  dplyr::select( reporting_year, high_debt, adapt_gap ) 
overall_time_trend$debt = ifelse( overall_time_trend$high_debt == 1, "High", "Low" )
# Save the data
write_xlsx( overall_time_trend, "../data for figures/figure 3/figure3_panelB1.xlsx" ) # Excel


graph = ggplot(overall_time_trend, aes(x = as.integer(reporting_year),
                                       y = adapt_gap,
                                       color = debt )) +
  geom_point( size=14, pch=20 ) + geom_line( size = 2) + 
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Adaptation gap", x = "", color = "Debt per capita\n" ) +
  scale_linetype( name = "Debt per capita\n", labels = c("High", "Low") ) +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  scale_color_manual(name="Debt per capita\n", values=c(   '#025196', "#E84A5F" )) +
  theme_ipsum() + theme( 
    legend.position="right",
    strip.text.x = element_text( size = 30),
    axis.title.y = element_text( size = 50 )
    ,legend.text = element_text( size = 45 )
    ,legend.title = element_text( size = 45 )
    ,axis.text.x = element_text( size = 35 )
    ,axis.text.y = element_text( size = 35 )
  ) 
graph

ggsave(paste0( '../results/figures/'
               ,  'adaptation_gap_by_debt_per_capita_flood_risk.pdf', sep = '' ), graph,  
       width = 17, height = 10, dpi = 300, units = "in") 

#-----------------------------------------------------------------
# (5) Plot by unrestricted fund balance
#-----------------------------------------------------------------

overall_time_trend = cc_data %>% group_by(reporting_year, high_ufb) %>% 
  summarise_each( funs(mean(., na.rm = TRUE)), adapt_gap ) %>% 
  dplyr::select( reporting_year, high_ufb, adapt_gap ) %>%
  filter( high_ufb ==  0 | high_ufb == 1 )
overall_time_trend$ufb = ifelse( overall_time_trend$high_ufb == 1, "High", "Low" )
# Save the data
write_xlsx( overall_time_trend, "../data for figures/figure 3/figure3_panelB2.xlsx" ) # Excel

graph = ggplot(overall_time_trend, aes(x = as.integer(reporting_year), 
                                       y = adapt_gap, 
                                       color = ufb )) +
  geom_point( size=14, pch=20 ) + geom_line( size = 2) + 
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Adaptation gap", x = "", color = "Unrestricted fund balance \n" ) +
  scale_linetype( name = "Unrestricted fund balance \n", labels = c("High", "Low") ) +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  scale_color_manual(name="UFB/Total Expense \n", values=c(   '#025196', "#E84A5F" )) +
  theme_ipsum() + theme(
    legend.position="right",
    strip.text.x = element_text( size = 30),
    axis.title.y = element_text( size = 50 )
    ,legend.text = element_text( size = 45 )
    ,legend.title = element_text( size = 45 )
    ,axis.text.x = element_text( size = 35 )
    ,axis.text.y = element_text( size = 35 )
  ) 
graph

ggsave(paste0( '../results/figures/'
               ,  'adaptation_gap_by_ufb_flood_risk.pdf', sep = '' ), graph,  
       width = 17, height = 10, dpi = 300, units = "in") 

#-----------------------------------------------------------------
# (9) Testing: plot exposure and adaptation by capital budget outlook
#-----------------------------------------------------------------



overall_time_trend = cc_data %>% group_by( reporting_year, high_cb_outlook ) %>% filter( high_cb_outlook ==  0 | high_cb_outlook == 1 ) %>%
  summarise_each( funs(mean(., na.rm = TRUE)), adapt_gap ) %>% 
  dplyr::select( reporting_year, high_cb_outlook, adapt_gap )
overall_time_trend$cb_outlook = ifelse( overall_time_trend$high_cb_outlook == 1, "High", "Low" )
# Save the data
write_xlsx( overall_time_trend, "../data for figures/figure 3/figure3_panelC.xlsx" ) # Excel

graph = ggplot(overall_time_trend, aes(x = as.integer(reporting_year),
                                       y = adapt_gap, 
                                       color = cb_outlook
)) +
  geom_point( size=14, pch=20 ) + geom_line( size = 2) + 
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Adaptation gap", x = "", color = "CB Outlook\n" ) +
  scale_linetype( name = "CB Outlook\n", labels = c("High", "Low") ) +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  scale_color_manual(name="CB Outlook\n", values=c(   '#025196', "#E84A5F" )) +
  theme_ipsum() + theme(
    legend.position="right",
    strip.text.x = element_text( size = 30),
    axis.title.y = element_text( size = 50 )
    ,legend.text = element_text( size = 45 )
    ,legend.title = element_text( size = 45 )
    ,axis.text.x = element_text( size = 35 )
    ,axis.text.y = element_text( size = 35 )
  ) 
graph

ggsave(paste0('../results/figures/'
               ,  'adaptation_gap_by_cb_outlook_flood_risk.pdf', sep = '' ), graph,  
       width = 17, height = 10, dpi = 300, units = "in") 

