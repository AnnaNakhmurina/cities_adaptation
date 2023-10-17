
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

#-----------------------------------------------------------------
# (1) Load the data
#-----------------------------------------------------------------

# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )
cc_data$flood_risk_quart = ifelse( cc_data$high_flood_risk_quartile == 1, "high", "low" )

# Data for each document type
cc_data_by_doc_type = read.fst ( "../data/cleaned_data/cc_data_by_doc_type.fst" )
cc_data_by_doc_type$flood_risk_quart = ifelse( cc_data_by_doc_type$high_flood_risk_quartile == 1, "high", "low" )

# Rename CAFR to ACFR (more appropriate to use)
cc_data_by_doc_type$docs_type = ifelse( cc_data_by_doc_type$docs_type == "CAFR", "ACFR", cc_data_by_doc_type$docs_type )

#-----------------------------------------------------------------
# (2) Number of keywords graphs
#-----------------------------------------------------------------

# Panel for adaptation measure aggregated across documents
overall_time_trend_all = cc_data %>% group_by(reporting_year, flood_risk_quart) %>% 
  summarise_each( funs(mean(., na.rm = TRUE)), keyword_adapt ) %>% 
  dplyr::select( reporting_year, flood_risk_quart, keyword_adapt ) 
overall_time_trend_all$docs_type = "All" # Label the aggregated measure accordingly

# Panel for each of the 3 disclosure sources
overall_time_trend_each_sources = cc_data_by_doc_type %>% group_by(docs_type, reporting_year, flood_risk_quart ) %>% 
  summarise_each( funs(mean(., na.rm = TRUE)), keyword_adapt ) %>% 
  dplyr::select( reporting_year, flood_risk_quart, keyword_adapt ) 

overall_time_trend = overall_time_trend_all %>% bind_rows( overall_time_trend_each_sources )
# Save the data for publication
write_xlsx( overall_time_trend, "../data for figures/supplementary figures/figureSN_6_1.xlsx" ) # Excel

# Specify the order in the graphs by do type
overall_time_trend$docs_type = factor(overall_time_trend$docs_type , levels = c("All", "Budget", "Bonds", "ACFR"))

graph = ggplot(overall_time_trend, aes(x = as.integer(reporting_year), y = keyword_adapt, 
                               linetype = flood_risk_quart,   fill= "All", color = "All") ) +
              geom_point( size=4, pch=20 ) + geom_line( size = 1) + facet_wrap(~ docs_type) +
              ggtitle("") + 
              theme(plot.title = element_text(hjust = 0.5)) +
              scale_linetype( name = "Flood risk\n", labels = c("High", "Low") ) +
              scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
              scale_color_manual(name=" ", values=c(All ='#2A363B')) + 
              scale_fill_manual(name=" ", values=c(All ='#2A363B')) +
              labs(y = "Number of adaptation keywords", x = "") + 
              theme_ipsum() + theme( 
                legend.position="bottom",
                strip.text.x = element_text( size = 30),
                axis.title.y=element_text( size=30 )
                ,axis.text.x = element_text( size = 27 )
                ,axis.text.y = element_text( size = 27 )
                ,legend.text=element_text( size = 29 )
                ,legend.title=element_text( size = 29 )
              ) +
              guides(color=FALSE, fill = F) 
graph

ggsave(paste0( '../results/figures/'
               ,  'adaptation_keywords.pdf', sep = '' ), graph,  
       width = 14, height = 11, dpi = 300, units = "in") 



#-----------------------------------------------------------------
# (3) Number of groups graphs
#-----------------------------------------------------------------

# Panel for adaptation measure aggregated across documents
overall_time_trend_all = cc_data %>% group_by(reporting_year, flood_risk_quart) %>% 
  summarise_each( funs(mean(., na.rm = TRUE)), ngroup_adapt ) %>% 
  dplyr::select( reporting_year, flood_risk_quart, ngroup_adapt ) 
overall_time_trend_all$docs_type = "All" # Label the aggregated measure accordingly

# Panel for each of the 3 disclosure sources
overall_time_trend_each_sources = cc_data_by_doc_type %>% group_by(docs_type, reporting_year, flood_risk_quart ) %>% 
  summarise_each( funs(mean(., na.rm = TRUE)), ngroup_adapt ) %>% 
  dplyr::select( reporting_year, flood_risk_quart, ngroup_adapt ) 

overall_time_trend = overall_time_trend_all %>% bind_rows( overall_time_trend_each_sources )
# Save the data for publication
write_xlsx( overall_time_trend, "../data for figures/supplementary figures/figureSN_6_2.xlsx" ) # Excel

# Specify the order in the graphs by do type
overall_time_trend$docs_type = factor(overall_time_trend$docs_type , levels = c("All", "Budget", "Bonds", "ACFR"))

graph = ggplot(overall_time_trend, aes(x = as.integer(reporting_year), y = ngroup_adapt, 
                                       linetype = flood_risk_quart,
                                       fill= "All", color = "All" )) +
  geom_point( size=4, pch=20 ) + geom_line( size = 1) + facet_wrap(~ docs_type) + 
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Number of adaptation groups", x = "", color = "Flood risk\n") +
  scale_linetype( name = "Flood risk\n", labels = c("High", "Low") ) +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  scale_color_manual(name=" ", values=c(All ='#2A363B')) + 
  scale_fill_manual(name=" ", values=c(All ='#2A363B')) +
  theme_ipsum() + theme( 
    legend.position="bottom",
    strip.text.x = element_text( size = 30),
    axis.title.y=element_text( size=30 )
    ,axis.text.x = element_text( size = 27 )
    ,axis.text.y = element_text( size = 27 )
    ,legend.text=element_text( size = 29 )
    ,legend.title=element_text( size = 29 )
  ) +
  guides(color=FALSE, fill = F)
graph
ggsave(paste0( '../results/figures/'
               ,  'adaptation_groups_by_flood_risk.pdf', sep = '' ), graph,  
       width = 14, height = 11, dpi = 300, units = "in")
