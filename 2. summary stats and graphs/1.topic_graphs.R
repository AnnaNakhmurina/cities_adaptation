

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
#---------------------------------------------
# RUN: Define functions
#----------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

#-----------------------------------------------------------------
# (1) Load the topics data
#-----------------------------------------------------------------

topics = read.csv( "../data/datasets/final_output_topic.csv"  )
topics = as.data.table( topics )

# Replace NA values with zeroes -- NA does mean zero in a context of topics
topics$topic_0 = ifelse( is.na(topics$topic_0), 0,  topics$topic_0 )
topics$topic_1 = ifelse( is.na(topics$topic_1), 0,  topics$topic_1 )
topics$topic_2 = ifelse( is.na(topics$topic_2), 0,  topics$topic_2 )
topics$topic_3 = ifelse( is.na(topics$topic_3), 0,  topics$topic_3 )
topics$topic_4 = ifelse( is.na(topics$topic_4), 0,  topics$topic_4 )

# Create a reporting year variable
topics$reporting_year = topics$year
topics$reporting_year = ifelse( topics$docs_type == 'Budget', topics$reporting_year - 1 , topics$reporting_year )

topics = topics %>% filter( reporting_year >= 2013 & reporting_year <= 2020 )

bonds = topics[ docs_type == "Bonds" ]
budgets = topics[ docs_type == "Budget" ]
ACFRs = topics[ docs_type == "CAFR" ]
budgets_bond = topics[ docs_type %in% c("Budget", "CAFR") ]

# Load the data on topic definitions

topic_defs = read_excel( "../data/datasets/topic_definition_final.xlsx" )
topic_defs = topic_defs %>% dplyr::select(  "topic_name"  ,  "Topic" )

#-----------------------------------------------------------------
# (3) Plot graphs over time
#-----------------------------------------------------------------
##---- Budget topics over time ------------

overall_time_trend_all = budgets %>% group_by(reporting_year) %>% 
  summarise_each( funs(median(., na.rm = TRUE)), topic_0,
                  topic_1, topic_2, topic_3, topic_4 ) %>% 
  dplyr::select( reporting_year, topic_0,
                 topic_1, topic_2, topic_3, topic_4) 

df <- overall_time_trend_all %>%
  dplyr::select(reporting_year, topic_0,
                topic_1, topic_2, topic_3, topic_4) %>%
  gather(key = "topic_name", value = "value", -reporting_year)

# add labels to the topics
df = left_join( df, topic_defs )
# Save the data for publication
write_xlsx( df, "../data for figures/figure 1/figure1_panelB.xlsx" ) # Excel

graph = ggplot(df, aes(x = reporting_year, y = value)) + 
  geom_line(aes(color = Topic), size = 2) +  
  scale_color_manual(values = c('#2A363B', '#019875','#F5C710', '#CC3311', '#33BBEE', '#E84A5F')) +
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Budgets", x = "") +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  theme_ipsum() + theme(
    legend.position="",
    strip.text.x = element_text( size = 30),
    axis.title.y=element_text( size=30 )
    ,axis.text.x = element_text( size = 27 )
    ,axis.text.y = element_text( size = 27 )
    ,legend.text=element_text( size = 35 )
    ,legend.title=element_text( size = 35 )
  ) 
graph

ggsave(paste0( '../results/figures/'
               ,  'budget_topics_adaptation.pdf', sep = '' ), graph,  
       width = 10, height = 8, dpi = 300, units = "in")

##---- ACFR topics over time ------------

overall_time_trend_all = ACFRs %>% group_by(reporting_year) %>% 
  summarise_each( funs(median(., na.rm = TRUE)), topic_0,
                  topic_1, topic_2, topic_3, topic_4 ) %>% 
  dplyr::select( reporting_year, topic_0,
                 topic_1, topic_2, topic_3, topic_4) 

df <- overall_time_trend_all %>%
  dplyr::select(reporting_year, topic_0,
         topic_1, topic_2, topic_3, topic_4) %>%
  gather(key = "topic_name", value = "value", -reporting_year)
head(df)
# add labels to the topics
df = left_join( df, topic_defs )
# Save the data for publication
write_xlsx( df, "../data for figures/figure 1/figure1_panelC.xlsx" ) # Excel

graph = ggplot(df, aes(x = reporting_year, y = value)) + 
  geom_line(aes(color = Topic), size = 2) +  
  scale_color_manual(values = c('#2A363B', '#019875','#F5C710', '#CC3311', '#33BBEE', '#E84A5F')) +
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "ACFRs", x = "", color = "") +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  theme_ipsum() + theme( 
    legend.position="",
    strip.text.x = element_text( size = 30),
    axis.title.y=element_text( size=30 )
    ,axis.text.x = element_text( size = 27 )
    ,axis.text.y = element_text( size = 27 )
    ,legend.text=element_text( size = 35 )
    ,legend.title=element_text( size = 35 )
  ) 
graph

ggsave(paste0(  '../results/figures/'
               ,  'ACFR_topics_adaptation.pdf', sep = '' ), graph,  
       width = 10, height = 8, dpi = 300, units = "in") # 14.7 x 10.5 in image


##---- Bond topics over time ------------

# Create data for graphs
overall_time_trend_all = bonds %>% group_by(reporting_year) %>% 
  summarise_each( funs(median(., na.rm = TRUE)), topic_0,
                  topic_1, topic_2, topic_3, topic_4 ) %>% 
  dplyr::select( reporting_year, topic_0,
                 topic_1, topic_2, topic_3, topic_4) 

df <- overall_time_trend_all %>%
  dplyr::select(reporting_year, topic_0,
                topic_1, topic_2, topic_3, topic_4) %>%
  gather(key = "topic_name", value = "value", -reporting_year)
head(df)

# add labels to the topics
df = left_join( df, topic_defs )
# Save the data for publication
write_xlsx( df, "../data for figures/figure 1/figure1_panelD.xlsx" ) # Excel


graph = ggplot(df, aes(x = reporting_year, y = value)) + 
  geom_line(aes(color = Topic), size = 2) +  
  scale_color_manual(values = c('#2A363B', '#019875','#F5C710', '#CC3311', '#33BBEE', '#E84A5F')) +
  ggtitle("") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs( y = "Bonds", x = "") +
  scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
  theme_ipsum() + theme(
    legend.position="right",
    strip.text.x = element_text( size = 30),
    axis.title.y=element_text( size=30 )
    ,axis.text.x = element_text( size = 27 )
    ,axis.text.y = element_text( size = 27 )
    ,legend.text=element_text( size = 35 )
    ,legend.title=element_text( size = 35 )
  ) 
graph

ggsave(paste0(  '../results/figures/'
               ,  'bonds_topics_adaptation.pdf', sep = '' ), graph,  
       width = 17, height = 8, dpi = 300, units = "in") 

