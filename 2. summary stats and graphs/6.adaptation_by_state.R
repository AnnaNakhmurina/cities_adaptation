
library(readxl)
library(dplyr)
library(raster)
library(rgdal)
library(tidyverse)
extrafont::loadfonts()
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
# (1) Load the adaptation data
#-----------------------------------------------------------------
# Main data
cc_data = read.fst( "../data/cleaned_data/cc_data_ACFRs_budgets.fst" )

#-----------------------------------------------------------------
# (2) Extract the number of states 
#----------------------------------------------------------------

states  = cc_data %>% group_by( stateabbv) %>%  
  summarise_each( funs(mean(., na.rm = TRUE) ),
                  fspropertiesatrisk2020pct ) %>% arrange( desc( fspropertiesatrisk2020pct ) )

#-----------------------------------------------------------------
# (3) Create state-by-measure data
#----------------------------------------------------------------

cc_data = as.data.table( cc_data )

overall_time_trend_all = cc_data %>% group_by(  StateAbbv ) %>% 
  summarise_each( funs(mean(., na.rm = TRUE), sd(., na.rm = T),  ),
                  measure_adapt, measure_sub_hardprotect, measure_sub_softprotect) %>% 
  dplyr::select( StateAbbv,  
                  measure_adapt_mean, measure_sub_hardprotect_mean, measure_sub_softprotect_mean,
                 measure_adapt_sd, measure_sub_hardprotect_sd, measure_sub_softprotect_sd ) 

state_by_measure = left_join( states, overall_time_trend_all, by = c( "stateabbv" = "StateAbbv" )  )

# Save the data for publication
write_xlsx( state_by_measure, "../data for figures/supplementary figures/supp_figure1.xlsx" ) # Excel

#-----------------------------------------------------------------
# (4) Plot adaptation by state
#-----------------------------------------------------------------

graph <- ggplot(data = state_by_measure,
                mapping = aes(x = measure_adapt_mean,  y =  reorder(stateabbv, fspropertiesatrisk2020pct),
                              fill= "State_data", color = "State_data", shape = "rect"  ) ) +
                              geom_point( size = 11 ) + 
                              geom_pointrange(mapping = aes(xmin = measure_adapt_mean - measure_adapt_sd,
                                                            xmax = measure_adapt_mean + measure_adapt_sd),
                                              linewidth = 4  ) +
                              scale_color_manual(name=" ", values=c(State_data ='#2A363B')) + 
                              scale_fill_manual(name=" ", values=c(State_data ='#2A363B')) +
                              scale_shape_manual(name=" ", values=c( rect = 22 )) +
                              labs( x = "Number of sentences", y = "" ) +
                              scale_x_continuous( limits = c(-50,150) )  +
                              theme_ipsum() + theme(
                                legend.position="none",
                                axis.title.x=element_text( size = 50 )
                                ,axis.text.x = element_text( size = 40 )
                                ,axis.text.y = element_text( size = 40 )
                              ) 
graph

# Shade all coastal states. 
graph = graph + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 44.5, ymax = 45.5 ), alpha = .005, color = NA) + # SC
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 43.5, ymax = 44.5 ), alpha = .005, color = NA) + # FL
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 42.5, ymax = 43.5 ), alpha = .005, color = NA) + # LA
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 41.5, ymax = 42.5 ), alpha = .005, color = NA) + # MS
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 40.5, ymax = 41.5 ), alpha = .005, color = NA) + # OR
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 38.5, ymax = 39.5 ), alpha = .005, color = NA) +  # NY
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 37.5, ymax = 38.5 ), alpha = .005, color = NA) +  # MA
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 36.5, ymax = 37.5 ), alpha = .005, color = NA) + # TX
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 35.5, ymax = 36.5 ), alpha = .005, color = NA) + # CT
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 32.5, ymax = 33.5 ), alpha = .005, color = NA) + # AL
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 30.5, ymax = 31.5 ), alpha = .005, color = NA) + # CA
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 29.5, ymax = 30.5 ), alpha = .005, color = NA) + # VA
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 28.5, ymax = 29.5 ), alpha = .005, color = NA) + # NH
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 26.5, ymax = 27.5 ), alpha = .005, color = NA) + # NJ
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 23.5, ymax = 24.5 ), alpha = .005, color = NA) + # GA
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 22.5, ymax = 23.5 ), alpha = .005, color = NA) +  # WA
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 21.5, ymax = 22.5 ), alpha = .005, color = NA) + # RI
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 20.5, ymax = 21.5 ), alpha = .005, color = NA) + # NC
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10.5, ymax = 11.5 ), alpha = .005, color = NA) + # DC
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = 8.5 ), alpha = .005, color = NA) + # ME
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5 ), alpha = .005, color = NA) + # DE
                geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 5.5, ymax = 6.5 ), alpha = .005, color = NA) # MD
graph

ggsave(paste0( '../results/figures/'
               ,  'adaptation_sentences_by_state.pdf', sep = '' ), graph,  
       width = 10, height = 40, dpi = 300, units = "in")


#-----------------------------------------------------------------
# (5) Plot hard adaptation by state
#-----------------------------------------------------------------

graph <- ggplot(data = state_by_measure,
                mapping = aes(x = measure_sub_hardprotect_mean,  y =  reorder(stateabbv, fspropertiesatrisk2020pct),
                              fill= "State_data", color = "State_data", shape = "rect"  ) )+
                          geom_point( size = 11 ) + 
                          geom_pointrange(mapping = aes(xmin = measure_sub_hardprotect_mean - measure_sub_hardprotect_sd,
                                                        xmax = measure_sub_hardprotect_mean + measure_sub_hardprotect_sd),
                                          linewidth = 4  ) +
                          scale_color_manual(name=" ", values=c(State_data ='#E84A5F')) + 
                          scale_fill_manual(name=" ", values=c(State_data ='#E84A5F')) +
                          scale_shape_manual(name=" ", values=c( rect = 22 )) +
                          labs( x = "Number of sentences", y = ""
                          ) +
                          scale_x_continuous( limits = c(-50,150) )  +
                          theme_ipsum() + theme( 
                            legend.position="none",
                            axis.title.x=element_text( size = 50 )
                            ,axis.text.x = element_text( size = 40 )
                            ,axis.text.y = element_text( size = 40 )
  ) 
graph

# Shade all coastal states. 
graph = graph + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 44.5, ymax = 45.5 ), alpha = .005, color = NA, fill = '#2A363B') + # SC
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 43.5, ymax = 44.5 ), alpha = .005, color = NA, fill = '#2A363B') + # FL
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 42.5, ymax = 43.5 ), alpha = .005, color = NA, fill = '#2A363B') + # LA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 41.5, ymax = 42.5 ), alpha = .005, color = NA, fill = '#2A363B') + # MS
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 40.5, ymax = 41.5 ), alpha = .005, color = NA, fill = '#2A363B') + # OR
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 38.5, ymax = 39.5 ), alpha = .005, color = NA, fill = '#2A363B') +  # NY
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 37.5, ymax = 38.5 ), alpha = .005, color = NA, fill = '#2A363B') +  # MA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 36.5, ymax = 37.5 ), alpha = .005, color = NA, fill = '#2A363B') + # TX
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 35.5, ymax = 36.5 ), alpha = .005, color = NA, fill = '#2A363B') + # CT
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 32.5, ymax = 33.5 ), alpha = .005, color = NA, fill = '#2A363B') + # AL
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 30.5, ymax = 31.5 ), alpha = .005, color = NA, fill = '#2A363B') + # CA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 29.5, ymax = 30.5 ), alpha = .005, color = NA, fill = '#2A363B') + # VA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 28.5, ymax = 29.5 ), alpha = .005, color = NA, fill = '#2A363B') + # NH
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 26.5, ymax = 27.5 ), alpha = .005, color = NA, fill = '#2A363B') + # NJ
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 23.5, ymax = 24.5 ), alpha = .005, color = NA, fill = '#2A363B') + # GA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 22.5, ymax = 23.5 ), alpha = .005, color = NA, fill = '#2A363B') +  # WA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 21.5, ymax = 22.5 ), alpha = .005, color = NA, fill = '#2A363B') + # RI
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 20.5, ymax = 21.5 ), alpha = .005, color = NA, fill = '#2A363B') + # NC
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10.5, ymax = 11.5 ), alpha = .005, color = NA, fill = '#2A363B') + # DC
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = 8.5 ), alpha = .005, color = NA, fill = '#2A363B') + # ME
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5 ), alpha = .005, color = NA, fill = '#2A363B') + # DE
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 5.5, ymax = 6.5 ), alpha = .005, color = NA, fill = '#2A363B') # MD
graph

ggsave(paste0( '../results/figures/'
               ,  'hard_adaptation_sentences_by_state.pdf', sep = '' ), graph,  
       width = 10, height = 40, dpi = 300, units = "in") 

#-----------------------------------------------------------------
# (6) Plot soft adaptation by state
#-----------------------------------------------------------------

graph <- ggplot(data = state_by_measure,
                mapping = aes(x = measure_sub_softprotect_mean,  y =  reorder( stateabbv, fspropertiesatrisk2020pct ),
                  fill= "State_data", color = "State_data", shape = "rect"  ) )+
                  geom_point( size = 11 ) + 
                  geom_pointrange(mapping = aes(xmin = measure_sub_softprotect_mean - measure_sub_softprotect_sd,
                                                xmax = measure_sub_softprotect_mean + measure_sub_softprotect_sd),
                                  linewidth = 4  ) +
                  scale_color_manual(name=" ", values=c(State_data ='#019875')) + 
                  scale_fill_manual(name=" ", values=c(State_data ='#019875')) +
                  scale_shape_manual(name=" ", values=c( rect = 22 )) +
                  labs( x = "Number of sentences", y = ""
                  ) +
                  scale_x_continuous( limits = c(-20,20) )  +
                  theme_ipsum() + theme(
                    legend.position="none",
                    axis.title.x=element_text( size = 50 )
                    ,axis.text.x = element_text( size = 40 )
                    ,axis.text.y = element_text( size = 40 )
                  ) 
graph

# Shade all coastal states. 
graph = graph + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 44.5, ymax = 45.5 ), alpha = .005, color = NA, fill = '#2A363B') + # SC
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 43.5, ymax = 44.5 ), alpha = .005, color = NA, fill = '#2A363B') + # FL
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 42.5, ymax = 43.5 ), alpha = .005, color = NA, fill = '#2A363B') + # LA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 41.5, ymax = 42.5 ), alpha = .005, color = NA, fill = '#2A363B') + # MS
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 40.5, ymax = 41.5 ), alpha = .005, color = NA, fill = '#2A363B') + # OR
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 38.5, ymax = 39.5 ), alpha = .005, color = NA, fill = '#2A363B') +  # NY
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 37.5, ymax = 38.5 ), alpha = .005, color = NA, fill = '#2A363B') +  # MA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 36.5, ymax = 37.5 ), alpha = .005, color = NA, fill = '#2A363B') + # TX
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 35.5, ymax = 36.5 ), alpha = .005, color = NA, fill = '#2A363B') + # CT
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 32.5, ymax = 33.5 ), alpha = .005, color = NA, fill = '#2A363B') + # AL
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 30.5, ymax = 31.5 ), alpha = .005, color = NA, fill = '#2A363B') + # CA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 29.5, ymax = 30.5 ), alpha = .005, color = NA, fill = '#2A363B') + # VA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 28.5, ymax = 29.5 ), alpha = .005, color = NA, fill = '#2A363B') + # NH
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 26.5, ymax = 27.5 ), alpha = .005, color = NA, fill = '#2A363B') + # NJ
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 23.5, ymax = 24.5 ), alpha = .005, color = NA, fill = '#2A363B') + # GA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 22.5, ymax = 23.5 ), alpha = .005, color = NA, fill = '#2A363B') +  # WA
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 21.5, ymax = 22.5 ), alpha = .005, color = NA, fill = '#2A363B') + # RI
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 20.5, ymax = 21.5 ), alpha = .005, color = NA, fill = '#2A363B') + # NC
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 10.5, ymax = 11.5 ), alpha = .005, color = NA, fill = '#2A363B') + # DC
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 7.5, ymax = 8.5 ), alpha = .005, color = NA, fill = '#2A363B') + # ME
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5 ), alpha = .005, color = NA, fill = '#2A363B') + # DE
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 5.5, ymax = 6.5 ), alpha = .005, color = NA, fill = '#2A363B') # MD
graph

ggsave(paste0( '../results/figures/'
               ,  'soft_adaptation_sentences_by_state.pdf', sep = '' ), graph,  
       width = 10, height = 40, dpi = 300, units = "in") 

