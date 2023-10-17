
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
# (2) Create data for plots over time
#-----------------------------------------------------------------

# create data to plot coefficients over time
main_over_time = felm( 
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct:as.factor(reporting_year)
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( main_over_time )
main_over_time$N


coefs_all = as.data.frame(summary(main_over_time)$coefficients)
toDrop <- c( "log(Population)", "log(total_number_of_sentences)") # Exclude control variables from the plot
coefs_all = coefs_all[ !(rownames(coefs_all) %in% toDrop), ] 
names(coefs_all)[2] = "se" 
coefs_all$t = c(  seq(2013, 2020, 1) )
coefs_all = coefs_all[which(names(coefs_all) %in% c("Estimate", "se", "t"))]
confidence =  1.96
coefs_all$interval = confidence*coefs_all$se

names(coefs_all) = c("Estimate_main", "se_main", "t", "interval_main")

hard_over_time = felm(
                        log( measure_sub_hardprotect + 1 ) ~
                        + fspropertiesatrisk2020pct:as.factor(reporting_year)
                        + log(Population) 
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data  )
summary( hard_over_time )
hard_over_time$N

coefs_hard = as.data.frame(summary(hard_over_time)$coefficients)
toDrop <- c( "log(Population)", "log(total_number_of_sentences)")
coefs_hard = coefs_hard[ !(rownames(coefs_hard) %in% toDrop), ] 
names(coefs_hard)[2] = "se" 
coefs_hard$t = c(  seq(2013, 2020, 1) )
coefs_hard = coefs_hard[which(names(coefs_hard) %in% c("Estimate", "se", "t"))]
confidence =  1.96 
coefs_hard$interval = confidence*coefs_hard$se

names(coefs_hard) = c("Estimate_hard", "se_hard", "t", "interval_hard")

# Merge the data from main and hard regressions
coefs_all = left_join( coefs_all, coefs_hard )


soft_over_time = felm(
                        log(  measure_sub_softprotect  + 1 )~ 
                          + fspropertiesatrisk2020pct:as.factor(reporting_year)
                        + log(Population) 
                        + log( total_number_of_sentences )
                        |    stateabbv + reporting_year   | 0 | stateabbv, data = cc_data 
)

coefs_soft = as.data.frame(summary(soft_over_time)$coefficients)
toDrop <- c( "log(Population)", "log(total_number_of_sentences)")
coefs_soft = coefs_soft[ !(rownames(coefs_soft) %in% toDrop), ] 
names(coefs_soft)[2] = "se" 
coefs_soft$t = c(  seq(2013, 2020, 1) )
coefs_soft = coefs_soft[which(names(coefs_soft) %in% c("Estimate", "se", "t"))]
confidence =  1.96 
coefs_soft$interval = confidence*coefs_soft$se

names(coefs_soft) = c("Estimate_soft", "se_soft", "t", "interval_soft")

coefs_all = left_join( coefs_all, coefs_soft )

all3_over_time = felm(
                        log( measure_adapt + 1 ) ~
                        + fspropertiesatrisk2020pct:as.factor(reporting_year)
                        + log(Population)
                        + log( total_number_of_sentences )
                        |   stateabbv + reporting_year   | 0 | stateabbv, data = cc_data3  )
summary( all3_over_time )

coefs_all3 = as.data.frame(summary(all3_over_time)$coefficients)
toDrop <- c( "log(Population)", "log(total_number_of_sentences)")
coefs_all3 = coefs_all3[ !(rownames(coefs_all3) %in% toDrop), ] 
names(coefs_all3)[2] = "se" 
coefs_all3$t = c(  seq(2013, 2020, 1) )
coefs_all3 = coefs_all3[which(names(coefs_all3) %in% c("Estimate", "se", "t"))]
confidence =  1.96
coefs_all3$interval = confidence*coefs_all3$se

names(coefs_all3) = c("Estimate_main_plus_bond", "se_main_plus_bond", "t", "interval_main_plus_bond")

coefs_all = left_join( coefs_all, coefs_all3 )

# Save the data
write_xlsx( coefs_all, "../data for figures/figure 2/figure2.xlsx" ) # Excel


#-----------------------------------------------------------------
# (3) Create plots
#-----------------------------------------------------------------


flood_risk = ggplot(coefs_all) +  geom_line(aes(x=t, y=Estimate_main, colour = "main"),  lwd=1.8) + 
  geom_ribbon(aes(x=t, ymin=Estimate_main - interval_main, ymax=Estimate_main + interval_main) , fill='#2A363B' , alpha=0.2, show.legend=F) +
  geom_point(aes(x=t, y=Estimate_main, colour = "main"), size=10, pch=20,  fill="white") +
  geom_line(aes(x=t, y=Estimate_main_plus_bond, colour = "main+bond"),  lwd=1.8) + 
  geom_ribbon(aes(x=t, ymin=Estimate_main_plus_bond- interval_main_plus_bond,
                  ymax=Estimate_main_plus_bond + interval_main_plus_bond) , fill='#99B898', alpha=0.2, show.legend=F) +
  geom_point(aes(x=t, y=Estimate_main_plus_bond, colour = "main+bond"), size=10, pch=20,  fill="white") +
  scale_colour_manual(name='', values=c("main" ='#2A363B',  "main+bond" ='#99B898')) +
  scale_fill_manual(name = '',  values=c("main"='#2A363B',  "main+bond" ='#99B898')) +
  scale_x_continuous( limits = c(2013, 2020),   breaks = seq(2013, 2020, 1)
  ) + 
  theme_bw() + theme( panel.grid.major = element_blank(),
                      legend.position="right",
                      text =  element_text(size=50),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank() ,
                      axis.line.x = element_line(size = 1.5), 
                      axis.line.y = element_line(size = 1.5)) +
  labs(x = "Year", y ="Effect on adaptation, %")  

flood_risk
ggsave(paste0( '../results/figures/'
               ,  'flood_risk_over_time_by_main.pdf', sep = '' ), flood_risk,
       width = 20, height = 15, dpi = 300, units = "in") # 14.7 x 10.5 in image

flood_risk = ggplot(coefs_all) +
  geom_line(aes(x=t, y=Estimate_hard, colour = "hard"),  lwd=1.8) + 
  geom_ribbon(aes(x=t, ymin=Estimate_hard - interval_hard, ymax=Estimate_hard + interval_hard) , fill='#E84A5F', alpha=0.2, show.legend=F) +
  geom_point(aes(x=t, y=Estimate_hard, colour = "hard"), size=10, pch=20,  fill="white") +
  geom_line(aes(x=t, y=Estimate_soft, colour = "soft"),  lwd=1.8) + 
  geom_point(aes(x=t, y=Estimate_soft, colour = "soft"), size=10, pch=20,  fill="white") +
  geom_ribbon(aes(x=t, ymin=Estimate_soft - interval_soft, ymax=Estimate_soft + interval_soft) , fill='#025196', alpha=0.2, show.legend=F) +
  scale_colour_manual(name='', values=c( "hard" = '#E84A5F',"soft" ='#025196')) +
  scale_fill_manual(name = '',  values=c( "hard" = '#E84A5F',"soft" ='#025196')) +
  scale_x_continuous( limits = c(2013, 2020),   breaks = seq(2013, 2020, 1)
  ) + 
  geom_vline(xintercept=-0.5, lty=2, lwd=0.5, colour="grey50") +
  theme_bw() + theme( panel.grid.major = element_blank(),
                      legend.position="right",
                      text =  element_text(size=50),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank() ,
                      axis.line.x = element_line(size = 1.5), 
                      axis.line.y = element_line(size = 1.5)) +
  labs(x = "Year", y ="Effect on adaptation, %")  

flood_risk

ggsave(paste0( '../results/figures/'
               ,  'flood_risk_over_time_by_hard_soft.pdf', sep = '' ), flood_risk,
       width = 20, height = 15, dpi = 300, units = "in") 
