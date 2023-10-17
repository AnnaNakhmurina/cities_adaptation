
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
library(ggrepel)
library(haven)
library("writexl")

#---------------------------------------------
# Brush up the data
#---------------------------------------------

df = read_dta( "../data/cleaned_data/adapt_figure1_data.dta" )
df$category = ifelse( df$keyword_group == "drainage",   "Drainage", "")
df$category = ifelse( df$keyword_group == "eros_control",   "Erosion control", df$category )
df$category = ifelse( df$keyword_group == "flood",   "Flood", df$category )
df$category = ifelse( df$keyword_group == "inlet",   "Inlet", df$category )
df$category = ifelse( df$keyword_group == "levee",   "Levee", df$category )
df$category = ifelse( df$keyword_group == "other",   "Other", df$category )
df$category = ifelse( df$keyword_group == "storm water",   "Stormwater", df$category )
df$category = ifelse( df$keyword_group == "seawall",   "Seawall", df$category )

df$importance = round( df$v1/sum(df$v1)*100 , 1)

df$category <- factor(df$category, levels = c("Stormwater", "Drainage", "Seawall", "Erosion control", "Flood", "Inlet", "Levee", "Other"))
df = df[order(df$category),]
# Save the data for publication
write_xlsx( df, "../data for figures/figure 1/figure1_panelA.xlsx" ) # Excel

# prepare the data to be plotted in a pie chart
df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(importance))), 
         pos = importance/2 + lead(csum, 1),
         pos = if_else(is.na(pos), importance/2, pos))

graph = ggplot(df2, aes(x = "", y = importance, fill = fct_inorder(category))) +
  geom_col(color = "white") +
  geom_text(aes(label = paste0(importance,"%")),size = 5,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  guides(fill = guide_legend(title = "Group")) +
  scale_y_continuous(breaks = df2$pos, labels = df2$category) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 17), 
        plot.margin       = margin(-10, 0, -40, 0),
        legend.position = "none", 
        panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values=c("#FEFBE9", "#E8EFB6", "#BEE1D3", "#95CFE2", "#7CB6E5", "#988EC8", "#91648B", "#999999"))

graph
ggsave(paste0( '../results/figures/'
               ,  'figure_pie_keywordsgroup.pdf', sep = '' ), graph,  
       width = 12, height = 12, dpi = 300, units = "in") 
