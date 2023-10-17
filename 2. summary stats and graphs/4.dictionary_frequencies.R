
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

#---------------------------------------------
# RUN: Define functions
#---------------------------------------------

'%!in%' <- function(x,y)!('%in%'(x,y))

winsorize_x = function(x, cut = 0.01){
  cut_point_top <- quantile(x, 1 - cut, na.rm = T)
  cut_point_bottom <- quantile(x, cut, na.rm = T)
  i = which(x >= cut_point_top) 
  x[i] = cut_point_top
  j = which(x <= cut_point_bottom) 
  x[j] = cut_point_bottom
  return(x)
}

#-------------------------------------------------------------------------------
# (3) Load the frequency data and put it into latex
#-------------------------------------------------------------------------------

adapt_freq  = read_excel(paste0( "../data/datasets/adapt_frequency_clean.xlsx") )
adapt_freq$frequency = formatC(adapt_freq$frequency, format="f", big.mark=",", digits=0)
adapt_freq$frequency = as.character( adapt_freq$frequency )

adapt_freq$keyword = gsub( "_", " ", adapt_freq$keyword )

adapt_freq = adapt_freq %>% filter( frequency > 0 )

dictionary_freq = data.frame(  adapt_freq$keyword[1:50], adapt_freq$frequency[1:50],
                               adapt_freq$keyword[51:100], adapt_freq$frequency[51:100],
                               adapt_freq$keyword[101:150], adapt_freq$frequency[101:150],
                               adapt_freq$keyword[151:200], adapt_freq$frequency[151:200]
)



summary_tex <- xtable( dictionary_freq )
print(summary_tex,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      sanitize.text.function=function(x){x},
      file=paste0( "../results/tables/dictionary_frequencies.tex"))
