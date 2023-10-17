
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
# (1) To produce the sensitivity graphs, download the corresponding textual data
#-----------------------------------------------------------------

# Clean the data processed in 2.clean_textual_measures.do

tune_distance_size_ngroups = data.frame()

p1 = paste0( "../data/datasets/FINAL_KEYWORDS_RESULTS_230628/")
selection = list.files(path = p1)

# Load and combine the files that relates to different number of groups with different distance size
for (i in 1:length(selection)){
  
  print(paste0( i ) ) 
  f = selection[i]
  d = read_excel( paste0(p1, f, "/adapt_diff_distance_size.xlsx") ) 
  d$stateabbv = f
  tune_distance_size_ngroups = rbind(tune_distance_size_ngroups, d)
  
}

tune_distance_size_ngroups = as.data.table(tune_distance_size_ngroups)

# Create reporting year variable consistent with main data
tune_distance_size_ngroups$reporting_year = tune_distance_size_ngroups$year
tune_distance_size_ngroups$reporting_year = ifelse( tune_distance_size_ngroups$docs_type == 'Budget',
                                                    tune_distance_size_ngroups$reporting_year - 1 , tune_distance_size_ngroups$reporting_year )
# Restrict to our sample period
tune_distance_size_ngroups = tune_distance_size_ngroups %>% filter( reporting_year >= 2013 & reporting_year <= 2020 )

# Clean up the city names
tune_distance_size_ngroups$city_name = gsub("City", "city", tune_distance_size_ngroups$city_name)
tune_distance_size_ngroups$city_name = gsub("Town", "town", tune_distance_size_ngroups$city_name)

tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Attleboro"   ,  "Attleboro city"   , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Amesbury town city"    , "Amesbury Town city"   , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Barnstable town"    , "Barnstable Town city"  , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Baytown"    , "Baytown city"  , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Beverly"    , "Beverly city"  , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Beverly"    , "Beverly city"  , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Boise city"   , "Boise City city"  , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Boise city city"  , "Boise City city"  , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Bossier city city" , "Bossier City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Boston"  , "Boston city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Braintree town city"  , "Braintree Town city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Brockton"  , "Brockton city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Cambridge"  , "Cambridge city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Charlottesvillecity"  , "Charlottesville city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Charolette city"  , "Charlotte city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==   "Everett"  , "Everett city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==    "Fall River" , "Fall River city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==    "Franklin town city" , "Franklin Town city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==    "Haverhill" , "Haverhill city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==      "Jersey city city"  , "Jersey City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==      "Kansas city city"  , "Kansas City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Lake Havasu city city"  , "Lake Havasu City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Las Vegas" , "Las Vegas city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Lawrence"  , "Lawrence city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "League city city" , "League City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Lowell", "Lowell city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Lynn", "Lynn city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Malden", "Malden city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Medford", "Medford city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Methuen town", "Methuen Town city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Missouri city city", "Missouri City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "New Bedford", "New Bedford city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Newton", "Newton city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "North Las Vegas", "North Las Vegas city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Oklahoma city city", "Oklahoma City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Quincy", "Quincy city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Revere", "Revere city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Salem", "Salem city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Salt Lake city city", "Salt Lake City city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Santa Rose city", "Santa Rosa city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Somerville", "Somerville city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "State College Borough", "State College borough" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Taunton", "Taunton city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Waltham", "Waltham city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Watertown town city", "Watertown Town city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Weymouth", "Weymouth Town city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Winthrop town city", "Winthrop Town city" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Washington city" & tune_distance_size_ngroups$state == "DC", "District of Columbia" , tune_distance_size_ngroups$city_name  )
tune_distance_size_ngroups$city_name = ifelse( tune_distance_size_ngroups$city_name ==  "Peabody", "Peabody city" , tune_distance_size_ngroups$city_name  )


# Clean up the data so that we have 1 city-docs_type-year observations
# Aggregate bonds. Needed since some cities have multiple bonds per year. 
tune_distance_size_ngroups_bonds = tune_distance_size_ngroups %>% filter(docs_type == "Bonds")
tune_distance_size_ngroups_bonds = tune_distance_size_ngroups_bonds %>% group_by(city_index, city_name, reporting_year, docs_type) %>% 
  dplyr::summarise_at(vars(number_of_sentences:distance_20_size_20), mean, na.rm = TRUE)

# Agregate ACFRs and Budgets (sometimes they have multiple pdfs per doc, just sum them up)
tune_distance_size_ngroups_other = tune_distance_size_ngroups %>% filter(docs_type != "Bonds")
tune_distance_size_ngroups_other = tune_distance_size_ngroups_other %>% group_by(city_index, city_name, reporting_year, docs_type) %>% 
  dplyr::summarise_at(vars(number_of_sentences:distance_20_size_20), funs(sum), na.rm = TRUE)

# Put Bond data and ACFR + Budget data back together
# Select only those city-year-documents that have more than 10 sentences:
tune_distance_size_ngroups = as.data.frame(rbind(tune_distance_size_ngroups_bonds, tune_distance_size_ngroups_other)) %>% filter(number_of_sentences > 10)


#-----------------------------------------------------------------
# (2) Create the variables 
#-----------------------------------------------------------------

# Pivot the data into the long format
pivot_groups = tune_distance_size_ngroups %>% group_by(reporting_year) %>%
                  summarise_each(funs(mean), distance_0_size_1:distance_20_size_20) %>% 
                        pivot_longer(cols = distance_0_size_1:distance_20_size_20, names_to = c("distance", "size"), 
                                          names_pattern = "distance_(.*)_size_(.*)", values_to = "scaled_number_of_groups" )

# Create distance and size variables
pivot_groups$size = factor(pivot_groups$size, levels = c(1:20))
pivot_groups$distance = factor(pivot_groups$distance, levels = c(0:20))

# Select group sizes of 2, 3, 6, 10, and 15 & distances of 0, 5, 10, and 15
pivot_groups_plot = pivot_groups %>% filter(size %in% c(2, 3, 6, 10, 15) & distance %in% seq(0, 15, 5) )

# Label groups correspondingly
pivot_groups_plot$distance <- factor(pivot_groups_plot$distance, levels = c("0", "5", "10", "15"),
                                labels = c( "Distance 0",  "Distance 5",  "Distance 10",  "Distance 15") )

# Save the data for publication
write_xlsx( pivot_groups_plot, "../data for figures/supplementary figures/figureSN_6_3.xlsx" ) # Excel


# Plot the graph 
graph = ggplot( pivot_groups_plot , 
                      aes(x = reporting_year, y = scaled_number_of_groups, color = size )) + 
                      geom_point( size=10, pch=20 ) + geom_line( size = 1) + 
                      facet_wrap(~ distance, nrow = 2) + 
                      scale_x_discrete( limits = c(2013:2020),  breaks = seq(2013, 2020, 1)) +
                      scale_color_manual(name="Group size", values=c( '#2A363B','#019875','#E84A5F','#FECEA8','#96281B')) +
                      labs(y = "Number of adaptation groups", x = "") + 
                      ggtitle("") + 
                      theme(plot.title = element_text(hjust = 0.5)) +
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
               ,  'adaptation_group_sensitivity.pdf', sep = '' ), graph,  
       width = 20, height = 15, dpi = 300, units = "in") 
