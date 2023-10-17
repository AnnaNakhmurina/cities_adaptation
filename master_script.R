
#-----------------------------------------------
# Create the textual data and clean it
#-----------------------------------------------
# These codes need to run separately in Python and Stata

# This code processes the textual data and creates the raw textual measures
# 1.keywords_extractor_codes.py

# This code cleans the raw textual measures and computes dictionary frequencies
# 2.clean_textual_measures.do

#-----------------------------------------------
# Combine the dataset and create variables
#-----------------------------------------------

# 1. Clean data 
source('1. combine cleaned data/1.clean_merge_data.R')
rm(list = ls())
gc()

# 2. Create variables
source('1. combine cleaned data/2.create_variables.R')
rm(list = ls())
gc()

#-----------------------------------------------
# Plot the graphs and create summary statistics
#-----------------------------------------------

# 3. Plot a pie chart (Figure 1 Panel A)
# First run the Stata code: 2. summary stats and graphs/1.create_figure_1.do
source('2. summary stats and graphs/1.pie_chart_most_common_keywords.R')
rm(list = ls())
gc()

# 4. Plot topics over time (Figure 1 Panels B, C, D)
# First, run the topic analyses in Python
# 2. summary stats and graphs/1.Extract_Topic_Modeling_with_Adapt_Index_Data.ipynb
# Then, run the below file:
source('2. summary stats and graphs/1.topic_graphs.R')
rm(list = ls())
gc()

# 5. Plot coefficients over time (Figure 2 Panels B, C)
source('2. summary stats and graphs/2.flood_risk_adaptation_over_time.R')
rm(list = ls())
gc()

# 6. Plot topics over time (Figure 3)
source('2. summary stats and graphs/3.gap_over_time.R')
rm(list = ls())
gc()

# 7. Create summary statistics
source('2. summary stats and graphs/4.summary_stats_tables.R')
rm(list = ls())
gc()


####### Create graphs for Appendix F
source('2. summary stats and graphs/5.keywords_groups_graphs.R')
rm(list = ls())
gc()

source('2. summary stats and graphs/5.adaptation_sensitivity_graphs.R')
rm(list = ls())
gc()

#########. Plot topics over time (Supplementary Figure 1)
source('2. summary stats and graphs/6.adaptation_by_state.R')
rm(list = ls())
gc()

#########. Plot topics over time (Supplementary Figure 2)
source('2. summary stats and graphs/7.adaptation_by_flood_risk_time.R')
rm(list = ls())
gc()


#-----------------------------------------------
# Run main results
#-----------------------------------------------

source('3. main results/1.flood_risk_regressions.R')
rm(list = ls())
gc()

source('3. main results/2.main_results.R')
rm(list = ls())
gc()


#-----------------------------------------------
# Validation and placebo tests
#-----------------------------------------------

# Validation regressions

source('4. validation tests/1.validation_fund_expense.R')
rm(list = ls())
gc()

source('4. validation tests/2.validation_insurance.R')
rm(list = ls())
gc()

source('4. validation tests/3.market_tests.R')
rm(list = ls())
gc()

# Placebo regressions

source('4. validation tests/1.placebo_fund_expense.R')
rm(list = ls())
gc()

source('4. validation tests/2.placebo_insurance.R')
rm(list = ls())
gc()

source('4. validation tests/3.placebo_market_tests.R')
rm(list = ls())
gc()

#-----------------------------------------------
# Robustness tests
#-----------------------------------------------

# Robustness to scaling by total number of sentences (instead of controlling for this variable)

source('5. robustness tests/1.flood_risk_regressions_scaled.R')
rm(list = ls())
gc()

source('5. robustness tests/2.main_results_scaled.R')
rm(list = ls())
gc()

# Robustness to two way clustering

source('5. robustness tests/1.flood_risk_regressions_two_way.R')
rm(list = ls())
gc()

source('5. robustness tests/2.main_results_two_way.R')
rm(list = ls())
gc()

