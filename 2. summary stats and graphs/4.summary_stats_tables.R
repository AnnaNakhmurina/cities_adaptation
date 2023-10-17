
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
cc_data_bonds = read.fst( "../data/cleaned_data/cc_data3.fst" )

#-----------------------------------------------------------------
# (2) Create summary stats tables
#-----------------------------------------------------------------

summary_data = cc_data

# Text-based variables: rename so that they can be processed through the code ("_" is not digested well by separate function below)
summary_data$all.adaptation = summary_data$measure_adapt
summary_data$all.adaptation.hard = summary_data$measure_sub_hardprotect
summary_data$all.adaptation.soft = summary_data$measure_sub_softprotect
summary_data$budget.adaptation = summary_data$budget_measure_adapt
summary_data$ACFR.adaptation = summary_data$ACFR_measure_adapt
summary_data$bond.adaptation = summary_data$bond_measure_adapt
summary_data$total.sentences = summary_data$total_number_of_sentences

# Other variables: rename so that they can be processed through the code ("_" is not digested well by separate function below)
summary_data$continuous.flood.risk = summary_data$fspropertiesatrisk2020pct
summary_data$election.term = summary_data$election_term
summary_data$CB.reported.outlook.years =  summary_data$CB_reported_outlook_years 
summary_data$scaled.debt.outstanding = summary_data$scaled_outstanding
summary_data$scaled.revenue = summary_data$scaled_revenue
summary_data$scaled.expense = summary_data$scaled_expense
summary_data$unrestricted.fund.balance.over.total.expense = summary_data$unrestricted_fund_balance_over_total_expense
summary_data$localofficials = summary_data$localofficials
summary_data$fund.scaled.expense = summary_data$fund_scaled_expense

summary_data$high.localofficials = summary_data$high_localofficials
summary_data$large.state.grant = summary_data$state_grant_above_median 
summary_data$PerHouseholdIncomeAvg = as.numeric(summary_data$PerHouseholdIncomeAvg)
summary_data$high.hh.income = summary_data$high_per_household_income

# Compose summary statistics for the main variables
distributions = summary_data %>%
  dplyr::select(  all.adaptation, all.adaptation.hard, all.adaptation.soft, 
                  budget.adaptation, 
                  ACFR.adaptation,
                  bond.adaptation,
                  total.sentences, 
                  CB.reported.outlook.years, Population, fspropertiesatrisk2020pct, 
                  scaled.debt.outstanding,
                  unrestricted.fund.balance.over.total.expense,
                  fund.scaled.expense,
                  crsclasscode, large.state.grant, high.localofficials, high.hh.income
                  ) %>% # select variables to summarise
  summarise_all( funs(min.d = min( ., na.rm = T ),
                     mean.d = mean(., na.rm = T), 
                     sd.d = sd(., na.rm = T), 
                     q25.d = quantile(., 0.25, na.rm = T), 
                     median.d = median(., na.rm = T), 
                     q75.d = quantile(., 0.75, na.rm = T),
                     n = sum(!is.na(.) )
  )  )

tidy_distributions <- distributions %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(var, n, mean.d, sd.d,
                q25.d, median.d, q75.d
  ) # reorder columns

tidy_distributions

# Compose summary statistics for the bond variables
summary_data_bonds = cc_data_bonds
summary_data_bonds$all.adaptation.bond = summary_data_bonds$measure_adapt

distributions_bonds = summary_data_bonds %>%
  dplyr::select(  all.adaptation.bond,
                  localofficials # include one more variable just to make the code work (will exclude it later on)
  ) %>% # select variables to summarise
  summarise_all( funs(min.d = min( ., na.rm = T ),
                      mean.d = mean(., na.rm = T), 
                      sd.d = sd(., na.rm = T), 
                      q25.d = quantile(., 0.25, na.rm = T), 
                      median.d = median(., na.rm = T), 
                      q75.d = quantile(., 0.75, na.rm = T),
                      n = sum(!is.na(.) )
  )  )


tidy_distributions_bonds <- distributions_bonds %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(var, n, mean.d, sd.d, # reorder columns
                q25.d, median.d, q75.d
  )  %>% filter( var != "localofficials" ) # exclude the above variable

# Add both summary stats together
tidy_distributions = rbind( tidy_distributions, tidy_distributions_bonds)

# Reorder the resulting table: 
order <- c( "all.adaptation", "all.adaptation.hard", "all.adaptation.soft",
            "budget.adaptation",
            "ACFR.adaptation", 
            "all.adaptation.bond",
            "bond.adaptation",
            "total.sentences", 
            "CB.reported.outlook.years", "Population", "fspropertiesatrisk2020pct", "scaled.debt.outstanding",
            "unrestricted.fund.balance.over.total.expense",
            "fund.scaled.expense",
            "crsclasscode", "large.state.grant", "high.localofficials", "high.hh.income")
tidy_distributions = tidy_distributions %>% arrange(factor(var, levels = order))

# Round summary stats to the second digit
summary = tidy_distributions %>% mutate_if( is.numeric, round, digits=2 )
# Name the variables
names_variable_list = c( "Main Adaptation", "Hard Adaptation", "Soft Adaptation",
                         "Budget Adaptation",
                         "ACFR Adaptation", 
                         "Main+Bonds Adaptation",
                         "Bonds Adaptation", 
                         "Total Sentences",
                         "Capital Budget Outlook","Population", "Flood Risk",
                         "Total Debt per Capita",
                         "UFB/Total Expense", "Fund Expense per Capita",
                         "CRS Class Code", "Large Grant", "High Local Officials",
                         "High H/H Income"
                         )

summary$var = names_variable_list

# Add comma for the thouthands:
cust_format <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 0)}
summary <- summary %>% mutate_at(vars('n'), cust_format)

cust_format2 <- function(x, digits){formatC(x, format="f", big.mark=",", digits = 2)}
summary <- summary %>% mutate_at(vars( "mean.d",      "sd.d",    "q25.d",  "median.d",     "q75.d"), cust_format2)

# Convert all variables into characters so they are printed out better: 
summary$n = as.character(summary$n)
summary$mean.d = as.character(summary$mean.d)
summary$sd.d = as.character(summary$sd.d)
summary$q25.d = as.character(summary$q25.d)
summary$median.d = as.character(summary$median.d)
summary$q75.d = as.character(summary$q75.d)

# Save summary stats table:
summary_tex <- xtable(summary, include.rownames=TRUE, digits=2)
print(summary_tex,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      sanitize.text.function=function(x){x},
      file= "../results/tables/cc_summary_stats.tex" )

# Summary statistics of categorical variables
party_affiliation =  c("N" = nrow(cc_data), table(cc_data %>% 
                                                    filter(party == "Democratic" | party == "Republican" | party == "Other" ) %>% 
                                                      dplyr::select(party))) 
party_affiliation = summary_data %>% dplyr::count( party ) 

n = tibble( "N", sum( party_affiliation$n ) )
names(n) = names(party_affiliation)

party_affiliation = rbind( party_affiliation, n ) 
party_affiliation <- party_affiliation %>% mutate_at(vars('n'), cust_format)

party_affiliation = party_affiliation %>%  column_to_rownames("party") %>%  t() %>%   as.data.frame() 

party_affiliation = party_affiliation[, c("N", "Democratic", "Republican", "Other")]

# Save summary stats as a latex file
summary_party_tex <- xtable((party_affiliation), include.rownames=TRUE, digits=0)
print(summary_party_tex,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      sanitize.text.function=function(x){x},
      file= "../results/tables//party_affiliation_summary_stats.tex" )


#-----------------------------------------------------------------
# (2) Create correlation table 
#-----------------------------------------------------------------
# Add a new column Republican (Republican = 1 if the party affiliation of the mayor is Republican, otherwise 0)
correlation = corrr::correlate(summary_data %>% mutate(Republican = ifelse(party == "Republican", 1, 0)) %>% 
                                    dplyr::select(
                                      all.adaptation, all.adaptation.hard, all.adaptation.soft, 
                                      budget.adaptation, 
                                      ACFR.adaptation,
                                      bond.adaptation,
                                      fspropertiesatrisk2020pct,
                                      CB.reported.outlook.years, Population, scaled.debt.outstanding,          
                                      unrestricted.fund.balance.over.total.expense,
                                      fund.scaled.expense,
                                      crsclasscode, large.state.grant, high.localofficials,
                                      high.hh.income
                                                  ), 
                                                    method = "pearson",  diagonal = 1.00) %>%
                                                        shave()
correlation

correlation[,2:16] = round(correlation[,2:16], 2)
correlation$term = c("{[1]}  Main", "{[2]}  Hard", "{[3]}  Soft",
                     "{[4]}  Budget",
                     "{[5]}  ACFR", 
                     "{[6]}  Main+Bonds", 
                     "{[7]}  Flood Risk",
                     "{[8]}  Capital Budget Outlook","{[9]} Population", "{[10]}  Total Debt per Capita",
                     "{[11]}  UFB/Total Expense", "{[12]}  Fund Expense per Capita",
                     "{[13]}  CRS Class Code", "{[14]}  Large Grant", "{[15]}  High Local Officials",
                     "{[16]} High H/H Income")

correlation = as.data.table( correlation )

correlation_tex <- xtable(correlation, include.rownames=TRUE, digits=2)

print(correlation_tex,
      only.contents=TRUE,
      include.rownames=FALSE,
      include.colnames = FALSE,
      type="latex",
      floating = FALSE,
      hline.after = NULL, comment = FALSE,
      sanitize.text.function=function(x){x},
      file="../results/tables/correlation.tex" )

