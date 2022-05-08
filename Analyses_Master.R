
# Analyses Master
# 
# This file contains all the calls to the necessary sub-scripts to run analyses
# for the project examining sexual reproductive 
# allocation in native and non-native cattails (Typha). This script contains 
# the genetic identifications, model selection justification, 
# mixed effects models, logistic regression analysis, and a mixed effects 
# ANOVA comparison of our method to standard allocation ratios.
# 
# AUTHOR
# Emily Jameson
# 
# DATE CREATED
# 05/04/2022

#### Start of Code ####

# All code should run smoothly from the base directory provided in this repository

##### Load Libraries ####

library(ggplot2)
# load tidyverse packages
library(tidyverse)
library(nlme)
library(lmerTest)
library(car)
library(broom)

#### Helper Functions ####

source("R Scripts/Functions.R")

#### Data Manipulation ####

source("R Scripts/Typha Data Formating.R")

#### Individual Sub-group Models ####

source("R Scripts/Ind Subgroup Models.R")


Sub_Mod_AIC_Results

# save the data frame as a csv (optional)
# write_csv(Sub_Mod_AIC_Results,
#           "Tables/Sub-Model Comparison AIC Results Diffs.csv")


#### Size Independent Model ####

source("R Scripts/Size Independent ME Model.R")

##### Model Results #####

summary(RA_ratio_ME_model)

RA_model_results

# write_csv(RA_model_results,
#           "Tables/RA_model_results.csv")

RA_anova_results

# write_csv(RA_anova_results,
#           "Tables/RA_ANOVA_results.csv")

##### Plasticity Estimates #####

RA_wide_table

# write_csv(RA_table_data_export,
#           "Tables/AR_plasticity_table2b.csv")


##### Figures ######

# Figure 1
RA_ratio_nl_site

# Supplemental Figure 1
RA_ratio_taxa_site

###### Save Graphs ######
# ggsave("RA Mean Ratio by Site and Taxon Updated BI.png",
#        plot = RA_ratio_taxa_site,
#        device = "png",
#        path = "/Figures",
#        width = 165, height = 200, units = "mm")
# 
# ggsave("RA Mean Ratio by Site and NLevel Updated BI2.png",
#        plot = RA_ratio_nl_site,
#        device = "png",
#        path = "/Figures",
#        width = 165, height = 200, units = "mm")

#### Size Dependent Model ####

source("R Scripts/Size Dependent ME Model.R")


##### Model Results #####
# These are the raw non-linear (size-dependent) mixed effects model results
summary(full_two_way_model)

model_results

# save the results as a csv (optional)
# write_csv(model_results,
#           "Tables/SizeDep_model_results.csv")

# Perform anova on the model to see which factors are important
anova.lme(full_two_way_model)

anova_results

# write_csv(anova_results,
#           "Figures/Graphs and Tables/Final_ANOVA_results.csv")

##### Model Assumptions ##### 

# See the sourced code for mode assumption checks

##### Plasticity Estimates #####

plasticity_table

# write_csv(plasticity_table_export,
#           "Tables/SizeDep_plasticity_table.csv")


##### Figures #####

# Figure 2
size_dep_site_NL

# Supplemental Figure 2
size_dep_site_taxon

###### Save Graphs ######
# ggsave("ME Predictions by Site and Taxon BI.png",
#        plot = size_dep_site_taxon,
#        device = "png",
#        path = "/Figures",
#        width = 165, height = 200, units = "mm")
# 
# ggsave("ME Predictions by Site and NLevel BI.png",
#        plot = size_dep_site_NL,
#        device = "png",
#        path = "/Figures",
#        width = 165, height = 200, units = "mm")

#### Continuous NL Model ####

source("R Scripts/Continuous NL Model.R")

# Compare the models
AIC(full_two_way_model, full_cont_mixed_model)

# Find the difference between the two models 
# model 2 - model 1 so the difference is positive
AIC(full_two_way_model, full_cont_mixed_model)$AIC[2] - AIC(full_two_way_model, full_cont_mixed_model)$AIC[1]

#### Logistic Regression Model ####

source("R Scripts/Log Reg Model.R")

##### Model Results ####

log_model_results

# write_csv(log_model_results,
#           "Tables/log_model_results.csv")

Log_T.NLG.VW_anova_results

# write_csv(Log_T.NLG.VW_anova_results,
#           "Figures/Graphs and Tables/Log_ANOVA_results.csv")

##### Compare Models AIC #####

Log_AIC_results

# write_csv(Log_AIC_results,
#           "Tables/Log AIC Results.csv")

##### Figure #####

Logistic_graph_by_Sp

# ggsave("Log Reg Model by Taxon Updated BI.png",
#        plot = Logistic_graph_by_Sp,
#        device = "png",
#        path = "/Figures",
#        width = 165, height = 200, units = "mm")

#### Additional Calculations ####

# Find the percent of observations that came from the main nutrient levels
Typha_log_graph_data_edited %>% 
  group_by(as.factor(N_level)) %>% 
  summarise(n_obs = n())

# find total observations
nrow(Typha_log_graph_data_edited)

# Percent from three intensive n-levels
(108 + 173 + 240)/860

# Find maximum number of samples for a given taxon in a given tank
Typha_log_graph_data_edited %>% 
  group_by(site, tank, Species) %>% 
  summarise(n_obs = n()) %>% 
  arrange(-n_obs)

# Find total numbers for the tank 
Typha_log_graph_data_edited %>% 
  group_by(site, tank) %>% 
  summarise(n_obs = n()) %>% 
  arrange(-n_obs)
