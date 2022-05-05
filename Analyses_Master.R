
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

#### Source Data Manipulation File ####

source("R_Scripts/Typha Data Formating.R")

#### Individual Sub-group Models ####

source("R Scripts/Ind Subgroup Models.R")


Sub_Mod_AIC_Results

# save the data frame as a csv (optional)
# write_csv(Sub_Mod_AIC_Results,
#           "Tables/Sub-Model Comparison AIC Results Diffs.csv")


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
# ggsave("ME Predictions by Site and Taxon Updated BI.png",
#        plot = size_dep_site_taxon,
#        device = "png",
#        path = "/Users/emilyjameson/Desktop/Research 2/Figures/Graphs and Tables",
#        width = 165, height = 200, units = "mm")
# 
# ggsave("ME Predictions by Site and NLevel Updated BI.png",
#        plot = size_dep_site_NL,
#        device = "png",
#        path = "/Users/emilyjameson/Desktop/Research 2/Figures/Graphs and Tables",
#        width = 165, height = 200, units = "mm")

#### Size Independent Model ####