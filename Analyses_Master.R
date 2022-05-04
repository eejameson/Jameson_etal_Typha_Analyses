
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

source("Sub_Scripts/Typha Data Final Formating.R")
