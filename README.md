# Jameson_etal_Typha_Analyses

This repository contains the code and data necessary to run all analyses and generate all figures found in the accompanying manuscript: *Size-dependent analyses provide insights into the reproductive allocation and plasticity of invasive and native* Typha by Emily E. Jameson, Kenneth Elgersma, Jason P. Martina, William S. Currie, and Deborah E. Goldberg

## Abstract

Invasive species increasingly threaten ecosystems worldwide making it important to better understand the traits, including sexual reproductive allocation and its plasticity, that make certain species more successful invaders than others. Size differences between native and non-native invasive congeners are common, yet, when comparing allocation within and among species many studies fail to consider the size-dependency (allometry) of allocation patterns. Using a mesocosm experiment conducted at two locations and incorporating a nutrient gradient, we compared sexual reproductive allocation and its plasticity (correcting for size) between three closely related taxa of cattails with varying degrees of invasiveness: *Typha latifolia* (native, non-invasive), *Typha angustifolia* (non-native, invasive), *Typha x glauca* (their hybrid, invasive). We found that the non-native and hybrid taxa (both invasive) allocated more to sexual reproduction than the native, non-invasive taxon even after correcting for aboveground plant size. However, the non-native and hybrid taxa did not differ from each other when accounting for plant size, even though a size-independent analysis indicated they did. This reveals these two taxa differed in plant size, not allocation patterns. Surprisingly, the most invasive taxon (the hybrid) was the least plastic in sexual reproductive allocation in response to nutrients at one site. Our study adds to the growing body of literature suggesting trait values rather than trait plasticity contribute to invasiveness, but ours is unique in its size-dependent analysis of sexual reproductive allocation, its plasticity, and differential taxon invasiveness.

## Files

The Analyses_Master.R is the main R script from which you can run all analyses and generate all figures. It calls on other R scripts, which are contained in the R Scripts folder to perform each statistical analysis separately. The main results and figures are contained in the Analyses_Master.R script while the supporting code is in the sub-scripts.

## Folders
Data - Contains the published data necessary for these analyses

Figures - Contains all figures generated from the code

R Scripts - Contains the following R scripts
  
  - Continuous NL Model.R
    - Model with nutrient level as a continuous variable rather than a factor/categorical variable
  - Functions.R
    - All user-defined functions used within the the code
  - Ind Subgroup Models.R
    -  Models for each taxon x site x nutrient level subgroup and the corresponding AIC analyisis
  - Log Reg Model.R
    - The logistic regression model to examine the minimum size of reproduction and corresponding model diagnostics 
  - Size Dependent ME Model.R
    - The size-dependent mixed effects model to examine sexual reproductive allocation and its plasticity
  - Size Independent ME Model.R
    - The size-independent mixed effects model using allocation ratio which does not account for size
  - Typha Data Formating.R
    - Data manipulation which formats data for analyses and figures

Tables - Contains all data tables and csv files generated from the code that were used in the manuscript

## R Version                         
R version 4.1.3 (2022-03-10)

nickname: One Push-Up 


