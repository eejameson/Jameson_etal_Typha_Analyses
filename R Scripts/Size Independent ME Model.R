#### RA Ratio Model ####

# Add the reproductive allocation ratio to the mixed model data
Typha_allocation_model_data <- Typha_allocation_model_data %>% 
  mutate(RA_ratio = Infl_Weight/Veg_Weight)


RA_ratio_ME_model <- lmer(RA_ratio ~ Species + N_Level_Group + site +
                            Species*N_Level_Group + N_Level_Group*site +
                            Species*site + (1 | u_tank),
                          data = Typha_allocation_model_data)

summary(RA_ratio_ME_model)


##### Model Results #####
# Save the model results as a data frame so that it can be exported to a csv
RA_model_results <- as_tibble(coefficients(summary(RA_ratio_ME_model)))

RA_model_results$coefficients <- row.names(coefficients(summary(RA_ratio_ME_model))) 
RA_model_results <- RA_model_results %>% 
  select(coefficients, everything())

RA_model_results

# write_csv(RA_model_results,
#           "Cattail_Reproductive_Allocation_Analyses/Data Summaries and Coefficient Comparisons/RA_model_results.csv")

# Perform anova on the model to see which factors are important
Anova(RA_ratio_ME_model, test.statistic = "F")

RA_anova_results <- as_tibble(Anova(RA_ratio_ME_model, test.statistic = "F"))
RA_anova_results$variable <- rownames(Anova(RA_ratio_ME_model, test.statistic = "F"))
RA_anova_results <- RA_anova_results %>% select(variable, everything())
RA_anova_results

# write_csv(RA_anova_results,
#           "Figures/Graphs and Tables/RA_ANOVA_results.csv")

##### Check Model Assumptions #####

r_lmer_model <- resid(summary(RA_ratio_ME_model), type = "normalized")

# Create plots to examine validity of model assumptions

plot(RA_ratio_ME_model)

hist(r_lmer_model, main="", xlab="Residuals", col="grey") 

{
  qqnorm(r_lmer_model)
  qqline(r_lmer_model)
}

# Our data
{
  NormalityPlot(Typha_allocation_model_data$Infl_Weight)
  title(main = "Inflorescence Mass Observations QQ-Plot")
}

{
  NormalityPlot(r_lmer_model)
  title(main = "Random Effects Group Residuals QQ-Plot")
}

{
  boxplot(r_lmer_model ~ Typha_allocation_model_data$Species, ylab="Residuals", xlab="Taxon", xaxt="n")
  axis(side=1, at=c(1,2,3), labels=c("TYAN", "TYGL", "TYLA"), las=1)
}


{
  boxplot(r_lmer_model ~ Typha_allocation_model_data$N_Level_Group, ylab="Residuals", 
          xlab="Nutrient Level Groups", xaxt = "n")
  axis(side=1, at=c(1,2,3), labels=c("Low", "Med", "High"), las=1)
}

{
  boxplot(r_lmer_model ~ Typha_allocation_model_data$site, ylab="Residuals", 
          xlab="Site", xaxt = "n")
  axis(side=1, at=c(1,2), labels=c("UMBS", "ESGR"), las=1)
}

##### Visualize RA Ratios #####

# We want to visualize the mean RA ratio for each subgroup
RA_summary_data <- Typha_allocation_model_data %>% 
  group_by(site, Species, N_Level_Group) %>% 
  # Find the mean RA ratio
  summarize(avg_RA_ratio = mean(RA_ratio),
            # calculate the sample standard deviations
            sd = sqrt(sum((RA_ratio - avg_RA_ratio)^2)/(n() - 1)),
            # Calculate the standard errors          
            se = sd/sqrt(n()),
            # Find number of observations in each group
            n_samples = n())

{
  levels(RA_summary_data$N_Level_Group)
  levels(RA_summary_data$Species)
  levels(RA_summary_data$site)
  
  RA_summary_data <- RA_summary_data %>% 
    mutate(N_Level_Group = factor(N_Level_Group, levels = rev(levels(N_Level_Group))))
  levels(RA_summary_data$N_Level_Group)
  
  levels(RA_summary_data$N_Level_Group) <- c("High",
                                             "Medium",
                                             "Low")
  levels(RA_summary_data$Species) <- c("T. angustifolia", "T. glauca",
                                       "T. latifolia")
  
  levels(RA_summary_data$site) <- c("North Site", "South Site")
  
  levels(RA_summary_data$N_Level_Group)
  levels(RA_summary_data$Species)
  levels(RA_summary_data$site) 
  
  
  RA_ratio_nl_site <- ggplot(data = RA_summary_data,
                             aes(x = Species, y = avg_RA_ratio, 
                                 fill = Species)) +
    geom_col() +
    geom_errorbar(aes(ymin = avg_RA_ratio - se, ymax = avg_RA_ratio + se),
                  width = 0.2,
                  size = 0.75) +
    # Edit Colors and Legend
    scale_fill_manual("Taxon",
                      labels = c("T. angustifolia", "T. glauca", "T. latifolia"),
                      values = c("tan1", "deepskyblue4", "deeppink3"),
                      guide = guide_legend(label.theme = element_text(face = "italic"))) +
    # Make individual graphs for each taxon
    facet_grid(rows = vars(N_Level_Group), cols = vars(site)) +
    # Add sample size to the figure
    geom_text(aes(label = paste("n =" ,n_samples), y = avg_RA_ratio + se + 0.05)) +
    # Specify title and axes lables
    xlab("Taxon") + 
    ylab("Reproductive Allocation Ratio") + 
    theme_bw() +
    theme(axis.title.x = element_text(size = 12),                # x-axis text size
          axis.title.y = element_text(size = 12),                # y-axis text size
          plot.title = element_text(size = 12, face = "bold"),                  # Title text size
          legend.text = element_text(size = 10),                 # Legend text size
          legend.title = element_text(face = 'bold', size = 10), # Legend title format (bold)
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     face = "italic"))
  
  RA_ratio_nl_site
  }
{
  RA_summary_data <- RA_summary_data %>% 
    mutate(N_Level_Group = factor(N_Level_Group, levels = rev(levels(N_Level_Group))))
  levels(RA_summary_data$N_Level_Group)
  
  RA_ratio_taxa_site <- ggplot(data = RA_summary_data,
                               aes(x = N_Level_Group, y = avg_RA_ratio, 
                                   fill = N_Level_Group)) +
    geom_col() +
    geom_errorbar(aes(ymin = avg_RA_ratio - se, ymax = avg_RA_ratio + se),
                  color = "dimgray",
                  size = 0.75,
                  width = 0.2) +
    # Edit Colors and Legend
    scale_fill_manual(guide = "legend", 
                      values = c("cadetblue1","cyan4", "midnightblue"), # Colors to fill with
                      name = 'Nutrient Level', # Name for legend
                      labels = c('Low', 'Medium', 'High')) +
    # Make individual graphs for each taxon
    facet_grid(rows = vars(Species), cols = vars(site)) +
    # Specify title and axes lables
    xlab("Nutrient Level") + 
    ylab("Reproductive Allocation Ratio") + 
    theme_bw() +
    theme(axis.title.x = element_text(size = 12),                # x-axis text size
          axis.title.y = element_text(size = 12),                # y-axis text size
          plot.title = element_text(size = 12, face = "bold"),                  # Title text size
          legend.text = element_text(size = 10),                 # Legend text size
          legend.title = element_text(face = 'bold', size = 10), # Legend title format (bold)
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          strip.text.y = element_text(face = "italic"),
          strip.background = element_blank())
  
  RA_summary_data <- RA_summary_data %>% 
    mutate(N_Level_Group = factor(N_Level_Group, levels = rev(levels(N_Level_Group))))
  levels(RA_summary_data$N_Level_Group)
  levels(RA_summary_data$N_Level_Group) <- c("high",
                                             "med",
                                             "low")
  levels(RA_summary_data$Species) <- c("TYAN", "TYGL", "TYLA")
  
  levels(RA_summary_data$site) <- c("UMBS", "ESGR")
  
  RA_ratio_taxa_site
}


###### Save Graphs ######
# ggsave("RA Mean Ratio by Site and Taxon Updated BI.png",
#        plot = RA_ratio_taxa_site,
#        device = "png",
#        path = "/Users/emilyjameson/Desktop/Research 2/Figures/Graphs and Tables",
#        width = 165, height = 200, units = "mm")
# 
# ggsave("RA Mean Ratio by Site and NLevel Updated BI2.png",
#        plot = RA_ratio_nl_site,
#        device = "png",
#        path = "/Users/emilyjameson/Desktop/Research 2/Figures/Graphs and Tables",
#        width = 165, height = 200, units = "mm")

##### Plasticity Calculations #####

RA_table_data <- RA_summary_data %>% 
  select(site, Species, avg_RA_ratio, N_Level_Group) %>% 
  group_by(site, Species) %>% 
  mutate(rank = if_else(avg_RA_ratio == min(avg_RA_ratio), "min",
                        if_else(avg_RA_ratio == max(avg_RA_ratio), "max", "mid"))) %>% 
  ungroup() %>% 
  mutate(num_Nlevel = if_else(N_Level_Group == "low", 1,
                              if_else(N_Level_Group == "med", 2,  3))) %>% 
  filter(rank == "max" | rank == "min") 

RA_lowN_table <- RA_table_data %>% 
  group_by(site, Species) %>% 
  filter(num_Nlevel == min(num_Nlevel))

RA_highN_table <- RA_table_data %>% 
  group_by(site, Species) %>% 
  filter(num_Nlevel == max(num_Nlevel)) %>% 
  rename(highN_ratio = avg_RA_ratio,
         high_NLG = N_Level_Group,
         highN_rank = rank,
         high_NL_rank = num_Nlevel)

RA_wide_table <- left_join(RA_lowN_table, RA_highN_table,
                           by = c("site", "Species"))  

RA_wide_table <- RA_wide_table %>% 
  unite("Comparison", c("high_NLG", "N_Level_Group"), sep = " - ",
        remove = FALSE) %>% 
  mutate(abs_diff = highN_ratio - avg_RA_ratio,
         per_diff = abs_diff/avg_RA_ratio*100) 

RA_wide_table

# Format data for exporting
RA_table_data_export <- RA_wide_table %>% 
  select(site, Species, Comparison, abs_diff, per_diff) %>% 
  mutate(Species = if_else(Species == "TYAN", "T. angustifolia",
                           if_else(Species == "TYGL", "T. glauca",
                                   "T. latifolia")),
         abs_diff = round(abs_diff, 2),
         per_diff = round(per_diff, 2)) %>% 
  select(site, Taxon = Species, Comparison,
         `Overall Difference in AR` = abs_diff,
         `Percent Change in AR` = per_diff) 

# write_csv(RA_table_data_export,
#           "Cattail_Reproductive_Allocation_Analyses/Data Summaries and Coefficient Comparisons/AR_plasticity_table2b.csv")
