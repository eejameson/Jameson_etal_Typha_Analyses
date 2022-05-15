
#### Mixed Effects Model ####

# In order for the model to converge I needed to build from the estimates of smaller
# models first and gradually add in more variables once we had some initial 
# starting estimates
taxon_model <- nlme(model = Infl_Weight ~ A*(Veg_Weight)^(alpha),
                    fixed = list(alpha ~ Taxon,
                                 A ~ 1),
                    random = alpha ~ 1 | u_tank,
                    data = Typha_allocation_model_data,
                    start = list( 
                      fixed = c(A = 0.2, 0,0,
                                alpha = 0.5)))
taxon_model
summary(taxon_model)

# Now try adding in nutrient level (without intercept)
tax_nlevel_model <- nlme(model = Infl_Weight ~ A*(Veg_Weight)^(alpha),
                         fixed = list(alpha ~ Taxon + N_Level_Group,
                                      A ~ 1),
                         random = alpha ~ 1 | u_tank,
                         data = Typha_allocation_model_data,
                         start = list( 
                           fixed = c(A = 0.2, 0,0,
                                     0, 0, 
                                     alpha = 0.5)))

tax_nlevel_model
summary(tax_nlevel_model)

# Now try adding in the interaction
# The start values are based on the previous model's estimates

base_start <- fixef(tax_nlevel_model)[[1]]
TYGL_start <- fixef(tax_nlevel_model)[[2]]
TYLA_start <- fixef(tax_nlevel_model)[[3]]
med_start <- fixef(tax_nlevel_model)[[4]]
high_start <- fixef(tax_nlevel_model)[[5]]
A_start <- fixef(tax_nlevel_model)[[6]]

# Add in interaction
tax_nlevel_model_int <- nlme(model = Infl_Weight ~ A*(Veg_Weight)^(alpha),
                             fixed = list(alpha ~ Taxon*N_Level_Group,
                                          A ~ 1),
                             random = alpha ~ 1 | u_tank,
                             data = Typha_allocation_model_data,
                             start = list( 
                               fixed = c(base_start, TYGL_start, TYLA_start,
                                         med_start, high_start, 
                                         0, 0, 0, 0,
                                         A_start)))
summary(tax_nlevel_model_int)

#### Final ME Model ####

# grab starting values
TYGL_med_start <- fixef(tax_nlevel_model_int)[[6]]
TYLA_med_start <- fixef(tax_nlevel_model_int)[[7]]
TYGL_high_start <- fixef(tax_nlevel_model_int)[[8]]
TYLA_high_start <- fixef(tax_nlevel_model_int)[[9]]

# Add in site without interaction three-way interaction
full_two_way_model <- nlme(model = Infl_Weight ~ A*(Veg_Weight)^(alpha),
                           fixed = list(alpha ~ Taxon + N_Level_Group + site +
                                          Taxon*N_Level_Group + N_Level_Group*site +
                                          Taxon*site,
                                        A ~ 1),
                           # the random effect has to be defined as below or else
                           # the model results in a syntax error. Seems like you 
                           # could just do ~ 1, but you cannot do ~ u_tank. We
                           # don't want just ~ 1 because that doesn't indicate
                           # we want things to be grouped by tank.
                           random = alpha ~ 1 | u_tank,
                           data = Typha_allocation_model_data,
                           start = list( 
                             fixed = c(base_start, 
                                       TYGL_start, 
                                       TYLA_start,
                                       med_start, 
                                       high_start, 
                                       0,
                                       TYGL_med_start, 
                                       TYLA_med_start, 
                                       TYGL_high_start, 
                                       TYLA_high_start,
                                       0, 0, 0, 0,
                                       A_start)))
summary(full_two_way_model)

TYGL_ESGR_start <- fixef(full_two_way_model)[[13]]
TYLA_ESGR_start <- fixef(full_two_way_model)[[14]]

# Add in three-way interaction
# This code is commented out because it does not converge and produces an error.
# To facilitate ease of running the analyses I have left it out.
# full_mixed_model <- nlme(model = Infl_Weight ~ A*(Veg_Weight)^(alpha),
#                          fixed = list(alpha ~ Taxon*N_Level_Group*site,
#                                       A ~ 1),
#                          # the random effect has to be defined as below or else
#                          # the model results in a syntax error. Seems like you 
#                          # could just do ~ 1, but you cannot do ~ u_tank. We
#                          # don't want just ~ 1 because that doesn't indicate
#                          # we want things to be grouped by tank.
#                          random = alpha ~ 1 | u_tank,
#                          data = Typha_allocation_model_data,
#                          start = list( 
#                            fixed = c(base_start, 
#                                      TYGL_start, 
#                                      TYLA_start,
#                                      med_start, 
#                                      high_start,
#                                      0,
#                                      TYGL_med_start, 
#                                      TYLA_med_start, 
#                                      TYGL_high_start, 
#                                      TYLA_high_start,
#                                      0,
#                                      0,
#                                      TYGL_ESGR_start,
#                                      TYLA_ESGR_start,
#                                      0, 0, 0, 0,
#                                      A_start)))
# 
# summary(full_mixed_model)
# This model doesn't converge. Likely because there are no observations in TYLA
# at low nutrient level at ESGR.


##### Model Results ####

summary(full_two_way_model)

# this is overall variance
full_two_way_model$sigma

# Percent variance explained by mesocosm/tank
((0.0387)^2/(full_two_way_model$sigma)^2 + (0.0387)^2) * 100

# Save the model results as a data frame so that it can be exported to a csv
model_results <- as_tibble(coefficients(summary(full_two_way_model)))
class(model_results)
model_results$coefficients <- row.names(coefficients(summary(full_two_way_model))) 
model_results <- model_results %>% 
  select(coefficients, everything())


# Perform anova on the model to see which factors are important
anova.lme(full_two_way_model)

anova_results <- as_tibble(anova.lme(full_two_way_model))
anova_results$variable <- rownames(anova.lme(full_two_way_model))
anova_results <- anova_results %>% select(variable, everything())

##### Check Model Assumptions #####

# Capture the residuals
r_full_mod <- resid(full_two_way_model, type = "normalized")

# Create plots to examine validity of model assumptions

std_resids_plot <-  plot(full_two_way_model)
std_resids_plot


resids_hist <- hist(r_full_mod, main="", xlab="Residuals", col="grey") 
resids_hist

resisd_norm_qq_plot <- {qqnorm(r_full_mod)
  qqline(r_full_mod)}


# Our data
{
  NormalityPlot(Typha_allocation_model_data$Infl_Weight)
  title(main = "Size-Dependent Inflorescence Mass QQ-Plot")
} 

{
  NormalityPlot(r_full_mod)
  title(main = "Size-Dependent Rand Effects Grp Resids QQ-Plot")
}


{
  boxplot(r_full_mod ~ Typha_allocation_model_data$Taxon, ylab="Residuals", xlab="Taxon", xaxt="n")
  axis(side=1, at=c(1,2,3), labels=c("TYAN", "TYGL", "TYLA"), las=1)
}


{
  boxplot(r_full_mod ~ Typha_allocation_model_data$N_Level_Group, ylab="Residuals", 
          xlab="Nutrient Level Groups", xaxt = "n")
  axis(side=1, at=c(1,2,3), labels=c("Low", "Med", "High"), las=1)
}


{
  boxplot(r_full_mod ~ Typha_allocation_model_data$site, ylab="Residuals", 
          xlab="Site", xaxt = "n")
  axis(side=1, at=c(1,2), labels=c("UMBS", "ESGR"), las=1)
}


##### Plasticity Calculations #####

# Calculate the overall coefficients for the three taxa at each site in the low
# and high nutrient groups to determine overall allocation

# UMBS Results

# Coefficients
TYAN_low_UMBS <- model_results[["Value"]][[1]]
TYAN_med_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[4]]
TYAN_high_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[5]]

# Save values as named vector
TYAN_UMBS_coeffs <- c(TYAN_low_UMBS = TYAN_low_UMBS, 
                      TYAN_med_UMBS = TYAN_med_UMBS, 
                      TYAN_high_UMBS = TYAN_high_UMBS)

# Coefficients
TYGL_low_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[2]]
TYGL_med_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[2]] +
  model_results[["Value"]][[4]] + model_results[["Value"]][[7]]
TYGL_high_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[2]] +
  model_results[["Value"]][[5]] + model_results[["Value"]][[9]]

# Save values as named vector
TYGL_UMBS_coeffs <- c(TYGL_low_UMBS = TYGL_low_UMBS,
                      TYGL_med_UMBS = TYGL_med_UMBS,
                      TYGL_high_UMBS = TYGL_high_UMBS)

# Coefficients
TYLA_low_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[3]]
TYLA_med_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[3]] +
  model_results[["Value"]][[4]] + model_results[["Value"]][[8]]
TYLA_high_UMBS <- model_results[["Value"]][[1]] + model_results[["Value"]][[3]] +
  model_results[["Value"]][[5]] + model_results[["Value"]][[10]]

# Save the values as a named vector
TYLA_UMBS_coeffs <- c(TYLA_low_UMBS = TYLA_low_UMBS,
                      TYLA_med_UMBS = TYLA_med_UMBS,
                      TYLA_high_UMBS = TYLA_high_UMBS)

# ESGR Results

# Coefficients
TYAN_low_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[6]]
TYAN_med_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[4]] +
  model_results[["Value"]][[6]] + model_results[["Value"]][[11]]
TYAN_high_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[5]] +
  model_results[["Value"]][[6]] + model_results[["Value"]][[12]]

# Save values as named vector
TYAN_ESGR_coeffs <- c(TYAN_low_ESGR = TYAN_low_ESGR,
                      TYAN_med_ESGR = TYAN_med_ESGR,
                      TYAN_high_ESGR = TYAN_high_ESGR)

# Coefficients
TYGL_low_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[2]] +
  model_results[["Value"]][[6]] + model_results[["Value"]][[13]]
TYGL_med_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[2]] +
  model_results[["Value"]][[4]] + model_results[["Value"]][[6]] +
  model_results[["Value"]][[7]] + model_results[["Value"]][[11]] +
  model_results[["Value"]][[13]]
TYGL_high_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[2]] +
  model_results[["Value"]][[5]] + model_results[["Value"]][[6]] +
  model_results[["Value"]][[9]] + model_results[["Value"]][[12]] +
  model_results[["Value"]][[13]]

# Save values as named vector
TYGL_ESGR_coeffs <- c(TYGL_low_ESGR = TYGL_low_ESGR,
                      TYGL_med_ESGR = TYGL_med_ESGR,
                      TYGL_high_ESGR = TYGL_high_ESGR)

# Coefficients
TYLA_med_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[3]] +
  model_results[["Value"]][[4]] + model_results[["Value"]][[6]] +
  model_results[["Value"]][[8]] + model_results[["Value"]][[11]] +
  model_results[["Value"]][[14]]
TYLA_high_ESGR <- model_results[["Value"]][[1]] + model_results[["Value"]][[3]] +
  model_results[["Value"]][[5]] + model_results[["Value"]][[6]] +
  model_results[["Value"]][[10]] + model_results[["Value"]][[12]] +
  model_results[["Value"]][[14]]

# Save values as named vector
TYLA_ESGR_coeffs <- c(TYLA_low_ESGR = NA,
                      TYLA_med_ESGR = TYLA_med_ESGR,
                      TYLA_high_ESGR = TYLA_high_ESGR)

# Combine Values into single table
coeff_table <- as_tibble(c(TYAN_UMBS_coeffs, TYGL_UMBS_coeffs, TYLA_UMBS_coeffs,
                           TYAN_ESGR_coeffs, TYGL_ESGR_coeffs, TYLA_ESGR_coeffs))
coeff_table$sp_nl_site <- names(c(TYAN_UMBS_coeffs, TYGL_UMBS_coeffs, TYLA_UMBS_coeffs,
                                  TYAN_ESGR_coeffs, TYGL_ESGR_coeffs, TYLA_ESGR_coeffs))
coeff_table <- coeff_table %>% 
  separate(col = sp_nl_site, into = c("Taxon", "N_Level_Group", "site"),
           sep = "_") %>% 
  group_by(site, Taxon) %>% 
  mutate(rank = if_else(value == min(value, na.rm = TRUE), "min",
                        if_else(value == max(value, na.rm = TRUE), 
                                "max", "mid"))) %>% 
  ungroup() %>% 
  mutate(num_Nlevel = if_else(N_Level_Group == "low", 1,
                              if_else(N_Level_Group == "med", 2,  3))) %>% 
  filter(rank == "max" | rank == "min") 

coeff_lowN_table <- coeff_table %>% 
  group_by(site, Taxon) %>% 
  filter(num_Nlevel == min(num_Nlevel))

coeff_highN_table <- coeff_table %>% 
  group_by(site, Taxon) %>% 
  filter(num_Nlevel == max(num_Nlevel)) %>% 
  rename(highN_val = value,
         high_NLG = N_Level_Group,
         highN_rank = rank,
         high_NL_rank = num_Nlevel)

plasticity_table <- left_join(coeff_lowN_table, coeff_highN_table,
                              by = c("site", "Taxon"))  

plasticity_table <- plasticity_table %>% 
  unite("Comparison", c("high_NLG", "N_Level_Group"), sep = " - ",
        remove = FALSE) %>% 
  mutate(abs_diff = highN_val - value,
         per_diff = abs_diff/value*100) 

plasticity_table

plasticity_table_export <- plasticity_table %>% 
  select(site, Taxon, Comparison, abs_diff, per_diff) %>% 
  mutate(Taxon = if_else(Taxon == "TYAN", "T. angustifolia",
                           if_else(Taxon == "TYGL", "T. glauca",
                                   "T. latifolia")),
         abs_diff = round(abs_diff, 2),
         per_diff = round(per_diff, 2)) %>% 
  select(site, Taxon = Taxon, Comparison,
         `Overall Difference` = abs_diff,
         `Percent Change` = per_diff) 




##### Visualize ME Model #####

# Create a different dataframe to use for graphing
Typha_allocation_graph_data <- Typha_allocation_model_data

# Need to create simulated data for the predict function
# Create test data set for visualizing model
sim_veg_mass <- 1:95
sim_nutrients <- c(0,6,15,33,45)
sim_Taxon <- c("TYAN", "TYGL", "TYLA")
# the simulated u_tank captures the sites within the U-tank variable without
# causing excessive replicates
sim_u_tank <- c(levels(Typha_allocation_model_data$u_tank))
# Create a factor variable simulation
sim_fct_nutrients <- factor(c("low", "med","high"), 
                            levels = c("low", "med", "high"),
                            ordered = TRUE)

# Create a very large data set with all combos of all factor variables
sim_fct_data_ME <- expand.grid(Veg_Weight = sim_veg_mass, 
                               N_Level_Group = sim_fct_nutrients, 
                               Taxon = sim_Taxon,
                               u_tank = sim_u_tank)

# Extract the site from the u_tank variable. Otherwise we end up with u_tanks 
# from ESGR mixing with sites
sim_fct_data_ME$site <-  as.factor(str_extract(sim_fct_data_ME$u_tank, pattern = "(^[^_]*)"))
sim_fct_data_ME <- sim_fct_data_ME %>% 
  mutate(site = factor(site, levels = rev(levels(site))))

levels(sim_fct_data_ME$site)

# Need to add ME predictions to the simulated data
sim_fct_data_ME <- sim_fct_data_ME %>% 
  mutate(ME_predictions = predict(full_two_way_model, 
                                  newdata = sim_fct_data_ME,
                                  # setting level = 1 results in obtaining predictions with
                                  # u_tank as a random effect
                                  level = 1),
         pop_predictions = predict(full_two_way_model, 
                                   newdata = sim_fct_data_ME,
                                   # set level = 0 to grab population level
                                   # predictions rather than tank-specific
                                   # predictions
                                   level = 0))  %>% 
  # We want to find the range of predictions for graphing purposes
  group_by(site, Taxon, N_Level_Group, Veg_Weight) %>% 
  mutate(max_tank_pred = max(ME_predictions),
         min_tank_pred = min(ME_predictions)) %>% 
  ungroup()

levels(sim_fct_data_ME$Taxon)
levels(Typha_allocation_graph_data$Taxon)

levels(sim_fct_data_ME$N_Level_Group)
levels(Typha_allocation_graph_data$N_Level_Group)

levels(sim_fct_data_ME$site)
levels(Typha_allocation_graph_data$site)

# Find the maximum observations for each tile within the two figures
max_obs <- Typha_allocation_graph_data %>% 
  group_by(site, Taxon, N_Level_Group) %>% 
  summarize(max_obs = max(Veg_Weight)) %>% 
  mutate(N_Level_Group = factor(N_Level_Group, ordered = T,
                                levels = levels(Typha_allocation_model_data$N_Level_Group)))

sim_res_data_ME <- left_join(sim_fct_data_ME, max_obs,
                             by = c("site", "Taxon", "N_Level_Group"))

# Remove the simulated data that have values greater than the max observation
sim_res_data_ME <- sim_res_data_ME %>% filter(Veg_Weight <= (max_obs + 5))

levels(sim_res_data_ME$Taxon)
levels(Typha_allocation_graph_data$Taxon)

levels(sim_res_data_ME$N_Level_Group)
levels(Typha_allocation_graph_data$N_Level_Group)

levels(sim_res_data_ME$site)
levels(Typha_allocation_graph_data$site)

# Shaded version
{ 
  # Reverse factor levels for simulation data
  
  sim_res_data_ME <- sim_res_data_ME %>% 
    mutate(N_Level_Group = factor(N_Level_Group, levels = rev(levels(N_Level_Group))))
  Typha_allocation_graph_data <- Typha_allocation_graph_data %>% 
    mutate(N_Level_Group = factor(N_Level_Group, levels = rev(levels(N_Level_Group))))
  
  levels(sim_res_data_ME$N_Level_Group)
  levels(Typha_allocation_graph_data$N_Level_Group)
  
  levels(sim_res_data_ME$Taxon)
  levels(Typha_allocation_graph_data$Taxon)
  
  levels(sim_res_data_ME$Taxon) <- c("T. angustifolia", "T. glauca",
                                       "T. latifolia")
  levels(Typha_allocation_graph_data$Taxon) <- c("T. angustifolia", "T. glauca",
                                                   "T. latifolia")
  
  levels(sim_res_data_ME$Taxon)
  levels(Typha_allocation_graph_data$Taxon)
  
  levels(sim_res_data_ME$site) 
  levels(Typha_allocation_graph_data$site)
  
  levels(sim_res_data_ME$site) <- c("North Site", "South Site")
  levels(Typha_allocation_graph_data$site) <- c("North Site", "South Site")
  
  levels(sim_res_data_ME$site) 
  levels(Typha_allocation_graph_data$site)
  
  size_dep_site_taxon <- ggplot() +
    # add in the model predictions
    geom_ribbon(data = sim_res_data_ME,
                aes(x = Veg_Weight, 
                    ymin = min_tank_pred,
                    ymax = max_tank_pred,
                    group = interaction(N_Level_Group, u_tank), 
                    #color = N_Level_Group,
                    fill = N_Level_Group),
                alpha = 0.02) + 
    # add in the model predictions
    geom_line(data = sim_res_data_ME,
              aes(x = Veg_Weight, y = pop_predictions,
                  group = N_Level_Group, 
                  color = N_Level_Group)) + 
    # Fill points by "treatment" column
    # Calls object with scatterplot
    geom_point(data = Typha_allocation_graph_data, 
               aes(x = Veg_Weight,
                   y = Infl_Weight, 
                   fill = N_Level_Group),
               shape = 21, size = 2) +
    # Changes color values and legend text for n-levels
    scale_color_manual(guide = "legend", 
                       values = c("midnightblue","cyan4", "cadetblue1"), # Colors to fill with
                       name = 'Nutrient Level', # Name for legend
                       labels = c('High', 'Medium', 'Low')) + # Legend labels
    scale_fill_manual(guide = "legend", 
                      values = c("midnightblue","cyan4", "cadetblue1"), # Colors to fill with
                      name = 'Nutrient Level', # Name for legend
                      labels = c('High', 'Medium', 'Low')) + # Legend labels
    # Make individual graphs for each taxon
    facet_grid(rows = vars(Taxon), cols = vars(site)) +
    # Specify title and axes lables
    xlab("Vegetative Biomass (g)") + 
    ylab("Reproductive Biomass (g)") + 
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
  
  # call the graph
  size_dep_site_taxon
}


# Shaded Version
{
  levels(Typha_allocation_graph_data$N_Level_Group)
  levels(sim_res_data_ME$N_Level_Group)
  
  
  levels(Typha_allocation_graph_data$N_Level_Group) <- c("High",
                                                         "Medium",
                                                         "Low")
  levels(sim_res_data_ME$N_Level_Group) <- c("High",
                                             "Medium",
                                             "Low")
  
  # try facet wrap
  # Low Nutrient Level
  size_dep_site_NL <- ggplot() +
    # add in the shaded tank range
    geom_ribbon(data = sim_res_data_ME,
                aes(x = Veg_Weight, 
                    ymin = min_tank_pred,
                    ymax = max_tank_pred,
                    group = interaction(Taxon, u_tank), 
                    fill = Taxon),
                alpha = 0.02) + 
    # add in the model predictions
    geom_line(data = sim_res_data_ME,
              aes(x = Veg_Weight, y = pop_predictions,
                  group = Taxon,
                  color = Taxon),
              alpha = 0.5) + 
    # Fill points by "treatment" column
    # Calls object with scatterplot
    geom_point(data = Typha_allocation_graph_data, 
               aes(x = Veg_Weight,
                   y = Infl_Weight, 
                   fill = Taxon,
                   shape = Taxon),
               size = 2) +
    # Edit Colors and Legend
    scale_fill_manual("Taxon",
                      labels = c("T. angustifolia", "T. glauca", "T. latifolia"),
                      values = c("tan1", "deepskyblue4", "deeppink3"),
                      guide = guide_legend(label.theme = element_text(face = "italic"))) +
    scale_color_manual("Taxon",
                       labels = c("T. angustifolia", "T. glauca", "T. latifolia"),
                       values = c("tan1", "deepskyblue4", "deeppink3"),
                       guide = guide_legend(label.theme = element_text(face = "italic"))) +
    scale_shape_manual("Taxon",
                       labels = c("T. angustifolia", "T. glauca", "T. latifolia"),
                       values = c(22, 21, 24),
                       guide = guide_legend(label.theme = element_text(face = "italic"))) +
    # Make individual graphs for each taxon
    facet_grid(rows = vars(N_Level_Group), cols = vars(site)) +
    xlim(0, 100) +
    # Specify title and axes lables
    xlab("Vegetative Biomass (g)") + 
    ylab("Reproductive Biomass (g)") + 
    theme_bw() +
    theme(axis.title.x = element_text(size = 12),                # x-axis text size
          axis.title.y = element_text(size = 12),                # y-axis text size
          plot.title = element_text(size = 12, face = "bold"),                  # Title text size
          legend.text = element_text(size = 10),                 # Legend text size
          legend.title = element_text(face = 'bold', size = 10), # Legend title format (bold)
          axis.text = element_text(size = 10),
          strip.text = element_text(size = 12),
          strip.background = element_blank())
  
  levels(Typha_allocation_graph_data$N_Level_Group) <- c("high",
                                                         "med",
                                                         "low")
  levels(sim_res_data_ME$N_Level_Group) <- c("high",
                                             "med",
                                             "low")
  
  levels(sim_res_data_ME$Taxon) <- c("TYAN", "TYGL", "TYLA")
  levels(Typha_allocation_graph_data$Taxon) <- c("TYAN", "TYGL", "TYLA")
  
  levels(sim_res_data_ME$site) <- c("UMBS", "ESGR")
  levels(Typha_allocation_graph_data$site) <- c("UMBS", "ESGR")
  
  size_dep_site_NL
}

