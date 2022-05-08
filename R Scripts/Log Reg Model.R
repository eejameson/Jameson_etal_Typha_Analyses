#### Logistic Regression Model ####

##### Check Data #####
sub_group_log_figure

# Note that there were not enough TYLA observations for the model to provide
# useful estimates on the probability of flowering. Including TYLA produced 
# confidence intervals with ranges from 0 to infinity, so it was excluded from
# the model

# Remove the TYLA observations
Typha_Log_Model_Data_Modified <- Typha_Log_Model_Data %>% 
  filter(Taxon != "TYLA") %>% 
  mutate(Taxon = factor(Taxon))

summary(Typha_Log_Model_Data_Modified$Taxon)

ggplot(data = Typha_Log_Model_Data_Modified) +
  geom_point(aes(x = Veg_Weight, y = RepStatusTF, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)

##### Full model (overfitted) #####
glm_Typha_fullA <- glm(RepStatusTF ~ Taxon*N_Level_Group*Veg_Weight*site,
                       data = Typha_Log_Model_Data_Modified, family = binomial)
summary(glm_Typha_fullA)
Log_full_anova_results <- Anova(glm_Typha_fullA)
Log_full_anova_results

# Find Odds Raiton and Wald's conf-int
OR_estimates <- Odds_ratio(glm_Typha_fullA)
OR_estimates
CI_estimates <- CI2(glm_Typha_fullA)
#view(CI_estimates)


###### Preliminary Influential Variable Check ####

# Check for influential variables

# Plot cooks distance to determine outliers
plot(glm_Typha_fullA, which = 4, id.n = 5)

# There are 2 observations (169, and 156) that have way higher cook's distance
# values than every other observation.

# extract model results
log_mod_data <- augment(glm_Typha_fullA) %>% 
  mutate(index = 1:n())

# Standardized Pearson Residuals by index
ggplot(log_mod_data, aes(index, .std.resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3))

# Standardized Pearson Residuals by probabilities
ggplot(log_mod_data, aes(.fitted, .std.resid, label = index)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  geom_text(aes(label = index))

# The two points that fall off the fitted vs std-resid line are 169, and 156
# Those are the same two points that have high leverage (see below) and high
# cook's distance

# Hat leverage by index 
ggplot(log_mod_data, aes(index, .hat)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(log_mod_data, aes(.fitted, .hat)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

# deviance residuals by index
ggplot(log_mod_data, aes(index, .resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(log_mod_data, aes(.fitted, .resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

# Find average leverage
mean_hat <- mean(log_mod_data$.hat)

# Find observations with leverage > 3*mean_hat
log_mod_data %>% filter(.hat > 3*mean_hat) %>% arrange(-.hat)

influential_points <- log_mod_data %>% 
  filter(index == 156 | index == 169 )

influential_points

# Lets find the points in the original data
Typha_Log_Model_Data_Modified %>% 
  filter(RepStatusTF == 0 & Veg_Weight > 70) %>% 
  select(UID)

influential_UID <- Typha_Log_Model_Data_Modified %>% 
  filter(RepStatusTF == 0 & Veg_Weight > 70) %>% 
  select(UID)

# When we examined these samples in the original data 
# both had dry masses that exceed their wet mass which is impossible so we 
# removed them.

# These two observations are the ones that are influencing the model so much.
influential_UIDs <- Typha_Log_Model_Data_Modified %>% 
  filter(RepStatusTF == 0 & Veg_Weight > 70) %>% 
  select(UID)

# Remove these observations from the data set
Typha_Log_Model_Data_Modified <- Typha_Log_Model_Data_Modified %>% 
  filter(!(UID %in% influential_UIDs$UID))

ggplot(data = Typha_Log_Model_Data_Modified) +
  geom_point(aes(x = Veg_Weight, y = RepStatusTF, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)

##### Full model (overfitted) 2 #####

glm_Typha_full <- glm(RepStatusTF ~ Taxon*N_Level_Group*Veg_Weight*site,
                      data = Typha_Log_Model_Data_Modified, family = binomial)
summary(glm_Typha_full)

###### Model Results #####
Anova(glm_Typha_full, test.statistic = "F")

Log_full_anova_results <- as_tibble(Anova(glm_Typha_full, test.statistic = "F"))
Log_full_anova_results$variables <- row.names(Anova(glm_Typha_full, test.statistic = "F"))
Log_full_anova_results <- Log_full_anova_results %>% 
  select(variables, everything())
Log_full_anova_results

# Find Odds Raito and Wald's conf-int
OR_estimates <- Odds_ratio(glm_Typha_full)
OR_estimates
CI_estimates <- CI2(glm_Typha_full)
#view(CI_estimates)

###### Check Model Assumptions #####

# Check multicollinearity
vif(glm_Typha_full)

sorta_Rsquared <- 1 - (glm_Typha_full$deviance/glm_Typha_full$null.deviance)
1/(1 - sorta_Rsquared )

Log_vif_results_full <- as_tibble(vif(glm_Typha_full))
Log_vif_results_full$variable <- rownames(vif(glm_Typha_full))
Log_vif_results_full <- Log_vif_results_full %>% 
  select(variable, everything())
Log_vif_results_full

# write_csv(Log_vif_results_full,
#           "Figures/Graphs and Tables/Full Log VIF Results.csv")

# Check that numeric variables are linearly related to the log-odds
probabilities_full <- predict(glm_Typha_full, type = "response")

Typha_Log_Model_Data_Modified$full_probs <- probabilities_full

Typha_Log_Model_Data_Modified <- Typha_Log_Model_Data_Modified %>% 
  mutate(logit = log(full_probs/(1 - full_probs)))

ggplot(data = Typha_Log_Model_Data_Modified,
       aes(x = Veg_Weight, y = logit,
           color = N_Level_Group,
           shape = Taxon)) +
  geom_point() 

# Check for influential variables
# extract model results
final_log_mod_data_full <- augment(glm_Typha_full) %>% 
  mutate(index = 1:n())

# Standardized Pearson Residuals by index
ggplot(final_log_mod_data_full, aes(index, .std.resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3))

# Standardized Pearson Residuals by probabilities
ggplot(final_log_mod_data_full, aes(.fitted, .std.resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3))

# Plot cooks distance to determine outliers
plot(glm_Typha_full, which = 4, id.n = 5)

final_log_mod_data_full %>% top_n(5, .cooksd)

# Hat leverage by index 
ggplot(final_log_mod_data_full, aes(index, .hat)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(final_log_mod_data_full, aes(.fitted, .hat)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(final_log_mod_data_full, aes(.fitted, .hat)) + 
  geom_point(aes(color = Taxon, shape = N_Level_Group), alpha = .5) +
  theme_bw() 

# deviance residuals by index
ggplot(final_log_mod_data_full, aes(index, .resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(final_log_mod_data_full, aes(.fitted, .resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

# See if there are any influential data points with |.std.resid | > 3
final_log_mod_data_full %>% filter(abs(.std.resid) > 3)


##### Taxon*NLG*Veg Mass Model #####

glm_Typha_T.NLG.VW <- glm(RepStatusTF ~ Taxon*N_Level_Group*Veg_Weight,
                          data = Typha_Log_Model_Data_Modified, family = binomial)
summary(glm_Typha_T.NLG.VW)

###### Model Results ########

# Save the model results as a data frame so that it can be exported to a csv
log_model_results <- as_tibble(coefficients(summary(glm_Typha_T.NLG.VW)))

log_model_results$coefficients <- row.names(coefficients(summary(glm_Typha_T.NLG.VW))) 
log_model_results <- log_model_results %>% 
  select(coefficients, everything())

log_model_results

# write_csv(log_model_results,
#           "Cattail_Reproductive_Allocation_Analyses/Data Summaries and Coefficient Comparisons/log_model_results.csv")

Anova(glm_Typha_T.NLG.VW, test.statistic = "F")
Log_T.NLG.VW_anova_results <- as_tibble(Anova(glm_Typha_T.NLG.VW, test.statistic = "F"))


Log_T.NLG.VW_anova_results$variables <- row.names(Anova(glm_Typha_T.NLG.VW, test.statistic = "F")) 
Log_T.NLG.VW_anova_results <- Log_T.NLG.VW_anova_results %>% 
  select(variables, everything())
Log_T.NLG.VW_anova_results

# write_csv(Log_T.NLG.VW_anova_results,
#           "Figures/Graphs and Tables/Log_ANOVA_results.csv")


# Find Odds Raiton and Wald's conf-int
OR_estimates_T.NLG.VW <- Odds_ratio(glm_Typha_T.NLG.VW)
OR_estimates_T.NLG.VW
CI_estimates_T.NLG.VW <- CI2(glm_Typha_T.NLG.VW)
#view(CI_estimates_T.NLG.VW)

###### Check Model Assumptions #####

# Check multicollinearity
vif(glm_Typha_T.NLG.VW)

sorta_Rsquared <- 1 - (glm_Typha_T.NLG.VW$deviance/glm_Typha_T.NLG.VW$null.deviance)
1/(1 - sorta_Rsquared )

Log_vif_results <- as_tibble(vif(glm_Typha_T.NLG.VW))
Log_vif_results$variable <- rownames(vif(glm_Typha_T.NLG.VW))
Log_vif_results <- Log_vif_results %>% 
  select(variable, everything())
Log_vif_results

# write_csv(Log_vif_results,
#           "Figures/Graphs and Tables/Log VIF Results.csv")

# Check that numeric variables are linearly related to the log-odds
probabilities <- predict(glm_Typha_T.NLG.VW, type = "response")

Typha_Log_Model_Data_Modified$probabilities <- probabilities

Typha_Log_Model_Data_Modified <- Typha_Log_Model_Data_Modified %>% 
  mutate(logit = log(probabilities/(1 - probabilities)))

ggplot(data = Typha_Log_Model_Data_Modified,
       aes(x = Veg_Weight, y = logit,
           color = N_Level_Group,
           shape = Taxon)) +
  geom_point() 

# Check for influential variables
# extract model results
final_log_mod_data <- augment(glm_Typha_T.NLG.VW) %>% 
  mutate(index = 1:n())

# Standardized Pearson Residuals by index
ggplot(final_log_mod_data, aes(index, .std.resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3))

# Standardized Pearson Residuals by probabilities
ggplot(final_log_mod_data, aes(.fitted, .std.resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() +
  scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3))

# Plot cooks distance to determine outliers
plot(glm_Typha_T.NLG.VW, which = 4, id.n = 5)

final_log_mod_data %>% top_n(5, .cooksd)

# Hat leverage by index 
ggplot(final_log_mod_data, aes(index, .hat)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(final_log_mod_data, aes(.fitted, .hat)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(final_log_mod_data, aes(.fitted, .hat)) + 
  geom_point(aes(color = Taxon, shape = N_Level_Group), alpha = .5) +
  theme_bw() 

# deviance residuals by index
ggplot(log_mod_data, aes(index, .resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

ggplot(log_mod_data, aes(.fitted, .resid)) + 
  geom_point(aes(color = as.factor(RepStatusTF)), alpha = .5) +
  theme_bw() 

# See if there are any influential data points with |.std.resid | > 3
final_log_mod_data %>% filter(abs(.std.resid) > 2.5)

##### Taxon*VW + NLG*VW Model #####

glm_Typha_T.VW_NLG.VW <- glm(RepStatusTF ~ Taxon*Veg_Weight + N_Level_Group*Veg_Weight,
                             data = Typha_Log_Model_Data_Modified, family = binomial)
summary(glm_Typha_T.VW_NLG.VW)
Log_T.VW_NLG.VW_anova_results <- Anova(glm_Typha_T.VW_NLG.VW)
Log_T.VW_NLG.VW_anova_results

# Find Odds Raiton and Wald's conf-int
OR_estimates_T.VW_NLG.VW <- Odds_ratio(glm_Typha_T.VW_NLG.VW)
OR_estimates_T.VW_NLG.VW
CI_estimates_T.VW_NLG.VW <- CI2(glm_Typha_T.VW_NLG.VW)
#view(CI_estimates_T.VW_NLG.VW)

##### Taxon*VW Model #####

glm_Typha_T.VW <- glm(RepStatusTF ~ Taxon*Veg_Weight,
                      data = Typha_Log_Model_Data_Modified, family = binomial)
summary(glm_Typha_T.VW)
Log_T.VW_anova_results <- Anova(glm_Typha_T.VW)
Log_T.VW_anova_results

# Find Odds Raiton and Wald's conf-int
OR_estimates_T.VW <- Odds_ratio(glm_Typha_T.VW)
OR_estimates_T.VW
CI_estimates_T.VW <- CI2(glm_Typha_T.VW)
#view(CI_estimates_T.VW)

##### Compare models using AIC #####

AIC(glm_Typha_full, glm_Typha_T.NLG.VW,
    glm_Typha_T.VW_NLG.VW, glm_Typha_T.VW)

anova(glm_Typha_T.NLG.VW, glm_Typha_full)

Log_AIC_results <- as_tibble(AIC(glm_Typha_full, glm_Typha_T.NLG.VW,
                                 glm_Typha_T.VW_NLG.VW, glm_Typha_T.VW))
Log_AIC_results$model <- rownames(AIC(glm_Typha_full, glm_Typha_T.NLG.VW,
                                      glm_Typha_T.VW_NLG.VW, glm_Typha_T.VW))
Log_AIC_results <- Log_AIC_results %>% 
  select(model, everything()) %>% 
  mutate(min_AIC = min(AIC),
         diff_AIC = AIC - min_AIC)
Log_AIC_results

# write_csv(Log_AIC_results,
#           "Figures/Graphs and Tables/Log AIC Results.csv")




##### Visualize Model #####

levels(Typha_Log_Model_Data_Modified$N_Level_Group)

Typha_log_graph_data <- Typha_Log_Model_Data_Modified %>% 
  mutate(N_Level_Group = factor(N_Level_Group, levels = rev(levels(N_Level_Group)))) %>% 
  select(site, tank, rametID, Taxon, N_Level_Group, N_level,
         Veg_Weight, Infl_Weight, RepStatusTF)

# add the TYLA observations from the original data set.
TYLA_log_data <- Typha_Log_Model_Data %>% 
  filter(Taxon == "TYLA") %>% 
  select(site, tank, rametID, Taxon, N_Level_Group, N_level,
         Veg_Weight, Infl_Weight, RepStatusTF)

Typha_log_graph_data_edited <- rbind(Typha_log_graph_data,
                                     TYLA_log_data)


levels(Typha_log_graph_data_edited$N_Level_Group)

levels(Typha_log_graph_data_edited$N_Level_Group) <- c("High",
                                                       "Medium",
                                                       "Low")
levels(Typha_log_graph_data_edited$Taxon)

levels(Typha_log_graph_data_edited$Taxon) <- c("T. angustifolia", "T. glauca",
                                                 "T. latifolia")

# Graph split by Taxa to compare N-levels within taxa
Logistic_graph_by_Sp <- ggplot() +
  geom_smooth(data = Typha_log_graph_data_edited[Typha_log_graph_data_edited$Taxon != "T. latifolia",],
              aes(x = Veg_Weight, y = probabilities, 
                  linetype = N_Level_Group,
                  color = Taxon)) +
  geom_jitter(data = Typha_log_graph_data_edited,
              aes(x = Veg_Weight, y = RepStatusTF, 
                  color = Taxon,
                  shape = N_Level_Group),
              height = 0.1) +
  facet_wrap(~ Taxon, ncol = 1) +
  scale_linetype_manual(guide = "legend",
                        values = c("solid","dashed", "dotted"), # Colors to fill with
                        name = 'Nutrients', # Name for legend
                        labels = c('High', 'Medium', 'Low')) +
  scale_color_manual(values = c("tan1", "deepskyblue4",  "deeppink3")) +
  scale_shape_manual("",
                     values = c(19, 10, 1)) +
  guides(color = "none",
         linetype = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  xlab("Vegetative Biomass (g)") +
  ylab("Estimated Probability of Flowering") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),                # x-axis text size
        axis.title.y = element_text(size = 12),                # y-axis text size
        plot.title = element_text(size = 12, face = "bold"),                  # Title text size
        legend.text = element_text(size = 10),                 # Legend text size
        legend.title = element_text(face = 'bold', size = 10), # Legend title format (bold)
        axis.text = element_text(size = 10),
        strip.text = element_text(size = 10, face = "italic"),
        strip.background = element_blank()) +
  scale_y_continuous(breaks = c(0, 0.5, 1))
Logistic_graph_by_Sp
