#### Continuous NL Variable ####

# Create the continuous nutrient level mixed effects model

full_cont_mixed_model <- nlme(model = Infl_Weight ~ A*(Veg_Weight)^(alpha),
                              fixed = list(alpha ~ Taxon + N_level + site +
                                             Taxon*N_level + N_level*site +
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
                                          TYGL_med_start, 
                                          TYLA_med_start, 
                                          0, 0, 0, 0,
                                          A_start)))

summary(full_cont_mixed_model)

##### Compare to Categorical #####
# Compare the models
AIC(full_two_way_model, full_cont_mixed_model)

# Find the difference between the two models 
# model 2 - model 1 so the difference is positive
AIC(full_two_way_model, full_cont_mixed_model)$AIC[2] - AIC(full_two_way_model, full_cont_mixed_model)$AIC[1]

