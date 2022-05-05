#### Individual Subgroup Models ####

# To determine which functional form to use in our larger models we compare the fit
# of individual models based on subgroups of the data where each subgroup gets
# a model for each of the following forms
# 
# Linear with a 0 intercept 
# Linear with a non 0 x-intercept
# Non-linear with a 0 intercept
# Non-linear with intercept
# 
# For models with intercepts, the value C represents the x-intercept, as that is
# more informative than a y-intercept. Additionally, the x intercepts are bound 
# above 0 because a negative x-intercept would predict a positive inflorescence
# mass when the plant has 0 vegetative mass, which is biologically impossible.

##### Sub-Models ####

# In some cases the initial starting parameter values did not result in convergence.
# In order to fix this I used two methods. The first was changing the starting
# values for the parameters based on the unconverged model output. By setting
# trace = T within nls() you can observe the values at which the model failed
# to converge. If changing the starting values to these estimates did not work
# I removed the A parameter (only applicable in non-linear with intercept comparison)
# so the model would "focus" on estimating B, the parameter we are using to 
# estimate sexual reproductive allocation. This is equivalent to setting A = 1.
# 


##### UMBS TYAN ######
{
  ###### Non-lin w/ int #######
  
  UMBS.TYAN.LowN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B, 
                                data = UMBS_TYAN_Low_N,
                                start = list(A = 1, B = 0.5, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(UMBS.TYAN.LowN.nls.int)
  
  UMBS.TYAN.MedN.nls.int <- nls(Infl_Weight ~ A*(Veg_Weight - C)^B, 
                                data = UMBS_TYAN_Med_N,
                                start = list(A = 1, B = 1, C = 4),
                                algorithm = "port",
                                #trace = T,
                                lower = c(-Inf, -Inf, 0), upper = c(Inf, Inf, 4.5))
  summary(UMBS.TYAN.MedN.nls.int)
  
  UMBS.TYAN.HighN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                 data = UMBS_TYAN_High_N,
                                 start = list(A = 1, B = 1, C = 1),
                                 algorithm = "port",
                                 #trace = T,
                                 lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(UMBS.TYAN.HighN.nls.int)
  
  ##### Non-lin w/out int #######
  
  UMBS.TYAN.LowN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B, 
                                   data = UMBS_TYAN_Low_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYAN.LowN.nls.no.int)
  
  UMBS.TYAN.MedN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B, 
                                   data = UMBS_TYAN_Med_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYAN.MedN.nls.no.int)
  
  UMBS.TYAN.HighN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                    data = UMBS_TYAN_High_N,
                                    start = list(A = 1, B = 0.5),
                                    algorithm = "port",
                                    lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYAN.HighN.nls.no.int)
  
  ##### Lin w/ int #######
  
  UMBS.TYAN.LowN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C), 
                                data = UMBS_TYAN_Low_N,
                                start = list(A = 1, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYAN.LowN.lin.int)
  
  UMBS.TYAN.MedN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C), 
                                data = UMBS_TYAN_Med_N,
                                start = list(A = 1, C = 0),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYAN.MedN.lin.int)
  
  UMBS.TYAN.HighN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                 data = UMBS_TYAN_High_N,
                                 start = list(A = 1, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYAN.HighN.lin.int)
  
  ##### Lin w/out int #######
  
  UMBS.TYAN.LowN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight), 
                                   data = UMBS_TYAN_Low_N,
                                   start = list(A = 1),
                                   algorithm = "port")
  summary(UMBS.TYAN.LowN.lin.no.int)
  
  UMBS.TYAN.MedN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight), 
                                   data = UMBS_TYAN_Med_N,
                                   start = list(A = 1),
                                   algorithm = "port")
  summary(UMBS.TYAN.MedN.lin.no.int)
  
  UMBS.TYAN.HighN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                    data = UMBS_TYAN_High_N,
                                    start = list(A = 1),
                                    algorithm = "port")
  summary(UMBS.TYAN.HighN.lin.no.int)
}
##### UMBS TYGL ######
{
  ###### Non-lin w/ int #######
  
  UMBS.TYGL.LowN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B, 
                                data = UMBS_TYGL_Low_N,
                                start = list(A = 1, B = 0.5, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(UMBS.TYGL.LowN.nls.int)
  
  
  UMBS.TYGL.MedN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                data = UMBS_TYGL_Med_N,
                                start = list(A = 1, B = 0.5, C = 0.5),
                                algorithm = "port",
                                lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(UMBS.TYGL.MedN.nls.int)
  
  UMBS.TYGL.HighN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                 data = UMBS_TYGL_High_N,
                                 start = list(A = 1, B = 0.5, C = 7),
                                 algorithm = "port",
                                 #trace = T,
                                 lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(UMBS.TYGL.HighN.nls.int)
  
  ##### Non-lin w/out int #######
  
  UMBS.TYGL.LowN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B, 
                                   data = UMBS_TYGL_Low_N,
                                   start = list(A = 6, B = 0.8),
                                   algorithm = "port",
                                   #trace = T,
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYGL.LowN.nls.no.int)
  
  UMBS.TYGL.MedN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                   data = UMBS_TYGL_Med_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYGL.MedN.nls.no.int)
  
  UMBS.TYGL.HighN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                    data = UMBS_TYGL_High_N,
                                    start = list(A = 1, B = 0.5),
                                    algorithm = "port",
                                    lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYGL.HighN.nls.no.int)
  
  ##### Lin w/ int #######
  
  UMBS.TYGL.LowN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C), 
                                data = UMBS_TYGL_Low_N,
                                start = list(A = 1, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYGL.LowN.lin.int)
  
  UMBS.TYGL.MedN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                data = UMBS_TYGL_Med_N,
                                start = list(A = 1, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYGL.MedN.lin.int)
  
  UMBS.TYGL.HighN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                 data = UMBS_TYGL_High_N,
                                 start = list(A = 1, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYGL.HighN.lin.int)
  
  ##### Lin w/out int #######
  
  UMBS.TYGL.LowN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight), 
                                   data = UMBS_TYGL_Low_N,
                                   start = list(A = 1),
                                   algorithm = "port",
                                   lower = c( -Inf), upper = c(Inf))
  summary(UMBS.TYGL.LowN.lin.no.int)
  
  UMBS.TYGL.MedN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                   data = UMBS_TYGL_Med_N,
                                   start = list(A = 1),
                                   algorithm = "port",
                                   lower = c( -Inf), upper = c(Inf))
  summary(UMBS.TYGL.MedN.lin.no.int)
  
  UMBS.TYGL.HighN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                    data = UMBS_TYGL_High_N,
                                    start = list(A = 1),
                                    algorithm = "port",
                                    lower = c( -Inf), upper = c(Inf))
  summary(UMBS.TYGL.HighN.lin.no.int)
}

##### UMBS TYLA ######
{
  ###### Non-lin w/ int #######
  
  # TYLA low
  # With only 2 observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison
  
  # TYLA medium
  # With only 3 observations at this site and medium nutrient level for TYLA we did
  # not run these models for this comparison
  
  # TYLA high
  # Could not get this model to converge while including A as an additional 
  # shape parameter. Removing A from the equation is equivalent to stating A = 1
  
  UMBS.TYLA.HighN.nls.int <- nls(Infl_Weight ~ (Veg_Weight - C)^B,
                                 data = UMBS_TYLA_High_N,
                                 start = list( B = 0.5, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYLA.HighN.nls.int)
  
  ##### Non-lin w/out int #######
  
  # With only 2 observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison
  
  # With only 3 observations at this site and medium nutrient level for TYLA we did
  # not run these models for this comparison
  
  # TYLA High
  
  UMBS.TYLA.HighN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                    data = UMBS_TYLA_High_N,
                                    start = list(A = 1, B = 0.5),
                                    algorithm = "port",
                                    lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(UMBS.TYLA.HighN.nls.no.int)
  
  ##### Lin w/ int #######
  
  # With only 2 observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison
  
  # With only 3 observations at this site and medium nutrient level for TYLA we did
  # not run these models for this comparison
  
  # TYLA High
  
  UMBS.TYLA.HighN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                 data = UMBS_TYLA_High_N,
                                 start = list(A = 1, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYLA.HighN.lin.int)
  
  ##### Lin w/out int #######
  
  # With only 2 observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison
  
  # With only 3 observations at this site and medium nutrient level for TYLA we did
  # not run these models for this comparison
  
  # TYLA High
  
  UMBS.TYLA.HighN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                    data = UMBS_TYLA_High_N,
                                    start = list(A = 1),
                                    algorithm = "port",
                                    lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(UMBS.TYLA.HighN.lin.no.int)
}

##### ESGR TYAN ######
{
  ###### Non-lin w/ int #######
  
  # TYAN Low 
  # Could not get this model to converge while including A as an additional 
  # shape parameter. Removing A from the equation is equivalent to stating A = 1
  
  ESGR.TYAN.LowN.nls.int <- nls(Infl_Weight ~ (Veg_Weight - C)^B, 
                                data = ESGR_TYAN_Low_N,
                                start = list( B = 0.4, C = 0.5),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c( Inf, Inf))
  summary(ESGR.TYAN.LowN.nls.int)
  
  # TYAN Med Combined
  ESGR.TYAN.MedN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B, 
                                data = ESGR_TYAN_Med_N,
                                start = list(A = 1, B = 0.5, C = 0),
                                algorithm = "port",
                                lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(ESGR.TYAN.MedN.nls.int)
  
  # TYAN High Combined
  ESGR.TYAN.HighN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                 data = ESGR_TYAN_High_N,
                                 start = list(A = 1, B = 0.5, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(ESGR.TYAN.HighN.nls.int)
  
  ##### Non-lin w/out int #######
  
  # TYAN Low Combined
  # Don't need to remove A in order for the model to converge
  ESGR.TYAN.LowN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B, 
                                   data = ESGR_TYAN_Low_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(rep(Inf,2)))
  summary(ESGR.TYAN.LowN.nls.no.int)
  
  # TYAN Med Combined
  ESGR.TYAN.MedN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B, 
                                   data = ESGR_TYAN_Med_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(ESGR.TYAN.MedN.nls.no.int)
  
  # TYAN High Combined
  ESGR.TYAN.HighN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                    data = ESGR_TYAN_High_N,
                                    start = list(A = 1, B = 0.5),
                                    algorithm = "port",
                                    lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(ESGR.TYAN.HighN.nls.no.int)
  
  ##### Lin w/ int #######
  
  # TYAN Low Combined
  ESGR.TYAN.LowN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C), 
                                data = ESGR_TYAN_Low_N,
                                start = list(A = 1, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYAN.LowN.lin.int)
  
  # TYAN Med Combined
  ESGR.TYAN.MedN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C), 
                                data = ESGR_TYAN_Med_N,
                                start = list(A = 1, C = 0),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYAN.MedN.lin.int)
  
  # TYAN High Combined
  ESGR.TYAN.HighN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                 data = ESGR_TYAN_High_N,
                                 start = list(A = 1, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYAN.HighN.lin.int)
  
  ##### Lin w/out int #######
  
  # TYAN Low Combined
  ESGR.TYAN.LowN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight), 
                                   data = ESGR_TYAN_Low_N,
                                   start = list(A = 1),
                                   algorithm = "port")
  summary(ESGR.TYAN.LowN.lin.no.int)
  
  # TYAN Med Combined
  ESGR.TYAN.MedN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight), 
                                   data = ESGR_TYAN_Med_N,
                                   start = list(A = 1),
                                   algorithm = "port")
  summary(ESGR.TYAN.MedN.lin.no.int)
  
  # TYAN High Combined
  ESGR.TYAN.HighN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                    data = ESGR_TYAN_High_N,
                                    start = list(A = 1),
                                    algorithm = "port")
  summary(ESGR.TYAN.HighN.lin.no.int)
}

##### ESGR TYGL ######
{
  ###### Non-lin w/ int #######
  
  # TYGL Low
  # With only 3 observations at this site and nutrient level for TYGL we did
  # not run these models for this comparison.
  
  # TYGL.LowN.nls.int.E <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B, 
  #                      data = ESGR_TYGL_Low_N,
  #                      start = list(A = 1, B = 0.5, C = 1),
  #                      algorithm = "port",
  #                      lower = c( -Inf, -Inf, 0), upper = c(rep(Inf,2), 100))
  # summary(TYGL.LowN.nls.int.E)
  
  # TYGL Med 
  ESGR.TYGL.MedN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                data = ESGR_TYGL_Med_N,
                                start = list(A = 1, B = 0.5, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(ESGR.TYGL.MedN.nls.int)
  
  # Assuming A = 1 in order to facilitate convergence
  ESGR.TYGL.HighN.nls.int <- nls(Infl_Weight ~ (Veg_Weight - C)^B,
                                 data = ESGR_TYGL_High_N,
                                 start = list(B = 0.5, C = 0),
                                 algorithm = "port",
                                 #trace = T,
                                 lower = c( -Inf, 0), upper = c( Inf, Inf))
  summary(ESGR.TYGL.HighN.nls.int)
  
  ##### Non-lin w/out int #######
  
  # TYGL Low
  # With only 3 observations at this site and nutrient level for TYGL we did
  # not run these models for this comparison.
  
  # TYGL.LowN.nls.no.int.E <- nls(Infl_Weight ~ A * (Veg_Weight)^B, 
  #                      data = ESGR_TYGL_Low_N,
  #                      start = list(A = 1, B = 0.5),
  #                      algorithm = "port",
  #                      lower = c( -Inf, -Inf), upper = c(rep(Inf,2)))
  # summary(TYGL.LowN.nls.no.int.E)
  
  # TYGL Med Combined
  ESGR.TYGL.MedN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                   data = ESGR_TYGL_Med_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(ESGR.TYGL.MedN.nls.no.int)
  
  # TYGL High Combined
  ESGR.TYGL.HighN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                    data = ESGR_TYGL_High_N,
                                    start = list(A = 1, B = 0.5),
                                    algorithm = "port",
                                    lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(ESGR.TYGL.HighN.nls.no.int)
  
  
  ##### Lin w/ int #######
  
  # TYGL Low
  # With only 3 observations at this site and nutrient level for TYGL we did
  # not run these models for this comparison.
  
  # TYGL.LowN.lin.int.E <- nls(Infl_Weight ~ A * (Veg_Weight - C), 
  #                      data = ESGR_TYGL_Low_N,
  #                      start = list(A = 1, C = 1),
  #                      algorithm = "port",
  #                      lower = c( -Inf, 0), upper = c(Inf, 100))
  # summary(TYGL.LowN.lin.int.E)
  
  # TYGL Med Combined
  ESGR.TYGL.MedN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                data = ESGR_TYGL_Med_N,
                                start = list(A = 1, C = 1),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYGL.MedN.lin.int)
  
  # TYGL High Combined
  ESGR.TYGL.HighN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                 data = ESGR_TYGL_High_N,
                                 start = list(A = 1, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYGL.HighN.lin.int)
  
  ##### Lin w/out int #######
  
  # TYGL Low
  # With only 3 observations at this site and nutrient level for TYGL we did
  # not run these models for this comparison.
  
  # TYGL.LowN.lin.no.int.E <- nls(Infl_Weight ~ A * (Veg_Weight), 
  #                      data = ESGR_TYGL_Low_N,
  #                      start = list(A = 1),
  #                      algorithm = "port",
  #                      lower = c( -Inf), upper = c(Inf))
  # summary(TYGL.LowN.lin.no.int.E)
  
  # TYGL Med Combined
  ESGR.TYGL.MedN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                   data = ESGR_TYGL_Med_N,
                                   start = list(A = 1),
                                   algorithm = "port",
                                   lower = c( -Inf), upper = c(Inf))
  summary(ESGR.TYGL.MedN.lin.no.int)
  
  # TYGL High Combined
  ESGR.TYGL.HighN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                    data = ESGR_TYGL_High_N,
                                    start = list(A = 1),
                                    algorithm = "port",
                                    lower = c( -Inf), upper = c(Inf))
  summary(ESGR.TYGL.HighN.lin.no.int)
}
##### ESGR TYLA ######
{
  ###### Non-lin w/ int #######
  
  # With no observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison.
  
  # TYLA medium
  ESGR.TYLA.MedN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                data = ESGR_TYLA_Med_N,
                                start = list(A = 1, B = 0.5, C = 0.1),
                                algorithm = "port",
                                lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(ESGR.TYLA.MedN.nls.int)
  
  # TYLA High
  ESGR.TYLA.HighN.nls.int <- nls(Infl_Weight ~ A * (Veg_Weight - C)^B,
                                 data = ESGR_TYLA_High_N,
                                 start = list(A = 0.7, B = 0.6, C = 11),
                                 algorithm = "port",
                                 #trace = T,
                                 lower = c( -Inf, -Inf, 0), upper = c(Inf, Inf, Inf))
  summary(ESGR.TYLA.HighN.nls.int)
  
  ##### Non-lin w/out int #######
  
  # With no observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison.
  
  # TYLA medium
  ESGR.TYLA.MedN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                   data = ESGR_TYLA_Med_N,
                                   start = list(A = 1, B = 0.5),
                                   algorithm = "port",
                                   lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(ESGR.TYLA.MedN.nls.no.int)
  
  # TYLA High
  ESGR.TYLA.HighN.nls.no.int <- nls(Infl_Weight ~ A * (Veg_Weight)^B,
                                    data = ESGR_TYLA_High_N,
                                    start = list(A = 1, B = 0.5),
                                    algorithm = "port",
                                    lower = c( -Inf, -Inf), upper = c(Inf, Inf))
  summary(ESGR.TYLA.HighN.nls.no.int)
  
  ##### Lin w/ int #######
  
  # With no observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison.
  
  # TYLA medium
  ESGR.TYLA.MedN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                data = ESGR_TYLA_Med_N,
                                start = list(A = 1, C = 0.1),
                                algorithm = "port",
                                lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYLA.MedN.lin.int)
  
  # TYLA High
  ESGR.TYLA.HighN.lin.int <- nls(Infl_Weight ~ A * (Veg_Weight - C),
                                 data = ESGR_TYLA_High_N,
                                 start = list(A = 1, C = 1),
                                 algorithm = "port",
                                 lower = c( -Inf, 0), upper = c(Inf, Inf))
  summary(ESGR.TYLA.HighN.lin.int)
  
  ##### Lin w/out int #######
  
  # With no observations at this site and low nutrient level for TYLA we did
  # not run these models for this comparison.
  
  # TYLA medium
  ESGR.TYLA.MedN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                   data = ESGR_TYLA_Med_N,
                                   start = list(A = 1),
                                   algorithm = "port",
                                   lower = c( -Inf), upper = c(Inf))
  summary(ESGR.TYLA.MedN.lin.no.int)
  
  # TYLA High
  ESGR.TYLA.HighN.lin.no.int <- nls(Infl_Weight ~ A * (Veg_Weight),
                                    data = ESGR_TYLA_High_N,
                                    start = list(A = 1),
                                    algorithm = "port",
                                    lower = c( -Inf), upper = c(Inf))
  summary(ESGR.TYLA.HighN.lin.no.int)
}

##### AIC Comparisons #####

# All the results are saved in a data frame so that they can be combined and
# compared at the end

###### UMBS #####
{
  # Comparing TYAN functions at low nutrient level
  UMBS.TYAN.low.AIC <- AIC(UMBS.TYAN.LowN.nls.int, UMBS.TYAN.LowN.nls.no.int, 
                           UMBS.TYAN.LowN.lin.int, UMBS.TYAN.LowN.lin.no.int)
  UMBS.TYAN.low.AIC$model <- row.names(UMBS.TYAN.low.AIC)
  
  # Comparing TYAN functions at med nutrient level
  UMBS.TYAN.med.AIC <- AIC(UMBS.TYAN.MedN.nls.int, UMBS.TYAN.MedN.nls.no.int, 
                           UMBS.TYAN.MedN.lin.int, UMBS.TYAN.MedN.lin.no.int)
  UMBS.TYAN.med.AIC$model <- row.names(UMBS.TYAN.med.AIC)
  
  # Comparing TYAN functions at high nutrient level
  UMBS.TYAN.high.AIC <- AIC(UMBS.TYAN.HighN.nls.int, UMBS.TYAN.HighN.nls.no.int, 
                            UMBS.TYAN.HighN.lin.int, UMBS.TYAN.HighN.lin.no.int)
  UMBS.TYAN.high.AIC$model <- row.names(UMBS.TYAN.high.AIC)
  
  # Comparing TYGL functions at low nutrient level
  UMBS.TYGL.low.AIC <- AIC(UMBS.TYGL.LowN.nls.int, UMBS.TYGL.LowN.nls.no.int, 
                           UMBS.TYGL.LowN.lin.int, UMBS.TYGL.LowN.lin.no.int)
  UMBS.TYGL.low.AIC$model <- row.names(UMBS.TYGL.low.AIC)
  
  # Comparing TYGL functions at med nutrient level
  UMBS.TYGL.med.AIC <- AIC(UMBS.TYGL.MedN.nls.int, UMBS.TYGL.MedN.nls.no.int, 
                           UMBS.TYGL.MedN.lin.int, UMBS.TYGL.MedN.lin.no.int)
  UMBS.TYGL.med.AIC$model <- row.names(UMBS.TYGL.med.AIC)
  
  # Comparing TYGL functions at high nutrient level
  UMBS.TYGL.high.AIC <- AIC(UMBS.TYGL.HighN.nls.int, UMBS.TYGL.HighN.nls.no.int, 
                            UMBS.TYGL.HighN.lin.int, UMBS.TYGL.HighN.lin.no.int)
  UMBS.TYGL.high.AIC$model <- row.names(UMBS.TYGL.high.AIC)
  
  # Comparing TYLA functions at high nutrient level
  # Could not compare other nutrient levels due to lack of data
  UMBS.TYLA.high.AIC <- AIC(UMBS.TYLA.HighN.nls.int, UMBS.TYLA.HighN.nls.no.int, 
                            UMBS.TYLA.HighN.lin.int, UMBS.TYLA.HighN.lin.no.int)
  UMBS.TYLA.high.AIC$model <- row.names(UMBS.TYLA.high.AIC)
}

###### ESGR #####

{
  # Comparing TYAN functions at low nutrient level
  ESGR.TYAN.low.AIC <- AIC(ESGR.TYAN.LowN.nls.int, ESGR.TYAN.LowN.nls.no.int, 
                           ESGR.TYAN.LowN.lin.int, ESGR.TYAN.LowN.lin.no.int)
  ESGR.TYAN.low.AIC$model <- row.names(ESGR.TYAN.low.AIC)
  
  # Comparing TYAN functions at med nutrient level
  ESGR.TYAN.med.AIC <- AIC(ESGR.TYAN.MedN.nls.int, ESGR.TYAN.MedN.nls.no.int, 
                           ESGR.TYAN.MedN.lin.int, ESGR.TYAN.MedN.lin.no.int)
  ESGR.TYAN.med.AIC$model <- row.names(ESGR.TYAN.med.AIC)
  
  # Comparing TYAN functions at high nutrient level
  ESGR.TYAN.high.AIC <- AIC(ESGR.TYAN.HighN.nls.int, ESGR.TYAN.HighN.nls.no.int, 
                            ESGR.TYAN.HighN.lin.int, ESGR.TYAN.HighN.lin.no.int)
  ESGR.TYAN.high.AIC$model <- row.names(ESGR.TYAN.high.AIC)
  
  # Comparing TYGL functions at low nutrient level
  # Didn't Run Because we only have 3 observations
  
  # Comparing TYGL functions at med nutrient level
  ESGR.TYGL.med.AIC <- AIC(ESGR.TYGL.MedN.nls.int, ESGR.TYGL.MedN.nls.no.int, 
                           ESGR.TYGL.MedN.lin.int, ESGR.TYGL.MedN.lin.no.int)
  ESGR.TYGL.med.AIC$model <- row.names(ESGR.TYGL.med.AIC)
  
  # Comparing TYGL functions at high nutrient level
  ESGR.TYGL.high.AIC <- AIC(ESGR.TYGL.HighN.nls.int, ESGR.TYGL.HighN.nls.no.int, 
                            ESGR.TYGL.HighN.lin.int, ESGR.TYGL.HighN.lin.no.int)
  ESGR.TYGL.high.AIC$model <- row.names(ESGR.TYGL.high.AIC)
  
  # Could not compare TYLA at low nutrient levels due to lack of data
  
  # Comparing TYLA functions at high nutrient level
  ESGR.TYLA.med.AIC <- AIC(ESGR.TYLA.MedN.nls.int, ESGR.TYLA.MedN.nls.no.int, 
                           ESGR.TYLA.MedN.lin.int, ESGR.TYLA.MedN.lin.no.int)
  ESGR.TYLA.med.AIC$model <- row.names(ESGR.TYLA.med.AIC)
  
  # Comparing TYLA functions at high nutrient level
  ESGR.TYLA.high.AIC <- AIC(ESGR.TYLA.HighN.nls.int, ESGR.TYLA.HighN.nls.no.int, 
                            ESGR.TYLA.HighN.lin.int, ESGR.TYLA.HighN.lin.no.int)
  ESGR.TYLA.high.AIC$model <- row.names(ESGR.TYLA.high.AIC)
}

###### Combined AIC Results ######
# combine all AIC results into single data frame
Sub_Mod_AIC_Results <- rbind(UMBS.TYAN.low.AIC, UMBS.TYAN.med.AIC, UMBS.TYAN.high.AIC,
                             UMBS.TYGL.low.AIC, UMBS.TYGL.med.AIC, UMBS.TYGL.high.AIC,
                             UMBS.TYLA.high.AIC,
                             ESGR.TYAN.low.AIC, ESGR.TYAN.med.AIC, ESGR.TYAN.high.AIC,
                             ESGR.TYGL.med.AIC, ESGR.TYGL.high.AIC,
                             ESGR.TYLA.med.AIC, ESGR.TYLA.high.AIC)

Sub_Mod_AIC_Results <- Sub_Mod_AIC_Results %>% 
  select(model, everything()) %>% 
  separate(col = model, sep = "\\.",
           into = c("site", "taxon", "n_level", NA, NA),
           extra = "drop", fill = "right",
           remove = FALSE) %>% 
  mutate(n_level = factor(n_level, levels = c("LowN", "MedN", "HighN"),
                          ordered = TRUE)) %>% 
  group_by(site, taxon, n_level) %>% 
  mutate(min_AIC = min(AIC)) %>% 
  ungroup() %>% 
  mutate(diff_AIC = AIC - min_AIC) %>% 
  arrange(site, taxon, n_level, df) %>% 
  select(model, diff_AIC, AIC, everything())
