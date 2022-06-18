# 
# 
# 
# 
# This file combines the Typha Data Adjustment and Typha Data Manipulation
# files into a single script. They were originally separate due to the need for
# an interim output for separate analyses. 


#### Import Data ####

# Import the published data CSV
Typha_Data_original <- read_csv("Data/Jameson_etal_Typha_Data.csv")

# Call helper functions
#source("R Scripts/Functions.R")


#### Initial Data Manipulation ####

# Typha_Data contains both ESGR and UMBS Data

# Select variables of interest
Typha_Data <- Typha_Data_original %>% 
  # Turn Taxon into a factor variable
  mutate(Taxon = as.factor(Taxon),
         # turn tank into a factor variable
         tank = as.factor(tank),
         u_tank = as.factor(str_c(site, tank, sep = "_"))) %>% 
  # Remove observations with missing rametID
  filter(!is.na(rametID)) %>% 
  # The genetic analyses revealed that back-crosses of some sort were planted in
  # ESGR tank 41 in place of one of the other taxa. We don't know which taxon
  # was replaced by the back-cross, so we decided to remove ESGR tank 41 
  # observations below
  filter(u_tank != "ESGR_41") %>% 
  # Create a unique identifier for each sample
  unite("UID", site:rametID, remove = FALSE, sep = "_")

# Calculate the number of tanks in our study that do not contain TYLA
Typha_Data %>% 
  complete(Taxon, u_tank) %>% 
  group_by(Taxon) %>% 
  summarise(n_missing = sum(is.na(UID))) 

# This should have one more level with 0 than indicated above because there were
# TYLA observations in ESGR tank 41
summary(Typha_Data[Typha_Data$Taxon == "TYLA", ]$u_tank)

summary(Typha_Data[Typha_Data$Taxon == "TYGL", ]$u_tank)

#### Dummy Variables ####

Complete_Typha_Data_edited <- Typha_Data %>%
  # dummy variable for n-level group medium
  mutate(dummy_NL_med = as.numeric(N_Level_Group == "med"),
         # dummy variable for n-level group high
         dummy_NL_high = as.numeric(N_Level_Group == "high"),
         # dummy variable for n-level group low
         dummy_NL_low = as.numeric(N_Level_Group == "low"),
         # dummy variable for TYAN
         dummy_TYAN = as.numeric(Taxon == "TYAN"), 
         # dummy variable for TYGL
         dummy_TYGL = as.numeric(Taxon == "TYGL"),
         # dummy variable for TYLA
         dummy_TYLA = as.numeric(Taxon == "TYLA"),
         # dummy variable for ESGR
         dummy_ESGR = as.numeric(site == "ESGR"),
         # Turn N-level-group into factor variable
         N_Level_Group = factor(N_Level_Group, ordered = FALSE, 
                                levels = c("low", "med", "high")),
         # Turn Site into factor variable
         site = factor(site, ordered = TRUE),
         site = factor(site, levels = rev(levels(site)), ordered = FALSE)) %>% 
  filter(Infl_Weight >= 0, Veg_Weight >= 0)

#### Check Data ####

# Check that all the factor variables are ordered properly

levels(Complete_Typha_Data_edited$N_Level_Group)
levels(Complete_Typha_Data_edited$Taxon)
levels(Complete_Typha_Data_edited$site)

class(Complete_Typha_Data_edited$N_Level_Group)
class(Complete_Typha_Data_edited$Taxon)
class(Complete_Typha_Data_edited$site)

# Check the data to make sure everything looks good
# Now we want to check flowering status
ggplot(data = Complete_Typha_Data_edited) + 
  geom_point(aes(y = RepStatusTF, x = Veg_Weight,
                 color = Taxon))

Complete_Typha_Data_edited %>% filter(is.na(RepStatusTF)) %>% print()

# Look at the data a different way 
ggplot() + 
  geom_point(data = Complete_Typha_Data_edited[Complete_Typha_Data_edited$RepStatusTF == 1,], 
             aes(x = Veg_Weight, y = Infl_Weight, color = Taxon))

# Double check that site isn't missing
Complete_Typha_Data_edited %>% 
  filter(is.na(site))

# At this point everything should be good.
# Save with non-flowering observations
Typha_Log_Model_Data <- Complete_Typha_Data_edited


###### Outlier Graph ######

# Keep only the flowering individuals
flower_data <- Complete_Typha_Data_edited %>% 
  filter(RepStatusTF == 1) %>% 
  mutate(assigned_ID = vquestion_sp_fct(FieldID))
  
# Compare vegetative weight to inflorescent weight
outlier_graph <- ggplot(data = flower_data, 
                        mapping = aes(x = Veg_Weight, y = Infl_Weight,
                                      color = assigned_ID, 
                                      shape = assigned_ID)) + 
  # add in initial open observations
  geom_point() +
  # Cover with the filled "weird" observations
  geom_point(aes(fill = assigned_ID, alpha = (Outlier))) +
  
  scale_color_manual("Field ID",
                     labels = c("Non-native", "Hybrid", "Native"),
                     values = c("tan1", "deepskyblue4", "deeppink3")) +
  scale_shape_manual("Field ID",
                     labels = c("Non-native", "Hybrid", "Native"),
                     values = c(22, 21, 24)) +
  scale_fill_manual("Field ID",
                    labels = c("Non-native", "Hybrid", "Native"),
                    values = c("tan1", "deepskyblue4", "deeppink3"),
                    na.value = NA) +
  scale_alpha_manual("Outlier",
                     labels = c("Non-outlier", "Outlier"),
                     values = c("FALSE" = 0, "TRUE" = 1),
                     guide = "none") +
  xlim(0, 100) +
  # Specify title and axes lables
  xlab("Vegetative Biomass (g)") + 
  ylab("Reproductive Biomass (g)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 12),                # x-axis text size
        axis.title.y = element_text(size = 12),                # y-axis text size
        plot.title = element_text(size = 12),                  # Title text size
        legend.text = element_text(size = 10),                 # Legend text size
        legend.title = element_text(face = 'bold', size = 10), # Legend title format (bold)
        axis.text = element_text(size = 10))

#outlier_graph

#### Final Mixed Effects Model Data #####

# Format the data appropriately for mixed effects model
Typha_allocation_model_data <- Complete_Typha_Data_edited %>% 
  filter(RepStatusTF == 1) %>% 
  # We need to remove the extra levels in u_tank that don't have any observations
  mutate(u_tank = factor(u_tank))

# Divide into UMBS and ESGR
UMBS_Typha <- Typha_allocation_model_data %>% filter(site == "UMBS")
ESGR_Typha <- Typha_allocation_model_data %>% filter(site == "ESGR")

##### Summary Figures #####

# Look at the new ID values in the graphs
Typha_allocation_model_data %>%
  ggplot(data = .) + 
  geom_point(aes(x = Veg_Weight, y = Infl_Weight, 
                 color = Taxon, shape = gen_checked)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(4,19)) +
  labs(title = "Combined Site Data")

ggplot(data = UMBS_Typha, aes(x = Veg_Weight, y = Infl_Weight)) +
  geom_point(aes(color = Taxon, shape = gen_checked)) +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(4,19)) +
  labs(title = "UMBS Data")

ggplot(data = ESGR_Typha, 
       mapping = aes(x = Veg_Weight, y = Infl_Weight, 
                     color = Taxon, shape = gen_checked)) + 
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(4,19)) +
  labs(title = "ESGR Data")

#### Sub-Group Datasets ####

# For comparing different functional forms for our model, we need to subset
# the data in smaller dataframes. Each mini-dataset has a unique combination
# of taxon, site, and nutrient level/group.

# Look at all the sub-groups

ggplot(data = Complete_Typha_Data_edited) +
  geom_point(aes(x = Veg_Weight, y = Infl_Weight, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)

ggplot(data = Typha_allocation_model_data) +
  geom_point(aes(x = Veg_Weight, y = Infl_Weight, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)

# Save the figure
sub_group_flowering <- ggplot(data = Typha_allocation_model_data) +
  geom_point(aes(x = Veg_Weight, y = Infl_Weight, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)
# 
# ggsave("SubGroup ME Data Figure.png",
#        plot = sub_group_flowering,
#        device = "png",
#        path = "/Users/emilyjameson/Desktop/Research 2/Figures/Graphs and Tables",
#        width = 8, height = 10, units = "in")

{
  # UMBS TYAN
  UMBS_TYAN_Low_N <- UMBS_Typha %>% 
    filter(Taxon == "TYAN", N_Level_Group == "low") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  UMBS_TYAN_Med_N <- UMBS_Typha %>% 
    filter(Taxon == "TYAN", N_Level_Group == "med") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  UMBS_TYAN_High_N <- UMBS_Typha %>% 
    filter(Taxon == "TYAN", N_Level_Group == "high") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  
  # UMBS TYGL
  UMBS_TYGL_Low_N <- UMBS_Typha %>% 
    filter(Taxon == "TYGL", N_Level_Group == "low") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  UMBS_TYGL_Med_N <- UMBS_Typha %>% 
    filter(Taxon == "TYGL", N_Level_Group == "med") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  UMBS_TYGL_High_N <- UMBS_Typha %>% 
    filter(Taxon == "TYGL", N_Level_Group == "high") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  
  # UMBS TYLA
  UMBS_TYLA_Low_N <- UMBS_Typha %>% 
    filter(Taxon == "TYLA", N_Level_Group == "low") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  UMBS_TYLA_Med_N <- UMBS_Typha %>% 
    filter(Taxon == "TYLA", N_Level_Group == "med") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  UMBS_TYLA_High_N <- UMBS_Typha %>% 
    filter(Taxon == "TYLA", N_Level_Group == "high") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  
  # ESGR TYAN
  ESGR_TYAN_Low_N <- ESGR_Typha %>% 
    filter(Taxon == "TYAN", N_Level_Group == "low") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  ESGR_TYAN_Med_N <- ESGR_Typha %>% 
    filter(Taxon == "TYAN", N_Level_Group == "med") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  ESGR_TYAN_High_N <- ESGR_Typha %>% 
    filter(Taxon == "TYAN", N_Level_Group == "high") %>%
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  
  # ESGR TYGL
  ESGR_TYGL_Low_N <- ESGR_Typha %>% 
    filter(Taxon == "TYGL", N_Level_Group == "low") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  ESGR_TYGL_Med_N <- ESGR_Typha %>%
    filter(Taxon == "TYGL", N_Level_Group == "med") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  ESGR_TYGL_High_N <- ESGR_Typha %>% 
    filter(Taxon == "TYGL", N_Level_Group == "high") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  
  # ESGR TYLA
  ESGR_TYLA_Low_N <- ESGR_Typha %>% 
    filter(Taxon == "TYLA", N_Level_Group == "low") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  ESGR_TYLA_Med_N <- ESGR_Typha %>%
    filter(Taxon == "TYLA", N_Level_Group == "med") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  ESGR_TYLA_High_N <- ESGR_Typha %>% 
    filter(Taxon == "TYLA", N_Level_Group == "high") %>% 
    mutate(Taxon = factor(Taxon),
           u_tank = factor(u_tank))
  }

#### Logistic Model Manipulation ####

# Check a couple variables

summary(Typha_Log_Model_Data$Taxon)
summary(Typha_Log_Model_Data$N_Level_Group)
summary(Typha_Log_Model_Data$site)
summary(Typha_Log_Model_Data$u_tank)

# Check data is organized
ggplot(data = Typha_Log_Model_Data) + geom_point(aes(y = Infl_Weight, x = Veg_Weight))

ggplot(data = Typha_Log_Model_Data) + 
  geom_point(aes(y = RepStatusTF, x = Veg_Weight,
                 color = Taxon))

ggplot(data = Typha_Log_Model_Data) +
  geom_point(aes(x = Veg_Weight, y = RepStatusTF, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)

# There appear to be a couple of outlying TYAN observations in ESGR Medium 
# nutrient level. This is addressed prior to the logistic regression model.
# Formal outlier analysis in the logistic regression code in the next script

# # Save the figure
sub_group_log_figure <- ggplot(data = Typha_Log_Model_Data) +
  geom_point(aes(x = Veg_Weight, y = RepStatusTF, color = Taxon)) +
  facet_grid(Taxon + site ~ N_Level_Group)
# 
# ggsave("SubGroup Log Data Figure.png",
#        plot = sub_group_log_figure,
#        device = "png",
#        path = "/Users/emilyjameson/Desktop/Research 2/Figures/Graphs and Tables",
#        width = 8, height = 10, units = "in")
