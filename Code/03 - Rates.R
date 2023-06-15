#################################################################################
#                     Fertility Postponement in the US                          #
#                      Edit Cohort Population Study                             #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


# edited by: Henrik-Alexander Schubert
# eduted on: 27.04.2023


### Preperations ------------------------------------------------

  # Clear the environment
  rm(list = ls())
  
  
  # Load packages, functions and graphic style
  source("Functions/Packages.R")
  source("Functions/Functions.R")
  source("Functions/Graphics.R")
  
  # Load the data
  load("Data/births_complete.Rda")
  load("Data/exposure.Rda")

### Estimate birth rates  -----------------------------------
 
# Set back the parity for births
exposure$Parity <- factor(exposure$Parity + 1)
    
# Combine the data
d <- inner_join(births_imputed, exposure)
  
# Estimate overall ASFR
asfr <- d %>% group_by(Age, Year) %>%
  summarise(ASFR = sum(n) / sum(Pop_nm), .groups = "drop")


# Estimate overall TFR
tfr <- asfr %>% group_by(Year) %>% 
  summarise(TFR = sum(5 *ASFR), .groups = "drop")

# Mean age of childbearing

# Estimate group specific-TFR
asfr_ethn <- d %>% group_by(Age, Year, Ethnicity) %>%
  summarise(ASFR = sum(n) / sum(mean_exp), .groups = "drop")  





### Plots --------------------------------------------------


# Plot the ASFR
ggplot(asfr, aes(Age, ASFR, colour = Year, group = Year)) +
  geom_smooth(se = F, method = "loess", alpha = 0.5) +
  scale_colour_gradient(low = "red", high = "navyblue")

# Plot the TFR
ggplot(tfr, aes(Year, TFR/2)) +
  geom_line()

# Plot the ASFR for groups
ggplot(asfr_ethn, aes(Age, ASFR, group = Ethnicity, colour = Ethnicity)) +
  geom_smooth(se = F) +
  facet_wrap(~ Year)



########              END                ################
