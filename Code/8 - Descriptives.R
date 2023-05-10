#################################################################################
#                     Fertility Postponement in the US                          #
#                      Edit Cohort Population Study                             #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


# Edited by: Henrik-Alexander Schubert
# Edited on: 25.04.2022


### Preparations ------------------------------------------------

# Clear the environment
rm(list = ls())


# Load packages, functions and graphic style
source("Functions/Packages.R")
source("Functions/Functions.R")
source("Functions/Graphics.R")


# Load the data
age_birth <- read.csv("Raw/birth_age_educ.csv", sep = ";", skip = 1)

# Years
years <- 1969:2021
ages  <- 15:50


### Plot the age at childbearing by educ ---------------------------------

# Create Plot
age_birth <- age_birth %>% pivot_longer(cols = !X, names_to = "Education", values_to = "Median Age") 

# Change the label
age_birth$Education <- str_replace_all(age_birth$Education, "\\.", " ")

# Create label
ggplot(age_birth, aes(X, `Median Age`, colour = Education, group = Education, shape = Education)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 3) +
  geom_text(data = subset(age_birth, X == "1981-198"), aes(X,  `Median Age`, label = Education), hjust = 0.7, vjust = -1.1, family = "serif", fontface = "bold") + 
  scale_colour_viridis_d() +
  scale_shape_discrete(solid = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Cohort") +
  guides(colour = "none", shape = "none") 
