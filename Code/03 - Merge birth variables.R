#################################################################################
#                     Fertility Postponement in the US                          #
#                      Edit Cohort Population Study                             #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


# edited by: Henrik-Alexander Schubert
# eduted on: 15.08.2022


### Preperations -------------------------------------

  # Clear the environment
  rm(list = ls())
  
  
  # Load packages, functions and graphic style
  source("Functions/Packages.R")
  source("Functions/Functions.R")
  source("Functions/Graphics.R")
  
  
  # Basic data set
  load("Data/US_fertility_1969.Rda")
  d <- dat

### Combine Birth variables ----------------------------------------

  
  # Load and combine the data
  for(year in 1970:2020){
  load(paste0("Data/US_fertility_", year, ".Rda"))
  d <- bind_rows(d, dat)
  }
  
  
  # Plot the development of fertility across educational groups
  ggplot(d, aes(year, count, fill = Education, group = Education)) +
    geom_col() +
    facet_grid(Age ~ Parity)
  
  # How many missings per year
  d %>% mutate(miss_edu = factor(if_else(is.na(Education), 1, 0))) %>% 
    group_by(year, miss_edu) %>% summarise(miss = sum(count)) %>% View() 
    ggplot(aes(x = year, miss, color = miss_edu, group = miss_edu)) + 
    geom_line()

