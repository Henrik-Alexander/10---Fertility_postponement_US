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
  
  # Load the data
  load("Data/births_complete.Rda")

  
  
  # Plot the development of fertility across educational groups
  ggplot(d, aes(year, count, fill = Education, group = Education)) +
    geom_col() +
    facet_grid(Age ~ Parity)
  
  # How many missings per year
  d %>% mutate(miss_edu = factor(if_else(is.na(Education), 1, 0))) %>% 
    group_by(year, miss_edu) %>% summarise(miss = sum(count)) %>% View() 
    ggplot(aes(x = year, miss, color = miss_edu, group = miss_edu)) + 
    geom_line()

