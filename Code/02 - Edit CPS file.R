#################################################################################
#                     Fertility Postponement in the US                          #
#                      Edit Cohort Population Study                             #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


  # Edited by: Henrik-Alexander Schubert
  # Edited on: 21.04.2022


### Preparations -------------------------------------

  # Clear the environment
  rm(list = ls())
  
  
  # Load packages, functions and graphic style
  source("Functions/Packages.R")
  source("Functions/Functions.R")
  source("Functions/Graphics.R")
  
  
  # Load the data
  d <- fread("Raw_data/cps_00003.csv")
  
  # Years
  years <- 1969:2021

### Recoding variablels -----------------------------------------


  # Make names small
  names(d) <- str_to_lower(names(d))
  
  # Filter the years and women
  d <- d[year %in% years & sex == 2 & month == 6] 
  
  # Create missing values
  d$frever[d$frever == 999] <- NA # Number of live births ever had
  d$frage1[d$frage1 == 999] <- NA # Women's age in months at birth of first child
  d$frage2[d$frage2 == 999] <- NA # Women's age in months at birth of second child
  d$frage3[d$frage3 == 999] <- NA # Women's age in months at birth of third child
  d$frage4[d$frage4 == 9999] <- NA # Women's age in months at birth of fourth child
  d$frbirthy1[d$frbirthy1 == 9999] <- NA # Birth year of first child
  d$frbirthy2[d$frbirthy2 == 9999] <- NA # Birth year of second child
  d$frbirthy3[d$frbirthy3 == 9999] <- NA # Birth year of third child
  d$frbirthy4[d$frbirthy4 == 9999] <- NA # Birth year of fourth child
  
  
  # Recode the race variable
  d$race <- ifelse(d$race == 100, "white",
         ifelse(d$race == 200, "black",
                ifelse(d$race == 650, "asian", 
         "others"))) 
  

  # Create the parity information
  d <- d %>% mutate(parity = case_when(frever == 0 ~ 0,
                                       frever == 1 ~ 1,
                                       frever == 2 ~ 2,
                                       frever >= 3 ~ 3)) 
  
  # Select the variables
  d <- d %>% select(cpsid, parity, frever, year, sex, age, race, wtfinl) 
  
### Load the background information --------------------------------
    

  # Load the normal data
  d2 <- fread("Raw_data/cps_19902019.csv")
  
  # Select the variables
  d2 <- d2[ ,.(cpsid, year, serial,  sex, age,  educ, asecwt)]
  
  # Change the age variable
  d2$age <- as.numeric(d2$age)
  
  
### Combine the fertility supplement with the basic data -----------------
  
  
  # Merge the files
  d3 <- left_join(d, d2, by = c("year", "cpsid", "age"), suffix = c("", "_fertility"))
  
  # Filter the variables
  d3 <- d3 %>% filter(age >= 15 & age <= 49 & sex == 2) 
  
  # Recode age, weight, sex and educatin variable
  d3 <- d3 %>% rename(weight = asecwt)
  
  # Create a tibble
  d3 <- as_tibble(d3)
  
  # Save the data
  save(d3, file = "Data/cps_complete.Rda")
  
  
### Clean the data ------------------------------------------------------
  
  # Make education variable
  
  # Create age-groups
  age_group = cut(age, breaks = seq(15, 49, by = 5))
  
### Plot the population structure ---------------------------------------

  # Calculate the population share
  distribution <- d3 %>% group_by(year, age_group) %>% mutate(Total = n()) %>% 
    group_by(age_group, education, race, year, parity) %>% summarise(Proportion = n()/Total) 
  
  
  # Complete cases
   Distribution <- distribution %>% 
   complete(age_group, education, race, year, parity, fill = list(Proportion = 0))

  # Plot the population structure
  ggplot(distribution, aes(x = age_group, y = Proportion, group = education, fill = education)) +
    geom_col() +
    facet_wrap(year ~ race)
  
  ggplot(distribution, aes(x = year, y = parity, fill = Proportion)) +
    geom_tile()+
    facet_grid(age_group ~ race + education) + 
    theme(legend.position = "bottom") +
    scale_fill_viridis_c(option = "H")
  
  ggplot(distribution, aes(x = year, y = Proportion, group = education, colour = education)) +
    geom_line() +
    geom_point() +
    facet_grid(age_group ~ race + education) + 
    theme(legend.position = "bottom")
