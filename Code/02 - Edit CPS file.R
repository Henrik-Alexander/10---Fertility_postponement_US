#################################################################################
#                     Fertility Postponement in the US                          #
#                      Edit Cohort Population Study                             #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


  # Edited by: Henrik-Alexander Schubert
  # Edited on: 21.04.2022


### Preparations ------------------------------------------------

  # Clear the environment
  rm(list = ls())
  
  
  # Load packages, functions and graphic style
  source("Functions/Packages.R")
  source("Functions/Functions.R")
  source("Functions/Graphics.R")
  
  
  # Load the data
  d <- fread("Raw/cps_00003.csv", integer64 = "numeric")
  
  # Years
  years <- 1969:2021
  ages  <- 15:50

### Recoding variables -----------------------------------------


  # Make names small
  names(d) <- str_to_lower(names(d))
  
  # Filter the years and women
  d <- d[year %in% years & sex == 2 & month == 6 & age %in% ages] 
  
  # Create missing values
  d$frever[d$frever == 999] <- NA # Number of live births ever had
  d$frage1[d$frage1 == 999] <- NA # Woman's age in months at birth of first child
  d$frage2[d$frage2 == 999] <- NA # Woman's age in months at birth of second child
  d$frage3[d$frage3 == 999] <- NA # Woman's age in months at birth of third child
  d$frage4[d$frage4 == 9999] <- NA # Woman's age in months at birth of fourth child
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
  d2 <- fread("Raw/cps_19902019.csv", integer64 = "numeric")
  
  # Select the variables
  d2 <- d2[ ,.(cpsid, year, serial,  sex, age,  educ, asecwt, hispan)]
  
  # Replace hispanic
  d2$hispanic <- ifelse(d2$hispan == "Do not know", NA_character_,
                        ifelse(d2$hispan == "Not Hispanic", "Non-Hispanic", "Hispanic"))
  
  # Change the age variable
  d2$age <- as.numeric(d2$age)
  
  
### Combine the fertility supplement with the basic data -----------------
  
  # Merge the files
  d3 <- inner_join(as_tibble(d), d2, by = c("year", "cpsid"), suffix = c("", "_fertility"))
  
  # Filter the variables
  d3 <- d3 %>% filter(age %in% ages & sex == 2) 
  
  # Recode age, weight, sex and educatin variable
  d3 <- d3 %>% rename(weight = asecwt)
  
  # Save the data
  save(d3, file = "Data/cps_complete.Rda")
  
  
### Clean the educational data -------------------------------------------
  
  # Subset women in the fertility survey
  d <- subset(d3, !is.na(parity))
  
  # Create educational classes
  educ1 <- c("None or preschool", "Grades 1, 2, 3, or 4", "Grades 5 or 6", "Grades 7 or 8", paste("Grade", 1:11), "12th grade, no diploma")
  educ2 <- c("12th grade, diploma unclear",  "High school diploma or equivalent")
  educ3 <- c("Professional school degree", "1 year of college", paste(2:5, "years of college"), "Some college but no degree")
  educ4 <- c("6+ years of college", "Associate's degree, academic program", "Associate's degree, occupational/vocational program","Bachelor's degree",  "Master's degree", "Doctorate degree")
  
  # Make education variable
  d$education <- NA
  d[d$educ %in% educ1, ]$education <- "No high school degree"
  d[d$educ %in% educ2, ]$education <- "High school diploma"
  d[d$educ %in% educ3, ]$education <- "Some college"
  d[d$educ %in% educ4, ]$education <- "College degree"
  d[d$educ == "NIU or blank", ]$education    <- NA
  
  # Make educatin a factor variable
  d$education <- factor(d$education, levels = c("No high school degree", "High school diploma", "Some college", "College degree"))
  
### Clean the educational data -------------------------------------------
  
  # Create age-groups
  d$age_group <- cut(d$age, breaks = seq(15, 50, by = 5), include.lowest = T, right = F)
  
### Plot the population structure ---------------------------------------

  # Plot the change in the age structure
  ggplot(d, aes(year,  fill = age_group)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0))
  
  
  # Plot the change in the educational structure
  ggplot(d, aes(year,  fill = education)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0))
  
  # Hispanic
  ggplot(d, aes(year,  fill = hispanic)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0))

  # Race
  ggplot(d, aes(year,  fill = race)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                       expand = c(0, 0))
  
  # Parity distribution
  ggplot(d, aes(year,  fill = as.factor(parity))) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0))

  # Conditional distribution of age and parity
  d %>% group_by(age_group, parity) %>% summarise(share = sum(wtfinl)) %>% 
    ggplot(aes(age_group, parity, fill = share)) + 
    geom_tile() +
    theme(legend.key.width = unit(3, "cm")) +
    scale_fill_viridis_c() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  
### Estimate the exposures -----------------------------------------
  
  
  # Estimate population count
  exposure <- d %>% group_by(age_group, parity, year, hispanic, race) %>% 
    summarise(Pop = sum(wtfinl))
  
  
  # Save the exposure data
  save(exposure, file = "Data/exosure.Rda")
  
##########        END           ###########