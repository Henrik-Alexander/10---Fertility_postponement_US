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
d <- fread("Raw_data/cps_00003.csv")


### Recoding variablels -----------------------------------------


# Make names small
names(d) <- str_to_lower(names(d))

# Create missing values
d$frever[d$frever == 999] <- NA
d$frage1[d$frage1 == 999] <- NA
d$frage1[d$frage1 == 999] <- NA
d$frage2[d$frage2 == 999] <- NA
d$frage3[d$frage3 == 999] <- NA
d$frage4[d$frage4 == 9999] <- NA
d$frbirthy1[d$frbirthy1 == 9999] <- NA
d$frbirthy2[d$frbirthy2 == 9999] <- NA
d$frbirthy3[d$frbirthy3 == 9999] <- NA
d$frbirthy4[d$frbirthy4 == 9999] <- NA


# Create parity
summary(d$ad )

# Drop people out of age boundaries, men, and other surveys
d <- d %>% filter(sex == 2 & month == 6 & !is.na(frever))

d <- d %>% mutate(parity = case_when(frever == 0 ~ 0,
                                     frever == 1 ~ 1,
                                     frever == 2 ~ 2,
                                     frever >= 3 ~ 3)) %>% select(cpsid, parity, frever, year, sex, age, race, wtfinl)

# Load the normal data
d2 <- haven::read_dta("Raw_data/cps_19902019.dta")
d2 <- d2 %>% select(cpsid, year, serial,  sex, age,  educ, asecwt)

# Merge the files
d3 <- left_join(d, d2, by = c("year", "cpsid", "age"), suffix = c("", "_fertility"))
d3 <- d3 %>% filter(age >= 15 & age <= 49 & sex == 2) %>% mutate( education = case_when(educ < 72 ~ "No high school degree",
                                   educ == 72 ~ "High school diploma",
                                   educ > 72 & educ <= 110 ~ "Some college",
                                   educ > 110 & educ < 999 ~ "College graduate",
                                   educ >= 999 | is.na(educ) ~ NA_character_),
                                   weight = asecwt,
                                   sex = sex,
                                   age_group = cut(age, breaks = seq(15, 49, by = 4)))






# Calculate the population share
distribution <- d3 %>% group_by(year, age_group) %>% mutate(Total = n()) %>% 
  group_by(age_group, education, race, year, parity) %>% summarise(Proportion = n()/Total) 

# Complete cases
# Distribution <- distribution %>% complete(age_group, education, race, year, parity, fill = list(Proportion = 0))

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
