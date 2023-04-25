## Functions


#### Collapse single ages -----------------------------------

coll_age <- function(age_vec){
  
  # Recode
  age_vec[age_vec < 15] <-                  "< 15"
  age_vec[age_vec >= 15 & age_vec < 20] <-  "15 - 19"
  age_vec[age_vec >= 20 & age_vec < 25] <-  "20 - 24"
  age_vec[age_vec >= 25 & age_vec < 30] <-  "25 - 29"
  age_vec[age_vec >= 30 & age_vec < 35] <-  "30 - 34"
  age_vec[age_vec >= 35 & age_vec < 40] <-  "35 - 39"
  age_vec[age_vec >= 40 & age_vec < 45] <-  "40 - 44"
  age_vec[age_vec >= 45 & age_vec <= 50] <- "45 - 50"

  
  
  
  # Return
  return(age_vec)
}


##### Collapse the variables  ---------------------------------------

# 1. Create the new variables: 1989 - 2002 --------------------------
collapse_vars_1 <- function(dat){
  
# Change the variable names
tmp <- {dat} %>% mutate(
                        # Create parity inforamtion
                        Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 10)),
                        # Create ethnicity
                        Ethnicity = case_when(
                         race %in% 1:5 ~ "Hispanic",
                         race == 6 ~ "Non-hispanic white", 
                         race == 7 ~ "Non-hispanic black",
                         race == 8 ~ "Others",
                         race == 9 ~ NA_character_),
                        # Create Education variable
                        Education = case_when(
                          education <= 8 ~ "No high school degree",
                          education > 8 & education <= 12 ~ "High school diploma",
                          education > 12 & education < 17 ~ "Some college",
                          education == 17  ~ "College graduate" ),
                        # Create education variable
                        Age = coll_age(age_of_mother)) 


# Summarise the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>% 
    summarise(Pop = sum(count), .groups = "drop") %>% 
    as_tibble()
  
return(tmp)
}


# 2. Create the new variables: 2003 - 2018 ---------------------------------
collapse_vars_2 <- function(dat){

  tmp <- {dat} %>% mutate(
                          # Create parity variable
                          Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 10)),
                          # Create the new ethnicity variable
                          Ethnicity = case_when(
                            race %in% 1:5 ~ "Hispanic",
                            race == 6 ~ "Non-hispanic white", 
                            race == 7 ~ "Non-hispanic black",
                            race == 8 ~ "Others",
                            race == 9 ~ NA_character_),
                          # Create the new education variable
                          Education = case_when(
                           education <= 2 ~ "No high school degree",
                           education == 3 ~ "High school diploma",
                           education > 3 & education < 6 ~ "Some college",
                           education > 6 & education != 9 ~ "College graduate",
                           education == 9 ~ NA_character_),
                          # Create the new age classification
                          Age = case_when(
                            age_of_mother == 1 ~ "< 15",
                            age_of_mother == 2 ~ "15 - 19",
                            age_of_mother == 3 ~ "20 - 24",
                            age_of_mother == 4 ~ "25 - 29",
                            age_of_mother == 5 ~ "30 - 34",
                            age_of_mother == 6 ~ "35 - 39",
                            age_of_mother == 7 ~ "40 - 44", 
                            age_of_mother %in% c(8, 9) ~ "45 - 50"))
    
    tmp <- tmp %>% group_by(Age, Parity, Education, Race, Year) %>%
      summarise(count = n(), .groups = "drop") %>% as_tibble()
    

  return(tmp)
}

# 3. Create the new variables: 2014 - 2021 ---------------------------------
collapse_vars_3 <- function(dat){
  
  # Rename the variables
  tmp <- {dat} %>% mutate(
                          # Make parity variable
                          Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 10)),
                          # Create Ethinicity variable
                          Ethnicity = case_when(
                            race == 1 ~ "Non-hispanic white",
                            race == 2 ~ "Non-hispanic black",
                            race %in% 3:6 ~ "Others", 
                            race == 7 ~ "Hispanic",
                            race == 8 ~ NA_character_),
                          # Create the new Education classification
                          Education = case_when(
                            education <= 2 ~ "No high school degree",
                            education == 3 ~ "High school diploma",
                            education > 3 & education < 6 ~ "Some college",
                            education > 6 & education != 9 ~ "College graduate",
                            education == 9 ~ NA_character_),
                          # Create the new age classification
                          Age = case_when(
                            age_of_mother == 1 ~ "< 15",
                            age_of_mother == 2 ~ "15 - 19",
                            age_of_mother == 3 ~ "20 - 24",
                            age_of_mother == 4 ~ "25 - 29",
                            age_of_mother == 5 ~ "30 - 34",
                            age_of_mother == 6 ~ "35 - 39",
                            age_of_mother == 7 ~ "40 - 44", 
                            age_of_mother %in% c(8, 9) ~ "45 - 50"))
  
  
  # Aggregate the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>%
    summarise(count = n(), .groups = "drop") %>% as_tibble()
  
  
  return(tmp)
}


# 4. Create the new variables: 2019 - 2021 ----------------------------------
collapse_vars_4 <- function(dat){
  
  # Rename the variables
  tmp <- {dat} %>% mutate(
                          # Create the parity variable
                          Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 10)),
                          # Create ethnicity variable
                          Ethnicity = case_when(
                            race == 1 ~ "Non-hispanic white",
                            race == 2 ~ "Non-hispanic black",
                            race %in% 3:6 ~ "Others", 
                            race == 7 ~ "Hispanic",
                            race == 8 ~ NA_character_),
                          # Create education variable
                          Education = case_when(
                            education <= 2 ~ "No high school degree",
                            education == 3 ~ "High school diploma",
                            education > 3 & education < 6 ~ "Some college",
                            education > 6 & education != 9 ~ "College graduate",
                            education == 9 ~ NA_character_),
                          # Create age variable
                          Age = coll_age(age_of_mother))
  
  
  # Aggregate the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>%
    summarise(count = n(), .groups = "drop") %>% as_tibble()
  
  
  return(tmp)
}



#### Negate in function -------------------------------------

`%!in%` <- negate(`%in%`)


#####               END               ########################




