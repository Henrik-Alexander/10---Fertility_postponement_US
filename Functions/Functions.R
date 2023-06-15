## Functions

#### Tabulate function --------------------------------------

tab <- function(...){
  tmp <- table(..., useNA = "always")
  return(tmp)
}

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

  
  # Make it a factor
  age_vec <- as.factor(age_vec)
  
  # Return
  return(age_vec)
}


##### Funcion for imputation ----------------------------------------

# Extract the imputated values
impute_births <- function(...){
  
  i <- (...)
  
  # Set the wd
  setwd("U:/male fertility/10 - Fertility_postponement_US/")
  
  # Preload the information
  source("Functions/Packages.R")
  source("Functions/Functions.R")
  
  # Load the file
  tmp <- fread(paste0("Raw/Imputation/", all_samples[i, ]))
  
  # Remove V1 column
  tmp <- tmp[, -c("V1")]

  # Expand
  tmp <- as.data.frame(lapply(tmp, rep, tmp$Births))
  
  # Deselect Births variable
  tmp <- subset(tmp, select = c(-Births, -Year))
  
  # Make all factor
  tmp <- mutate(tmp, across(everything(), as.factor))
  
  # Create predictors
  meth <- c("", "polyreg", "polyreg", "polyreg")
  
  # Create the matrix
  pred <- cbind(c(0, 1, 1, 1),
                c(1, 0, 1, 1),
                c(1, 1, 0, 1),
                c(1, 1, 1, 0))
  
  # Create names
  rownames(pred) <- colnames(pred) <- names(meth) <- c("Age", "Parity", "Education", "Ethnicity")
  
  # Impute education and create 5 datasets
  imp2 <- mice(tmp, maxit = 5,
               predictorMatrix = pred,
               method = meth, print = T)
  
  
  # Create flags for imputed values
  tmp$Imputed_Parity <- NA
  tmp$Imputed_Ethinicty <- NA
  tmp$Imputed_Education <- NA
  
  
  for(j in c("Parity", "Education", "Ethnicity")){
    
    # Get the vector of imputed values  
    imputed <- imp2$imp[[j]]
    
    
    # Select the most frequently imputed value
    # If you want to add uncertainty of imputation, do here
    imputed <- rowMode(imputed, ties = "random")

    
    # Create a flag
    tmp[, paste0("Imputed_", j)] <- imp2$where[, j]
    
    
    # Create the value
    tmp[imp2$where[, j] == 1, j] <- imputed
    
    
  }
  
  # Create a year column
  tmp$Year <- 1988 + i
  
  # Return the data
  return(tmp)
}


#### Row mode function ---------------------------------------

rowMode <- function(x, ties = NULL, include.na = FALSE) {
  # input checks data
  if ( !(is.matrix(x) | is.data.frame(x)) ) {
    stop("Your data is not a matrix or a data.frame.")
  }
  # input checks ties method
  if ( !is.null(ties) && !(ties %in% c("random", "first", "last")) ) {
    stop("Your ties method is not one of 'random', 'first' or 'last'.")
  }
  # set ties method to 'random' if not specified
  if ( is.null(ties) ) ties <- "random"
  
  # create row frequency table
  rft <- table(c(row(x)), unlist(x), useNA = c("no","ifany")[1L + include.na])
  
  # get the mode for each row
  colnames(rft)[max.col(rft, ties.method = ties)]
}

##### Function to clean the data ------------------------------------

clean_birth <- function(year, collapse, oldnames, newnames, keep){
  
  # Print the Year 
  cat(paste("Year:", year, "\n"))
  
  # Load data
  wd <- "U:/projects/1 Hooray for the J/Code Review/Raw_data/Births/"
  file <-  paste0(wd, "natl", year, ".csv")
  dat  <- fread(file=file)
  
  
  # Make lower if years > 2018
  if(year >= 2018) names(dat) <- tolower(names(dat))
  
  # Generate variables
  dat$Year  <- year
  
  # Rename variables
  setnames(dat,
           old=oldnames,
           new=newnames)
  
  # Subset
  dat <- subset(dat,
                subset=restatus != 4,
                select=keep)
  
  # Data
  dat <- collapse(dat)
  
  # Save
  file <- paste0("US_fertility_",year,".Rda")
  save(dat, file= paste0("Data/", file))
  
  # Return 
  return(dat)
  
}

##### Collapse the variables  ---------------------------------------

# 1. Create the new variables: 1989 - 2002 --------------------------
collapse_vars_1 <- function(dat){
  
# Change the variable names
tmp <- {dat} %>% filter(age_of_mother %in% 0:50) %>% 
                 mutate(
                        # Create parity inforamtion
                        Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 20), labels = FALSE),
                        # Create ethnicity
                        Ethnicity = as.factor(case_when(
                         race %in% 1:5 ~ "Hispanic",
                         race == 6 ~ "Non-hispanic white", 
                         race == 7 ~ "Non-hispanic black",
                         race == 8 ~ "Others",
                         race == 9 ~ NA_character_)),
                        # Create Education variable
                        Education = as.factor(case_when(
                          education <= 8 ~ "No high school degree",
                          education > 8 & education <= 12 ~ "High school diploma",
                          education > 12 & education < 17 ~ "Some college",
                          education == 17  ~ "College graduate" )),
                        # Create education variable
                        Age = coll_age(age_of_mother)) 


# Summarise the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>% 
    summarise(Births = sum(count), .groups = "drop") %>% 
    as_tibble()
  
return(tmp)
}


# 2. Create the new variables: 2003 - 2007 ---------------------------------
collapse_vars_2 <- function(dat){

  tmp <- {dat} %>% mutate(
                          # Create parity variable
                          Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 20), labels = FALSE),
                          # Create the new ethnicity variable
                          Ethnicity = as.factor(case_when(
                            race %in% 1:5 ~ "Hispanic",
                            race == 6 ~ "Non-hispanic white", 
                            race == 7 ~ "Non-hispanic black",
                            race == 8 ~ "Others",
                            race == 9 ~ NA_character_)),
                          # Create Education variable
                          Education = as.factor(case_when(
                            education <= 8 ~ "No high school degree",
                            education > 8 & education <= 12 ~ "High school diploma",
                            education > 12 & education < 17 ~ "Some college",
                            education == 17  ~ "College graduate" )),
                          # Create the new age classification
                          Age = as.factor(case_when(
                            age_of_mother == 1 ~ "< 15",
                            age_of_mother == 2 ~ "15 - 19",
                            age_of_mother == 3 ~ "20 - 24",
                            age_of_mother == 4 ~ "25 - 29",
                            age_of_mother == 5 ~ "30 - 34",
                            age_of_mother == 6 ~ "35 - 39",
                            age_of_mother == 7 ~ "40 - 44", 
                            age_of_mother %in% c(8, 9) ~ "45 - 50")))
    
    tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>%
      summarise(Births = n(), .groups = "drop") %>% as_tibble()
    

  return(tmp)
}

# 3. Create the new variables: 2008 - 2014 ---------------------------------
collapse_vars_3 <- function(dat){
  
  # Rename the variables
  tmp <- {dat} %>% mutate(
    # Make parity variable
    Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 20), labels = FALSE),
    # Create the new ethnicity variable
    Ethnicity = as.factor(case_when(
      race %in% 1:5 ~ "Hispanic",
      race == 6 ~ "Non-hispanic white", 
      race == 7 ~ "Non-hispanic black",
      race == 8 ~ "Others",
      race == 9 ~ NA_character_)),
    # Create the new Education classification
    Education = as.factor(case_when(
      education <= 2 ~ "No high school degree",
      education == 3 ~ "High school diploma",
      education > 3 & education < 6 ~ "Some college",
      education > 6 & education != 9 ~ "College graduate",
      education == 9 ~ NA_character_)),
    # Create the new age classification
    Age = as.factor(case_when(
      age_of_mother == 1 ~ "< 15",
      age_of_mother == 2 ~ "15 - 19",
      age_of_mother == 3 ~ "20 - 24",
      age_of_mother == 4 ~ "25 - 29",
      age_of_mother == 5 ~ "30 - 34",
      age_of_mother == 6 ~ "35 - 39",
      age_of_mother == 7 ~ "40 - 44", 
      age_of_mother %in% c(8, 9) ~ "45 - 50")))
  
  
  # Aggregate the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>%
    summarise(Births = n(), .groups = "drop") %>% as_tibble()
  
  
  return(tmp)
}


# 4. Create the new variables: 2014 - 2018 ---------------------------------
collapse_vars_4 <- function(dat){
  
  # Rename the variables
  tmp <- {dat} %>% mutate(
                          # Make parity variable
                          Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 20), labels = FALSE),
                          # Create Ethinicity variable
                          Ethnicity = as.factor(case_when(
                            race == 1 ~ "Non-hispanic white",
                            race == 2 ~ "Non-hispanic black",
                            race %in% 3:6 ~ "Others", 
                            race == 7 ~ "Hispanic",
                            race == 8 ~ NA_character_)),
                          # Create the new Education classification
                          Education = as.factor(case_when(
                            education <= 2 ~ "No high school degree",
                            education == 3 ~ "High school diploma",
                            education > 3 & education < 6 ~ "Some college",
                            education > 6 & education != 9 ~ "College graduate",
                            education == 9 ~ NA_character_)),
                          # Create the new age classification
                          Age = as.factor(case_when(
                            age_of_mother == 1 ~ "< 15",
                            age_of_mother == 2 ~ "15 - 19",
                            age_of_mother == 3 ~ "20 - 24",
                            age_of_mother == 4 ~ "25 - 29",
                            age_of_mother == 5 ~ "30 - 34",
                            age_of_mother == 6 ~ "35 - 39",
                            age_of_mother == 7 ~ "40 - 44", 
                            age_of_mother %in% c(8, 9) ~ "45 - 50")))
  
  
  # Aggregate the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>%
    summarise(Births = n(), .groups = "drop") %>% as_tibble()
  
  
  return(tmp)
}


# 5. Create the new variables: 2019 - 2021 ----------------------------------
collapse_vars_5 <- function(dat){
  
  # Rename the variables
  tmp <- {dat} %>% mutate(
                          # Create the parity variable
                          Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 20), labels = FALSE),
                          # Create ethnicity variable
                          Ethnicity = as.factor(case_when(
                            race == 1 ~ "Non-hispanic white",
                            race == 2 ~ "Non-hispanic black",
                            race %in% 3:6 ~ "Others", 
                            race == 7 ~ "Hispanic",
                            race == 8 ~ NA_character_)),
                          # Create education variable
                          Education = as.factor(case_when(
                            education <= 2 ~ "No high school degree",
                            education == 3 ~ "High school diploma",
                            education > 3 & education < 6 ~ "Some college",
                            education > 6 & education != 9 ~ "College graduate",
                            education == 9 ~ NA_character_)),
                          # Create the new age classification
                          Age = as.factor(case_when(
                            age_of_mother == 1 ~ "< 15",
                            age_of_mother == 2 ~ "15 - 19",
                            age_of_mother == 3 ~ "20 - 24",
                            age_of_mother == 4 ~ "25 - 29",
                            age_of_mother == 5 ~ "30 - 34",
                            age_of_mother == 6 ~ "35 - 39",
                            age_of_mother == 7 ~ "40 - 44", 
                            age_of_mother %in% c(8, 9) ~ "45 - 50")))
  
  
  # Aggregate the data
  tmp <- tmp %>% group_by(Age, Parity, Education, Ethnicity, Year) %>%
    summarise(Births = n(), .groups = "drop") %>% as_tibble()
  
  
  return(tmp)
}



#### Negate in function -------------------------------------

`%!in%` <- negate(`%in%`)


#####               END               ########################




