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
  
  # Years
  years <- 1969:2021
  ages  <- 15:50

### Recoding variables -----------------------------------------

  
if(file.exists("Data/cps_fert_cleaned.Rda")){
  
  # Load the cleaned fertility data
  load("Data/cps_fert_cleaned.Rda")
  
}else{
  
  # Load the data
  d <- fread("Raw/cps_00003.csv", integer64 = "numeric")

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
  d <- d |>  mutate(parity = case_when(frever == 0 ~ 0,
                                       frever == 1 ~ 1,
                                       frever == 2 ~ 2,
                                       frever >= 3 ~ 3)) 
  
  # Select the variables
  d <- d |>  select(cpsid, parity, frever, year, sex, age, race, wtfinl) 
  
  # Save the data
  save(d, file = "Data/cps_fert_cleaned.Rda")
}
  
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
  
  if(file.exists("Data/cps_complete.Rda")){
    
    # Load the data
    load("Data/cps_complete.Rda")
  }else{
  
  # Merge the files
  d3 <- inner_join(as.data.frame(d), d2, by = c("year", "cpsid"), suffix = c("", "_fertility"))
  
  # Filter the variables
  d3 <- d3 |>  filter(age %in% ages & sex == 2) 
  
  # Recode age, weight, sex and educatin variable
  d3 <- d3 |>  rename(weight = asecwt)
  
  # Save the data
  save(d3, file = "Data/cps_complete.Rda")
  }
  
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
  
### Clean the age data --------------------------------------------------
  
  # Create age-groups
  d$age_group <- coll_age(d$age)
  
### Harmonize the race data ---------------------------------------------
  
  # Ethnicity of mixed origin
  d$ethnicity <- "Others"
  
  # Non-Hispanic White
  d[d$hispanic == "Non-Hispanic" & d$race == "white" & !is.na(d$hispanic) & !is.na(d$race), ]$ethnicity <- "Non-hispanic white"
  
  # Non-Hispanic Black
  d[d$hispanic == "Non-Hispanic" & d$race == "black" & !is.na(d$hispanic) & !is.na(d$race), ]$ethnicity <- "Non-hispanic black"
  
  # Hispanics
  d[d$hispanic == "Hispanic" & !is.na(d$hispanic), ]$ethnicity <- "Hispanic"
  
  # Missing
  d[is.na(d$hispanic) | is.na(d$race), ] <- NA
  
### Remove missings -----------------------------------------------------
  
  # Filter missing data
  d <- d[!is.na(d$cpsid), ]

### Plot the population structure ---------------------------------------

  # Plot the change in the age structure
  ggplot(d, aes(year,  fill = age_group)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0))
  
  # Save the plot
  ggsave(last_plot(), filename = "Figures/age_distr_cbs.pdf")
  
  # Plot the change in the educational structure
  ggplot(subset(d, !is.na(education)), aes(year,  fill = education)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  # Save the plot
  ggsave(last_plot(), filename = "Figures/edu_distr_cbs.pdf")
  
  # Hispanic
  ggplot(d, aes(year,  fill = hispanic)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d(name = "") +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0))
  
  # Save the plot
  ggsave(last_plot(), filename = "Figures/hisp_distr_cbs.pdf")

  # Race
  ggplot(d, aes(year,  fill = race)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                       expand = c(0, 0))
  
  # Save the plot
  ggsave(last_plot(), filename = "Figures/race_distr_cbs.pdf")
  
  
  # Ethnicity
  ggplot(d, aes(year,  fill = ethnicity)) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                       expand = c(0, 0))
  
  # Save the plot
  ggsave(last_plot(), filename = "Figures/ethnicity_distr_cbs.pdf")
  
  # Parity distribution
  ggplot(d, aes(year,  fill = as.factor(parity))) +
    geom_histogram(aes(weight = wtfinl)) +
    ylab(NULL) + 
    scale_fill_viridis_d(name = "Parity:") +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), expand = c(0, 0)) +
    ggtitle("Parity distribution over time in the CPS")

  # Save the plot
  ggsave(last_plot(), filename = "Figures/par_distr_cbs.pdf")
  
  # Conditional distribution of age and parity
  d |> 
    group_by(age_group, parity) |>  
    summarise(share = sum(wtfinl)) |>  
    ggplot(aes(age_group, parity, fill = share)) + 
    geom_tile() + xlab("Age") +
    theme(legend.key.width = unit(3, "cm")) +
    scale_fill_viridis_c(labels = unit_format(unit = "M", scale = 1e-6)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    ggtitle("Age and parity distribution in the CPS")
  
  # Save the plot
  ggsave(last_plot(), filename = "Figures/par-age_distr_cbs.pdf")
  
### Estimate the exposures -----------------------------------------
  
  
  # Estimate population count with non-missing data
  exp_nm <- na.omit(d) |>  
    group_by(age_group, parity, year, hispanic, ethnicity, education) |>  
    summarise(Pop_nm = sum(wtfinl), .groups = "drop") |>  
    rename(age = age_group)
  
  # Create names
  names(exp_nm) <- str_to_title(names(exp_nm))
  
  
### Multiple imputation of education -------------------------------
  
  if(file.exists("Data/imputed_cps.Rda")){
    
    # Load the data
    load("Data/imputed_cps.Rda")
  
    }else{
  
  # Imputations
  n_imp <- 5
  
  # Run multiple imputation
  imp <- mice(d, maxit = 0)
  
  # Extract predictor matrix and methods of imputation
  meth <- imp$method
  predM <- imp$predictorMatrix
  
  # Variables not used for imputation, set 0
  predM[, c("age_fertility")] <- 0
  meth[meth == "pmm"] <- ""
  
  
  # Impute education and create 5 datasets
  imp2 <- mice(d, maxit = n_imp,
               predictorMatrix = predM,
               method = meth, print = T)
  
  # Extract the imputated values
  imp_educ <- imp2$imp$education
  
  # Extract where missign-values are located
  where <- imp2$where[, "education"]
  
### Impute the data ------------------
  
  # Create a container
  exp_imp <- list()

  # Estimate with multiple imputations
  for(i in 1:n_imp){
    
    # Create a temporary data
   tmp <- d
    
    # Insert the imputed values
    tmp[where, "education"] <- imp_educ[, i]
    
    # Aggregate the results
    tmp <- tmp |>  
      group_by(age_group, parity, year, hispanic, ethnicity, education) |>  
      summarise(Iter = i, Pop = sum(wtfinl), .groups = "drop") |>  
      rename(age = age_group)  
    
    # Create names
     names(tmp) <- str_to_title(names(tmp))
    
    # Store the results
    exp_imp[[i]] <- tmp
  }
  
  # Combine 
  exp_imp <- do.call(rbind, exp_imp)
  
  # Save the imputed data
  save(exp_imp, file = "Data/imputed_cps.Rda")
  
}

### Aggregate the data -----------------------------------
  

  # Low, mean, high
  exp_imp <- exp_imp |> 
    group_by(Age, Parity, Year, Hispanic, Ethnicity, Education) |>  
    summarise(min_exp = min(Pop),
              mean_exp = mean(Pop),
              max_exp = max(Pop), 
              .groups = "drop")
  
  
  # Join the different exposures
  exposure <- inner_join(exp_imp, exp_nm)
  
  
  # Save the exposure data
  save(exposure, file = "Data/exposure.Rda")
  
##########        END           ###########