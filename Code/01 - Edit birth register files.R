#################################################################################
#                       Fertility Postponement in the US#
#                      Edit Birth Register Files                                #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


### Edit birth register files: Consistent names etc. ------------

  ## Last edited: 25.04.2023
  ## Last edited by: Henrik-Alexander Schubert

  ## Data available from:
  ## https://www.nber.org/data/vital-statistics-natality-data.html

  ## Notes:
  ## Depending on the year the data files look differently
  ## Because of this they are edited in several chunks of code


### Loading the natality files ###############################################

  # Increase the timeout time
  #options(timeout = max(300, getOption("timeout")))
  
  #for(i in 1989:2021){
    
    # Temporary file direction
  #  temp <- tempfile()
    
    # Create the web-page
  #  webpage <- paste0("https://data.nber.org/natality/", i, "/natl", i, ".csv.zip")
    
    # Download the file
  #  download.file(webpage, temp, quite = T)
    
    # Load the data
  #  unzip(temp, exdir = "Raw/Births")
    
  #  cat("Data for ", i, "is saved in /Raw. \n")
    
  #} 

### Packages & settings -------------------------------------------

  # Identifying missing packages
  rm(list = ls())

  # Load packages, functions and graphic style
  source("Functions/Packages.R")
  source("Functions/Functions.R")
  source("Functions/Graphics.R")

### Loading the data ----------------------------------------------

  # Location of the old time series
  wd <- "U:/male fertility/1 Hooray for the J/Code Review/Raw_data/Births/" 
    
  # Years to cover
  years <- 1989:2004

  
  # Group of years 1: 1989-2002
  years_1   <- 1989:2002
  oldnames_1 <- c("dmage", "dlivord", "dmeduc",  "orracem", "recwt") # statenat
  newnames_1 <- c("age_of_mother", "birth_order",
                  "education", "race", "count")
  keep_1     <- c(newnames_1,"Year")
  
  
  # Group of years 2: 2003-2007
  years_2   <- 2003:2006
  oldnames_2 <- c("mager9", "lbo_rec", "dmeduc", "mracehisp")
  newnames_2 <- c("age_of_mother", "birth_order",
                  "education", "race")
  keep_2       <- c(newnames_2, "Year")
  
  # Group of years 3: 2003-2007
  years_3   <- 2007:2013
  oldnames_3 <- c("mager9", "lbo_rec", "meduc", "mracehisp")
  
  # Group of years 4: 2014-2018
  years_4   <- 2014:2018
  oldnames_4 <- c("mager9", "lbo_rec", "meduc", "mracehisp")
 
  
  # Group of years 5: 2019-2021
  years_5   <- 2019:2021
  oldnames_5 <- c("mager9", "lbo_rec", "meduc", "mracehisp")
  
  

### Clean the data -----------------------------------------------  
  
# Create the container
result <- list()
  
### First group of years 
result[[1]] <- mclapply(years_1, FUN = clean_birth,
                   collapse_vars_1, oldnames_1, newnames_1, keep_1)
  
    
### Second group of years
result[[2]] <- mclapply(years_2, FUN = clean_birth,
                   collapse_vars_2, oldnames_2, newnames_2, keep_2)

  
  
### Third group of years
result[[3]] <- mclapply(years_3, FUN = clean_birth,
                   collapse_vars_3, oldnames_3, newnames_2, keep_2)  
  
  
### Fourth group of years
result[[4]] <- mclapply(years_4, FUN = clean_birth,
                   collapse_vars_4, oldnames_4, newnames_2, keep_2)  
  
### Fifth group of years 
result[[5]] <- mclapply(years_5, FUN = clean_birth,
                   collapse_vars_5, oldnames_5, newnames_2, keep_2) 
  


### Combine the results --------------------------------------

# Combine the results 
d <- rbind(do.call(rbind, result[[1]]),
      do.call(rbind, result[[2]]),
      do.call(rbind, result[[3]]),
      do.call(rbind, result[[4]]),
      do.call(rbind, result[[5]]))


  # Make factor varibles
  d <- mutate(d, across(c(Age, Parity, Education, Ethnicity), as.factor))


  # Save the complete data
  save(d, file = "Data/births_complete.Rda")
  
### Births graphs ------------------------------------------
  
  # Plot the distribution of births over time
  ggplot(d, aes(Year, Births, fill = Education)) +
    geom_col() +
    facet_grid(Age ~ Ethnicity) +
    scale_x_continuous(n.breaks = 10) +
    theme(axis.text.x = element_text(angle = 45, vjust = -0.001))
  
  
  
### Multiple imputation of education, ethnicity and parity --------------------
  
  load("Data/births_complete.Rda")
  
  # Save the data
  lapply(1989:2021, function(x){  write.csv(d[d$Year == x, ], file = paste0("Raw/Imputation/imp_", x, ".csv"))})
  
  # Create the names vector
  all_samples <- as.data.frame(list.files("Raw/Imputation/"), include.dirs = T)
  seq_id_all <- seq_along(1:nrow(all_samples))

# --------
  
  # Get the number of cores
  no_cores <- detectCores(logical = T)
  
  # Register number of Cluster
  cl <- makeCluster(33)
  
  # Export to clusters
  clusterExport(cl, list('impute_births', 'all_samples'))
  
# -------
  
  # Run the imputation
  births_imputed <- parLapply(cl, seq_id_all, fun = impute_births)
  
  # Combine the results
  births_imputed <- do.call(rbind, births_imputed)
  
  # Save the exposure data
  save(births_imputed, file = "Data/births_imputed.Rda")
  
  # Aggregate
  births_imputed <- births_imputed %>% group_by(Age, Parity,  Education, Ethnicity, Year) %>% 
    count()
  
  # Save the data
  save(births_imputed, file = "Data/births_complete.Rda")
  
  # Plot the development of fertility across educational groups
  ggplot(births_imputed, aes(x = Year, y = n, fill = Education, group = Education)) +
    geom_col() +
    facet_grid(Age ~ Parity)
  

    
###############        END         ###########################  
