#################################################################################
#                       Fertility Postponement in the US#
#                      Edit Birth Register Files                                #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


### Edit birth register files: Consistent names etc. ------------

  ## Last edited: 25.04.2022
  ## Last edited by: Henrik-Alexander Schubert

  ## Data available from:
  ## https://www.nber.org/data/vital-statistics-natality-data.html

  ## Notes:
  ## Depending on the year the data files look differently
  ## Because of this they are edited in several chunks of
  ## code



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
  years <- 1985:2004

  
  # Group of years 1: 1989-2002
  years_1   <- 1989:2002
  oldnames_1 <- c("dmage", "dlivord", "dmeduc",  "orracem", "recwt") # statenat
  newnames_1 <- c("age_of_mother", "birth_order",
                  "education", "race", "count")
  keep_1     <- c(newnames_1,"Year")
  
  
  # Group of years 2: 2003-2013
  years_2   <- 2003:2013
  oldnames_2 <- c("mager9", "lbo_rec", "meduc", "mracehisp")
  newnames_2 <- c("age_of_mother", "birth_order",
                  "education", "race")
  keep_2       <- c(newnames_2, "Year")
  
  
  # Group of years 3: 2014-2018
  years_3   <- 2014:2018
  oldnames_3 <- c("mager9", "lbo_rec", "meduc", "mracehisp")
 
  
  # Group of years 3: 2014-2018
  years_4   <- 2014:2018
  oldnames_4 <- c("mager", "lbo_rec", "meduc", "mracehisp")

### First group of years ----------------------------------------------
  
  for(year in years_1) {
  
    # Load data
    file <-  paste0(wd, "natl", year, ".csv")
    dat  <- fread(file=file)
    
    # Generate variables
    dat$Year  <- year

    # Rename variables
    setnames(dat,
             old=oldnames_1,
             new=newnames_1)
    
    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_1)
    
    # Data
    dat <- collapse_vars_1(dat)
    
    # Save
    file <- paste0("US_fertility_",year,".Rda")
    save(dat, file= paste0("Data/", file))
  
  }
  
    
### Second group of years ----------------------------------------------
  
  for(year in years_2) {
    
    # Load data
    file <- paste0(wd, "natl",year,".csv")
    dat  <- fread(file=file)
    
    # Generate variables
    dat$Year  <- year


    # Rename variables
    setnames(dat,
             old=oldnames_2,
             new=newnames_2)

    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_2)
    
    # Data
    dat <- collapse_vars_2(dat)
    
    # Save
    file <- paste0("US_fertility_",year,".Rda")
    save(dat, file= paste0("Data/", file))
    
  }  
  
  
### Third group of years ----------------------------------------------
  
  for(year in years_3) {
    
    # Load data
    file <- paste0(wd, "natl",year,".csv")
    dat  <- fread(file= file)
    
    # Make lower letters
    if(year >= 2019) names(dat) <- tolower(names(dat))
    
    # Generate variables
    dat$Year  <- year
    
    # Rename variables
    setnames(dat,
             old=oldnames_3,
             new=newnames_2)
    
    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_2)
    
    # data
    dat <- collapse_vars_3(dat)
    
    
    # Save
    file <- paste0("US_fertility_",year,".Rda")
    save(dat, file= paste0("Data/", file))
    
  }    
  
  ### Third group of years ----------------------------------------------
  
  for(year in years_4) {
    
    # Load data
    file <- paste0(wd, "natl",year,".csv")
    dat  <- fread(file= file)
    
    # Make lower letters
    names(dat) <- tolower(names(dat))
    
    # Generate variables
    dat$Year  <- year
    
    # Rename variables
    setnames(dat,
             old=oldnames_3,
             new=newnames_2)
    
    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_2)
    
    # data
    dat <- collapse_vars_4(dat)
    
    
    # Save
    file <- paste0("US_fertility_",year,".Rda")
    save(dat, file= paste0("Data/", file))
    
  }    
### Combine cross-sections -----------------------------------
  
  
  # Basic data set
  d <- dat
  
  # Load and combine the data
  for(year in 1969:2019){
    load(paste0("Data/US_fertility_", year, ".Rda"))
    d <- bind_rows(d, dat)
  }
  
  
  # Harmonize the education coding
  d$Education <- ifelse(d$Education == "High-school diploma", "High school diploma", d$Education)
  
  # Save the complete data
  save(d, file = "Data/births_complete.Rda")
  
### Multiple-Imputation  -----------------------------------

  # Scenario 1: Missing at random
  
  # Scenario 2: Basic-education missing
  
  # Scenario 3: High-education missing
  
  # Scenario 4: Medium-education missing
  
  
  
    
###############        END         ###########################  
