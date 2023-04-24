#################################################################################
#                       Fertility Postponement in the US#
#                      Edit Birth Register Files                                #
#               Max-Planck Institute for Demographic Research                   #
#################################################################################


### Edit birth register files: Consistent names etc. ------------

  ## Last edited: 12.08.2022
  ## Last edited by: Henrik-Alexander Schubert

  ## Data available from:
  ## https://www.nber.org/data/vital-statistics-natality-data.html

  ## Notes:
  ## Depending on the year the data files look differently
  ## Because of this they are edited in several chunks of
  ## code



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
  years <- 1969:2004

  # Group of years 1: 1969-1977
  years_1    <- 1969:1977
  oldnames_1 <- c("dmage","dlivord", "dmeduc", "mrace" ) # statenat
  newnames_1 <- c("age_of_mother", "birth_order",
                "education", "race")
  keep_1     <- c(newnames_1,"year","count")
  
  # Group of years 2: 1978-2002
  years_2    <- 1978:2002
  oldnames_2 <- c("dmage","dlivord", "dmeduc",  "mrace" , "recwt") # statenat
  newnames_2 <- c("age_of_mother", "birth_order",
                  "education", "race", "count")
  keep_2     <- c(newnames_2,"year")
  
  
  # Group of years 4: 2003-2014
  years_4   <- 2003:2014
  oldnames_4 <- c("mager14", "lbo_rec", "meduc", "mrace")
  newnames_4 <- c("age_of_mother", "birth_order",
                  "education", "race")
  keep_4       <- c(newnames_4, "year")
  
  
  # Group of years 4: 2015-2021
  years_4   <- 2015:2021
  oldnames_4 <- c("mager14", "lbo_rec", "meduc", "mrace")
  newnames_4 <- c("age_of_mother", "birth_order",
                  "education", "race")
  keep_4       <- c(newnames_4, "year")
 
  
  # Aggregation formula
  agg_formula <- as.formula("count~age_of_mother+birth_order+education+race+year")
  
  
  
### First group of years ----------------------------------------------
  
 # for(year in years_1) {
  
    # Load data
    file <-  paste0(wd, "natl", year, ".csv")
    dat  <- fread(file=file)
    
    # Generate variables
    dat$year  <- year

    # Rename variables
    setnames(dat,
             old=oldnames_1,
             new=newnames_1)
    
    # Weight variable
    if(year < 1972) {
      dat$count <- 2} else {
      setnames(dat,old="recwt",new="count")
      }
    
    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_1)
    
    
    # Change the age coding
    dat$age <- coll_age(dat$age_of_mother)
    
    
    # data
    dat <- collapse_vars(dat, year)
    
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
    dat$year  <- year


    # Rename variables
    setnames(dat,
             old=oldnames_2,
             new=newnames_2)

    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_2)
    
    # data
    dat <- collapse_vars(dat, year)
    
    # Save
    file <- paste0("US_fertility_",year,".Rda")
    save(dat, file= paste0("Data/", file))
    
  }  
  
  
### Third group of years ----------------------------------------------
  
  for(year in years_3) {
    
    # Load data
    file <- paste0(wd, "natl",year,".csv")
    dat  <- fread(file= file)
    
    # Generate variables
    dat$year  <- year
    if(year==2003) dat$mager <- dat$mager41+13
    
    
    # Rename variables
    setnames(dat,
             old=oldnames_3,
             new=newnames_3)
    
    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_3)
    
    # data
    dat <- collapse_vars(dat, year)
    
    
    # Save
    file <- paste0("US_fertility_",year,".Rda")
    save(dat, file= paste0("Data/", file))
    
  }    
  
  
### third group of years ----------------------------------------------
  
  
  for(year in years_4) {
    
    # Load data
    dat  <- fread(file=paste0("Raw/natl", year,".csv"))
    
    # Generate variables
    dat$year  <- year
    
    # adjust variable label for race
    if(year >= 2014) oldnames_4[4] <- "mrace6"
    if(year >= 2019){
      oldnames_4 <- str_to_upper(oldnames_4)
      dat <- rename(dat, restatus = RESTATUS )
    }
    
    # Rename variables
    setnames(dat,
             old=oldnames_4,
             new=newnames_4)
    

    
    # Subset
    dat <- subset(dat,
                  subset=restatus != 4,
                  select=keep_4) %>% as.data.frame()
    
    # data
    dat <- collapse_vars(dat, year)
    
    
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
