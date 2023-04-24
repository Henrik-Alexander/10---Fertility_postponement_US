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
  age_vec[age_vec >= 45 & age_vec < 50] <-  "45 - 49"
  age_vec[age_vec == 50] <-                 "50"
  
  
  
  # Return
  return(age_vec)
}


##### Collapse the variables  #######################

# create the new variables
collapse_vars <- function(dat, year){
  
  
  
  
tmp <- {dat} %>% mutate(Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 10)),
                      Race = case_when(race == 1 ~ "white",
                                       race == 2 ~ "black", 
                                       race %!in% c(1, 2) & orm)) 

if(year >= 2003 ){
 tmp <- mutate(tmp, Education = case_when(education <= 2 ~ "No high school degree",
                        education == 3 ~ "High school diploma",
                        education > 3 & education < 6 ~ "Some college",
                        education > 6 & education != 9 ~ "College graduate"),
                    Age = case_when(age_of_mother == 1 ~ "< 15",
                                            age_of_mother == 2 ~ "15 - 19",
                                            age_of_mother == 3 ~ "20 - 24",
                                            age_of_mother == 4 ~ "25 - 29",
                                            age_of_mother == 5 ~ "30 - 34",
                                            age_of_mother == 6 ~ "35 - 39",
                                            age_of_mother == 7 ~ "40 - 44", 
                                            age_of_mother == 8 ~ "45 - 49",
                                            age_of_mother == 9 ~ "50"))
 
 tmp <- tmp %>% group_by(Age, Parity, Education, Race, year) %>% summarise(count = n(), .groups = "drop")
 
} else {
  tmp <- mutate(tmp, Education = case_when(education <= 8 ~ "No high school degree",
                                           education > 8 & education <= 12 ~ "High school diploma",
                                           education > 12 & education < 17 ~ "Some college",
                                           education == 17  ~ "College graduate" ),
                Age = coll_age(age_of_mother))

  tmp <- tmp %>% group_by(Age, Parity, Education, Race, year) %>% summarise(count = sum(count), .groups = "drop") 
  }



return(tmp)
}



#### Negate in function -------------------------------------

#`%!in%` <- negate(`%in%`)





