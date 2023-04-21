## Functions

##### Collapse the variables  #######################

# create the new variables
collapse_vars <- function(dat, year){
  
tmp <- {dat} %>% mutate(Age = cut(age_of_mother, breaks = seq(10, 49, 4)),
                      Parity = cut(birth_order, breaks = c(0, 1, 2, 3, 10)),
                      Race = if_else(race == 1, "white", "else"))

if(year > 2004 ){
 tmp <- mutate(tmp, Education = case_when(education <= 2 ~ "No high school degree",
                        education == 3 ~ "High-school diploma",
                        education > 3 & education < 6 ~ "Some college",
                        education > 6 & education != 9 ~ "College graduate" ))
 
 tmp <- tmp %>% group_by(Age, Parity, Education, Race, year) %>% summarise(count = n()) %>% ungroup()
 
} else {
  tmp <- mutate(tmp, Education = case_when(education <= 8 ~ "No high school degree",
                                           education > 8 & education <= 12 ~ "High school diploma",
                                           education > 12 & education < 17 ~ "Some college",
                                           education == 17  ~ "College graduate" ))

  tmp <- tmp %>% group_by(Age, Parity, Education, Race, year) %>% summarise(count = sum(count)) %>% ungroup()
  }



return(tmp)
}



#### Negate in function ###############################

#`%!in%` <- negate(`%in%`)
