## Male fertility in household panels


## This file loads the required packages for 
## reproducing the results of the paper:
## "Catching Up: The impact of postponement potential on the recent fertility decline"

# Load the packages
library(tidyverse)
library(data.table)
library(dtplyr)
library(reshape2)
library(naniar)
library(survey)
library(scales)
library(mice)
library(parallel)
