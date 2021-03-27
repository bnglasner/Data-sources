# Ben Glasner
# County level Nonemployer data with CBSA Uber treatment
# 7/16/2019

##########################
#         Set Up         #  
##########################

library(readxl)
library(dplyr)
library(lubridate)
library(dummies)
library(readstata13)
library(readr)
library(statar)

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/Data sources/County Characteristics"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/Data sources/County Characteristics"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"/Data")

setwd(paste0(path_data))


# Load in county population stats
load("county_population.RData")
colnames(population)[5] <- "YEAR.id"

fips <- unique(population[c(1,2,7)])
population <- population[c(1,2,5,6)]

save(fips, file = "fips_1997_2018.RData")
save(population, file = "county_population_2000_2018.RData")
