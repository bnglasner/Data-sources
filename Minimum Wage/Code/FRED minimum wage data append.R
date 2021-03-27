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
library(stringi)


#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/Data sources/Minimum Wage"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/Data sources/Minimum Wage"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"/Data/USMINWG_csv_2")
# Path where matched samples should be saved 
path_output <- paste0(path_project,"/Data")

# add in minimum wage data for a FIPS combo. Use the highest minimum wage in the county-year observed at any point during the year
setwd(paste0(path_output))
load("County_year_list_1997_2020.RData")

VZ_substate_changes <- read_excel("VZ_substate_changes.xlsx", sheet = "merge")
VZ_substate_changes <- VZ_substate_changes %>% dplyr::select("FIPS combo", 
                                                             "YEAR.id",
                                                             "local_minimum_wage")

local_min_wages <- merge(st_cty_year,VZ_substate_changes)

rm(VZ_substate_changes)
setwd(paste0(path_data))



# load in the NES county data 1968 - 2019
files <- list.files()
myfiles <- lapply(files, read.csv)


# Clean the data
for (i in c(1:51)) {
  colnames(myfiles[[i]])[2] <- "Annual_State_Minimum"
}

# append NES data
state_min_wage <- do.call(rbind, myfiles)
rm(myfiles)
state_min_wage <- state_min_wage %>% dplyr::select( "st",
                                                    "YEAR.id",
                                                    "Annual_State_Minimum") 
YEAR.id <- sort(unique(state_min_wage$YEAR.id))
fed_min<- c(4.75,5.15,5.15,5.15,
5.15,5.15,5.15,5.15,5.15,5.15,5.15,5.85,6.55,7.25,
7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25,7.25)

fed_min <- as.data.frame(cbind(fed_min,YEAR.id))
colnames(fed_min)[1] <- "Annual_Federal_Minimum"

minimum_wage <- merge(state_min_wage, fed_min) # combine the state and federal minimum wages
rm(state_min_wage, fed_min)
minimum_wage <- merge(minimum_wage,st_cty_year) # link state and federal minimum wages to st_county_year list
rm(st_cty_year)
minimum_wage <- merge(minimum_wage,local_min_wages, all.x = TRUE) # link local minimum wages to st_county_year list and state/federal min wages
rm(local_min_wages)

minimum_wage$local_minimum_wage[is.na(minimum_wage$local_minimum_wage)] <- 0

minimum_wage$minimum_wage <- pmax(minimum_wage$`Annual_Federal_Minimum`,minimum_wage$Annual_State_Minimum, minimum_wage$local_minimum_wage) 
minimum_wage$minimum_wage_state_fed <- pmax(minimum_wage$`Annual_Federal_Minimum`,minimum_wage$Annual_State_Minimum) 

inflation_adj <- c(1.5,1.47,1.44,1.39,1.36,
                   1.33,1.30,1.27,1.23,1.19,
                   1.16,1.11,1.12,1.10,1.07,
                   1.05,1.03,1.01,1.01,1,
                   .98,.96,.94,.93)
YEAR.id <-c(1997,1998,1999,2000,2001,
            2002,2003,2004,2005,2006,
            2007,2008,2009,2010,2011,
            2012,2013,2014,2015,2016,
            2017,2018,2019,2020)

inflation_adj_df <- as.data.frame(cbind(inflation_adj,YEAR.id))

minimum_wage<- merge(minimum_wage, inflation_adj_df)

minimum_wage$minimum_wage_inf <- minimum_wage$minimum_wage*minimum_wage$inflation_adj
minimum_wage$state_minimum_wage_inf <- minimum_wage$Annual_State_Minimum*minimum_wage$inflation_adj
minimum_wage$federal_minimum_wage_inf <- minimum_wage$Annual_Federal_Minimum*minimum_wage$inflation_adj
minimum_wage$local_minimum_wage_inf <- minimum_wage$local_minimum_wage*minimum_wage$inflation_adj
minimum_wage$minimum_wage_state_fed_inf <- minimum_wage$minimum_wage_state_fed*minimum_wage$inflation_adj

setwd(paste0(path_output))
save(minimum_wage, file = "minimum_wage_v4.RData")









