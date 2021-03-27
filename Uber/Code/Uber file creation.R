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
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/Data sources/Uber"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/Data sources/Uber"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"/Data")

setwd(paste0(path_data))


# Load Uber Data
Uber_timing <- read_excel("NES_georef12.xlsx") # Load the Uber deployment timing

# Add all US geo-code variables
statistical_areas <- read_excel("core_based_statistical_areas.xlsx") # Load CBSA cross walks
statistical_areas$GEO.id2 <- statistical_areas$`CBSA Code`
colnames(statistical_areas)[11] <- "cty"
colnames(statistical_areas)[10] <- "st"
statistical_areas$st <- as.numeric(statistical_areas$st)
statistical_areas$cty <- as.numeric(statistical_areas$cty)
cbsa_names <- unique(statistical_areas[c(14,4)])
state_names <- unique(statistical_areas[c(9,10)])

statistical_areas <- statistical_areas %>% dplyr::select("CBSA Code", "st", "cty", "Central/Outlying County" , "Metropolitan/Micropolitan Statistical Area","CBSA Title")

Uber_cbsa <- merge(Uber_timing,statistical_areas)
cbsa_uber <- unique(Uber_cbsa[c(5,4)])
cbsa_uber <- cbsa_uber[complete.cases(cbsa_uber), ]

cbsa_uber <- cbsa_uber[order(cbsa_uber$`CBSA Code`, cbsa_uber$min), ] #sort by id and reverse of abs(value)
cbsa_uber <- cbsa_uber[!duplicated(cbsa_uber$`CBSA Code`),]  
colnames(cbsa_uber)[2]<-"cbsa_min"

# Uber_cbsa <- Uber_cbsa[c(1,2,3,5,8,9,15)]
Uber_cbsa <- merge(Uber_cbsa,cbsa_uber, all.x = TRUE)
Uber_timing <- merge(Uber_timing, Uber_cbsa, all.x = TRUE)

rm(Uber_cbsa, cbsa_uber, statistical_areas)

# combine the County and CBSA wave times
# define Uber deployment primarily as the county level, if still missing then define it as the CBSA level
Uber_timing$min_uber <- Uber_timing$min 
Uber_timing$min_uber[is.na(Uber_timing$min_uber)] <- Uber_timing$cbsa_min[is.na(Uber_timing$min_uber)]


save(Uber_timing, file = "Uber_2000_2018.RData")
