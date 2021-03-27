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
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/Data sources/Nonemployer Statistics"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/Data sources/Nonemployer Statistics"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"/Data/NES State")
path_output <- paste0(path_project,"/Data")

setwd(paste0(path_data))


# load in the NES county data 1997 - 2016
files <- list.files()
myfiles <- lapply(files, read.csv)

# Clean the data
for (i in 1:11) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("ST", "NAICS", "ESTAB") 
  myfiles[[i]]$YEAR.id <- 1996+i
  colnames(myfiles[[i]])[1] <- "st"
  colnames(myfiles[[i]])[2] <- "naics"
  colnames(myfiles[[i]])[3] <- "estab"
  
}

for (i in 12:19) {
  myfiles[[i]] <- subset(myfiles[[i]], lfo=="-")
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("st", "naics", "estab") 
  myfiles[[i]]$YEAR.id <- 1996+i
}
for (i in 20:21) {
  myfiles[[i]] <- subset(myfiles[[i]], LFO=="-")
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("ST", "NAICS", "ESTAB") 
  myfiles[[i]]$YEAR.id <- 1996+i
  colnames(myfiles[[i]])[1] <- "st"
  colnames(myfiles[[i]])[2] <- "naics"
  colnames(myfiles[[i]])[3] <- "estab"
}

# append NES data
NES <- do.call(rbind, myfiles)

NES <- subset(NES, naics == "11"| naics == "71"| naics == "23"| naics == "61"| naics == "52"| 
                naics == "62"| naics == "51"| naics == "31-33"| naics == "21"| naics == "81"|
                naics == "54"| naics == "53"| naics == "44-45"| naics == "4853"| naics == "22"|
                naics == "42"| naics == "00"| naics == "56" |naics == "4859" | naics == "48-49" | naics == "72")

NES$estab[NES$estab==0] <- NA # assume that all zeros that are censured are actually 2 nonemployers

industry_cross_walk <- data.frame(c("agr","arts","construction","educ",
                                    "finance","health","info","man","mining",
                                    "other","sci","realestate","retail","taxi",
                                    "utilities","wholesale","total","waste", "Other Transit",
                                    "Transportation-Warehousing", "Accomodation"),
                                  c("11","71","23","61",
                                    "52","62","51","31-33","21",
                                    "81","54","53","44-45","4853",
                                    "22","42","00","56", "4859", "48-49", "72"))
colnames(industry_cross_walk)[1] <- "industry"
colnames(industry_cross_walk)[2] <- "naics"

NES <- merge(NES, industry_cross_walk)

setwd(paste0(path_output))

save(NES, file = "Nonemployer_statistics_state_1997_2017.RData")


rm(NES)

