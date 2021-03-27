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
path_data <- paste0(path_project,"/Data")
# Path where matched samples should be saved 

setwd(paste0(path_data))


# load in the NES county data 1997 - 2016
files <- list.files(pattern="Non-Employment data*")
myfiles <- lapply(files, read.dta13)

# Clean the data
for (i in 1:5) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("st", "county", "naics", "estab") 
  colnames(myfiles[[i]])[2] <- "cty" # Match column names
  myfiles[[i]]$YEAR.id <- 1996+i
}
for (i in 6:24) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("st", "cty", "naics", "estab") 
  myfiles[[i]]$YEAR.id <- 2001+(i-5)
}

# append NES data
NES <- do.call(rbind, myfiles)
rm(list=setdiff(ls(), "NES")) # clear out everything other than NES

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

save(NES, file = "Nonemployer_statistics_1997_2017.RData")


rm(NES)

#######################################################################
####                Fillin missing County totals                    ###
#######################################################################

# load in the NES county data 1997 - 2016
files <- list.files(pattern="Non-Employment data*")
myfiles <- lapply(files, read.dta13)

# Clean the data
for (i in 1:5) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("st", "county", "naics", "estab", "rcptot") 
  colnames(myfiles[[i]])[2] <- "cty" # Match column names
  myfiles[[i]]$YEAR.id <- 1996+i
}
for (i in 6:24) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("st", "cty", "naics", "estab", "rcptot") 
  myfiles[[i]]$YEAR.id <- 2001+(i-5)
}

# append NES data
NES <- do.call(rbind, myfiles)
rm(list=setdiff(ls(), "NES")) # clear out everything other than NES

NES <- subset(NES, naics == "11"| naics == "71"| naics == "23"| naics == "61"| naics == "52"| 
              naics == "62"| naics == "51"| naics == "31-33"| naics == "21"| naics == "81"|
              naics == "54"| naics == "53"| naics == "44-45"| naics == "22"|
              naics == "42"| naics == "00"| naics == "56" | naics == "48-49" | naics == "72")

NES$estab[NES$estab==0] <- 2 # assume that all zeros that are censured are actually 2 nonemployers

NES$id <- as.factor(paste(NES$st, NES$cty, NES$naics, sep = "-")) # create unique id for each county-state
NES$cty_st <- as.factor(paste(NES$st, NES$cty, sep = "-")) # create unique id for each county-state

# create an observation for counties which are excluded in a given industry-year due to an absence of nonemployers
NES <- NES %>% group_by(id) %>% fill_gap(YEAR.id, full = TRUE) # Equivalent of the fillin command in Stata
NES$estab[is.na(NES$estab) & as.numeric(as.character(NES$YEAR.id))<2019] <- 0
NES$rcptot[is.na(NES$rcptot) & as.numeric(as.character(NES$YEAR.id))<2019] <- 0

NES_st_cty <- NES %>% dplyr::select("st","cty","id","cty_st")
NES_st_cty <- unique(NES_st_cty[complete.cases(NES_st_cty),])
NES <- merge(NES,NES_st_cty)

industry_cross_walk <- data.frame(c("All Nonemployers","Agr./Forestry/Fish/Hunting","Mining/Quarrying/Oil/Gas","Utilities","Construction",
                                    "Manufacturing","Wholesale Trade","Retail Trade","Trans./Warehousing",
                                    "Information","Finance/Insurance","Real Estate/Rental/Leasing","Prof./Sci./Technical Services",
                                    "Management of Companies and Enterprises","Admin./Support/Waste/Remediation","Educational Services","Health Care/Social Assist.",
                                    "Arts/Entertainment/Recreation","Accommodation/Food Services","Other Services","Public Administration"),
                                  c("00","11","21","22","23","31-33","42","44-45","48-49","51","52",
                                    "53","54","55","56","61","62","71","72","81","92"))
colnames(industry_cross_walk)[1] <- "industry"
colnames(industry_cross_walk)[2] <- "naics"

NES <- merge(NES, industry_cross_walk)


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

NES<- merge(NES, inflation_adj_df)

NES$rcptot <- NES$rcptot*NES$inflation_adj

save(NES, file = "Nonemployer_statistics_fillin_1997_2018.RData")
