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
library(naniar)

# source("http://jtilly.io/install_github/install_github.R")
# install_github("jtilly/cbpR")
# library("cbpR")

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/Data sources/CBP"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/Data sources/CBP"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"/Data")

setwd(paste0(path_data))

# load in the CBP county data 2000 - 2017
files <- list.files(pattern="cbp*")
myfiles <- lapply(files, read.csv)


# Clean the data
for (i in c(1:15, 17)) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("fipstate","fipscty","naics","emp", "est",
                                                 "n1_4","n5_9","n10_19","n20_49","n50_99",
                                                 "n100_249","n250_499","n500_999","n1000","n1000_1",
                                                 "n1000_2","n1000_3","n1000_4") 
  myfiles[[i]]$YEAR.id <- 1999+i
}
for (i in 16) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("FIPSTATE","FIPSCTY","NAICS","EMP","EST",
                                                 "N1_4","N5_9","N10_19","N20_49","N50_99",
                                                 "N100_249","N250_499","N500_999","N1000","N1000_1",
                                                 "N1000_2","N1000_3","N1000_4") 
  colnames(myfiles[[i]]) <- c("fipstate","fipscty","naics","emp", "est",
                              "n1_4","n5_9","n10_19","n20_49","n50_99",
                              "n100_249","n250_499","n500_999","n1000","n1000_1",
                              "n1000_2","n1000_3","n1000_4")
  myfiles[[i]]$YEAR.id <- 2014+(i-15)
}
for (i in 18) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("fipstate","fipscty","naics","emp", "est",
                                                 "n.5","n5_9","n10_19","n20_49","n50_99",
                                                 "n100_249","n250_499","n500_999","n1000","n1000_1",
                                                 "n1000_2","n1000_3","n1000_4") 
  colnames(myfiles[[i]]) <- c("fipstate","fipscty","naics","emp", "est",
                              "n1_4","n5_9","n10_19","n20_49","n50_99",
                              "n100_249","n250_499","n500_999","n1000","n1000_1",
                              "n1000_2","n1000_3","n1000_4")
  myfiles[[i]]$YEAR.id <- 2017
}
for (i in 19) {
  myfiles[[i]] <- myfiles[[i]] %>% dplyr::select("fipstate","fipscty","naics","emp", "est",
                                                 "n.5","n5_9","n10_19","n20_49","n50_99",
                                                 "n100_249","n250_499","n500_999","n1000","n1000_1",
                                                 "n1000_2","n1000_3","n1000_4") 
  colnames(myfiles[[i]]) <- c("fipstate","fipscty","naics","emp", "est",
                              "n1_4","n5_9","n10_19","n20_49","n50_99",
                              "n100_249","n250_499","n500_999","n1000","n1000_1",
                              "n1000_2","n1000_3","n1000_4")
  myfiles[[i]]$YEAR.id <- 2018
}

# append NES data
CBP <- do.call(rbind, myfiles)
rm(myfiles) # clear out everything other than CBP

CBP <- subset(CBP, naics == "------") # Only keep total naics. If you want to do industry level then change or drop this line.

CBP <-CBP %>% dplyr::select("fipstate","fipscty","emp", "est",
                            "n1_4","n5_9","n10_19","n20_49","n50_99",
                            "n100_249","n250_499","n500_999","n1000","n1000_1",
                            "n1000_2","n1000_3","n1000_4", "YEAR.id")
CBP$st <- as.numeric(CBP$fipstate)         
CBP$cty <- as.numeric(CBP$fipscty)         

########  input 2017 HHI from 2016 HHI
CBP$n1_4 <- as.numeric(as.character(CBP$n1_4))
CBP$n5_9 <- as.numeric(as.character(CBP$n5_9))
CBP$n10_19 <- as.numeric(as.character(CBP$n10_19))
CBP$n20_49 <- as.numeric(as.character(CBP$n20_49))
CBP$n50_99 <- as.numeric(as.character(CBP$n50_99))
CBP$n100_249 <- as.numeric(as.character(CBP$n100_249))
CBP$n250_499 <- as.numeric(as.character(CBP$n250_499))
CBP$n500_999 <- as.numeric(as.character(CBP$n500_999))
CBP$n1000 <- as.numeric(as.character(CBP$n1000))
CBP$n1000_1 <- as.numeric(as.character(CBP$n1000_1))
CBP$n1000_2 <- as.numeric(as.character(CBP$n1000_2))
CBP$n1000_3 <- as.numeric(as.character(CBP$n1000_3))
CBP$n1000_4 <- as.numeric(as.character(CBP$n1000_4))

CBP_2017 <- subset(CBP, YEAR.id==2017)
CBP_2018 <- subset(CBP, YEAR.id==2018)
CBP <-subset(CBP, YEAR.id<2017)

input <-subset(CBP, YEAR.id==2016)
input <- input %>% dplyr::select("cty","st",
                                 "n1_4","n5_9","n10_19","n20_49",
                                 "n50_99","n100_249","n250_499","n500_999",
                                 "n1000","n1000_1","n1000_2","n1000_3",
                                 "n1000_4")
names(input) <- c("cty","st",
                  "n1_4_2016","n5_9_2016","n10_19_2016","n20_49_2016",
                  "n50_99_2016","n100_249_2016","n250_499_2016","n500_999_2016",
                  "n1000_2016","n1000_1_2016","n1000_2_2016","n1000_3_2016",
                  "n1000_4_2016")

################################
input$YEAR.id <- 2017
CBP_2017 <- merge(CBP_2017,input, all = TRUE)

CBP_2017$n1_4[is.na(CBP_2017$n1_4)] <- CBP_2017$n1_4_2016[is.na(CBP_2017$n1_4)]
CBP_2017$n5_9[is.na(CBP_2017$n5_9)] <- CBP_2017$n5_9_2016[is.na(CBP_2017$n5_9)]
CBP_2017$n10_19[is.na(CBP_2017$n10_19)] <- CBP_2017$n10_19_2016[is.na(CBP_2017$n10_19)]
CBP_2017$n20_49[is.na(CBP_2017$n20_49)] <- CBP_2017$n20_49_2016[is.na(CBP_2017$n20_49)]
CBP_2017$n50_99[is.na(CBP_2017$n50_99)] <- CBP_2017$n50_99_2016[is.na(CBP_2017$n50_99)]
CBP_2017$n100_249[is.na(CBP_2017$n100_249)] <- CBP_2017$n100_249_2016[is.na(CBP_2017$n100_249)]
CBP_2017$n250_499[is.na(CBP_2017$n250_499)] <- CBP_2017$n250_499_2016[is.na(CBP_2017$n250_499)]
CBP_2017$n500_999[is.na(CBP_2017$n500_999)] <- CBP_2017$n500_999_2016[is.na(CBP_2017$n500_999)]
CBP_2017$n1000[is.na(CBP_2017$n1000)] <- CBP_2017$n1000_2016[is.na(CBP_2017$n1000)]
CBP_2017$n1000_1[is.na(CBP_2017$n1000_1)] <- CBP_2017$n1000_1_2016[is.na(CBP_2017$n1000_1)]
CBP_2017$n1000_2[is.na(CBP_2017$n1000_2)] <- CBP_2017$n1000_2_2016[is.na(CBP_2017$n1000_2)]
CBP_2017$n1000_3[is.na(CBP_2017$n1000_3)] <- CBP_2017$n1000_3_2016[is.na(CBP_2017$n1000_3)]
CBP_2017$n1000_4[is.na(CBP_2017$n1000_4)] <- CBP_2017$n1000_4_2016[is.na(CBP_2017$n1000_4)]
CBP_2017 <- CBP_2017 %>% dplyr::select(names(CBP))

################################

input$YEAR.id <- 2018
CBP_2018 <- merge(CBP_2018,input, all = TRUE)

CBP_2018$n1_4[is.na(CBP_2018$n1_4)] <- CBP_2018$n1_4_2016[is.na(CBP_2018$n1_4)]
CBP_2018$n5_9[is.na(CBP_2018$n5_9)] <- CBP_2018$n5_9_2016[is.na(CBP_2018$n5_9)]
CBP_2018$n10_19[is.na(CBP_2018$n10_19)] <- CBP_2018$n10_19_2016[is.na(CBP_2018$n10_19)]
CBP_2018$n20_49[is.na(CBP_2018$n20_49)] <- CBP_2018$n20_49_2016[is.na(CBP_2018$n20_49)]
CBP_2018$n50_99[is.na(CBP_2018$n50_99)] <- CBP_2018$n50_99_2016[is.na(CBP_2018$n50_99)]
CBP_2018$n100_249[is.na(CBP_2018$n100_249)] <- CBP_2018$n100_249_2016[is.na(CBP_2018$n100_249)]
CBP_2018$n250_499[is.na(CBP_2018$n250_499)] <- CBP_2018$n250_499_2016[is.na(CBP_2018$n250_499)]
CBP_2018$n500_999[is.na(CBP_2018$n500_999)] <- CBP_2018$n500_999_2016[is.na(CBP_2018$n500_999)]
CBP_2018$n1000[is.na(CBP_2018$n1000)] <- CBP_2018$n1000_2016[is.na(CBP_2018$n1000)]
CBP_2018$n1000_1[is.na(CBP_2018$n1000_1)] <- CBP_2018$n1000_1_2016[is.na(CBP_2018$n1000_1)]
CBP_2018$n1000_2[is.na(CBP_2018$n1000_2)] <- CBP_2018$n1000_2_2016[is.na(CBP_2018$n1000_2)]
CBP_2018$n1000_3[is.na(CBP_2018$n1000_3)] <- CBP_2018$n1000_3_2016[is.na(CBP_2018$n1000_3)]
CBP_2018$n1000_4[is.na(CBP_2018$n1000_4)] <- CBP_2018$n1000_4_2016[is.na(CBP_2018$n1000_4)]
CBP_2018 <- CBP_2018 %>% dplyr::select(names(CBP))

##############################

CBP <- rbind(CBP,CBP_2017)
CBP <- rbind(CBP,CBP_2018)

########  HHI assuming the minimum number
# we could also assume the maximum of the range or the average. I found it made little difference, but it is worth considering it as a robustness

CBP$n1_4_HHI_lower <- ((CBP$n1_4*(1)/CBP$emp)*100)^2
CBP$n5_9_HHI_lower <- ((CBP$n5_9*(5)/CBP$emp)*100)^2
CBP$n10_19_HHI_lower <- ((CBP$n10_19*(10)/CBP$emp)*100)^2
CBP$n20_49_HHI_lower <- ((CBP$n20_49*(20)/CBP$emp)*100)^2
CBP$n50_99_HHI_lower <- ((CBP$n50_99*(50)/CBP$emp)*100)^2
CBP$n100_249_HHI_lower <- ((CBP$n100_249*(100)/CBP$emp)*100)^2
CBP$n250_499_HHI_lower <- ((CBP$n250_499*(250)/CBP$emp)*100)^2
CBP$n500_999_HHI_lower <- ((CBP$n500_999*(500)/CBP$emp)*100)^2
CBP$n1000_1_HHI_lower <- ((CBP$n1000_1*(1000)/CBP$emp)*100)^2
CBP$n1000_2_HHI_lower <- ((CBP$n1000_2*(1500)/CBP$emp)*100)^2
CBP$n1000_3_HHI_lower <- ((CBP$n1000_3*(2500)/CBP$emp)*100)^2
CBP$n1000_4_HHI_lower <- ((CBP$n1000_4*(5000)/CBP$emp)*100)^2

CBP$HHI_lower <- CBP$n1_4_HHI_lower + CBP$n5_9_HHI_lower + CBP$n10_19_HHI_lower +
  CBP$n20_49_HHI_lower + CBP$n50_99_HHI_lower + CBP$n100_249_HHI_lower +
  CBP$n250_499_HHI_lower + CBP$n500_999_HHI_lower + CBP$n1000_1_HHI_lower +
  CBP$n1000_2_HHI_lower + CBP$n1000_3_HHI_lower + CBP$n1000_4_HHI_lower 

quantiles_25 <- data.frame(aggregate(x = CBP$HHI_lower, by = list(CBP$YEAR.id), FUN = 'quantile', probs=c(.25), na.rm = TRUE))
colnames(quantiles_25)[1] <- "YEAR.id"
colnames(quantiles_25)[2] <- "quant_25"
quantiles_75 <- data.frame(aggregate(x = CBP$HHI_lower, by = list(CBP$YEAR.id), FUN = 'quantile', probs=c(.75), na.rm = TRUE))
colnames(quantiles_75)[1] <- "YEAR.id"
colnames(quantiles_75)[2] <- "quant_75"

CBP <- merge(CBP, quantiles_25)
CBP <- merge(CBP, quantiles_75)

CBP$concentration_lower[CBP$HHI_lower > CBP$quant_75] <- 1 # "High Concentration"
CBP$concentration_lower[CBP$HHI_lower < CBP$quant_25] <- 3 # "Low Concentration"
CBP$concentration_lower[CBP$HHI_lower >= CBP$quant_25 & CBP$HHI_lower <= CBP$quant_75] <- 2 # "Moderate Concentration"

CBP$concentration_lower <- as.factor(CBP$concentration_lower)

cbp_full <- CBP

CBP <- CBP %>% dplyr::select("YEAR.id",
                             "fipstate",
                             "fipscty",
                             "emp",
                             "est",
                             "HHI_lower",
                             "concentration_lower")

setwd(paste0(path_data))
save(CBP, file = "CBP_2000_2018.RData")
save(cbp_full, file = "CBP_full_2000_2018.RData")



