# Ben Glasner
# reshape and clean county LAU
# wide to long
# v1

#Set up
#################################################

library("tidyr")
library("readxl")
library("stringi")

if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/Data sources/LAU"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/Data sources/LAU"
}
# Path to saved cohort data 
path_data <- paste0(path_project,"/Data")

setwd(paste0(path_data))

# load in the NES county data 1997 - 2016
files <- list.files(pattern="laucnt*")
myfiles <- lapply(files, read_excel)
LAU <- do.call(rbind, myfiles)

LAU$st <- as.numeric(LAU$st)
LAU$cty <- as.numeric(LAU$cty)

save(LAU, file = "LAU.RData")
