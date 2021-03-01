rm(list = ls(all = TRUE))
options(shiny.reactlog = TRUE)


options(stringsAsFactors = F)
library(dashboardthemes)
library(doParallel)
library(dplyr)
library(FAMEFMR)
library(fasterize)
library(foreach)
library(gdalUtils)
library(knitr)
library(Matrix.utils)
library(plotly)
library(raster)
library(Rfast)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(tabularaster)
library(tibble)
library(tidyr)
library(tools)


#get version of FAMEFMR in use and set app version 
versionDate = "Version 1.9 February 28 2021"
versionFAMEFMR = paste ("R", getRversion(),"FAMEFMR",packageVersion("FAMEFMR"))

#loads functions used in TFI and RA calculations
#source("EcoResFunctionsShiny.r")
#source("TFI_functionsShiny.r")
source("ButtonDisableHelpers.r")


#temporary debugging for function overwrites package function with version stored in temp.r
#source("temp.r")



#set the number of cores to use
# algorithm here to work out the best number of cores to use as a ratio of RAM currently assuming requirement of 8GB per core
if (Sys.info()[1] == "Windows") {
  maxCores <- as.integer(memory.limit(size = NA) / (1024 ^ 2 * 8))
  
} else {
  maxCores <-
    as.integer(as.integer(system(
      "awk '/MemFree/ {print $2}' /proc/meminfo", intern = T
    )) / (1024 ^ 2 * 8))
}
totalCores <- detectCores()
Ncores <- ifelse(totalCores > maxCores, maxCores, totalCores - 1)


Ncores = 4
print(paste("Using", Ncores, "cores"))

#Set the maximum size of files for upload/ download

options(shiny.maxRequestSize = 2 * 1024 ^ 3)


# code to implement action on closing browser window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
####################################################################################

#MAKE RESULTS DIRECTORIES
#create a unique results directory for each run of scenarios
#using starting time as a string for Results directory name
#this is zipped for downlaod of


StartTimeString <- format(Sys.time(), "%Y%m%d_%H%M")

WD <- getwd()
#cleans up old resultsdirs which are empty
removeEmptyDirs(rootDir = "./results")




#Makes resultsDir
ResultsDir <- file.path("./results", StartTimeString)


for (d in c(ResultsDir)) {
  dir.create(d)
}
rm(d)
dir.create(file.path(ResultsDir, "RA_Rasters"))
dir.create(file.path(ResultsDir, "TFI_Rasters"))


source("makeLUTS.R")

cellSizes <- c(225, 75)
HDMVals225 <- "./HDMS/HDMVals225.rdata"
writeSpRasters = FALSE #TRUE if rasters are to be output ( large number of files and considerable disk space)

#Lookup table from EFG to TFI attributes ( csv version of CGDL lookup table)
TFI_LUT <-
  read.csv("./ReferenceTables/EFG_EVD_TFI.csv")[, c("EFG_NUM", "MIN_LO_TFI", "MIN_HI_TFI", "MAX_TFI", "EFG_NAME")]
names(TFI_LUT)[1] <- "EFG"


EFG_TSF_4GS <-
  read.csv("./ReferenceTables/EFG_TSF_4GScorrectedAllEFGto400yrsV2.csv")[, c('EFG_NO', 'GS4_NO', "YSF")]
