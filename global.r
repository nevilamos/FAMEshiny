rm(list = ls(all = TRUE))
options(shiny.reactlog = TRUE)


options(stringsAsFactors = F)

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("aws.s3",
             "dashboardthemes",
             "doParallel",
             "dplyr",
             "fasterize",
             "foreach",
             "gdalUtils",
             "knitr",
             "Matrix.utils",
             "plotly",
             "raster",
             "Rfast",
             "rlang",
             "sf",
             "shiny",
             "shinycssloaders",
             "shinydashboard",
             "shinyFiles",
             "shinyjs",
             "tabularaster",
             "tibble",
             "tidyr",
             "tools")

## Now load or install&load all packages from cran
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

#check that FAMEFMR package is installed and get it fro github if not.
if(require(FAMEFMR==FALSE)){
devtools::install_github("nevilamos/FAMEFMR")
library(FAMEFMR)}

#run once only to copy all the input files from AWS bucket-----------------------------
source 
#for debugging------------------
options(warn = -1)


#library(shinythemes)

#get version of FAMEFMR in use and set app version 
versionDate = "Version 1.9 February 28 2021"
versionFAMEFMR = paste ("R", getRversion(),"FAMEFMR",packageVersion("FAMEFMR"))



source("ButtonDisableHelpers.r")




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

print(106)
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
