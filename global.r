rm(list=ls(all=TRUE))
options(shiny.reactlog=TRUE) 


options(stringsAsFactors = F)
library(Matrix.utils)
library(tools)
#library(Rfast)
library (tidyr)
library (raster)
library(fasterize)
library(sf)
library(foreach)
library(doParallel)
library(gdalUtils)
library(dplyr)
library(tibble)
#library(tiff)
#library(reshape2)
library(plotly)
library(shiny)
library (shinydashboard)
library(tabularaster)
library(knitr)
library(shinyFiles)
library(shinycssloaders)
library(shinyjs)
library(FAMEFMR)

#for debugging------------------
options(warn=-1)


#library(shinythemes)

#loads functions used in TFI and RA calculations
#source("EcoResFunctionsShiny.r")
#source("TFI_functionsShiny.r")
source("ButtonDisableHelpers.r")


#temporary debugging for fucntion overwrites package function with version stored in temp.r
source("temp.r")



#set the number of cores to use
# algorithm here to work out the best number of cores to use as a ratio of RAM currently assuming requirement of 8GB per core
if(Sys.info()[1]=="Windows"){
  maxCores<-as.integer(memory.limit(size = NA)/(1024^2*8))
  
} else {
  maxCores<-as.integer(as.integer(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=T))/(1024^2 * 8))
}
totalCores<-detectCores()
Ncores<-ifelse(totalCores>maxCores,maxCores,totalCores-1)


Ncores=4
print(paste("Using",Ncores,"cores"))

#Set the maximum size of files for upload/ download 

options(shiny.maxRequestSize=2*1024^3) 


# code to implement action on closing browser window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
####################################################################################

#MAKE RESULTS DIRECTORIES
#create a unique results directory for each run of scenarios 
#using starting time as a string for Results directory name
#this is zipped for downlaod of 


StartTimeString<-format(Sys.time(), "%Y%m%d_%H%M")

WD<-getwd()
#cleans up old resultsdirs which are empty
removeEmptyDirs(rootDir = "./results")




#Makes resultsDir
ResultsDir<-file.path("./results",StartTimeString)


for (d in c(ResultsDir)){dir.create(d)}
rm(d)
dir.create(file.path(ResultsDir,"RA_Rasters"))
dir.create(file.path(ResultsDir,"TFI_Rasters"))
# list of options for choice of region/ state or adhoc polygon for analysis
REG_NO <- c("WHOLE OF STATE"=99,
            "BARWON SOUTH WEST"=1,
            "GIPPSLAND"=2 ,
            "GRAMPIANS"=3,
            "HUME"=4,
            "LODDON MALLEE"=5,
            "PORT PHILLIP"=6,
            "USER DEFINED POLYGON"=7)

source("makeLUTS.R")

cellSizes<-c(225, 75)
HDMVals225<-"./HDMS/HDMVals225.rdata"
writeSpRasters=FALSE #TRUE if rasters are to be output ( large number of files and considerable disk space)

#Lookup table from EFG to TFI attributes ( csv version of CGDL lookup table)
TFI_LUT<-read.csv("./ReferenceTables/EFG_EVD_TFI.csv")[,c("EFG_NUM","MIN_LO_TFI","MIN_HI_TFI","MAX_TFI","EFG_NAME")]
names(TFI_LUT)[1]<-"EFG"


EFG_TSF_4GS<-read.csv("./ReferenceTables/EFG_TSF_4GScorrectedAllEFGto400yrsV2.csv")[,c('EFG_NO','GS4_NO',"YSF")]


