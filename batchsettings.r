#Settings file to run a number of alternative fire history scenarios for the same area of interest though fame as a batch.  

# the batch will run through each of the raw fire history scenarios specified  by the shapefile paths in rawFHPath  in series putting the results in a separate subdirectory of the resultsDir.  All other inputs are constant throughout the batch

#whether analysis should be undertaken only on public land ( public only if TRUE)
public = FALSE
#raster resolution 75 or 225
RasterRes = 225

#path(s) to your rawFH file ( output of the ARCGIS preprocessing tool) each given as a separate string of the full path or ./path relative to the working directory. 
rawFHPaths = c("D:/FAMEshiny/rawFH/FIRE_HISTORY20210310_DemoAdHocPolygon.shp")

#the root directory in which results directories should be created and outputs saved
resultsDir = "./results"
#the integer value of the region number 1-6 for FFR regions,7 for user supplied adHoc polygon, 99 for Statewide
REGION_NO = "7"
#path to the ad Hoc polygon if REGION_NO 			== 7 otherwise NULL
AdHocPath = "D:\\FAMEshiny\\AdHocPolygons\\DemoAdHocPolygon.shp"

doSpeciesCalculations= TRUE

# whether or not custom species list is used ( TRUE for custom species list) otherwise FALSE
spListChoice = TRUE #FALSE
customSpList = "D:/FAMEshiny/CustomCSV/5spWithAreas.csv"

#whether the species response file contains abundance data by 4 growth stages ( TRUE) or YSF (FALSE) 0-400
abundByGS = TRUE # FALSE
# whether the defualt or custom response file is used for abundance responses
spResponseChoice = FALSE
#path of custom response abundance file if spResponseChoice==TRUE
customResponseFile = "D:/RobfilesV2/FAME_V2_FaunaAbund_LUT_20210916.csv" 

#Path with  HDM cell values file
HDMValsPath<-"D:/FAMEshiny/HDMS/HDMValsMaskedUnthresholded225list.qs"#paste0("./HDMS/HDMVals",  RasterRes,  "list.qs")

#value to use for cases where fire type is "OTHER" or "UNKNOWN",1 ="BURN",2="BUSHFIRE",NA = Fire excluded from analysis default is 2 ("BUSHFIRE")
otherUnknown = 2


#"first season for which output is wanted ( four digit year as integer)
startTimespan = 1990

#start and end baseline years if single year then the two values should be equal
startBaseline = 1990
endBaseline = 2000
endSEASON = NULL


#whether to write TFI rasters
makeTFIrasters = TRUE
#whether to write BBTFI rasters
makeBBTFIrasters =TRUE

#whether to write Species Relative Abundance rasters
makeRArasters = FALSE

#whether a PUpolys file is to be used - this is required for autoJFMP analysis, and can also be used as a work around to allow grouping of objects e.g. by fire district for the FAME dashboard ( by providing a PUPolys file where the PU correspond to fire districts rather than  burn units.)
usePUpolys = FALSE

##Settings if JFMP analysis is also to be run

runJFMP = FALSE
JFMPSeason0 = 2018

runCompareJFMP =FALSE
puPath = "D:/FAMEshiny/PUPolygons/DemoPUPolysVG94.shp"
zoneWtFile = "D:/FAMEshiny/CustomCSV/DemoJFMP/DemoJFMPZoneWt.csv"
jfmpMetricWtFile= "D:/FAMEshiny/CustomCSV/DemoJFMP/DemoJFMPMetricWt.csv"
targetHaFilepath<-"D:/FAMEshiny/CustomCSV/DemoJFMP/DemoJFMPTargets.csv"




