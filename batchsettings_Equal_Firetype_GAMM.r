#Settings file to run a number of alternative fire history scenarios for the same area of interest though fame as a batch.  

# the batch will run through each of the raw fire history scenarios specified  by the shapefile paths in rawFHPath  in series putting the results in a separate subdirectory of the resultsDir.  All other inputs are constant throughout the batch

#path(s) to your rawFH file ( output of the ARCGIS preprocessing tool) each given as a separate string of the full path or ./path relative to the working directory. 
rawFHPaths = c("fame-obm/rawFH/RaawFH_FireHistory_01-0pc_zones_2022to2050/RawFH_FireHistory_01-0pc_zones_2022to2050_r01_merged.shp", 
               "fame-obm/rawFH/RaawFH_FireHistory_01-0pc_zones_2022to2050/RawFH_FireHistory_01-0pc_zones_2022to2050_r02_merged.shp")
#the root directory in which results directories should be created and outputs saved
resultsDir = "./results/Equal_Firetype_GAMM"
#the integer value of the region number 1-6 for FFR regions,7 for user supplied adHoc polygon, 99 for Statewide
REGION_NO = "99"
#path to the ad Hoc polygon if REGION_NO 			== 7 otherwise NULL
AdHocPath = "./AdHocPolygons/DemoAdHocPolygon.shp"

doSpeciesCalculations= TRUE

# whether or not custom species list is used ( TRUE for custom species list) otherwise FALSE
spListChoice = FALSE
customSpList = "./CustomCSV/FAMEIn_OBRMSppListAllSpp_update.csv"

#whether the species response file contains abundance data by 4 growth stages ( TRUE) or YSF (FALSE) 0-400
abundByGS =FALSE#TRUE
# whether the defualt or custom response file is used for abundance responses
spResponseChoice = TRUE
#path of cstum response abundance file if spResponseChoice==TRUE
customResponseFile = "CustomCSV/EO_cutOff.3.csv" 

#value to use for cases where fire type is "OTHER" or "UNKNOWN",1 ="BURN",2="BUSHFIRE",NA = Fire excluded from analysis default is 2 ("BUSHFIRE")
otherUnknown = 2
#whether analysis should be undertaken only on public land ( public only if TRUE)
public = FALSE
#raster resolution 75 or 225
RasterRes = 225

#"first season for which output is wanted ( four digit year as integer)
startTimespan = 1980

#start and end baseline years if single year then the two values should be equal
startBaseline = 1980
endBaseline = 2000
endSEASON = NULL


#whether to write TFI rasters
makeTFIrasters = FALSE
#whether to write BBTFI rasters
makeBBTFIrasters =FALSE

#whether to write Species Relative Abundance rasters
makeRArasters = FALSE


#whether a PUpolys file is to be used - this is required for autoJFMP analysis, and can also be used as a work around to allow grouping of objects e.g. by fire district for the FAME dashboard ( by providing a PUPolys file where the PU correspond to fire districts rather than  burn units "LF_DISTRICT_with_PU_field".)
usePUpolys = TRUE #FALSE
puPath = "./ReferenceShapefiles/LF_DISTRICT_with_PU_field.dbf"
#Settings if JFMP analysis is also to be run this is not working yet so  keep runJFMP = FALSE


runJFMP = FALSE

# Nev took out a heap JFMP related stuff here because there was a problem with having JFMPseason impacting on endseason in the batch process
