#Settings file to run a number of alternative fire history scenarios for the same area of interest though fame as a batch.  

# the batch will run through each of the raw fire history scenarios specified  by the shapefile paths in rawFHPath  in series putting the results in a separate subdirectory of the resultsDir.  All other inputs are constant throughout the batch

#path(s) to your rawFH file ( output of the ARCGIS preprocessing tool) each given as a separate string of the full path or ./path relative to the working directory. 
rawFHPaths =c("C:/Data/FAMEshiny/jennyOtawysSbbdebug/rawFH_FIRE_HISTORY_LF_DISTRICT_1755.gpkg")#FIRE_HISTORY20210310_DemoAdHocPolygon.gpkg") 
#the root directory in which results directories should be created and outputs saved
resultsDir = "./results"
#the integer value of the region number 1-6 for FFR regions,7 for user supplied adHoc polygon, 99 for Statewide
REGION_NO = "99"  #"99"

#path to the ad Hoc polygon if REGION_NO 			== 7 otherwise NULL
AdHocPath = "C:/Data/FAMEshiny/jennyOtawysSbbdebug/OtwaysNP_vicgrid.shp"

doSpeciesCalculations= TRUE

# whether or not custom species list is used ( TRUE for custom species list) otherwise FALSE
spListChoice = TRUE
customSpList = "C:/Data/FAMEshiny/jennyOtawysSbbdebug/SpeciesListSBB_statewide.csv"

#whether the species response file contains abundance data by 4 growth stages ( TRUE) or YSF (FALSE) 0-400
abundByGS =FALSE
# whether the default or custom response file is used for abundance responses
spResponseChoice = TRUE
#path of custom response abundance file if spResponseChoice==TRUE
customResponseFile = "C:/Data/FAMEshiny/jennyOtawysSbbdebug/sbb_gam1_fame_test.csv"#

#value to use for cases where fire type is "OTHER" or "UNKNOWN",1 ="BURN",2="BUSHFIRE",NA = Fire excluded from analysis default is 2 ("BUSHFIRE")
otherUnknown = 2
#whether analysis should be undertaken only on public land ( public only if TRUE)
public = TRUE
#raster resolution 75 or 225
RasterRes = 75

#"first season for which output is wanted ( four digit year as integer)
startTimespan = 2020
endTimespan =2030
#start and end baseline years if single year then the two values should be equal
startBaseline = 2020
endBaseline = 2020
endSEASON = NULL


#whether to write TFI rasters
makeTFIrasters = TRUE
#whether to write BBTFI rasters
makeBBTFIrasters = TRUE

#whether to write Species Relative Abundance rasters
makeRArasters = TRUE


# which years to write rasters if makeRArasters = TRUE
yearsForRasters = NULL#  c(2020)vector of SEASONS within limit of , or NULL for all years


#whether a PUpolys file is to be used - this is required for autoJFMP analysis, and can also be used as a work around to allow grouping of objects e.g. by fire district for the FAME dashboard ( by providing a PUPolys file where the PU correspond to fire districts rather than  burn units "LF_DISTRICT_with_PU_field".)
usePUpolys = FALSE
puPath = "./ReferenceShapefiles/LF_DISTRICT_with_PU_field.shp"
#Settings if JFMP analysis is also to be run this is not working yet so  keep runJFMP = FALSE

runJFMP = FALSE
JFMPSeason0 = endTimespan
