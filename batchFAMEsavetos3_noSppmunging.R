source("global.r")
unlink(resultsDir,recursive = TRUE)
source("batchsettings.r")
#this mounts the s3 fame-obs bucket to ec2 instance that has been setup following this process https://cloudkul.com/blog/mounting-s3-bucket-linux-ec2-instance/
system("s3fs fame-obm -o use_cache=/tmp -o allow_other -o uid=1001 -o mp_umask=002 -o multireq_max=5 /home/rstudio/ShinyApps/FAME/fame-obm")

#(rv name in kept for consistency with rv <- reactiveValues in shiny app)
rv<-list()
for (myFH in rawFHPaths)try({
  #  rawFH file to be run ----
  rv$rawFHPath <- myFH
  
  
  rv$rawFHName <- basename(rv$rawFHPath)
  
  #AdHoc shapefile file to be run ----
  rv$AdHocPath <- AdHocPath
  rv$AdHocName <- basename(rv$AdHocPath)
  
  # whether or not to make make TFI rasters----
  rv$makeTFIrasters <- makeTFIrasters
  
  # whether or not to make  BBTFI rasters----
  rv$makeBBTFIrasters <- makeBBTFIrasters
  
  #whether or not to run species calculations
  
  #customSpList be run ----
  
  rv$customSpList <- customSpList
  rv$customSpListName <- basename(rv$customSpList)
  
  # Observer of choice for PU polys sets values to null if FALSE  ----
  rv$usePUpolys = usePUpolys
  if (!rv$usePUpolys == TRUE) {
    rv$puPath <- NULL
    rv$puName <- NULL
  } else {
    # PU shapefile file to be run ----
    rv$puPath <- puPath
    rv$puName <- basename(rv$puPath)
    
    # # zone wt File be run ----
    # rv$zoneWtFile <- zoneWtFile
    # rv$zoneWtFileName <- basename(rv$zoneWtFile)
    # 
    # # jfmp metric wt File be run ----
    # rv$jfmpMetricWtFile <- jfmpMetricWtFile
    # rv$jfmpMetricWtFileName <- basename(rv$jfmpMetricWtFile)
    # 
    # # JFMP Area Target file  ----
    # 
    # rv$targetHaFilepath <- targetHaFilepath
    # rv$targetHaFileName <- basename(rv$targetHaFilepath)
  }
  
  
  
  # Observer of custom relative abundance table choice----
  
  rv$spResponseChoice <- spResponseChoice
  
  if (rv$spResponseChoice == TRUE){
    # customResponseFile be run ----
    
    rv$customResponseFile <- customResponseFile
    rv$customResponseName <- basename(rv$customResponseFile) 
  }
  
  
  
  
  # Observer of custom species list choice  ----
  
  rv$spListChoice <- spListChoice
  
  
  
  # Observer of custom relative abundance table choice----
  
  rv$spResponseChoice <- spResponseChoice
  
  
  # Observer of Relative abundance table by growth stage choice ----
  
  rv$abundByGS <- abundByGS
  
  # Observer of make rasters choice ----
  rv$makeRArasters <- makeRArasters
  
  
  
  
  # OBSERVERS of NUMERIC SETTINGS  ----
  # Observer for RasterRes----
  
  rv$RasterRes <- RasterRes
  
  
  # Observer for First season for analysis output (startTimespan)----
  
  rv$startTimespan <- startTimespan
  # Observer for start baseline----
  rv$startBaseline <- startBaseline
  
  # Observer for end baseline----
  
  rv$endBaseline <- endBaseline
  
  
  
  # Observer for jfmpSEASON0----
  
  #rv$JFMPSeason0 <- JFMPSeason0
  
  
  
  # OBSERVERS FOR RADIOBUTTON CHOICES  ----
  
  # Observer for public land----
  
  rv$public <- public
  
  
  # Observer for other and unknown fires----
  
  rv$otherUnknown <- otherUnknown
  
  # Observer for allOrSomeYears for writing rasters----
  
  #rv$allOrSomeYears <- allOrSomeYears
  
  
  
  # OBSERVERS for NON-FILE SELECT INPUTS ----
  
  # Observer for select SEASONS for write rasters (yearsForRasters)  ----
  # this is not working at the moment to reload the seasons selected in previous session
  
  #rv$yearsForRasters <- yearsForRasters
  
  #rv$runCompareJFMP<-runCompareJFMP
  
  # Observer for choose a region  ----
  rv$REGION_NO <- REGION_NO
  
  # OBSERVERS TO RUN MAIN FUNCTIONS----
  
  # Observer to runFH analysis ----
  
  rv$outputFH <- file_path_sans_ext(basename(rv$rawFHPath))
  if (rv$usePUpolys == TRUE) {
    rv$outputFH <- paste(rv$outputFH, file_path_sans_ext(basename(rv$puName)), sep = "_")
  }
  
  #output directories creation
  
  rv$resultsDir<-file.path(resultsDir,rv$outputFH)
  for (i in c("RA_Rasters","TFI_Rasters","BBTFI_Rasters")){
    dir.create(file.path(rv$resultsDir,i),recursive = TRUE)
  }
  #log.file
  my_log <- file(file.path(rv$resultsDir,"my_log.txt")) # File name of output log
#using the sink function to store the console output:
    
    
  sink(my_log, append = TRUE, type = "output") # Writing console output to log file
  sink(my_log, append = TRUE, type = "message")
  
  
  
  myREG_NO <- as.integer(rv$REGION_NO)
  RasterRes <- as.integer(rv$RasterRes)
  print(paste("RasterRes =", RasterRes))
  HDM_RASTER_PATH <-
    paste0("./HDMS/", rv$RasterRes, "m/BinaryThresholded")
  
  if (rv$REGION_NO == 7) {
    clipShape <- rv$AdHocPath
  } else {
    clipShape <- "./ReferenceShapefiles/LF_DISTRICT.shp"
  }
  
  
  
  
  
  
  cropRasters <- cropNAborder(
    REG_NO = myREG_NO,
    myRasterRes = RasterRes,
    PUBLIC_LAND_ONLY = rv$public,
    myPoly = file.path(clipShape),
    generalRasterDir = "./InputGeneralRasters"
  )
  
  cropRasters$HDM_RASTER_PATH <- HDM_RASTER_PATH
  rv$cropRasters <- cropRasters
  
  
  
  
  
  FHAnalysis <- fhProcess(
    rawFH = rv$rawFHPath,
    start.SEASON = rv$startTimespan,
    end.SEASON = rv$endSEASON,
    OtherAndUnknown = rv$otherUnknown,
    validFIRETYPE = c("BURN", "BUSHFIRE", "UNKNOWN", "OTHER")
  )
  # Save input settings to a list and then append into FH analysis object
  # FHAnalysis$AnalysisInputs<-list(
  FHAnalysis$FireScenario <- rv$rawFHName
  FHAnalysis$RasterRes <- rv$RasterRes
  FHAnalysis$ClipPolygonFile <- clipShape
  FHAnalysis$Region_No <- myREG_NO
  FHAnalysis$PUBLIC_ONLY <- rv$public
  FHAnalysis$Start_Season <- NULL
  
  FHAnalysis$name <- paste0("FH_Analysis_", rv$outputFH)
  
  st_write(FHAnalysis$OutDF,
           file.path(rv$resultsDir, paste0(FHAnalysis$name, ".shp")),
           append = FALSE
  )
  # )
  print("Save input settings to a list and then append into FH analysis object")
  FHAnalysis$FH_IDr <-
    fasterize(
      sf = FHAnalysis$OutDF,
      raster = cropRasters$Raster,
      field = "ID",
      fun = "first"
    )
  print("made FHAnalysis$FH_IDr")
  
  ##following if is only needed for JFMP
  # FHAnalysis <- FHAnalysis
  # # check if pupoly is to be used
  # if (rv$usePUpolys) {
  #   
  myPuPoly <- rv$puPath
  #   # update the FHAnalysis$OutDF  with noburn columns
  #   FHAnalysis$OutDF <-
  #     FHAnalysis$OutDF %>% bind_cols(make_JFMPNoBurnTab(
  #       myFHAnalysis = FHAnalysis,
  #       JFMPSeason0 = rv$JFMPSeason0
  #     ))
  #   FHAnalysis$YSFNames <- c(FHAnalysis$YSFNames, "YSFNoBurn")
  #   FHAnalysis$LBYNames <- c(FHAnalysis$LBYNames, "LBYNoBurn")
  #   FHAnalysis$LFTNames <- c(FHAnalysis$LFTNames, "LFTNoBurn")
  #   
  #   print("appended JFMPNoBurnCols")
  # } else {
  #   myPuPoly <- NULL
  # }
  
  rv$FHAnalysis <- FHAnalysis
  
  
  allCombs <- calcU_All_Combs(
    myFHAnalysis = rv$FHAnalysis,
    myCropRasters = rv$cropRasters,
    myRasterRes = RasterRes,
    puPoly = myPuPoly
  )
  rv$allCombs <- allCombs
  print("made allcombs")
  
  
  
  
  print("finished FH analysis")
  #save completed FHanalysis rv with 
  FHanalysisPath<-file.path(resultsDir ,paste0(gsub(".shp","",rv$outputFH),"FHanalysis",".qs"))
  qsave(rv, FHanalysisPath)
  #system(paste("aws s3 cp ",FHanalysisPath," s3://fame-obm/results"))
  system("aws s3 sync ~/ShinyApps/FAME/results s3://fame-obm/results")
  checkSaved<-system(paste0("aws s3 ls "," s3://fame-obm/results/",basename(FHanalysisPath)))
  if(checkSaved == 0){file.remove(FHanalysisPath)}
  
  #end of fhAnalysis block-------
  
  #block for TFI and GS calcuations in try()----
  try({
    # TFI  related calculations----
    
    print("running TFI calc")
    
    rv$TFI <- calc_TFI_2(
      myFHAnalysis = rv$FHAnalysis,
      myAllCombs = rv$allCombs,
      myTFI_LUT = TFI_LUT,
      OutputRasters = rv$makeTFIrasters,
      myResultsDir = rv$resultsDir
    )
    
    
    
    # need to change the sort order for the factor to get correct stacking
    # order ( no alphabetical) on chart
    rv$TFI$TFI_STATUS <-
      factor(
        rv$TFI$TFI_STATUS,
        levels = c(
          "BELOW_MIN_TFI",
          "WITHIN_TFI",
          "ABOVE_MAX_TFI",
          "NONE"
        )
      )
    
    
    
    # write results out to csv files
    readr::write_csv(rv$TFI,
                     file = file.path(rv$resultsDir, "TFI_LONG.csv")
    )
    readr::write_csv(
      rv$TFI %>%
        group_by(EFG_NAME, SEASON, TFI_STATUS) %>%
        summarise(AreaHa = sum(Hectares)) %>%
        pivot_wider(
          names_from = SEASON,
          values_from = AreaHa,
          values_fill = 0
        ),
      file = file.path(rv$resultsDir, "TFI_EFG_SUMMARY.csv")
    )
    
    print("Finished TFI calculations")
    
    # BBTFI  related calculations----
    
    
    print("calculating BBTFI")
    
    rv$BBTFI <- calcBBTFI_2(
      myFHAnalysis = rv$FHAnalysis,
      myAllCombs = rv$allCombs,
      makeBBTFIrasters = rv$makeBBTFIrasters,
      myResultsDir = rv$resultsDir
    )
    print("finished BBTFI calcs")
    
    
    
    write.csv(rv$BBTFI$BBTFI_LONG,
              file = file.path(
                rv$resultsDir,
                "BBTFI_LONG.csv"
              )
    )
    write.csv(
      rv$BBTFI$BBTFI_LONG %>%
        group_by(EFG_NAME, TBTFI) %>%
        summarise(AreaHa = sum(Hectares)) %>%
        pivot_wider(names_from = TBTFI, values_from = AreaHa),
      file = file.path(
        rv$resultsDir,
        "TimesBBTFI_SUMMARY.csv"
      )
    )
    
    write.csv(rv$BBTFI$BBTFI_WIDE,
              file = file.path(
                rv$resultsDir,
                "BBTFI_WIDE.csv"
              )
    )
    
    # Run GS calculations----
    
    
    print("GS Calculations")
    GS_Summary <-
      makeGS_Summary(
        myFHAnalysis = rv$FHAnalysis,
        myAllCombs = rv$allCombs
      )
    
    
    rv$GS_Summary <- GS_Summary
    
    rv$GS_Summary_PU_LONG <-rv$GS_Summary$GS_Summary_wide%>% dplyr::select(-c(MIN_LO_TFI, 
                                    MIN_HI_TFI, MAX_TFI, Index, FH_ID, FIRE_REG, FIREFMZ, 
                                    DELWP)) %>% tidyr::pivot_longer(tidyselect::all_of(TimeNames), 
                                                                    names_to = "SEASON", values_to = "GS") %>% 
      dplyr::group_by(EFG, EFG_NAME, PLM, FIRE_FMZ_NAME, FIRE_FMZ_SHORT_NAME, 
                      FIRE_REGION_NAME,PU, DELWP_REGION, SEASON, GS) %>% dplyr::summarise(Pixels = sum(nPixel), 
                                                                                       Hectares = sum(Hectares))
    GS_Summary_Long <- dplyr::left_join(GS_Summary_Long, GS_LUT)
    
    readr::write_csv(rv$GS_Summary$GS_Summary_Long,
                     file = file.path(
                       rv$resultsDir,
                       "GS_LONG.csv"
                     )
    )
    
    readr::write_csv(rv$GS_Summary$GS_Summary_wide,
                     file = file.path(
                       rv$resultsDir,
                       "GS_WIDE.csv"
                     )
    )
    print("finished GS calcs")
    
    

  })
  
  # species calculations ------
  if(doSpeciesCalculations){
    try({
      # run relative abundance analysis  ----
      
      startBaseline <- as.integer(rv$startBaseline)
      endBaseline <- as.integer(rv$endBaseline)
      
      Baseline <- startBaseline:endBaseline
      if (rv$spListChoice == FALSE) {
        rv$TaxonList <-
          read_csv("./ReferenceTables/FAME_TAXON_LIST.csv")
      } else {
        rv$TaxonList <- read_csv(rv$customSpList)
      }
      
      
      
      HDMSpp_NO <-
        rv$TaxonList$TAXON_ID[rv$TaxonList$Include == "Yes"]
      writeSp <-
        rv$TaxonList$TAXON_ID[rv$TaxonList$WriteSpeciesRaster == "Yes"]
      writeSp <- writeSp[writeSp %in% HDMSpp_NO]
      
      
      print("getting HDMvals")
      HDMVals <- qread(paste0(
        "./HDMS/HDMVals",
        rv$FHAnalysis$RasterRes,
        "list.qs"
      ))
      
      print("Loaded HDMVals")
      
      if (rv$spResponseChoice == FALSE) {
        mySpGSResponses <- "./ReferenceTables/OrdinalExpertLong.csv"
      } else {
        mySpGSResponses <-
          file.path(rv$customResponseFile)
      }
      # Select the file giving the fauna relative abundance inputs you wish to use----
      if (rv$abundByGS == TRUE) {
        AbundDataByGS <- read_csv(mySpGSResponses)[, c(
          "EFG_NO",
          "GS4_NO",
          "FireType",
          "Abund",
          "TAXON_ID"
        )]
        
        # If abundance data is provide by growth stage rather than time since fire expand it to the full time since fire long format ----
        AbundDataLong <- AbundDataByGS %>%
          dplyr::mutate(FireTypeNo = if_else(FireType == "High", 2, if_else(FireType == "Low", 1, 0))) %>%
          dplyr::left_join(EFG_TSF_4GS, by = c("EFG_NO", "GS4_NO")) %>%
          dplyr::arrange(TAXON_ID)
      } else {
        # Read abundance data already in full long format  ----
        AbundDataLong <- read_csv(mySpGSResponses) %>%
          dplyr::arrange(TAXON_ID)
      }
      
      # Make the lookup list of arrays for fast calculation of cell by cell species abundance ----
      print("making Spp abund LU List")
      LU_List <- make_Spp_LU_list(
        myHDMSpp_NO = HDMSpp_NO,
        myAbundDataLong = AbundDataLong
      )
      
      print("finished  Spp abund LU List")
      
      print("Making spYearSumm")
      
      source("calc_SpeciesRAmod.R")
      # Run the main function to get species abundance by cells ----
      rv$SpYearSumm <- calc_SpeciesRA(
        myFHAnalysis = rv$FHAnalysis,
        myAllCombs <- rv$allCombs,
        myHDMSpp_NO = HDMSpp_NO,
        myWriteSpRasters = rv$makeRArasters,
        myResultsDir = rv$resultsDir,
        myLU_List = LU_List,
        myHDMVals = HDMVals,
        myTaxonList = rv$TaxonList,
        writeYears = NULL,
        # rv$yearsForRasters,
        myWriteSp = writeSp,
        myIDX = rv$cropRasters$IDX
      )
      gc()
      # Save abundance summary outputs to csv files ----
      
      
      readr::write_csv(
        rv$SpYearSumm$SpYearSummLong,
        file.path(rv$resultsDir, "SpYearSummLong.csv")
      )
      readr::write_csv(
        rv$SpYearSumm$SpYearSummWide,
        file.path(rv$resultsDir, "SpYearSummWide.csv")
      )
      analysisPath<-file.path(resultsDir ,
                              paste0(gsub(".shp","",rv$outputFH),
                                     ifelse(is.null(rv$customSpListName),
                                            "DefaultTaxa",
                                            gsub(".csv","",rv$customSpListName)),
                                     "preWrangleData",
                                     ".qs"))
      qsave(rv, analysisPath)
      
      system("aws s3 sync ~/ShinyApps/FAME/results s3://fame-obm/results")
      checkSaved<-system(paste0("aws s3 ls "," s3://fame-obm/results/",basename(analysisPath)))
      
      if(checkSaved == 0){file.remove(analysisPath)}
    })
  }
  #system("aws s3 cp ~/ShinyApps/FAME/results s3://fame-obm/results --recursive")
  system("aws s3 sync ~/ShinyApps/FAME/results s3://fame-obm/results")
  checkSaved<-system(paste0("aws s3 ls "," s3://fame-obm/results/",basename(rv$resultsDir)))
  if(checkSaved == 0){unlink(rv$resultsDir,recursive = TRUE)}
  print(paste("Saved all results for ",rv$outputFH))
  close.connection(my_log)
  file.create(file.path(rv$resultsDir,"finished",Sys.time()))
  rm(rv)
  gc()
  
})







