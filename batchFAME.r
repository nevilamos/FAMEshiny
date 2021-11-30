source("global.r")
unlink(resultsDir,recursive = TRUE)
source("batchsettings.r")


#(rv name in kept for consistency with rv <- reactiveValues in shiny app)
rv<-list()
for (myFH in rawFHPaths){
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
      
      # zone wt File be run ----
      rv$zoneWtFile <- zoneWtFile
      rv$zoneWtFileName <- basename(rv$zoneWtFile)
      
      # jfmp metric wt File be run ----
      rv$jfmpMetricWtFile <- jfmpMetricWtFile
      rv$jfmpMetricWtFileName <- basename(rv$jfmpMetricWtFile)
      
      # JFMP Area Target file  ----
      
      rv$targetHaFilepath <- targetHaFilepath
      rv$targetHaFileName <- basename(rv$targetHaFilepath)
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
  
  rv$JFMPSeason0 <- JFMPSeason0
  
  
  
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
  
  rv$runCompareJFMP<-runCompareJFMP
  
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
  
  
  if (rv$usePUpolys == TRUE) {
    rv$endSEASON <- rv$JFMPSeason0 + 4
  } else {
    rv$endSEASON <- NULL
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
  
  
  FHAnalysis <- FHAnalysis
  # check if pupoly is to be used
  if (rv$usePUpolys) {
    
    myPuPoly <- rv$puPath
    # update the FHAnalysis$OutDF  with noburn columns
    FHAnalysis$OutDF <-
      FHAnalysis$OutDF %>% bind_cols(make_JFMPNoBurnTab(
        myFHAnalysis = FHAnalysis,
        JFMPSeason0 = rv$JFMPSeason0
      ))
    FHAnalysis$YSFNames <- c(FHAnalysis$YSFNames, "YSFNoBurn")
    FHAnalysis$LBYNames <- c(FHAnalysis$LBYNames, "LBYNoBurn")
    FHAnalysis$LFTNames <- c(FHAnalysis$LFTNames, "LFTNoBurn")
    
    print("appended JFMPNoBurnCols")
  } else {
    myPuPoly <- NULL
  }
  
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
  
  
  if(doSpeciesCalculations){
    
    # Observer to run relative abundance analysis  ----
    
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
    # Wrangle grouped summary species abundance depending on whether or not it has Planning unit column ( for JFMP calcautions) ----
    
    rv$grpSpYearSummLong <- rv$SpYearSumm$grpSpYearSumm %>%
      dplyr::rename(Index_AllCombs = `myAllCombs$Index_AllCombs`) %>%
      tidyr::pivot_longer(
        -tidyr::one_of("TAXON_ID", "Index_AllCombs"),
        names_to = "SEASON",
        values_to = "sumRA"
      )
    # checks to see if planning punits/burn units (PU) used
    if (match("PU", names(rv$allCombs$U_AllCombs_TFI), nomatch = 0) >
        0) {
      rv$grpSpYearSummLong <- rv$grpSpYearSummLong %>%
        dplyr::mutate(
          PU = rv$allCombs$U_AllCombs_TFI$PU[Index_AllCombs],
          EFG_NAME = rv$allCombs$U_AllCombs_TFI$EFG_NAME[Index_AllCombs]
        ) %>%
        dplyr::group_by(TAXON_ID, PU, EFG_NAME, SEASON)
    } else {
      rv$grpSpYearSummLong <- rv$grpSpYearSummLong %>%
        dplyr::mutate(EFG_NAME = rv$allCombs$U_AllCombs_TFI$EFG_NAME[Index_AllCombs]) %>%
        dplyr::group_by(TAXON_ID, EFG_NAME, SEASON)
    }
    
    rv$grpSpYearSummLong <- rv$grpSpYearSummLong %>%
      dplyr::summarise(sumRA = sum(sumRA)) %>%
      dplyr::mutate(TAXON_ID = as.integer(TAXON_ID))
    
    
    readr::write_csv(
      rv$grpSpYearSummLong,
      file.path(rv$resultsDir, "grpSpYearSummLong.csv")
    )
    
    gc()
    print("finished sp year summ")
    
    # calculate changes in abundance relative to baseline years
    rv$raDeltaAbundWide <-
      calcDeltaAbund(
        SpYearSumm = rv$SpYearSumm$SpYearSummWide,
        myFHAnalysis = rv$FHAnalysis,
        myBaseline = Baseline
      )
    readr::write_csv(
      rv$raDeltaAbundWide,
      file.path(
        rv$resultsDir,
        "SppSummChangeRelativetoBaseline.csv"
      )
    )
    gc()
    
    
    # make long form for plotting charts
    rv$raDeltaAbundLong <- rv$raDeltaAbundWide %>%
      dplyr::select(
        -tidyr::one_of(
          "TAXON_ID",
          "DIVNAME",
          "EPBC_ACT_STATUS",
          "VIC_ADVISORY_STATUS",
          "CombThreshold",
          "NoLessthanThreshhold",
          "LastLessThanThreshold"
        )
      ) %>%
      tidyr::pivot_longer(
        -tidyr::one_of(
          "COMMON_NAME",
          "SCIENTIFIC_NAME",
          "Baseline"
        ),
        names_to = "SEASON",
        values_to = "DeltaRA"
      ) %>%
      dplyr::mutate(SEASON = as.integer(SEASON))
    print("finished deltaabund")
    
    
  }
  
  # Observer to run TFI  related calculations----
  
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
  
  # Observer to run BBTFI  related calculations----
  
  
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
  
  # Observer to run GS calculations----
  
  
  print("GS Calculations")
  GS_Summary <-
    makeGS_Summary(
      myFHAnalysis = rv$FHAnalysis,
      myAllCombs = rv$allCombs
    )
  
  
  rv$GS_Summary <- GS_Summary
  
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
  # Observer to run JFMP1  related calculations----
  if(runJFMP == TRUE)  {
    
    rv$zoneWt <- read_csv(rv$zoneWtFile)
    rv$jfmpMetricWt <- read_csv(rv$jfmpMetricWtFile)
    
    puDF <- jfmp1(
      myPUPath = rv$puPath,
      grpSpYearSumm = rv$SpYearSumm$grpSpYearSumm,
      myAllCombs = rv$allCombs,
      myTaxonList = rv$TaxonList,
      myBBTFI = rv$BBTFI,
      myJFMPSeason0 = rv$JFMPSeason0,
      zoneWt = rv$zoneWt,
      jfmpMetricWt = rv$jfmpMetricWt
    )
    rv$puDF <- puDF
    readr::write_csv(
      rv$puDF,
      file.path(
        rv$resultsDir,
        paste0(
          "PU_Rankings_",
          tools::file_path_sans_ext(rv$puName),
          ".csv"
        )
      )
    )
    gc()
    
    print(" jfmp1 PU_Rankings output saved to disk")
    
    print("running autoJFMP")
    rv$targetHa <- read_csv(rv$targetHaFilepath)
    rv$autoJFMP <- autoJFMP(
      myJFMP1 = rv$puDF,
      myTargetHa = rv$targetHa
    )
    
    readr::write_csv(
      rv$autoJFMP,
      file.path(
        rv$resultsDir,
        paste0(
          "AutoJFMP_",
          tools::file_path_sans_ext(rv$puName),
          ".csv"
        )
      )
    )
    
    autoJFMPsummary <- jfmpSummary(rv$autoJFMP)
    rv$autoJFMPsummary <- autoJFMPsummary
    
    readr::write_csv(
      rv$autoJFMPsummary,
      file.path(
        rv$resultsDir,
        paste0(
          "AutoJFMP_summary_",
          tools::file_path_sans_ext(rv$puName),
          ".csv"
        )
      )
    )
    
    print("finished auto JFMP")
  }
  
  
  # observer for draft JFMP comparison ----
  if(rv$runCompareJFMP == TRUE){
    
    draftFileName <- file_path_sans_ext(basename(rv$draftJFMPFile))
    
    rv$draftJfmpOut <- draftJfmpOut
    
    write_csv(
      rv$draftJfmpOut,
      file.path(
        rv$resultsDir,
        paste0(
          "allJFMP_Summary_",
          draftFileName, ".csv"
        )
      )
    )
    # Table with one row for each District
    # region , and columns for:
    #   – Hectares allocated to burns in draft JFMP in each zone
    # – Total hectares allocated to burn
    # –	Score for each metric (x4) if JFMP implemented
    # –	Score for each metrics (x4) if JFMP not implemented
    # this is same format as the autoJFMP summary but for draft JFMP
    draftJFMPSummary <- jfmpSummary(myDraftJfmpOut = rv$draftJfmpOut)
    rv$draftJFMPSummary <- draftJFMPSummary
    write_csv(
      draftJFMPSummary,
      file.path(
        rv$resultsDir,
        paste0(
          "Summary_draftJfmpOut_",
          draftFileName, ".csv"
        )
      )
    )
    print("Finished Draft JFMP summary")
    
    
    # JFMP_NeverBBTFI_Region_Summary ----
    jfmpBBTFISumm <- jfmpBBTFISumm(mydraftJfmpOut = rv$draftJfmpOut)
    rv$jfmpBBTFISumm <- jfmpBBTFISumm
    
    
    print("Finished JFMP BBTFI summary")
    
    jfmpRASumm <- jfmpRASumm(
      myDraftJfmpOut = rv$draftJfmpOut,
      myGrpSpYearSummLong = rv$grpSpYearSummLong[!is.na(rv$grpSpYearSummLong$PU), ],
      myTaxonList = rv$TaxonList,
      myStartBaseline = rv$startBaseline,
      myEndBaseline = rv$endBaseline,
      myJFMPSeason0 = rv$JFMPSeason0
    )
    
    rv$jfmpRASumm <- jfmpRASumm
    
    print(rv$jfmpRASumm)
    
    nBelowThreshHold <- rv$jfmpRASumm %>%
      group_by(JFMP_Name) %>%
      summarise(n_BelowThreshold = sum(BelowThreshold))
    
    rv$nBelowThreshHold <- nBelowThreshHold
    print(rv$nBelowThreshHold)
    write_csv(
      rv$jfmpRASumm,
      file.path(rv$resultsDir, "jfmpSppRaSumm.csv")
    )
    write_csv(
      rv$nBelowThreshHold,
      file.path(rv$resultsDir, "jfmpCountSpeciesBelowThreshHold.csv")
    )
    print("Finished JFMP RA summary") 
  }
  
  analysisPath<-file.path(resultsDir ,paste0(gsub(".shp","",rv$outputFH),ifelse(is.null(rv$customSpListName),"DefaultTaxa",gsub(".csv","",rv$customSpListName)),".qs"))
  qsave(rv, analysisPath)
}


