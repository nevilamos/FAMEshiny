server <- function(session, input, output) {
  rv <- reactiveValues()
  observe({
    rv$resultsDir <- resultsDir
    output$resultsDir <- renderText(rv$resultsDir)
    if (dir.exists(rv$resultsDir)) {

    } else {
      dir.create(rv$resultsDir)
      dir.create(file.path(rv$resultsDir, "RA_Rasters"))
      dir.create(file.path(rv$resultsDir, "TFI_Rasters"))
    }
  })

  # INPUT FILE SELECTION OBSERVERS  ----
  # Observer to get rawFH file to be run ----
  observe({
    roots <- c(wd = "./rawFH")
    shinyFileChoose(input,
      id = "selectRawFH",
      roots = roots,
      filetypes = "shp"
    )
    fileinfo <- parseFilePaths(roots, input$selectRawFH)
    if (nrow(fileinfo) > 0) {
      rv$rawFHPath <- as.character(fileinfo$datapath)
      rv$rawFHName <- basename(rv$rawFHPath)
    }
  })
  # Observer to display selected rawFH Shapefile in UI
  observeEvent(rv$rawFHName, {
    output$rawFHName <- renderText(basename(rv$rawFHName))
  })


  # Observer to get AdHoc shapefile file to be run ----
  observeEvent(input$selectAdHoc, {
    roots <- c(wd = "./AdHocPolygons")
    shinyFileChoose(input,
      id = "selectAdHoc",
      roots = roots,
      filetypes = "shp"
    )
    fileinfo <- parseFilePaths(roots, input$selectAdHoc)
    if (nrow(fileinfo) > 0) {
      rv$AdHocPath <- as.character(fileinfo$datapath)
      rv$AdHocName <- basename(rv$AdHocPath)
    }
  })
  # Observer to display selected Ad Hoc Shapefile in UI
  observeEvent(rv$AdHocName, {
    output$AdHocName <- renderText(basename(rv$AdHocName))
  })

  # Observer to get PU shapefile file to be run ----
  observeEvent(input$selectPU, {
    roots <- c(wd = "./PUPolygons")
    shinyFileChoose(input,
      id = "selectPU",
      roots = roots,
      filetypes = "shp"
    )
    fileinfo <- parseFilePaths(roots, input$selectPU)
    if (nrow(fileinfo) > 0) {
      rv$puPath <- as.character(fileinfo$datapath)
      rv$puName <- basename(rv$puPath)
    }
  })
  # Observer to display selected PU Shapefile in UI
  observe({
    if (is.null(rv$puName)) {
      myPuName <- ""
    } else {
      myPuName <- rv$puName
    }
    output$puName <- renderText(myPuName)
  })


  # Observer to get customSpList be run ----
  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "customSpList",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$customSpList)
    if (nrow(fileinfo) > 0) {
      rv$customSpList <- as.character(fileinfo$datapath)
      rv$customSpListName <- basename(rv$customSpList)
    }
  })

  # Observer to display selected customSpListName in UI
  observeEvent(rv$customSpListName, {
    output$customSpListName <- renderText(rv$customSpListName)
  })

  # Observer to get customResponseFile be run ----
  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "customResponseFile",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$customResponseFile)
    if (nrow(fileinfo) > 0) {
      rv$customResponseFile <- as.character(fileinfo$datapath)
      rv$customResponseName <- basename(rv$customResponseFile)
    }
  })
  # Observer to display selected customResponseName in UI
  observeEvent(rv$customResponseName, {
    output$customResponseName <- renderText(rv$customResponseName)
  })

  # Observer to get zone wt File be run ----
  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "zoneWtFile",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$zoneWtFile)
    if (nrow(fileinfo) > 0) {
      rv$zoneWtFile <- as.character(fileinfo$datapath)
      rv$zoneWtFileName <- basename(rv$zoneWtFile)
    }
  })
  # Observer to display selected jfmpMetricWtFileName in UI
  observeEvent(rv$zoneWtFileName, {
    output$zoneWtFileName <- renderText(rv$zoneWtFileName)
  })

  # Observer to get jfmp metric wt File be run ----
  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "jfmpMetricWtFile",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$jfmpMetricWtFile)
    if (nrow(fileinfo) > 0) {
      rv$jfmpMetricWtFile <- as.character(fileinfo$datapath)
      rv$jfmpMetricWtFileName <- basename(rv$jfmpMetricWtFile)
    }
  })
  # Observer to display selected zoneWtFileName in UI
  observeEvent(rv$jfmpMetricWtFileName, {
    output$jfmpMetricWtFileName <- renderText(rv$jfmpMetricWtFileName)
  })

  # Observer to get  draft jfmp input file be run ----
  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "draftJFMPFile",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$draftJFMPFile)
    if (nrow(fileinfo) > 0) {
      rv$draftJFMPFile <- as.character(fileinfo$datapath)
      rv$draftJFMPName <- basename(rv$draftJFMPFile)
    }
  })
  # Observer to display selected draft jfmpName in UI
  observeEvent(rv$draftJFMPName, {
    output$draftJFMPName <- renderText(rv$draftJFMPName)
  })



  # OBSERVERS of CHECKBOXES ----
  # Observer of choice for PU polys----
  observeEvent(input$usePUpolys, ignoreInit = T, {
    rv$usePUpolys <- input$usePUpolys
    rv$makeTFIrasters <- FALSE
    rv$makeBBTFIrasters <- FALSE
  })
  observeEvent(rv$usePUpolys, {
    if (rv$usePUpolys == TRUE) {
      updateCheckboxInput(
        session = session,
        inputId = "usePUpolys",
        value = TRUE
      )
    } else {
      updateCheckboxInput(
        session = session,
        inputId = "usePUpolys",
        value = FALSE
      )
      rv$puPath <- NULL
      rv$puName <- NULL
    }
  })

  # Observer of JFMP Area Target  ----

  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "targetHaFile",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$targetHaFile)
    if (nrow(fileinfo) > 0) {
      rv$targetHaFilepath <- as.character(fileinfo$datapath)
      rv$targetHaFileName <- basename(rv$targetHaFilepath)
    }
  })
  # Observer for makeTFIrasters----
  observeEvent(input$makeTFIrasters, {
    rv$makeTFIrasters <- input$makeTFIrasters
  })
  # Observer for makeBBTFIrasters----
  observeEvent(input$makeBBTFIrasters, {
    rv$makeBBTFIrasters <- input$makeBBTFIrasters
  })

  # Observer to display selected customSpListName in UI
  observeEvent(rv$targetHaFileName, {
    output$targetHaFileName <- renderText(rv$targetHaFileName)
  })
  # Observer to get customResponseFile be run ----
  observe({
    roots <- c(wd = "./CustomCSV")
    shinyFileChoose(input,
      id = "customResponseFile",
      roots = roots,
      filetypes = "csv"
    )
    fileinfo <- parseFilePaths(roots, input$customResponseFile)
    if (nrow(fileinfo) > 0) {
      rv$customResponseFile <- as.character(fileinfo$datapath)
      rv$customResponseName <- basename(rv$customResponseFile)
    }
  })
  # Observer to display selected customSpListName in UI
  observeEvent(rv$customResponseName, {
    output$customResponseName <- renderText(rv$customResponseName)
  })


  # Observer of custom species list choice  ----
  observeEvent(input$spListChoice, {
    rv$spListChoice <- input$spListChoice
  })
  observeEvent(rv$spListChoice, {
    if (rv$spListChoice == TRUE) {
      updateCheckboxInput(
        session = session,
        inputId = "spListChoice",
        value = TRUE
      )
    } else {
      updateCheckboxInput(
        session = session,
        inputId = "spListChoice",
        value = FALSE
      )
    }
  })

  # Observer of custom relative abundance table choice----
  observeEvent(input$spResponseChoice, {
    rv$spResponseChoice <- input$spResponseChoice
  })
  observeEvent(rv$spResponseChoice, {
    if (rv$spResponseChoice == TRUE) {
      updateCheckboxInput(
        session = session,
        inputId = "spResponseChoice",
        value = TRUE
      )
    } else {
      updateCheckboxInput(
        session = session,
        inputId = "spResponseChoice",
        value = FALSE
      )
    }
  })

  # Observer of Relative abundance table by growth stage choice ----
  observeEvent(input$abundByGS, {
    rv$abundByGS <- input$abundByGS
  })
  observeEvent(rv$abundByGS, {
    if (rv$abundByGS == TRUE) {
      updateCheckboxInput(
        session = session,
        inputId = "abundByGS",
        value = TRUE
      )
    } else {
      updateCheckboxInput(
        session = session,
        inputId = "abundByGS",
        value = FALSE
      )
    }
  })

  # Observer of make rasters choice ----
  observeEvent(input$makeRArasters, {
    rv$makeRArasters <- input$makeRArasters
  })
  observeEvent(rv$makeRArasters, {
    if (rv$makeRArasters == TRUE) {
      updateCheckboxInput(
        session = session,
        inputId = "makeRArasters",
        value = TRUE
      )
    } else {
      updateCheckboxInput(
        session = session,
        inputId = "makeRArasters",
        value = FALSE
      )
    }
  })


  # OBSERVERS of NUMERIC SETTINGS  ----
  # Observer for RasterRes----
  observeEvent(input$RasterRes, {
    rv$RasterRes <- as.integer(input$RasterRes)
  })
  observeEvent(rv$RasterRes, {
    updateRadioButtons(
      session = session,
      inputId = "RasterRes",
      selected = rv$RasterRes
    )
  })

  # Observer for First season for analysis output (startTimespan)----
  observeEvent(input$startTimespan, {
    rv$startTimespan <- input$startTimespan
  })

  observeEvent(rv$startTimespan, {
    updateNumericInput(
      session = session,
      inputId = "startTimespan",
      value = rv$startTimespan
    )
  })

  # Observer for start baseline----
  observeEvent(input$startBaseline, {
    rv$startBaseline <- input$startBaseline
  })

  observeEvent(rv$startBaseline, {
    updateNumericInput(
      session = session,
      inputId = "startBaseline",
      value = rv$startBaseline
    )
  })
  # Observer for end baseline----
  observeEvent(input$endBaseline, {
    rv$endBaseline <- input$endBaseline
  })

  observeEvent(rv$startBaseline, {
    updateNumericInput(
      session = session,
      inputId = "endBaseline",
      value = rv$endBaseline
    )
  })

  # Observer for jfmpSEASON0----
  observeEvent(input$JFMPSeason0, {
    rv$JFMPSeason0 <- input$JFMPSeason0
  })

  observeEvent(rv$JFMPSeason0, {
    updateNumericInput(
      session = session,
      inputId = "JFMPSeason0",
      value = rv$JFMPSeason0
    )
  })



  # OBSERVERS FOR RADIOBUTTON CHOICES  ----

  # Observer for public land----
  observeEvent(input$public, {
    rv$public <- input$public
  })

  observeEvent(rv$public, {
    updateRadioButtons(
      session = session,
      inputId = "public",
      selected = rv$public
    )
  })

  # Observer for other and unknown fires----
  observeEvent(input$otherUnknown, {
    rv$otherUnknown <- input$otherUnknown
  })
  observeEvent(rv$otherUnknown, {
    updateRadioButtons(
      session = session,
      inputId = "otherUnknown",
      selected = rv$otherUnknown
    )
  })

  # Observer for allOrSomeYears for writing rasters----
  observeEvent(input$allOrSomeYears, {
    rv$allOrSomeYears <- input$allOrSomeYears
    rv$writeYears = NULL
  })
  # observeEvent(rv$allOrSomeYears, {
  #   updateRadioButtons(
  #     session = session,
  #     inputId = "allOrSomeYears",
  #     selected = rv$allOrSomeYears
  #   )
  #   
  # })

  # OBSERVERS for NON-FILE SELECT INPUTS ----

  # Observer for select SEASONS for write rasters (yearsForRasters)  ----
  # this is not working at the moment to reload the seasons selected in previous session
  observeEvent(input$yearsForRasters, {
    rv$yearsForRasters <- input$yearsForRasters
    rv$writeYears = rv$yearsForRasters
  })
  observeEvent(rv$yearsForRasters, {
    updateSelectInput(
      session = session,
      inputId = "yearsForRasters",
      selected = rv$yearsForRasters
    )
  })

  # Observer for choose a region  ----

  observeEvent(input$REGION_NO, {
    rv$REGION_NO <- as.integer(input$REGION_NO)
  })
  observeEvent(rv$REGION_NO, {
    updateSelectInput(
      session = session,
      inputId = "REGION_NO",
      selected = rv$REGION_NO
    )
  })

  # OBSERVERS TO RUN MAIN FUNCTIONS----
  # Observer to runFH analysis ----
  observeEvent(input$runFH, {
    validate(need(rv$rawFHPath, "You need to select a raw FH to run analysis"))
    # if(rv$usePUPolys == TRUE){validate(need(!is.null(rv$puPath)), 'You need to select a PU/burn unit file to run analysis')}
    withBusyIndicatorServer("runFH", {
      rv$outputFH <- file_path_sans_ext(basename(rv$rawFHPath))
      if (input$usePUpolys == 1) {
        rv$outputFH <- paste(rv$outputFH, rv$puName, sep = "_")
      }
      
      
      print(paste("RasterRes =", rv$RasterRes))
      HDM_RASTER_PATH <-
        paste0("./HDMS/", rv$RasterRes, "m/BinaryThresholded")

      if (rv$REGION_NO == 7) {
        rv$clipShape <- rv$AdHocPath
      } else {
        rv$clipShape <- "./ReferenceShapefiles/LF_DISTRICT.shp"
      }


      if (input$usePUpolys == TRUE) {
        rv$endSEASON <- rv$JFMPSeason0 + 4
      } else {
        rv$endSEASON <- NULL
      }


      source("../FAMEFMR/R/cropNAborder.R")
      rv$cropRasters <- cropNAborder(
        REG_NO = rv$REGION_NO,
        myRasterRes = rv$RasterRes,
        PUBLIC_LAND_ONLY = rv$public,
        myPoly = rv$clipShape,
        generalRasterDir = "./InputGeneralRasters")

      rv$FHAnalysis <- fhProcess(
        rawFH = rv$rawFHPath,
        start.SEASON = rv$startTimespan,
        end.SEASON = rv$endSEASON,
        OtherAndUnknown = rv$otherUnknown,
        validFIRETYPE = c("BURN", "BUSHFIRE", "UNKNOWN", "OTHER")
      )
      # Save input settings to a list and then append into FH analysis object
      # FHAnalysis$AnalysisInputs<-list(
      rv$FHAnalysis$FireScenario <- rv$rawFHPath
      rv$FHAnalysis$RasterRes <- rv$RasterRes
      rv$FHAnalysis$ClipPolygonFile <- rv$clipShape
      rv$FHAnalysis$Region_No <- rv$REGION_NO
      rv$FHAnalysis$PUBLIC_ONLY <- rv$public
      rv$FHAnalysis$Start_Season <- NULL

      rv$FHAnalysis$name <- paste0("FH_Analysis_", rv$outputFH)

      st_write(rv$FHAnalysis$OutDF,
        file.path(rv$resultsDir, paste0(rv$FHAnalysis$name, ".shp")),
        append = FALSE
      )


      rv$FHAnalysis$FH_IDr <-
        rasterize(
          x = terra::vect(rv$FHAnalysis$OutDF),
          y= eval(rv$cropRasters$rasterDef),#rv$cropRasters$Raster
          field = "ID"
        )
      rv$cropRasters$FH_ID<-values(rv$FHAnalysis$FH_IDr)
      print("made FHAnalysis$FH_IDr")


      
      # check if pupoly is to be used
      if (input$usePUpolys) {
        validate(
          need(
            rv$puPath,
            "You  have selected to require a PU/BU polygon file but have not selected one"
          )
        )
        # <- rv$puPath
        # update the FHAnalysis$OutDF  with noburn columns
        rv$FHAnalysis$OutDF <-
          rv$FHAnalysis$OutDF %>% bind_cols(make_JFMPNoBurnTab(
            myFHAnalysis = rv$FHAnalysis,
            JFMPSeason0 = rv$JFMPSeason0
          ))
        rv$FHAnalysis$YSFNames <- c(rv$FHAnalysis$YSFNames, "YSFNoBurn")
        rv$FHAnalysis$LBYNames <- c(rv$FHAnalysis$LBYNames, "LBYNoBurn")
        rv$FHAnalysis$LFTNames <- c(rv$FHAnalysis$LFTNames, "LFTNoBurn")

        print("appended JFMPNoBurnCols")
      } else {
        rv$puPath <- NULL
      }

      


      rv$allCombs <- calcU_All_Combs(
        myFHAnalysis = rv$FHAnalysis,
        myCropRasters = rv$cropRasters,
        myRasterRes = rv$RasterRes,
        puPoly = rv$puPath
      )
      
      print("made allcombs")
    })



    print("finished FH analysis")
  })
  # Observer to run relative abundance analysis  ----
  observeEvent(
    {
      input$runRA | input$runRA_TFI | input$runJFMP1
    },
    ignoreInit = T,
    {
      withBusyIndicatorServer("runRA", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(need(
            rv$FHAnalysis,
            "You need to run or load a FH analysis first"
          ))
          startBaseline <- as.integer(rv$startBaseline)
          endBaseline <- as.integer(rv$endBaseline)
          validate(
            need(
              endBaseline >= startBaseline,
              "must have baseline start season <= end season"
            )
          )
          Baseline <- startBaseline:endBaseline
          if (rv$spListChoice == FALSE) {
            rv$TaxonList <-
              read_csv("./ReferenceTables/FAME_TAXON_LIST.csv")
          } else {
            validate(need(
              rv$customSpList,
              "You need to select a species list to use"
            ))
            print(rv$customSpList)
            rv$TaxonList <-
              read_csv(rv$customSpList)
          }



          rv$HDMSpp_NO <-
            rv$TaxonList$TAXON_ID[rv$TaxonList$Include == "Yes"]
          writeSp <-
            rv$TaxonList$TAXON_ID[rv$TaxonList$WriteSpeciesRaster == "Yes"]
          writeSp <- writeSp[writeSp %in% rv$HDMSpp_NO]

# 
#           print("getting HDMvals")
#           HDMVals <- qread(paste0(
#             "./HDMS/HDMVals",
#             rv$FHAnalysis$RasterRes,
#             "list.qs"
#           ))
# 
#           print("Loaded HDMVals")

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
            rv$AbundDataLong <- AbundDataByGS %>%
              dplyr::mutate(FireTypeNo = if_else(FireType == "High", 2, if_else(FireType == "Low", 1, 0))) %>%
              dplyr::left_join(EFG_TSF_4GS, by = c("EFG_NO", "GS4_NO"),multiple = "all") %>%
              dplyr::arrange(TAXON_ID)
          } else {
            # Read abundance data already in full long format  ----
            rv$AbundDataLong <- read_csv(mySpGSResponses) %>%
              dplyr::arrange(TAXON_ID)
          }

          # Make the lookup list of arrays for fast calculation of cell by cell species abundance ----
          print("making Spp abund LU List")
          rv$LU_List <- make_Spp_LU_list(
            myHDMSpp_NO = rv$HDMSpp_NO,
            myAbundDataLong = rv$AbundDataLong
          )

          print("finished  Spp abund LU List")

          print("Making spYearSumm")


          # Run the main function to get species abundance by cells ----
          rv$SpYearSumm <- calc_SpeciesRA(
            myFHAnalysis = rv$FHAnalysis,
            myAllCombs <- rv$allCombs,
            myHDMSpp_NO = rv$HDMSpp_NO,
            myWriteSpRasters = rv$makeRArasters,
            myResultsDir = rv$resultsDir,
            myLU_List = rv$LU_List,
            #myHDMVals = HDMVals,
            myTaxonList = rv$TaxonList,
            writeYears = rv$writeYears,
            myWriteSp = writeSp,
            myCropRasters = rv$cropRasters
            #myIDX = rv$cropRasters$IDX
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
        })
      })
    }
  )
  # Observer to get update years for calculations----
  observeEvent(rv$FHAnalysis$TimeSpan, {
    updateSelectInput(
      session,
      "yearsForRasters",
      choices = rv$FHAnalysis$TimeSpan,
      selected = min(rv$FHAnalysis$TimeSpan)
    )
    updateSelectInput(
      session,
      "startBaseline",
      choices = rv$FHAnalysis$TimeSpan,
      selected = min(rv$FHAnalysis$TimeSpan)
    )
    updateSelectInput(
      session,
      "endBaseline",
      choices = rv$FHAnalysis$TimeSpan,
      selected = min(rv$FHAnalysis$TimeSpan)
    )
  })

  # Observer to run TFI  related calculations----
  observeEvent(
    {
      input$runTFI | input$runRA_TFI
    },
    ignoreInit = T,
    {
      withBusyIndicatorServer("runTFI", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(need(
            rv$FHAnalysis,
            "You need to select a FH analysis to use"
          ))
          print("running TFI calc")
          

          rv$TFI <- calc_TFI_2(
            myFHAnalysis = rv$FHAnalysis,
            myAllCombs = rv$allCombs,
            myTFI_LUT = TFI_LUT,
            OutputRasters = rv$makeTFIrasters,
            myResultsDir = rv$resultsDir,
            myCropRasters = rv$cropRasters
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
        })
      })
    }
  )
  # Observer to run BBTFI  related calculations----
  observeEvent(
    {
      input$runTFI | input$runRA_TFI | input$runJFMP1
    },
    ignoreInit = T,
    {
      withBusyIndicatorServer("runTFI", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(
            need(
              rv$FHAnalysis,
              "You need to select a FH analysis to use"
            )
          )

          print("calculating BBTFI")

          rv$BBTFI <- calcBBTFI_2(
            myFHAnalysis = rv$FHAnalysis,
            myAllCombs = rv$allCombs,
            myCropRasters = rv$cropRasters,
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
        })
      })
    }
  )

  # Observer to run GS calculations----
  observeEvent(input$runGS | input$runRA_TFI,
    ignoreInit = T,
    {
      withBusyIndicatorServer("runGS", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(need(
            rv$FHAnalysis,
            "You need to select a FH analysis to use"
          ))

          print("GS Calculations")
          GS_Summary <-
            makeGS_Summary(
              myFHAnalysis = rv$FHAnalysis,
              myAllCombs = rv$allCombs
            )

          print(names(GS_Summary))
          print(GS_Summary)


          rv$GS_Summary <- GS_Summary
          print(rv$GS_Summary)
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
      })
    }
  )

  # Observer to run JFMP1  related calculations----
  observeEvent(
    {
      input$runJFMP1
    },
    ignoreInit = T,
    {
      withBusyIndicatorServer("runJFMP1", {
        validate(
          need(rv$usePUpolys == TRUE, message = "The FHAnalysis does not contain planning/burn units for JFMP calculations")
        )
        validate(need(length(rv$jfmpMetricWtFile) > 0, message = "You need to select a JFMP metric weight file"))
        validate(need(length(rv$zoneWtFile) > 0, message = "You need to select a JFMP zone Weight file"))
        print("doing JFMP1")
        # read in lookup tables for weighting of JFMP
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
      })
    }
  )

  # observer for draft JFMP comparison ----
  observeEvent(
    input$runCompareJFMP,
    ignoreInit = T,
    tryCatch({{ draftJfmpOut <-
      joinDraftJFMP(
        myDraftJFMPFile = rv$draftJFMPFile,
        myAutoJFMP = rv$autoJFMP
      )


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
    print("Finished JFMP RA summary") }},
    warning = function(warn) {
      showNotification(paste0(warn), type = "warning")
    },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
    )
  )

  # Observer prints the details of currently selected FHanalysis ----
  observeEvent(rv$FHAnalysis$name, ignoreInit = T, {
    output$selected_FH_name <- renderText({
      paste(
        "FH Analysis selected =",
        as.character(rv$FHAnalysis$name)
      )
    })
  })



  # Observer to display TFI and BBTFI plots when available ----

  observeEvent(
    {
      rv$TFI
    },
    ignoreInit = T,
    {
      myChoices <- unique(rv$TFI$EFG_NAME)
      myChoices <- myChoices[!is.na(myChoices)]
      updateSelectInput(session, "EFGChoices", choices = myChoices)
      # updateTabItems(session, "tabs", "TFIplots")

      minSEASON <- min(rv$TFI$SEASON)
      maxSEASON <- max(rv$TFI$SEASON)
      updateSliderInput(
        session,
        "tfiSeasonChoices",
        min = minSEASON,
        max = maxSEASON,
        value = c(1980, maxSEASON)
      )
      # Make plot of area by TFI status ----
      output$TFItrendPlot <- renderPlotly({
        rv$TFI %>%
          filter(EFG_NAME == input$EFGChoices) %>%
          group_by(TFI_STATUS, SEASON) %>%
          summarise(Area = sum(Hectares)) %>%
          plot_ly(
            x = ~SEASON,
            y = ~Area,
            group = ~TFI_STATUS,
            type = "bar",
            color = ~TFI_STATUS,
            colors = c(
              "BELOW_MIN_TFI" = "#fb8072",
              "WITHIN_TFI" = "#8dd3c7",
              "ABOVE_MAX_TFI" = "#80b1d3",
              "NONE" = "#ffffb3"
            )
          ) %>%
          layout(
            title = paste0(input$EFGChoices, "\n", "TFI Status"),
            yaxis = list(rangemode = "tozero", title = "Area (ha)"),
            xaxis = list(range = (
              input$tfiSeasonChoices + c(-0.5, 0.5)
            )),
            barmode = "stack",
            showlegend = T
          )
      })

      # if(nrow(bbtfivals)>0){

      output$BBTFIPlot <- renderPlotly({
        bbtfivals <- rv$BBTFI$BBTFI_LONG %>%
          filter(EFG_NAME == input$EFGChoices) %>%
          mutate(TBTFI = as.factor(TBTFI)) %>%
          group_by(TBTFI, SEAS) %>%
          summarise(Area = sum(Hectares)) %>%
          drop_na()
        # work around to maintain column width where there are gaps between values
        if (nrow(bbtfivals) > 0) {
          myYears <- input$tfiSeasonChoices[1]:input$tfiSeasonChoices[2]
          SEAS <- myYears[!myYears %in% unique(bbtfivals$SEAS)]
          SEASL <- length(SEAS)
          if (SEASL > 0) {
            TBTFI <- (rep(NA, SEASL))
            Area <- rep(0, SEASL)
            Padding <- data.frame(TBTFI, SEAS, Area)
            bbtfivals <- rbind(bbtfivals, Padding)
          }
        }

        bbtfivals %>%
          plot_ly(
            x = ~SEAS,
            y = ~Area,
            type = "bar",
            color = ~TBTFI
          ) %>%
          layout(
            title = paste0(input$EFGChoices, "\n", "Times burned below TFI"),
            yaxis = list(rangemode = "tozero", title = "Area (ha)"),
            xaxis = list(range = input$tfiSeasonChoices + c(-0.5, 0.5)),
            barmode = "stack",
            showlegend = T
          )
      })
      # output$BBTFIPlot <- renderUI(plotlyOutput("myBBTFIPlot"))
      # }
    }
  )


  # Observer to display GS plots when available ----
  observeEvent(
    rv$GS_Summary,
    ignoreInit = T,
    {
      myChoices <- unique(rv$GS_Summary$GS_Summary_Long$EFG_NAME)
      myChoices <- myChoices[!is.na(myChoices)]
      updateSelectInput(session, "GSEFGChoices", choices = myChoices)
      # updateTabItems(session, "tabs", "GSplots")
      minSEASON <- min(rv$GS_Summary$GS_Summary_Long$SEASON)
      maxSEASON <- max(rv$GS_Summary$GS_Summary_Long$SEASON)
      updateSliderInput(
        session,
        "GSSeasonChoices",
        min = minSEASON,
        max = maxSEASON,
        value = c(1980, maxSEASON)
      )
      output$GSPlot <- renderPlotly({
        rv$GS_Summary$GS_Summary_Long %>%
          filter(EFG_NAME == input$GSEFGChoices) %>%
          group_by(GROWTH_STAGE, SEASON) %>%
          summarise(Area = sum(Hectares)) %>%
          # reordering the factor for GROWTH STAGE so they plot stacked appropriately
          mutate(GROWTH_STAGE = factor(
            GROWTH_STAGE,
            levels = c(
              "Juvenile",
              "Adolescent",
              "Mature",
              "Old"
            )
          )) %>%
          plot_ly(
            x = ~SEASON,
            y = ~Area,
            type = "bar",
            color = ~GROWTH_STAGE,
            colors = c(
              "Old" = "#80b1d3",
              "Mature" = "#8dd3c7",
              "Adolescent" = "#fb8072",
              "Juvenile" = "#ffffb3"
            )
          ) %>%
          layout(
            title = paste0(input$GSEFGChoices, "\n", "Growth Stages"),
            yaxis = list(rangemode = "tozero", title = "Area (ha)"),
            xaxis = list(range = input$GSSeasonChoices + c(-0.5, 0.5)),
            barmode = "stack",
            showlegend = T
          )
      })
    }
  )


  # Observers make RA charts when available----
  observeEvent(rv$SpYearSumm, ignoreInit = T, {
    myChoices <- unique(rv$SpYearSumm$SpYearSummLong$COMMON_NAME)
    updateSelectizeInput(session, "raSpChoices", choices = myChoices)
    # updateTabItems(session, "tabs", "RAplots")
    # gets the seasons that have been calculated and removes the dummy no abund ( SEASON =9999) so that this does not inflate the axes
    allSEASONS <- rv$SpYearSumm$SpYearSummLong$SEASON
    displaySEASONS <- allSEASONS[allSEASONS != 9999]
    minSEASON <- min(displaySEASONS)
    maxSEASON <- max(displaySEASONS)
    updateSliderInput(
      session,
      "raSeasonChoices",
      min = minSEASON,
      max = maxSEASON,
      value = c(1980, maxSEASON)
    )
  })

  observeEvent(input$raSpChoices, ignoreInit = T, {
    output$RAtrendPlot <- renderPlotly({
      if (length(input$raSpChoices) == 0) {
        return()
      } else {
        rv$SpYearSumm$SpYearSummLong %>%
          filter(COMMON_NAME %in% input$raSpChoices) %>%
          plot_ly(
            x =  ~SEASON,
            y =  ~SUM_RAx100,
            type = "scatter",
            mode = "lines+markers",
            color =  ~COMMON_NAME
          ) %>%
          layout(
            yaxis = list(
              rangemode = "tozero",
              title = "Sum of relative abundance x 100"
            ),
            xaxis = list(range = input$raSeasonChoices + c(-0.5, 0.5)),
            showlegend = T
          )
      }
    })
    output$RADeltaPlot <- renderPlotly({
      if (length(input$raSpChoices) == 0) {
        return()
      } else {
        rv$raDeltaAbundLong %>%
          filter(COMMON_NAME %in% input$raSpChoices) %>%
          plot_ly(
            x =  ~SEASON,
            y =  ~DeltaRA,
            type = "scatter",
            mode = "lines",
            color =  ~COMMON_NAME
          ) %>%
          layout(
            yaxis = list(
              rangemode = "tozero",
              title = "Change in relative abundance\ncompared to baseline"
            ),
            xaxis = list(range = input$raSeasonChoices + c(-0.5, 0.5)),
            showlegend = T
          )
      }
    })
  })

  # Calculate custom species list----
  observeEvent(input$runDSpList, {
    withBusyIndicatorServer("runDSpList", {
      req(input$spREGION_NO)
      if (input$spREGION_NO == 7) {
        myPoly <- file.path("./AdHocPolygons", input$spAdHocShape)
      } else {
        myPoly <- "./ReferenceShapefiles/LF_DISTRICT.shp"
      }
      print("calcuating draft species List")
      myDraftSpList <- calc_DraftSpList(
        REG_NO = input$spREGION_NO,
        RasterRes = 225,
        myPoly = myPoly,
        PUBLIC_LAND_ONLY = input$sppublic,
        myHDMVals = "./HDMS/HDMVals225.qs",
        splist = "./ReferenceTables/FAME_TAXON_LIST.csv"
      )
      print(head(myDraftSpList))
      readr::write_csv(
        myDraftSpList,
        file.path(rv$resultsDir, "myDraftspList.csv")
      )
      print("made draft species List")
    })
  })
  # Calculate SppEFGLMU for GSO----
  observeEvent(input$runspEFGpList, {
    withBusyIndicatorServer("runspEFGpList", {
      req(input$spREGION_NO)
      if (input$spREGION_NO == 7) {
        myPoly <- file.path("./AdHocPolygons", input$spAdHocShape)
      } else {
        myPoly <- "./ReferenceShapefiles/LF_DISTRICT.shp"
      }


      myEFG_LMU <- make_Draft_GSO_inputs(
        REG_NO = input$spREGION_NO,
        # REG_NO of defined region from input (1:6) or 0 for statewide or 7 for Ad Hoc Poly),
        RasterRes = 225,
        PUBLIC_LAND_ONLY = input$sppublic,
        myPoly = myPoly,
        # shapefile ofLF_REGIONs( default)or  adhoc region,
        generalRasterDir = "./InputGeneralRasters",
        splist = "./ReferenceTables/FAME_TAXON_LIST.csv",
        myHDMVals = "./HDMS/HDMVals225.qs",
        # EFGRas="./InputGeneralRasters/EFG_NUM_225.tif",
        TFI_LUT = TFI_LUT
      )
      # print(myEFG_LMU)
      write_csv(
        myEFG_LMU$LMU_EFG_AREA,
        file.path(rv$resultsDir, "LMU_Area.csv")
      )
      write_csv(
        myEFG_LMU$Spp_EFG_LMU,
        file.path(rv$resultsDir, "Spp_EFG_LMU.csv")
      )
      write_csv(
        myEFG_LMU$LMU_Scenario,
        file.path(rv$resultsDir, "Draft_LMU_Scenarios.csv")
      )
    })
  })


  # })
  # Run Aspatial GSO----

  # this section runs the rmd script and documents for aspatial GSO that was written by Paul Moloney in 2017. and modified to use shiny GUI
  # Observers for GSO .csv uploads  ----
  observe({
    myInput <- input$addGSOCSV
    savePath <- "./GSOInputs"
    file.copy(
      myInput$datapath,
      file.path(savePath, myInput$name)
    )
    updateSelectInput(
      session,
      inputId = "spEFGLMU",
      label = "Select Spp_EFG_LMU.csv file",
      choice = c(list.files("./GSOInputs/", pattern = "Spp_EFG_LMU.csv$"))
    )
    updateSelectInput(session,
      "lmuArea",
      "LMU_Area.csv file",
      choice = c(list.files("./GSOInputs/", pattern = "LMU_Area.csv$"))
    )
    updateSelectInput(session,
      "lmuScenarios",
      "LMU_Scenarios.csv file",
      choice = c(
        list.files("./GSOInputs/", pattern = "LMU_Scenarios.csv$")
      )
    )
    updateSelectInput(session,
      "ObsData",
      "ObsData.csv file",
      choice = c(list.files("./GSOInputs/", pattern = "ObsData.csv$"))
    )
  })


  observeEvent(input$lmuScenarios,
    ignoreInit = T,
    {
      myScenarios <- unique(read_csv(file.path(
        WD,
        "GSOInputs",
        input$lmuScenarios
      ))$Scenario)
      updateSelectInput(session,
        "GSOBaseLine",
        choices = c("Optimisation", myScenarios)
      )
    }
  )

  observeEvent(input$runGSO, {
    withBusyIndicatorServer("runGSO", {
      fileConn <- file("./GSO/GSOSettings.r")



      writeLines(
        c(
          paste0("GSOResultsDir ='", file.path(WD, rv$resultsDir), "'"),
          paste0("FireType ='", input$GSOFireType, "'"),
          paste0("Comparison = '", input$GSOBaseLine, "'"),
          paste0("Classes = '", input$GSOFaunaClasses, "'"),
          paste0("Rule = '", input$GSOrule, "'"),
          paste0("dWt = ", input$GSOdwt),
          paste0("nrep = ", input$GSOnrep),
          paste0("nsim = ", input$GSOnsim),
          paste0(
            "SpEFGLMU = read_csv('",
            file.path(WD, "GSOInputs", input$spEFGLMU),
            "',na='NA')"
          ),
          paste0(
            "GSOScen<-read_csv('",
            file.path(WD, "GSOInputs", input$lmuScenarios),
            "',na='NA')"
          ),
          paste0(
            "GSOArea<-read_csv('",
            file.path(WD, "GSOInputs", input$lmuArea),
            "',na='NA')"
          ),
          paste0(
            "SurveyData<-read_csv('",
            file.path(WD, "GSOInputs", input$ObsData),
            "',na='NA')"
          )
        ),
        fileConn
      )
      close(fileConn)
      rmarkdown::render(
        "./GSO/GSOAnalysisOutput.Rmd",
        output_dir = rv$resultsDir,
        clean = T
      )
    })
  })

  # Observer to save current analysis reactive values to file as list----
  observeEvent(input$saveAnalysis, {
    roots <- c("UserFolder" = "./FH_Outputs")
    myRvList <- reactiveValuesToList(rv)
    shinyFileSave(input, "saveAnalysis", roots = roots)
    fileinfo <- parseSavePath(roots, input$saveAnalysis)

    if (nrow(fileinfo) > 0) {
      qsave(myRvList, as.character(fileinfo$datapath))
      gc()
    }
  })
  # UTILITIES - LOADING PREVIOUS ANALYSIS AND SERVER SHUTDOWN
  # Observer to load analysis from list as qs file----
  observe({
    roots <- c("UserFolder" = "./FH_Outputs")
    shinyFileChoose(input,
      id = "loadAnalysis",
      roots = roots,
      filetypes = "qs"
    )

    fileinfo <- parseFilePaths(roots, input$loadAnalysis)
    if (nrow(fileinfo) > 0) {
      for (i in names(rv)) {
        rv[[i]] <- NULL
      }
      myRvList <- qread(as.character(fileinfo$datapath))
      x <- (names(myRvList))
      for (i in x) {
        rv[[i]] <- myRvList[[i]]
      }
      gc()
    }
  })

  # Observer  to switch to settings page when analysis loaded----
  observeEvent(
    input$loadAnalysis,
    {
      updateTabItems(
        session,
        "tabs",
        "AnalysisSettings"
      )
    }
  )

  # Observer to shut down server ----
  # THIS IS NOT WORKING WITH CURRENT UBUNTU VERSION DUE TO SECURITY CHANGES
  # SHOULD BE REPLACED BY aws TIME OUT SETTINGS ON SERVER
  observeEvent(input$close, {
    js$closeWindow()
    system("sudo shutdown -h now")
  })

  # UPLOADS AND DOWNLOADS OF FILES AND RESULTS ----
  # Observer for loading fire scenario shapefiles ----
  # This code is repeated with modifications for each shapefile load ideally would be made into function or module
  observe({
    myInput <- input$rawFH
    savePath <- "./rawFH"
    if (is.null(myInput)) {
      return()
    }
    shapefile_components <- c("shp", "shx", "prj", "dbf")
    y <- NULL
    x <- NULL
    x <- length(unique(tools::file_path_sans_ext(
      tools::file_path_sans_ext(myInput$name)
    )) == 1)
    y <-
      (length(myInput$name) == 4 &
        sum(shapefile_components %in% tools::file_ext(myInput$name)) == 4)


    if (x == T) {
      if (y == T) {
        rawFHPath <- file.path(savePath, myInput$name)
        rv$rawFHPath <- rawFHPath

        file.copy(
          myInput$datapath,
          file.path(rv$rawFHPath)
        )
        # updateSelectInput(
        #   session,
        #   'unionedFH',
        #   'Select fire scenario shapefile',
        #   choice = c("", list.files('./rawFH/', pattern =
        #                               ".shp$"))
        # )
        myText <- "shapefile uploaded"
        showtable <- "YES"
        output$rawFHTable <- renderTable(myInput[, 1:2])
      } else {
        myText <- paste(
          "<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>"
        )
        showtable <- "NO"
      }
    } else {
      if (y == T) {
        myText <- paste(
          "<span style=\"color:red\"all elements of shapefile do not have same basename</span>"
        )
        output$showtable <- "NO"
      } else {
        myText <- paste("<span style=\"color:red\">wrong file elements selected</span>")
        showtable <- "NO"
      }
    }

    output$message_text <- renderText({
      myText
    })
    output$panelStatus <- reactive({
      showtable
    })
    outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
  })




  # Observer for loading AdHoc shapefiles ----
  observe({
    myInput <- input$adHocPoly
    savePath <- "./AdHocPolygons"
    if (is.null(myInput)) {
      return()
    }
    shapefile_components <- c("shp", "shx", "prj", "dbf")
    y <- NULL
    x <- NULL
    x <- length(unique(tools::file_path_sans_ext(
      tools::file_path_sans_ext(myInput$name)
    )) == 1)
    y <-
      (length(myInput$name) == 4 &
        sum(shapefile_components %in% tools::file_ext(myInput$name)) == 4)


    if (x == T) {
      if (y == T) {
        file.copy(
          myInput$datapath,
          file.path(
            savePath,
            myInput$name
          )
        )
        # update
        rv$AdHocPoly <- myInput$name
        myText1 <- "shapefile uploaded"
        showtable1 <- "YES"
        output$rawFHTable <- renderTable(myInput[, 1:2])
        updateSelectInput(
          session,
          inputId = "spAdHocShape",
          choices = c("", list.files("./AdHocPolygons", pattern = ".shp$"))
        )
      } else {
        myText1 <- paste(
          "<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>"
        )
        showtable1 <- "NO"
      }
    } else {
      if (y == T) {
        myText1 <- paste(
          "<span style=\"color:red\"all elements of shapefile do not have same basename</span>"
        )
        output1$showtable <- "NO"
      } else {
        myText1 <- paste("<span style=\"color:red\">wrong file elements selected</span>")
        showtable1 <- "NO"
      }
    }

    output$message_text1 <- renderText({
      myText1
    })
    output$panelStatus1 <- reactive({
      showtable1
    })
    outputOptions(output, "panelStatus1", suspendWhenHidden = FALSE)
  })

  # Observer for loading PUPoly shapefiles ----
  observe({
    myInput <- input$puPoly
    savePath <- "./PUPolygons"
    if (is.null(myInput)) {
      return()
    }
    shapefile_components <- c("shp", "shx", "prj", "dbf")
    y <- NULL
    x <- NULL
    x <- length(unique(tools::file_path_sans_ext(
      tools::file_path_sans_ext(myInput$name)
    )) == 1)
    y <-
      (length(myInput$name) == 4 &
        sum(shapefile_components %in% tools::file_ext(myInput$name)) == 4)


    if (x == T) {
      if (y == T) {
        file.copy(
          myInput$datapath,
          file.path(
            savePath,
            myInput$name
          )
        )
        # update
        rv$AdHocPoly <- myInput$name
        myText1 <- "shapefile uploaded"
        showtable1 <- "YES"
        output$rawFHTable <- renderTable(myInput[, 1:2])
        updateSelectInput(session,
          "puShape",
          "Select AdHoc Area shapefile",
          choice = c("", list.files("./PUPolygons/",
            pattern =
              ".shp$"
          ))
        )
      } else {
        myText1 <- paste(
          "<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>"
        )
        showtable1 <- "NO"
      }
    } else {
      if (y == T) {
        myText1 <- paste(
          "<span style=\"color:red\"all elements of shapefile do not have same basename</span>"
        )
        output1$showtable <- "NO"
      } else {
        myText1 <- paste("<span style=\"color:red\">wrong file elements selected</span>")
        showtable1 <- "NO"
      }
    }

    output$message_text2 <- renderText({
      myText1
    })
    output$panelStatus1 <- reactive({
      showtable1
    })
    outputOptions(output, "panelStatus1", suspendWhenHidden = FALSE)
  })
  # Observer for custom .csv uploads  ----
  observe({
    myInput <- input$addCustomCSV
    savePath <- "./CustomCSV"
    file.copy(
      myInput$datapath,
      file.path(savePath, myInput$name)
    )
    updateSelectInput(
      session,
      inputId = "customSpList",
      choices = c("", list.files("./CustomCSV", pattern = ".csv$"))
    )
    updateSelectInput(
      session,
      inputId = "customResponseFile",
      choices = c("", list.files("./CustomCSV", pattern = ".csv$"))
    )
  })

  # Download handlers for utilities page----
  downloadToolFileName <-
    "./FAMEPreProcessing/FAMEPreProcessing.zip"
  output$downloadTool <- downloadHandler(
    filename = function() {
      basename(downloadToolFileName)
    },
    content = function(file) {
      file.copy(
        from = downloadToolFileName,
        to = file,
        overwrite = T
      )
    }
  )


  downloadManualFileName <- "./Manual/FAMEv3_User_Manual.pdf"
  output$downloadManual <- downloadHandler(
    filename = function() {
      downloadManualFileName
    },
    content = function(file) {
      file.copy(
        from = downloadManualFileName,
        to = file,
        overwrite = T
      )
    }
  )

  # Choose results files to download ----
  roots <- c(wd = "./results")

  shinyFileChoose(input, "files",
    roots = roots
  )

  output$rawInputValue <- renderPrint({
    str(input$files)
  })

  output$filepaths <-
    renderTable({
      parseFilePaths(roots, input$files)
    })

  # Download handler to download files chosen for download ----

  output$downloadFiles <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep = ".")
    },
    content = function(fname) {
      fs <- as.character(parseFilePaths(roots, input$files)$datapath)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  # render tabs menu for UI ----
  output$tabsmenu <- renderMenu({
    tabs_list1 <- list(
      # menuItem("Home",
      #          tabName = "Intro",
      #          icon = icon("home")),

      menuItem(
        "Settings for Spatial Analyses",
        tabName = "AnalysisSettings",
        icon = icon("fire")
      ),
      menuItem(
        "Settings for Aspatial GSO",
        tabName = "GSO",
        icon = icon("calculator")
      ),
      menuItem(
        text = "Utilities",
        tabName = "util",
        icon = icon("cloud-upload-alt")
      )
    )

    tabs_list2 <- list(
      menuItem(
        "TFI Plots",
        tabName = "TFIplots",
        icon = icon("chart-line")
      ),
      menuItem(
        "GS Plots",
        tabName = "GSplots",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Fauna RA Plots",
        tabName = "RAplots",
        icon = icon("dove")
      )
    )

    tabs_list3 <- list(
      # menuItem("User Manual",
      #          tabName = "Manual",
      #          icon = icon("book")),
      menuItem(text = versionDate),
      menuItem(text = versionFAMEFMR)
    )
    if (input$usePUpolys == 0) {
      tabs_list <- c(tabs_list1, tabs_list2, tabs_list3)
    } else {
      tabs_list <- c(tabs_list1, tabs_list3)
    }
    sidebarMenu(tabs_list)
  })
}
