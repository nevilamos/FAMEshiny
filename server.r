server <- function(session, input, output) {
  rv <- reactiveValues()
  # Observer for loading fire scenario shapefiles ----
  #This code is repeated with modifications for each shapefile load ideally would be made into function or module
  observe({
    myInput = input$rawFH
    savePath = "./rawFH"
    if (is.null(myInput))
      return()
    shapefile_components <- c("shp", "shx", "prj", "dbf")
    y = NULL
    x = NULL
    x = length(unique(tools::file_path_sans_ext(
      tools::file_path_sans_ext(myInput$name)
    )) == 1)
    y <-
      (length(myInput$name) == 4 &
         sum(shapefile_components %in% tools::file_ext(myInput$name)) == 4)
    
    
    if (x == T) {
      if (y == T) {
        rawFHPath <- file.path(savePath, myInput$name)
        rv$rawFHPath <- rawFHPath
        
        file.copy(myInput$datapath,
                  file.path(rawFHPath))
        updateSelectInput(
          session,
          'unionedFH',
          'Select fire scenario shapefile',
          choice = c("", list.files('./rawFH/', pattern =
                                      ".shp$"))
        )
        myText = "shapefile uploaded"
        showtable = "YES"
        output$rawFHTable <- renderTable(myInput[, 1:2])
        
      } else{
        myText = paste(
          "<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>"
        )
        showtable = "NO"
      }
    } else{
      if (y == T) {
        myText = paste(
          "<span style=\"color:red\"all elements of shapefile do not have same basename</span>"
        )
        output$showtable = "NO"
      } else{
        myText = paste("<span style=\"color:red\">wrong file elements selected</span>")
        showtable = "NO"
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
  
  
  
  
  # Observer for loading AdHoc shapefiles ---------------------------------
  observe({
    myInput = input$adHocPoly
    savePath = "./AdHocPolygons"
    if (is.null(myInput))
      return()
    shapefile_components <- c("shp", "shx", "prj", "dbf")
    y = NULL
    x = NULL
    x = length(unique(tools::file_path_sans_ext(
      tools::file_path_sans_ext(myInput$name)
    )) == 1)
    y <-
      (length(myInput$name) == 4 &
         sum(shapefile_components %in% tools::file_ext(myInput$name)) == 4)
    
    
    if (x == T) {
      if (y == T) {
        file.copy(myInput$datapath,
                  file.path(savePath,
                            myInput$name))
        #update
        rv$AdHocPoly = myInput$name
        myText1 = "shapefile uploaded"
        showtable1 = "YES"
        output$rawFHTable <- renderTable(myInput[, 1:2])
        updateSelectInput(
          session,
          'AdHocShape',
          'Select AdHoc Area shapefile',
          choice = c("", list.files('./AdHocPolygons/', pattern =
                                      ".shp$"))
        )
      } else{
        myText1 = paste(
          "<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>"
        )
        showtable1 = "NO"
      }
    } else{
      if (y == T) {
        myText1 = paste(
          "<span style=\"color:red\"all elements of shapefile do not have same basename</span>"
        )
        output1$showtable = "NO"
      } else{
        myText1 = paste("<span style=\"color:red\">wrong file elements selected</span>")
        showtable1 = "NO"
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
  
  #Observer for loading PUPoly shapefiles ---------------------------------
  observe({
    myInput = input$puPoly
    savePath = "./PUPolygons"
    if (is.null(myInput))
      return()
    shapefile_components <- c("shp", "shx", "prj", "dbf")
    y = NULL
    x = NULL
    x = length(unique(tools::file_path_sans_ext(
      tools::file_path_sans_ext(myInput$name)
    )) == 1)
    y <-
      (length(myInput$name) == 4 &
         sum(shapefile_components %in% tools::file_ext(myInput$name)) == 4)
    
    
    if (x == T) {
      if (y == T) {
        file.copy(myInput$datapath,
                  file.path(savePath,
                            myInput$name))
        #update
        rv$AdHocPoly = myInput$name
        myText1 = "shapefile uploaded"
        showtable1 = "YES"
        output$rawFHTable <- renderTable(myInput[, 1:2])
        updateSelectInput(
          session,
          'puShape',
          'Select AdHoc Area shapefile',
          choice = c("", list.files('./PUPolygons/', pattern =
                                      ".shp$"))
        )
      } else{
        myText1 = paste(
          "<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>"
        )
        showtable1 = "NO"
      }
    } else{
      if (y == T) {
        myText1 = paste(
          "<span style=\"color:red\"all elements of shapefile do not have same basename</span>"
        )
        output1$showtable = "NO"
      } else{
        myText1 = paste("<span style=\"color:red\">wrong file elements selected</span>")
        showtable1 = "NO"
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
  # Observer for custom .csv uploads  ------------------
  observe({
    myInput = input$addCustomCSV
    savePath = "./CustomCSV"
    file.copy(myInput$datapath,
              file.path(savePath, myInput$name))
    updateSelectInput(session,
                      inputId = 'customSpList',
                      label = 'Select custom csv file',
                      choices = c("", list.files('./CustomCSV', pattern = ".csv$")))
    updateSelectInput(session,
                      inputId = 'customResponseFile',
                      label = 'Select custom csv file',
                      choices = c("", list.files('./CustomCSV', pattern = ".csv$")))
  })
  
  # Download handlers for utilities page---------
  downloadToolFileName <-
    "./FAMEPreProcessing/FAMEPreProcessing.zip"
  output$downloadTool <- downloadHandler(
    filename = function() {
      basename(downloadToolFileName)
    },
    content = function(file) {
      file.copy(from = downloadToolFileName,
                to = file,
                overwrite = T)
    }
  )
  
  
  downloadManualFileName <- "./Manual/FAMEv2_User_Manual.pdf"
  output$downloadManual <- downloadHandler(
    filename = function() {
      downloadManualFileName
    },
    content = function(file) {
      file.copy(from = downloadManualFileName,
                to = file,
                overwrite = T)
    }
  )
  
  
  roots =  c(wd = './results')
  
  shinyFileChoose(input, 'files',
                  roots =  roots)
  
  output$rawInputValue <- renderPrint({
    str(input$files)
  })
  
  output$filepaths <-
    renderTable({
      parseFilePaths(roots, input$files)
    })
  
  
  
  output$downloadFiles <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep = ".")
    },
    content = function(fname) {
      fs = as.character(parseFilePaths(roots, input$files)$datapath)
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  

  # Observer to runFH analysis ---------------------------------
  observeEvent(input$runFH, {
    validate(need(input$unionedFH, 'You need to select a raw FH to run analysis'))
    withBusyIndicatorServer("runFH", {
      rv$outputFH <- file_path_sans_ext(basename(input$unionedFH))
      #    showModal("FH analysis running")
      myREG_NO <- as.integer(input$REGION_NO)
      RasterRes <- as.integer(input$RasterRes)
      print(paste("RasterRes =", RasterRes))
      HDM_RASTER_PATH <-
        paste0("./HDMS/", input$RasterRes, "m/BinaryThresholded")
      
      if (myREG_NO == 7) {
        clipShape = file.path("./AdHocPolygons", input$AdHocShape)
      } else{
        clipShape = "./ReferenceShapefiles/LF_DISTRICT.shp"
      }
      
      
      if (input$usePUpolys  == TRUE){
        rv$JFMPSeason0 <- input$JFMPSeason0
        rv$endSEASON <- input$JFMPSeason0 + 4
      } else {
        rv$endSEASON <- NULL
      }
      
      
      
      cropRasters <- cropNAborder(
        REG_NO = myREG_NO,
        myRasterRes = RasterRes,
        PUBLIC_LAND_ONLY = input$public,
        myPoly = file.path(clipShape),
        generalRasterDir = "./InputGeneralRasters"
      )
      
      cropRasters$HDM_RASTER_PATH = HDM_RASTER_PATH
      rv$cropRasters = cropRasters
      

      
      

      FHAnalysis <- fhProcess(
        rawFH =  file.path("./rawFH/", input$unionedFH),
        start.SEASON = input$startTimespan,
        end.SEASON = rv$endSEASON,
        OtherAndUnknown = input$otherUnknown,
        validFIRETYPE = c("BURN", "BUSHFIRE", "UNKNOWN", "OTHER")
      )
      #Save input settings to a list and then append into FH analysis object
      #FHAnalysis$AnalysisInputs<-list(
      FHAnalysis$FireScenario = input$unionedFH
      FHAnalysis$RasterRes = input$RasterRes
      FHAnalysis$ClipPolygonFile = clipShape
      FHAnalysis$Region_No = myREG_NO
      FHAnalysis$PUBLIC_ONLY = input$public
      FHAnalysis$Start_Season = NULL
      FHAnalysis$name <- paste0("FH_Analysis_", rv$outputFH)
      st_write(FHAnalysis$OutDF,
               file.path(ResultsDir, paste0(FHAnalysis$name, ".shp")),
               append = FALSE)
      #)
      print("Save input settings to a list and then append into FH analysis object")
      FHAnalysis$FH_IDr <-
        fasterize(
          sf = FHAnalysis$OutDF,
          raster =  cropRasters$Raster,
          field = "ID",
          fun = "first"
        )
      print("made FHAnalysis$FH_IDr")
      
      
      FHAnalysis<-FHAnalysis
      #check if pupoly is to be used
      if(input$usePUpolys){
        validate(need(input$puShape, 'You  have selected to require \n a PU/BU polygon file but have not \n selected one '))
        myPuPoly = input$puShape
        #update the FHAnalysis$OUTdf with noburn columns
        FHAnalysis$OutDF<-FHAnalysis$OutDF%>%bind_cols(make_JFMPNoBurnTab(myFHAnalysis = FHAnalysis,JFMPSeason0 = rv$JFMPSeason0))
        FHAnalysis$YSFNames<-c(FHAnalysis$YSFNames,"YSFNoBurn")
        FHAnalysis$LBYNames<-c(FHAnalysis$LBYNames,"LBYNoBurn")
        FHAnalysis$LFTNames<-c(FHAnalysis$LFTNames,"LFTNoBurn")
        
        print("appended JFMPNoBurnCols")
      }else{
        myPuPoly = NULL
        
      }
      
      rv$FHAnalysis <- FHAnalysis
      
      
        allCombs <- calcU_All_Combs(
        myFHAnalysis =  rv$FHAnalysis,
        myCropRasters = rv$cropRasters,
        myRasterRes = RasterRes,
        puPoly = myPuPoly
        )
      rv$allCombs <- allCombs
      print("made allcombs")
      
      
      #save analysis to enable reloading
      save(FHAnalysis,
           cropRasters,
           allCombs,
           file = file.path(
             "./FH_Outputs",
             paste0(FHAnalysis$name, input$RasterRes, ".rdata")
           ))
      print("saved FHAnalysis to enable reloading")
      
      
      updateSelectInput(session,
                        'FHAnalysis',
                        'Select FH dataset (.rdata)',
                        choice = c("", list.files('./FH_Outputs/', pattern =
                                                    ".rdata$")))
      
    })
    
    
  })
  # Observer to run relative abundance analysis  -------------------------------------------------------
  observeEvent({
    input$runRA | input$runRA_TFI | input$runJFMP1
    
  }, ignoreInit = T, {
    
    withBusyIndicatorServer("runRA", {
      withBusyIndicatorServer("runRA_TFI", {
        validate(need(rv$FHAnalysis,
                      'You need to select a FH analysis to use'))
        startBaseline<-as.integer(input$startBaseline)
        endBaseline <- as.integer(input$endBaseline)
        validate(need(endBaseline>=startBaseline,"baseline start season must be less than or equal to end season"))
        Baseline <-startBaseline:endBaseline
        if (input$spListChoice == FALSE) {
          rv$TaxonList <-
            read_csv("./ReferenceTables/FAME_TAXON_LIST.csv")
        } else{
          validate(need(
            input$customSpList,
            'You need to select a species list to use'
          ))
          print(input$customSpList)
          rv$TaxonList <-
            read_csv(file.path("./CustomCSV", input$customSpList))
          
        }
        
        
        
        HDMSpp_NO <-
          rv$TaxonList$TAXON_ID[rv$TaxonList$Include == "Yes"]
        writeSp <-
          rv$TaxonList$TAXON_ID[rv$TaxonList$WriteSpeciesRaster == "Yes"]
        writeSp <- writeSp[writeSp %in% HDMSpp_NO]
        
        
        print("getting HDMvals")
        HDMVals <- qread(paste0("./HDMS/HDMVals",
                                rv$FHAnalysis$RasterRes,
                                "list.qs"))
        
        print("Loaded HDMVals")
        
        if (input$spResponseChoice == FALSE) {
          mySpGSResponses <- "./ReferenceTables/OrdinalExpertLong.csv"
        } else{
          mySpGSResponses <-
            file.path("./CustomCSV", input$customResponseFile)
        }
        if(input$abundByGS == TRUE){
          AbundDataByGS  <-  read_csv(mySpGSResponses)[, c("EFG_NO",
                                                           "GS4_NO",
                                                           "FireType",
                                                           "Abund",
                                                           "TAXON_ID")]  #Select the file giving the fauna relative abundance inputs you wish to use
          

          AbundDataLong = AbundDataByGS%>%
            dplyr::mutate(FireTypeNo = if_else(FireType == "High",2,if_else(FireType == "Low",1,0)))%>%
            dplyr::left_join( EFG_TSF_4GS, by = c('EFG_NO', 'GS4_NO'))%>%
            dplyr::arrange(TAXON_ID)


        } else {
          AbundDataLong <- read_csv(mySpGSResponses)%>%
            dplyr::arrange(TAXON_ID)
          
        }
        print("making Spp abund LU List")
        LU_List <- make_Spp_LU_list(myHDMSpp_NO = HDMSpp_NO,
                                    myAbundDataLong = AbundDataLong)
        
        print("finished  Spp abund LU List")
        
        print("Making spYearSumm")
        
        SpYearSumm <<- calc_SpeciesRA(
          myFHAnalysis = rv$FHAnalysis,
          myAllCombs <- rv$allCombs,
          myHDMSpp_NO = HDMSpp_NO,
          myWriteSpRasters = input$makeRArasters,
          myResultsDir = ResultsDir,
          myLU_List = LU_List,
          myHDMVals = HDMVals,
          myTaxonList = rv$TaxonList,
          writeYears = NULL,
          #input$yearsForRasters,
          myWriteSp = writeSp,
          myIDX = rv$cropRasters$IDX
        )
        
        rv$SpYearSumm <- SpYearSumm
        readr::write_csv(SpYearSumm$SpYearSummLong,
                         file.path(ResultsDir, "SpYearSummLong.csv"))
        readr::write_csv(SpYearSumm$SpYearSummWide,
                         file.path(ResultsDir, "SpYearSummWide.csv"))
        
        
        grpSpYearSummLong<-rv$SpYearSumm$grpSpYearSumm %>%
          dplyr::rename(Index_AllCombs = `myAllCombs$Index_AllCombs`) %>%
          tidyr::pivot_longer(
            -tidyr::one_of("TAXON_ID","Index_AllCombs"),
            names_to = "SEASON",
            values_to = "sumRA"
          ) %>%
          dplyr::mutate(PU = rv$allCombs$U_AllCombs_TFI$PU[Index_AllCombs],
                        EFG_NAME = rv$allCombs$U_AllCombs_TFI$EFG_NAME[Index_AllCombs]) %>%
          dplyr::group_by(TAXON_ID,PU,EFG_NAME,SEASON) %>%
          dplyr::summarise(sumRA = sum(sumRA)) %>%
          dplyr::mutate(TAXON_ID = as.integer(TAXON_ID))
        
        
        readr::write_csv(grpSpYearSummLong,file.path(ResultsDir,"grpSpYearSummLong.csv"))

        
        print("finished sp year summ")
        
        
        print("calcuated myBaseline")
        print(Baseline)
        raDeltaAbund <-
          calcDeltaAbund(
            SpYearSumm = SpYearSumm$SpYearSummWide,
            myFHAnalysis = rv$FHAnalysis,
            myBaseline = Baseline
          )
        readr::write_csv(raDeltaAbund,file.path(ResultsDir,"SppSummChangeRelativetoBaseline.csv"))
        rv$raDeltaAbundWide <- raDeltaAbund
        
        #make long form for plotting charts
        rv$raDeltaAbundLong <- raDeltaAbund %>%
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
            -tidyr::one_of("COMMON_NAME", "SCIENTIFIC_NAME","Baseline"),
            names_to = "SEASON",
            values_to = "DeltaRA"
          ) %>%
          dplyr::mutate(SEASON = as.integer(SEASON))
        print("finished deltaabund")
        
        
      })
    })
  })
  # Observer to get update years for calculations-----------------------------------------------------
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
  
  # Observer to run TFI  related calculations---------------------
  observeEvent({
    input$runTFI | input$runRA_TFI
  },
  ignoreInit = T, {
    withBusyIndicatorServer("runTFI", {
      withBusyIndicatorServer("runRA_TFI", {
        validate(need(rv$FHAnalysis,
                      'You need to select a FH analysis to use'))
        print("running TFI calc")
        
        TFI <- calc_TFI_2(
          myFHAnalysis = rv$FHAnalysis,
          myAllCombs = rv$allCombs,
          myTFI_LUT = TFI_LUT,
          OutputRasters = input$makeTFIrasters,
          myResultsDir = ResultsDir
        )
        
        
        
        
        # need to change the sort order for the factor to get correct stacking
        # order ( no alphabetical) on chart
        TFI$TFI_STATUS <-
          factor(
            TFI$TFI_STATUS,
            levels = c(
              "BELOW_MIN_TFI",
              "WITHIN_TFI",
              "ABOVE_MAX_TFI",
              "NONE"
            )
          )
        
        
        rv$TFI <- TFI
        #save TFI to the FHAnalysis.rdata
        #rv$FHAnalysis$TFI <- TFI
        
        #write results out to csv files
        readr::write_csv(TFI,
                         file = file.path(ResultsDir, "TFI_LONG.csv"))
        readr::write_csv(TFI%>%
                           group_by(EFG_NAME,SEASON,TFI_STATUS)%>%
                           summarise(AreaHa = sum(Hectares))%>%
                           pivot_wider(names_from = SEASON,
                                       values_from = AreaHa,
                                       values_fill = 0),
                         file = file.path(ResultsDir, "TFI_EFG_SUMMARY.csv"))
        
        
        print("Finished TFI calcualtions")
        
        
      })
    })
  })
  # Observer to run BBTFI  related calculations---------------------
  observeEvent({
    input$runTFI | input$runRA_TFI | input$runJFMP1
  },
  ignoreInit = T, {
    withBusyIndicatorServer("runTFI", {
      withBusyIndicatorServer("runRA_TFI", {
        validate(need(rv$FHAnalysis,
                      'You need to select a FH analysis to use'))
        
        print ("calculating BBTFI")
        
        BBTFI <- calcBBTFI_2(
          myFHAnalysis = rv$FHAnalysis,
          myAllCombs = rv$allCombs,
          makeBBTFIrasters = input$makeBBTFIrasters,
          myResultsDir = ResultsDir
        )
        print("finished BBTFI calcs")
        
        rv$BBTFI <- BBTFI
        
        write.csv(BBTFI$BBTFI_LONG,
                  file = file.path(ResultsDir,
                                   "BBTFI_LONG.csv"))
        write.csv(BBTFI$BBTFI_LONG%>%
                    group_by(EFG_NAME,TBTFI)%>%
                    summarise(AreaHa = sum(Hectares))%>%
                    pivot_wider(names_from = TBTFI,values_from = AreaHa),
                  file = file.path(ResultsDir,
                                   "TimesBBTFI_SUMMARY.csv"))
        
        write.csv(BBTFI$BBTFI_WIDE,
                  file = file.path(ResultsDir,
                                   "BBTFI_WIDE.csv"))
        
        
        
        
      })
    })
  })
  # Observer to run GS calculations-----------------------
  observeEvent(input$runGS | input$runRA_TFI,
               ignoreInit = T, {
                 withBusyIndicatorServer("runGS", {
                   withBusyIndicatorServer("runRA_TFI", {
                     validate(need(rv$FHAnalysis,
                                   'You need to select a FH analysis to use'))
                     
                     print ("GS Calculations")
                     GS_Summary <-
                       makeGS_Summary(myFHAnalysis = rv$FHAnalysis,
                                      myAllCombs = rv$allCombs)
                     print("finished GS calcs")
                     
                     rv$GS_Summary <- GS_Summary
                     #rv$FHAnalysis$GS_Summary <- GS_Summary
                     
                     readr::write_csv(GS_Summary$GS_Summary_Long,
                               file = file.path(ResultsDir,
                                                "GS_LONG.csv"))
                     
                     readr::write_csv(GS_Summary$GS_Summary_wide,
                               file = file.path(ResultsDir,
                                                "GS_WIDE.csv"))
                     

                   })
                 })
               })
  # Observer to run JFMP1  related calculations---------------------
  observeEvent({
    input$runJFMP1
  },
  ignoreInit = T, {
    withBusyIndicatorServer("runJFMP1", {
      print("doing JFMP1")

#wrangles the SpYearSummRA grouped on indexof all combs, plus the TaxonList that includes count of cells in area of interest to get the weighted sum of change all species in area of interest for each PU ------

      PU_WeightedSumRA<<-rv$SpYearSumm$grpSpYearSumm %>%
        dplyr::rename(Index_AllCombs = `myAllCombs$Index_AllCombs`) %>%
        tidyr::pivot_longer(
          -tidyr::one_of("TAXON_ID","Index_AllCombs"),
          names_to = "SEASON",
          values_to = "sumRA"
        ) %>%
        dplyr::filter(SEASON %in% c(as.character(rv$endSEASON),"NoBurn")) %>% 
        dplyr::mutate(SEASON = ifelse(SEASON == "NoBurn","NoBurn","Burn")) %>%
        dplyr::mutate(PU = rv$allCombs$U_AllCombs_TFI$PU[Index_AllCombs]) %>%
        dplyr::group_by(TAXON_ID,PU,SEASON) %>%
        dplyr::summarise(sumRA = sum(sumRA)) %>%
        dplyr::mutate(TAXON_ID = as.integer(TAXON_ID)) 
          print("here")
          
          
        PU_WeightedSumRA<<-PU_WeightedSumRA%>%
        dplyr::left_join(
          rv$TaxonList %>% dplyr::select(TAXON_ID,cellsInArea)
          )%>%
        dplyr::mutate(weightedRA = sumRA/cellsInArea) %>%
        dplyr::group_by(PU,SEASON)%>%
        dplyr::summarise(WeightedSumRA = sum(weightedRA))

    print("calculated PU weighted RA")  
  
#Data wrangling of BBTFI outputs to extract area BBTFI for each PU, this requires inclusion ONLY of the first time areas are buned below TFI, these are then categorised as "PastBBTFI" for all years up to JFMPSeason0, and then as BBTFI (for the first time) by the JFMP)----
      print("doing JFMPBBTFI")

      
      PU_BBTFI_Summ<<-rv$BBTFI$BBTFI_LONG%>%
        dplyr::filter(TBTFI==1)%>%
        dplyr::mutate(JFMP_BURN = ifelse(
          SEAS> rv$JFMPSeason0,"JFMP_BBTFI","PastBBTFI"
          )
          )%>%
        dplyr::group_by(PU,JFMP_BURN)%>%
        dplyr::summarise(Hectares = sum (Hectares))%>%
        tidyr::pivot_wider(names_from = JFMP_BURN,values_from = Hectares)
      
     print( "Finished JFMP1")
      
    })
  })
  
# Observer prints the details of currently selected analysis ------
  observeEvent(rv$FHAnalysis$name, ignoreInit = T,
               {
                 output$selected_FH_name <- renderText({
                   paste("FH Analysis selected =" ,
                         as.character(rv$FHAnalysis$name))
                 })
               })
  
  
  
# Observer to display TFI and BBTFI plots when available ------
  
  observeEvent({
    rv$TFI
  }, ignoreInit = T, {
    myChoices <- unique(rv$TFI$EFG_NAME)
    myChoices <- myChoices[!is.na(myChoices)]
    updateSelectInput(session, "EFGChoices", choices = myChoices)
    updateTabItems(session, "tabs", "TFIplots")
    minSEASON <- min(rv$TFI$SEASON)
    maxSEASON <- max(rv$TFI$SEASON)
    updateSliderInput(
      session,
      "tfiSeasonChoices",
      min = minSEASON,
      max = maxSEASON,
      value = c(1980, maxSEASON)
    )
    # Make plot of area by TFI status
    output$TFItrendPlot <- renderPlotly({
      rv$TFI %>%
        filter(EFG_NAME == input$EFGChoices) %>%
        group_by(TFI_STATUS, SEASON) %>%
        summarise(Area = sum(Hectares)) %>%
        plot_ly(
          x =  ~ SEASON,
          y =  ~ Area,
          group =  ~ TFI_STATUS,
          type = "bar",
          color =  ~ TFI_STATUS,
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
          barmode = 'stack',
          showlegend = T
        )
    })
    
    #if(nrow(bbtfivals)>0){
    
    output$BBTFIPlot <- renderPlotly({
      
      bbtfivals <- rv$BBTFI$BBTFI_LONG %>%
        filter(EFG_NAME == input$EFGChoices) %>%
        mutate(TBTFI = as.factor(TBTFI)) %>%
        group_by(TBTFI, SEAS) %>%
        summarise(Area = sum(Hectares))%>%
        drop_na()
      #work around to maintain column width where there are gaps between values
      if(nrow(bbtfivals)>0){
        myYears<-input$tfiSeasonChoices[1]:input$tfiSeasonChoices[2]
        SEAS<-myYears[!myYears%in%unique(bbtfivals$SEAS)]
        SEASL<-length(SEAS)
        if (SEASL>0){
          TBTFI = (rep(NA,SEASL))
          Area = rep(0,SEASL)
          Padding<-data.frame(TBTFI,SEAS,Area)
          bbtfivals<-rbind(bbtfivals,Padding)
          
        }
      }
      
      bbtfivals %>%  
        plot_ly(
          x =  ~ SEAS,
          y =  ~ Area,
          type = "bar",
          color =  ~ TBTFI
        ) %>%
        layout(
          title = paste0(input$EFGChoices, "\n", "Times burned below TFI"),
          yaxis = list(rangemode = "tozero", title = "Area (ha)"),
          xaxis = list(range = input$tfiSeasonChoices + c(-0.5, 0.5)),
          barmode = 'stack',
          showlegend = T
        )
    })
    #output$BBTFIPlot <- renderUI(plotlyOutput("myBBTFIPlot"))
    #}
    
  })
  
  
  # Observer to display GS plots when available--------------------------------------------
  
  observeEvent({
    rv$GS_Summary
  }, ignoreInit = T, {
    myChoices <- unique(rv$GS_Summary$GS_Summary_Long$EFG_NAME)
    myChoices <- myChoices[!is.na(myChoices)]
    updateSelectInput(session, "GSEFGChoices", choices = myChoices)
    updateTabItems(session, "tabs", "GSplots")
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
        #reordering the factor for GROWTH STAGE so they plot stacked appropriately not working
        mutate(GROWTH_STAGE = factor(
          GROWTH_STAGE,
          levels = c("Juvenile",
                     "Adolescent",
                     "Mature",
                     "Old")
        )) %>%
        plot_ly(
          x =  ~ SEASON,
          y =  ~ Area,
          type = "bar",
          color =  ~ GROWTH_STAGE,
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
          barmode = 'stack',
          showlegend = T
        )
    })
  })
  
  
  # Observers make RA charts when available--------------------------------------------
  observeEvent(rv$SpYearSumm, ignoreInit = T, {
    myChoices <- unique(rv$SpYearSumm$SpYearSummLong$COMMON_NAME)
    updateSelectizeInput(session, "raSpChoices", choices = myChoices)
    updateTabItems(session, "tabs", "RAplots")
    #gets the seasons that have been calcuated and removes the dummy no abund ( SEASON =999) so that this does not inflate the axes
    allSEASONS <- rv$SpYearSumm$SpYearSummLong$SEASON
    displaySEASONS <- allSEASONS[allSEASONS!=9999]
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
      } else{
        rv$SpYearSumm$SpYearSummLong %>%
          filter(COMMON_NAME %in% input$raSpChoices) %>%
          plot_ly(
            x =  ~ SEASON,
            y =  ~ SUM_RAx100,
            type = "scatter",
            mode = "lines+markers",
            color =  ~ COMMON_NAME
          ) %>%
          layout(
            yaxis = list(rangemode = "tozero",
                         title = "Sum of relative abundance x 100"),
            xaxis = list(range = input$raSeasonChoices + c(-0.5, 0.5)),
            showlegend = T
          )
        
        
      }
    })
    output$RADeltaPlot <- renderPlotly({
      if (length(input$raSpChoices) == 0) {
        return()
      } else{
        rv$raDeltaAbundLong %>%
          filter(COMMON_NAME %in% input$raSpChoices) %>%
          plot_ly(
            x =  ~ SEASON,
            y =  ~ DeltaRA,
            type = "scatter",
            mode = "lines",
            color =  ~ COMMON_NAME
          ) %>%
          layout(
            yaxis = list(rangemode = "tozero",
                         title = "Change in relative abundance\ncompared to baseline"),
            xaxis = list(range = input$raSeasonChoices + c(-0.5, 0.5)),
            showlegend = T
          )
      }
    })
  })
  
  # Calculate custom species list-----------------------------------------------------------
  observeEvent(input$runDSpList, {
    withBusyIndicatorServer("runDSpList", {
      req(input$spREGION_NO)
      if (input$spREGION_NO == 7) {
        myPoly = file.path("./AdHocPolygons", input$spAdHocShape)
      } else{
        myPoly = "./ReferenceShapefiles/LF_DISTRICT.shp"
      }
      print ("calcuating draft species List")
      myDraftSpList <- calc_DraftSpList(
        REG_NO = input$spREGION_NO,
        RasterRes = 225,
        myPoly = myPoly,
        PUBLIC_LAND_ONLY = input$sppublic,
        myHDMVals = "./HDMS/HDMVals225.qs",
        splist = "./ReferenceTables/FAME_TAXON_LIST.csv"
      )
      print (head(myDraftSpList))
      readr::write_csv(myDraftSpList,
                file.path(ResultsDir, "myDraftspList.csv"))
      print("made draft species List")
    })
  })
  # Calculate SppEFGLMU for GSO-----------------------------------------------------------
  observeEvent(input$runspEFGpList, {
    withBusyIndicatorServer("runspEFGpList", {
      req(input$spREGION_NO)
      if (input$spREGION_NO == 7) {
        myPoly = file.path("./AdHocPolygons", input$spAdHocShape)
      } else{
        myPoly = "./ReferenceShapefiles/LF_DISTRICT.shp"
      }
      
      
      myEFG_LMU <- make_Draft_GSO_inputs(
        REG_NO = input$spREGION_NO,
        #REG_NO of defined region from input (1:6) or 0 for statewide or 7 for Ad Hoc Poly),
        RasterRes = 225,
        PUBLIC_LAND_ONLY = input$sppublic,
        myPoly = myPoly,
        #shapefile ofLF_REGIONs( default)or  adhoc region,
        generalRasterDir = "./InputGeneralRasters",
        splist = "./ReferenceTables/FAME_TAXON_LIST.csv",
        myHDMVals = "./HDMS/HDMVals225.qs",
        #EFGRas="./InputGeneralRasters/EFG_NUM_225.tif",
        TFI_LUT = TFI_LUT
      )
      #print(myEFG_LMU)
      write_csv(
        myEFG_LMU$LMU_EFG_AREA,
        file.path(ResultsDir, "LMU_Area.csv")
      )
      write_csv(myEFG_LMU$Spp_EFG_LMU,
                file.path(ResultsDir, "Spp_EFG_LMU.csv"))
      write_csv(myEFG_LMU$LMU_Scenario,
                file.path(ResultsDir, "Draft_LMU_Scenarios.csv"))
    })
  })
  
  
  # })
  # Run Aspatial GSO-------------------------------------------------
  
  # this section runs the rmd script and documents for aspatial GSO that was written by Paul Moloney in 2017. and modified to use shiny GUI
  # Observers for GSO .csv uploads  ---------------------------------
  observe({
    myInput = input$addGSOCSV
    savePath = "./GSOInputs"
    file.copy(myInput$datapath,
              file.path(savePath, myInput$name))
    updateSelectInput(session,
                      inputId = 'spEFGLMU',
                      label = 'Select Spp_EFG_LMU.csv file',
                      choice = c(
                        list.files('./GSOInputs/', pattern = "Spp_EFG_LMU.csv$")))
    updateSelectInput(session,
                      'lmuArea',
                      'LMU_Area.csv file',
                      choice = c(
                        list.files('./GSOInputs/', pattern = "LMU_Area.csv$")))
    updateSelectInput(session,
                      'lmuScenarios',
                      'LMU_Scenarios.csv file',
                      choice = c(
                        list.files('./GSOInputs/', pattern = "LMU_Scenarios.csv$")
                      ))
    updateSelectInput(session,
                      'ObsData',
                      'ObsData.csv file',
                      choice = c(
                        list.files('./GSOInputs/', pattern = "ObsData.csv$")
                      ))
    
  })
  
  
  observeEvent(input$lmuScenarios,
               ignoreInit = T, {
                 myScenarios = unique(read_csv(file.path(WD,
                                                         "GSOInputs",
                                                         input$lmuScenarios))$Scenario)
                 updateSelectInput(session,
                                   "GSOBaseLine",
                                   choices = c("Optimisation", myScenarios))
               })
  
  observeEvent(input$runGSO, {
    withBusyIndicatorServer("runGSO", {
      fileConn <- file("./GSO/GSOSettings.r")
      
      
      
      writeLines(
        c(
          paste0("GSOResultsDir ='", file.path(WD, ResultsDir), "'"),
          paste0("FireType ='", input$GSOFireType, "'"),
          paste0("Comparison = '" , input$GSOBaseLine, "'"),
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
      rmarkdown::render("./GSO/GSOAnalysisOutput.Rmd",
                        output_dir = ResultsDir,
                        clean = T)
    })
  })
  
  # observer to save current analysis reactive values to file as list----
  observe({
    roots <- c("UserFolder"="./FH_Outputs")
    myRvList<-reactiveValuesToList(rv)
    shinyFileSave(input, "saveAnalysis", roots=roots)
    fileinfo <- parseSavePath(roots, input$saveAnalysis)
    
    if (nrow(fileinfo) > 0) {
      qsave(myRvList, as.character(fileinfo$datapath))
    }
  })  

  
    
  
  
  
  # observer to load analysis from list as qs file-----
  observe({
    roots <- c("UserFolder"="./FH_Outputs")  
    shinyFileChoose(
      input,
      id = "loadAnalysis",
      roots = roots,
      filetypes = "qs"
    )
    
    fileinfo <- parseFilePaths(roots, input$loadAnalysis)
    if (nrow(fileinfo) > 0) {
      for ( i in names(rv)){
        rv[[i]]<-NULL
      }
      myRvList<-qread(as.character(fileinfo$datapath))
      x<-(names(myRvList))
      for(i in x){
        rv[[i]]<- myRvList[[i]]
        print(i)
      }
    }
    
  })
  
  
  # observer to shut down server
  observeEvent(input$close, {
    js$closeWindow()
    system("shutdown")
  })
  
  roots = c(root = "./results")
  
  shinyFileChoose(input, 'files',
                  root = roots)
  
}
