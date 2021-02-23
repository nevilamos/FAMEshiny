server <- function(session,input, output) {
  rv<-reactiveValues()
  #Observer for loading AdHoc shapefiles ---------------------------------
  observe({myInput=input$adHocPoly
  savePath="./AdHocPolygons"
  if (is.null(myInput)) return() 
  shapefile_components<-c("shp","shx","prj","dbf")
  y=NULL
  x=NULL
  x=length(unique(tools::file_path_sans_ext(tools::file_path_sans_ext(myInput$name)))==1)
  y<-(length(myInput$name)==4 & sum(shapefile_components%in%tools::file_ext(myInput$name))==4)
  
  
  if(x==T){
    if(y==T){
      file.copy(
        myInput$datapath,
        file.path(savePath,
                  myInput$name
        )
      )
      #update
      rv$AdHocPoly = myInput$name
      myText1="shapefile uploaded"
      showtable1="YES"
      output$rawFHTable<-renderTable(
        myInput[,1:2]
      )
      updateSelectInput(session,'AdHocShape','Select AdHoc Area shapefile',
                        choice = c("",list.files('./AdHocPolygons/',pattern=".shp$")))
    }else{
      myText1=paste("<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>")
      showtable1="NO"
    }
  }else{
    if(y==T){
      myText1=paste("<span style=\"color:red\"all elements of shapefile do not have same basename</span>")
      output1$showtable="NO"
    }else{
      myText1=paste("<span style=\"color:red\">wrong file elements selected</span>")
      showtable1="NO"
    }
  }
  
  output$message_text1 <- renderText({myText1}) 
  output$panelStatus1 <- reactive({showtable1})
  outputOptions(output, "panelStatus1", suspendWhenHidden = FALSE)   
  
  }
  )
  #download handlers for utilities page --------------------------------------------------
  downloadToolFileName<-"FAMEPreProcessing.zip"
  output$downloadTool <- downloadHandler(
    filename = function(){
      downloadToolFileName
    },
    content = function(downloadToolFileName) {
      fs <- dir("./FAMEPreProcessing",full.names = T)
      zip(zipfile=downloadToolFileName, files=fs)
    },
    contentType = "application/zip"
  )
  
  downloadManualFileName<-"FAME_Manual.pdf"
  output$downloadManual <- downloadHandler(
    filename = function(){
      downloadManualFileName
    },
    content = function(file) {
      file.copy(from = downloadManualFileName,to=file,overwrite=T)
    }
  )   
  
  
  roots =  c(wd = './results')
  
  shinyFileChoose(input, 'files', 
                  roots =  roots)
  
  output$rawInputValue <- renderPrint({str(input$files)})
  
  output$filepaths <- renderTable({parseFilePaths(roots, input$files)})
  
  
  
  output$downloadFiles <- downloadHandler(
    
    
    
    filename = function() {
      paste("output", "zip", sep=".")
    },
    content = function(fname) {
      fs = as.character(parseFilePaths(roots, input$files)$datapath)
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  #Observer for loading fire scenario shapefiles ---------------------------------  
  #This code is repeated with modifications for each shapefile load ideally would be made into function or module
  observe({myInput=input$rawFH
  savePath="./rawFH"
  if (is.null(myInput)) return() 
  shapefile_components<-c("shp","shx","prj","dbf")
  y=NULL
  x=NULL
  x=length(unique(tools::file_path_sans_ext(tools::file_path_sans_ext(myInput$name)))==1)
  y<-(length(myInput$name)==4 & sum(shapefile_components%in%tools::file_ext(myInput$name))==4)
  
  
  if(x==T){
    if(y==T){
      rawFHPath<-file.path(savePath,myInput$name)
      rv$rawFHPath<-rawFHPath
      
      file.copy(
        myInput$datapath,
        file.path(rawFHPath
        )
      )
      updateSelectInput(session,'unionedFH','Select fire scenario shapefile',
                        choice = c("",list.files('./rawFH/',pattern=".shp$")))
      myText="shapefile uploaded"
      showtable="YES"
      output$rawFHTable<-renderTable(
        myInput[,1:2]
      )
      
    }else{
      myText=paste("<span style=\"color:red\">one or more of .shp,.shx,.dbf,.prj are missing\n Or additional files selected</span>")
      showtable="NO"
    }
  }else{
    if(y==T){
      myText=paste("<span style=\"color:red\"all elements of shapefile do not have same basename</span>")
      output$showtable="NO"
    }else{
      myText=paste("<span style=\"color:red\">wrong file elements selected</span>")
      showtable="NO"
    }
  }
  
  output$message_text <- renderText({myText}) 
  output$panelStatus <- reactive({showtable})
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)   
  
  }
  )
  
  
  
  #Observer for custom .csv uploads  ---------------------------------
  observe({ myInput = input$addCustomCSV
  savePath ="./CustomCSV"
  file.copy(
    myInput$datapath,
    file.path(savePath,myInput$name)
  )
  updateSelectInput(session,'customSpList','Select csutom csv file',
                    choice = c("",list.files('./CustomCSV',pattern=".csv$")))
  updateSelectInput(session,'customResponseFile','Select csutom csv file',
                    choice = c("",list.files('./CustomCSV',pattern=".csv$")))
  })
  #Observer for loading a previous FH analysis---------------------------------
  observeEvent(input$FHOutputLoad,{
    
    if(input$FHOutputLoad=="use current loaded FH analysis"){
      return()  
    }else{
      load(input$FHOutputLoad)
      rv$FHAnalysis<-FHAnalysis
      rv$cropRasters<-cropRasters
      rv$allCombs<-allCombs
      #print(names(rv$FHAnalysis)) 
      
    }
    
  })
  
  # Observer to runFH analysis ---------------------------------
  observeEvent(input$runFH,{
    
    validate(
      need(input$unionedFH, 'You need to select a raw FH to run analysis')
    )
    withBusyIndicatorServer("runFH", {
      rv$outputFH<-file_path_sans_ext(basename(input$unionedFH))
      #    showModal("FH analysis running")
      myREG_NO<-as.integer(input$REGION_NO)
      RasterRes<-as.integer(input$RasterRes)
      print(paste("RasterRes =",RasterRes))
      HDM_RASTER_PATH <-paste0("./HDMS/",input$RasterRes,"m/BinaryThresholded")
      
      if (myREG_NO == 7) {
        clipShape = file.path("./AdHocPolygons", input$AdHocShape)
      } else{
        clipShape = "./ReferenceShapefiles/LF_DISTRICT.shp"
      }
        
      cropRasters <- cropNAborder(
          REG_NO = myREG_NO,
          myRasterRes = RasterRes,
          PUBLIC_LAND_ONLY = input$public,
          myPoly = file.path(clipShape),
          generalRasterDir = "./InputGeneralRasters"
        )

      cropRasters$HDM_RASTER_PATH=HDM_RASTER_PATH
      rv$cropRasters = cropRasters
      FHAnalysis<-fhProcess(
        rawFH =  file.path("./rawFH/",input$unionedFH),
        start.SEASON = input$startTimespan,
        end.SEASON = NULL,
        OtherAndUnknown = input$otherUnknown,
        validFIRETYPE = c("BURN","BUSHFIRE","UNKNOWN","OTHER")
      )
      #Save input settings to a list and then append into FH analysis object
      #FHAnalysis$AnalysisInputs<-list(
      FHAnalysis$FireScenario= input$unionedFH
      FHAnalysis$RasterRes = input$RasterRes
      FHAnalysis$ClipPolygonFile = clipShape
      FHAnalysis$Region_No = myREG_NO
      FHAnalysis$PUBLIC_ONLY = input$public
      FHAnalysis$Start_Season = NULL
      FHAnalysis$name<-paste0("FH_Analysis_",rv$outputFH)
      st_write(FHAnalysis$OutDF,file.path(ResultsDir,paste0(FHAnalysis$name,".shp")),append=FALSE)
      #) 
      print("Save input settings to a list and then append into FH analysis object")
      FHAnalysis$FH_IDr<-fasterize(sf=FHAnalysis$OutDF,raster =  cropRasters$Raster,field = "ID",fun="first")
      print("made FHAnalysis$FH_IDr")
      
      rv$FHAnalysis<-FHAnalysis
      
      print("run function to calculate all combinations (function from calc_U_AllCombs)")
      print(paste("RasterRes =",RasterRes))
      allCombs <- calcU_All_Combs(
        myFHAnalysis =  rv$FHAnalysis,
        myCropRasters = rv$cropRasters,
        myRasterRes = RasterRes)
      rv$allCombs<-allCombs
      print("made allcombs")
      
      print(paste ("line", 249, ls(pattern="FHAnalysis" ),"exists"))
      #save analysis to enable reloading
      save(FHAnalysis,cropRasters,allCombs,
           file=file.path("./FH_Outputs",paste0(FHAnalysis$name,input$RasterRes,".rdata")))
      print("saved FHAnalysis to enable reloading")
      
      
      updateSelectInput(session,'FHAnalysis','Select FH dataset (.rdata)',
                        choice = c("",list.files('./FH_Outputs/',pattern=".rdata$")))
      
    })
    
    
  })
  # Observer to run relative abundance analysis  -------------------------------------------------------
  observeEvent({input$runRA|input$runRA_TFI},ignoreInit = T,{
    withBusyIndicatorServer("runRA", {

      withBusyIndicatorServer("runRA_TFI", {

        if(input$spListChoice == FALSE){
          rv$TaxonList<-read.csv("./ReferenceTables/DraftTaxonListStatewidev2.csv")
        }else{
          rv$TaxonList<-read.csv(file.path("./CustomCSV",input$customSpList))
        }
        

        
        HDMSpp_NO <- rv$TaxonList$TAXON_ID[rv$TaxonList$Include=="Yes"]
        writeSp <- rv$TaxonList$TAXON_ID[rv$TaxonList$WriteSpeciesRaster == "Yes"]
        writeSp <- writeSp[writeSp %in% HDMSpp_NO]
        
        
        print("getting HDMvals")
        if(rv$FHAnalysis$RasterRes == "225"){
          load(paste0("./HDMS/HDMVals", rv$FHAnalysis$RasterRes, ".rdata"))
          HDMVals <- HDMVals[rv$cropRasters$IDX, as.character(HDMSpp_NO)]
        }else{
          print("doing 75m version makeHDMValsfromRasters")
          HDMVals<-makeHDMValsfromRasters(myHDMSpp_NO = HDMSpp_NO,
                                           myCropDetails = rv$cropRasters)
          
        }
        #print(paste("dim hdmvals =",dim(HDMVals)))
        print("Loaded HDMVals")
        
        if(input$spResponseChoice == FALSE){
          mySpGSResponses <- "./ReferenceTables/OrdinalExpertLong.csv"
        }else{
          mySpGSResponses <- file.path("./CustomCSV",input$customResponseFile)
        }
        
        AbundDataByGS  <-  read.csv(mySpGSResponses)[,c("EFG_NO",
                                                        "GS4_NO",
                                                        "FireType",
                                                        "Abund",
                                                        "VBA_CODE")]  #Select the file giving the fauna relative abundance inputs you wish to use
        AbundDataByGS$FireTypeNo[AbundDataByGS$FireType=="High"]<-2
        AbundDataByGS$FireTypeNo[AbundDataByGS$FireType=="Low"]<-1

        
        AbundDataLong = merge(AbundDataByGS, EFG_TSF_4GS, by=c('EFG_NO','GS4_NO'))
        AbundDataLong <- AbundDataLong[order(AbundDataLong$VBA_CODE),]
        print("making Spp abund LU List")
        LU_List <- make_Spp_LU_list(myHDMSpp_NO = HDMSpp_NO,
                                     myAbundDataLong = AbundDataLong)
        
        
        if (exists("rv$TaxonList"))
          print("Making spYearSumm")
        
        SpYearSummWide <- calc_SpeciesRA(myFHAnalysis=rv$FHAnalysis,
                                      myAllCombs <- rv$allCombs,
                                      myHDMSpp_NO = HDMSpp_NO, 
                                      myWriteSpRasters = input$makeRArasters,
                                      myResultsDir = ResultsDir,
                                      myLU_List = LU_List,
                                      myHDMVals = HDMVals,
                                      myTaxonList = rv$TaxonList,
                                      writeYears = NULL,#input$yearsForRasters,
                                      myWriteSp = writeSp)
        rv$SpYearSummWide <- SpYearSummWide
        #write.csv(SpYearSumm,file.path(ResultsDir,"SpYearSumm.csv"),row.names=F)
        print("finished sp year summ")    
        Baseline<-ifelse(input$endBaseline>input$startBaseline,input$startBaseline:input$endBaseline,input$startBaseline)
        print("calcuated myBaseline")
        myDeltaAbund <- calcDeltaAbund(SpYearSumm = SpYearSummWide,
                                       myFHAnalysis=rv$FHAnalysis,
                                       myBaseline = Baseline,
                                       myResultsDir = ResultsDir)
        print("finished deltaabund")
        })
    })
  })
  # Observer to get update years for calculations-----------------------------------------------------
  observeEvent(rv$FHAnalysis$TimeSpan,{
    updateSelectInput(session,"yearsForRasters",
                      choices = rv$FHAnalysis$TimeSpan,
                      selected = min(rv$FHAnalysis$TimeSpan)
    )
    updateSelectInput(session,"startBaseline",
                      choices = rv$FHAnalysis$TimeSpan,
                      selected = min(rv$FHAnalysis$TimeSpan)
    )
    updateSelectInput(session,"endBaseline",
                      choices = rv$FHAnalysis$TimeSpan,
                      selected = min(rv$FHAnalysis$TimeSpan)
    )
    
  }
  )
  
  # Observer to run TFI related calcuations---------------------
  observeEvent(
    input$runTFI|input$runRA_TFI,
    ignoreInit = T,{
      withBusyIndicatorServer("runTFI", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(
            need(rv$FHAnalysis,
                 'You need to select a FH analysis to use')
          )
          print("running TFI calc")
          
          TFI_Result <- calc_TFI_2(
            myFHAnalysis = rv$FHAnalysis,
            myAllCombs = rv$allCombs,
            myTFI_LUT = TFI_LUT,
            OutputRasters = input$makeTFIrasters,
            myResultsDir = ResultsDir)
          
          
          print("Finished my TFI")
          
          rv$TFI<-TFI_Result
          
          save(TFI_Result,
               file=file.path(
                 ResultsDir,
                 paste(file_path_sans_ext(rv$FHAnalysis$name),
                       "TFI.rdata")
               )
          )
          
        })
      })
    }
  )
  
  # Observer to run BBTFI calcuations---------------------
  observeEvent(
    input$runBBTFI|input$runRA_TFI,
    ignoreInit = T,{
      withBusyIndicatorServer("runBBTFI", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(
            need(rv$FHAnalysis,
                 'You need to select a FH analysis to use')
          )
          
          print ("calculating BBTFI")

          BBTFI<-calcBBTFI_2(
            myFHAnalysis = rv$FHAnalysis,
            myAllCombs = rv$allCombs,
            makeBBTFIrasters = input$makeBBTFIrasters,
            myResultsDir = ResultsDir
          )
          print("finished BBTFI calcs")
          
          rv$BBTFI<-BBTFI
          
          save(myBBTFI,
               
               file=file.path(
                 ResultsDir,
                 paste(file_path_sans_ext(rv$FHAnalysis$name),
                       "BBTFI.rdata")
                 )
          )
          
          })
      })
    }
  )
  # Observer to run GS calculations-----------------------
  observeEvent(
    input$runGS|input$runRA_TFI,
    ignoreInit = T,{
      withBusyIndicatorServer("runGS", {
        withBusyIndicatorServer("runRA_TFI", {
          validate(
            need(rv$FHAnalysis,
                 'You need to select a FH analysis to use')
          )
          
          print ("GS Calculations")
          GS_Summary<-makeGS_Summary(
            myFHAnalysis = rv$FHAnalysis,
            myAllCombs = rv$allCombs
          )
          print("finished GS calcs")
          
          rv$GS_Summary<-GS_Summary
          
          save(GS_Summary,
               file=file.path(
                 ResultsDir,
                 paste(file_path_sans_ext(rv$FHAnalysis$name),
                       "GS_Summary.rdata")
               )
          )
          
        })
      })
    }
  )
  # Observer prints the details of currently selected analysis-----------------------------------------
  observeEvent(rv$FHAnalysis$name,ignoreInit = T,
               {output$selected_FH_name<-renderText({
                 paste( "FH Analysis selected =" ,as.character(rv$FHAnalysis$name))
               })})  
  
  
  
  # Observer to display TFI and BBTFI plots when availible--------------------------------------------
  
  observeEvent(rv$TFI,ignoreInit = T,{
    myChoices<-unique(rv$TFI$EFG_NAME)
    myChoices<-myChoices[!is.na(myChoices)]
    updateSelectInput(session,"EFGChoices",choices=myChoices)
    updateTabItems(session,"tabs","TFIplots")
    minSEASON<-min(rv$TFI$SEASON)
    maxSEASON<-max(rv$TFI$SEASON)
    updateSliderInput(session,"tfiSeasonChoices",min=minSEASON,max=maxSEASON,value=c(1980,maxSEASON))
    
  })
  output$TFItrendPlot<-renderPlotly({
    rv$TFI%>% 
      filter(EFG_NAME == input$EFGChoices)%>%
      group_by(TFI_STATUS,SEASON) %>%
      summarise(Area = sum(Hectares))%>%
      plot_ly(x=~SEASON, y=~Area, group=~TFI_STATUS,
              type="bar",color=~TFI_STATUS)%>%
      layout(title = paste0(input$EFGChoices,"\n","TFI Status"),
             yaxis = list(rangemode = "tozero",title = "Area (ha)"),
             #xaxis= list(range = as.numeric(input$tfiSeasonChoices)),
             barmode = 'stack')
    
  })
  
  
  
  
  observeEvent(rv$myBBTFI,ignoreInit = T,{
    #cdata <- session$clientData
    
    
    output$BBTFIPlot<-renderPlotly({
      p2<-ggplot(data = rv$myBBTFI$BBTFI_EFG_Area_SEASON[rv$myBBTFI$BBTFI_EFG_Area_SEASON$EFG==input$EFGChoices,],
                 aes(x = SEASON,
                     y = ha,
                     fill=as.factor(Times_BBTFI))) +
        geom_col()+
        labs(fill = "Times BBTFI")+
        ggtitle(paste0("Number of times burned below TFI\n",rv$EFG_text))+
        xlim(input$tfiSeasonChoices)+
        ylab("Area (ha)")
      ggplotly(p2)#,width = cdata$output_pid_width, height = cdata$output_pid_height
    }
    )
    
  })
  # Observers make RA charts when availible--------------------------------------------
  observeEvent(rv$SpYearSumm,ignoreInit = T,{
    
    myChoices<-unique(rv$SpYearSumm$COMMON_NAME)
    updateSelectizeInput(session,"raSpChoices", choices = myChoices)
    updateTabItems(session,"tabs","RAplots")
    minSEASON<-min(rv$SpYearSumm$SEASON)
    maxSEASON<-max(rv$SpYearSumm$SEASON)
    updateSliderInput(session,"raSeasonChoices",min=minSEASON,max=maxSEASON,value=c(1980,maxSEASON))
  })
  
  observeEvent(input$raSpChoices,ignoreInit = T,{
    output$RAtrendPlot <- renderPlotly({
      if (length(input$raSpChoices) == 0) {
        return()
      }else{
        ggplot(data = rv$SpYearSumm[rv$SpYearSumm$COMMON_NAME%in%input$raSpChoices,],
               aes(x = SEASON,
                   y = SUM_RAx100,
                   color=COMMON_NAME)) +
          geom_line()+
          ggtitle("Summed relative abundance")+
          xlim(input$raSeasonChoices)+
          expand_limits(y=0)
      }
    })
  })
  
  # Calculate custom species list-----------------------------------------------------------  
  observeEvent(input$runDSpList,{
    withBusyIndicatorServer("runDSpList", {
      req(input$spREGION_NO)
      if(input$spREGION_NO == 7){
        myPoly=file.path("./AdHocPolygons",input$spAdHocShape)
      }else{
        myPoly="./ReferenceShapefiles/LF_DISTRICT.shp"
      }
      print ("calcuating draft species List")
      myDraftSpList<-calc_DraftSpList(REG_NO = input$spREGION_NO,
                                      RasterRes = 225,
                                      myPoly=myPoly,
                                      PUBLIC_LAND_ONLY = input$sppublic,
                                      myHDMVals=HDMVals225 )
      print (head(myDraftSpList))
      write.csv(myDraftSpList,file.path(ResultsDir,"myDraftspList.csv"))
      print("made draft species List")
    })
  })
  # Calculate SppEFGLMU for GSO-----------------------------------------------------------  
  observeEvent(input$runspEFGpList,{
    withBusyIndicatorServer("runspEFGpList", {
      req(input$spREGION_NO)
      if(input$spREGION_NO == 7){
        myPoly=file.path("./AdHocPolygons",input$spAdHocShape)
      }else{
        myPoly="./ReferenceShapefiles/LF_DISTRICT.shp"
      }
      
      
      calc_Spp_EFG_LMU(REG_NO = input$spREGION_NO,#REG_NO of defined region from input (1:6) or 0 for statewide or 7 for Ad Hoc Poly),
                      RasterRes = 225,
                      PUBLIC_LAND_ONLY = input$sppublic,
                      myPoly = myPoly,#shapefile ofLF_REGIONs( default)or  adhoc region,
                      generalRasterDir = "./InputGeneralRasters",
                      splist ="./ReferenceTables/DraftTaxonListStatewidev2.csv",
                      myHDMVals=HDMVals225,
                      #EFGRas="./InputGeneralRasters/EFG_NUM_225.tif",
                      TFI_LUT = TFI_LUT)
      
    })
  })
  

  # })
  #Run Aspatial GSO-------------------------------------------------
  
  # this section runs the rmd script and documents for aspatial GSO that was written by Paul Moloney in 2017. and modified to use shiny GUI
  #Observers for GSO .csv uploads  ---------------------------------
  observe( {myInput = input$addGSOCSV
  savePath ="./GSOInputs"
  file.copy(
    myInput$datapath,
    file.path(savePath,myInput$name)
  )
  }
  )
  
  
  observeEvent(input$lmuScenarios,
               ignoreInit = T,{
                 myScenarios=unique(read.csv(file.path(WD,"GSOInputs",input$lmuScenarios))$Scenario)
                 updateSelectInput(session,"GSOBaseLine",choices=c("Optimisation",myScenarios))
               }
  )
  
  observeEvent(input$runGSO,{
    withBusyIndicatorServer("runGSO", {
      fileConn<-file("./GSO/tempsettings.r")
      
      
      
      writeLines(c(paste0("GSOResultsDir ='",file.path(WD,ResultsDir),"'"),
                   paste0("FireType ='",input$GSOFireType,"'"),
                   paste0("Comparison = '" ,input$GSOBaseLine,"'"),
                   paste0("Classes = '",input$GSOFaunaClasses,"'"),
                   paste0("Rule = '",input$GSOrule,"'"),
                   paste0("dWt = ",input$GSOdwt ),
                   paste0("nrep = ",input$GSOnrep),
                   paste0("nsim = ",input$GSOnsim),
                   paste0("SpEFGLMU = read.csv('",file.path(WD,"GSOInputs",input$spEFGLMU),"',na='NA')"),
                   paste0("GSOScen<-read.csv('",file.path(WD,"GSOInputs",input$lmuScenarios),"',na='NA')"),
                   paste0("GSOArea<-read.csv('",file.path(WD,"GSOInputs",input$lmuArea),"',na='NA')"),
                   paste0("SurveyData<-read.csv('",file.path(WD,"GSOInputs",input$ObsData),"',na='NA')")
      ),
      
      fileConn)
      close(fileConn)
      rmarkdown::render("./GSO/GSOAnalysisOutput.Rmd",output_dir = ResultsDir,clean=T)
    })
  })
  

# observer to shut down server
  observeEvent(input$close, {
    js$closeWindow()
    system("sudo shutdown")
  })
  
  roots =c(root="./results")
  
  shinyFileChoose(input, 'files', 
                  root=roots)
  
  
  
}
