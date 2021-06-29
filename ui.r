ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      color = "green",
      menuItem("Home", tabName = "Intro", icon = icon("home")),
      menuItem(
        text = "Utilities",
        tabName = "util",
        icon = icon("cloud-upload-alt")
      ),
      menuItem(
        "Settings for Analysis",
        tabName = "AnalysisSettings",
        icon = icon("fire")
      ),
      conditionalPanel(
        condition = "input.usePUpolys == 0",
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
      ),
      menuItem(
        "Aspatial GSO Inputs" ,
        tabName = "GSO",
        icon = icon("calculator")
      ),
      
      p(versionDate),
      p(versionFAMEFMR),
      p(""),
      # save and reload analysis  buttons -----
      withBusyIndicatorUI(
        shinyFilesButton(
          id = "loadAnalysis",
          label = "load existing analysis file",
          title = "select analysis to load",
          multiple = FALSE,
          buttonType = "default",
          class = NULL,
          icon = icon("folder-open"),
          style = NULL,
          viewtype = "icon"
        )  
      ),
      withBusyIndicatorUI(
        shinySaveButton(id = "saveAnalysis",
                        label = "save analysis to file",
                        title = "save analysis file as...",
                        filename = "savedValue",
                        filetype=list(qs="qs"),
                        icon = icon("file-export"),
                        viewtype = "icon"
        )
      )
      
      
      
    ),
    absolutePanel(
      conditionalPanel(
        #style = "background-color:#FFFFFF",
        condition = "($('html').hasClass('shiny-busy'))",
        img(
          src = "Fire-animation.gif",
          width = "300",
          height = "200"
        ),
        #Creative Commons Attribution-Share Alike 3.0 Unported license.https://commons.wikimedia.org/wiki/File:BurningFlame0.gif
        h6(
          "https://commons.wikimedia.org/wiki/File:BurningFlame0.gif"
        )
      )
    )
    
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
 
    tabItems(
      tabItem(tabName = "Intro",
              fluidRow(column(
                8,
                
                img(src = "FAME.png"),
                img(src = "08732250_before_after_2014_fire.jpg")
                
              ),
              column(4))),
      # Upload and other utilities tab--------------------------------------------
      tabItem(tabName = "util",
              fluidRow(
                column(
                  8,
                  box(width =12,background = "light-blue",title="Select and upload files",
                      fileInput(
                        inputId = "rawFH",
                        label = "Select 4 elements of raw fire sequence shapefile\n.shp, .shx, .prj, .dbf",
                        multiple = TRUE
                      ),
                      htmlOutput("message_text"),
                      fileInput(
                        inputId = "adHocPoly",
                        label = "Select 4 elements of ad hoc area shapefile\n.shp, .shx, .prj, .dbf",
                        multiple = TRUE
                      ),
                      fileInput(
                        inputId = "puPoly",
                        label = "Select 4 elements of PU/BU shapefile\n.shp, .shx, .prj, .dbf",
                        multiple = TRUE
                      ),
                      htmlOutput("message_text1"),
                      fileInput(
                        inputId = "addCustomCSV",
                        label = "Add Custom Species List or FaunaAbundLUT CSV",
                        accept = c("csv", ".csv")
                      )
                      
                  )
                ),
                column(
                  4,
                  box(width =12,background = "light-blue",title= "Create draft species lists",
                      selectInput("spREGION_NO", "Choose an area for species list",
                                  choices = as.list(c(REG_NO))),
                      conditionalPanel(
                        condition = "input.spREGION_NO == '7'",
                        selectInput(
                          'spAdHocShape',
                          'Select AdHoc Area shapefile',
                          choice = c("", list.files('./AdHocPolygons/', pattern =
                                                      ".shp$"))
                        )
                      ),
                      radioButtons("sppublic", "Restrict analysis to Public Land", c("Yes", "No")),
                      
                      # runscript button
                      withBusyIndicatorUI(actionButton("runDSpList", label = "Run draft species list")),
                      withBusyIndicatorUI(
                        actionButton("runspEFGpList", label = "Run Spp EFG LMU for list for GSO")
                      )
                ),
                  
                  
                )
              ),
              fluidRow(
                column(4,
                       
                       box(width =12,background = "light-blue",title= "Downloads",
                           downloadButton("downloadTool", "Download FAME ArcGIS preproccessing tool"),
                           downloadButton("downloadManual", "Download FAME manual"),
                           tags$h1("Revised, alternate 2"),
                           HTML("<p>Download FAME manual <a https://github.com/nevilamos/FAMEshiny/blob/main/Manual/FAMEv2_User_Manual.pdf</a>!</p>")
                       ),
                       box(width =12, background = "red",title = "WARNING BUTTON BELOW WILL SHUT DOWN SERVER",
                           useShinyjs(),
                           extendShinyjs(text = jscode, functions = c("closeWindow")),
                           
                           h5("Make sure you have downloaded all your data first"),
                           actionButton("close", "Shut Down Server")
                       )),
                column(8,
                       box(width =12,title="Download results",
                           h5("Current Results Directory is:"),
                           h5(textOutput("resultsDir")),
                           shinyFilesButton(
                             'files',
                             label = 'Select file(s) to download',
                             title = '',
                             multiple = T
                           ) ,
                           tableOutput('filepaths') ,
                           downloadButton("downloadFiles", "Download Files")
                       )),
                
              )
              
      ),
      
      
      
      # fhAnalysis tab content---------------------------------------------------------
      tabItem(tabName = "AnalysisSettings",
              fluidRow(
                column(6,
                       box(title ="FH Analysis Settings",
                           width = 12,
                           background = "light-blue",
                           
                           splitLayout(shinyFilesButton(
                             id = "selectRawFH",
                             label = "select raw FH",
                             title = "select raw Fire history shapefile to run",
                             multiple = FALSE,
                             buttonType = "default",
                             class = NULL,
                             icon = NULL,
                             style = NULL,
                             viewtype = "detail",
                             
                           ),
                           textOutput("rawFHName")),
                           
                           
                           #select region or user defined area to run analysis on ----
                           # shinyWidgets::pickerInput(inputId = "REGION_NO",
                           #                           label = "Choose a Region",
                           #                           choices = as.list(c(REG_NO)),
                           #                           width = "fit",),
                           
                           selectInput("REGION_NO", "Choose a Region",
                                       choices = as.list(c(REG_NO)),,width = "50%"),
                           conditionalPanel(
                             condition = "input.REGION_NO == '7'",
                             
                             splitLayout(shinyFilesButton(
                               id = "selectAdHoc",
                               label = "select user defined shapefile",
                               title = "select user defined shapefile for analysis area", 
                               multiple = FALSE,
                               buttonType = "default",
                               class = NULL,
                               icon = NULL,
                               style = NULL,
                               viewtype = "detail",
                               
                             ),
                             textOutput("AdHocName")),
                             
                           ),
                           
                           fluidRow(
                             column(4,
                                    radioButtons("RasterRes", "Select Raster Resolution", cellSizes),
                             ),
                             column(4,
                                    radioButtons("public", "Restrict to Public Land", c("Yes", "No"))
                             ),
                             column(4,
                                    radioButtons(
                                      inputId = "otherUnknown",
                                      label = "Value for other and unknown fires",
                                      choices = c(
                                        "Bushfire" = 2,
                                        "Burn" = 1,
                                        "NA" = NA
                                      )
                                      
                                    )
                             )
                           ),
                           
                           numericInput("startTimespan",
                                        "First season for analysis output",
                                        1980, 1980,width = "25%"),
                           
                           
                           
                           
                           
                           
                           
                       ),
                       box(title = "JFMP Settings",width = 12,background = "light-blue",
                           checkboxInput(
                             inputId = "usePUpolys",
                             label = "Incude burn unit/planning unit shapefile for JFMP analysis",
                             value = FALSE
                           ),
                           
                           #select planning unit shapefile----
                           conditionalPanel(
                             condition = "input.usePUpolys",
                             splitLayout(
                               shinyFilesButton(
                                 id = "selectPU",
                                 label = "select planning unit shapefile",
                                 title = "select planning unit shapefile for analysis area", 
                                 multiple = FALSE,
                                 buttonType = "default",
                                 class = NULL,
                                 icon = NULL,
                                 style = NULL,
                                 viewtype = "detail",
                                 
                               ),
                               textOutput("puName")
                             ),
                           numericInput(
                               "JFMPSeason0",
                               "JFMP SEASON 0",
                               as.integer(format(Sys.Date(), "%Y")))
                           )
                       )
                       
                ),
                
                
                #Fauna abundance headings conditional on whether JFMP or "standard" analysis is been run -----
                column(
                  6,
                  box(width = 12,background = "light-blue",
                      conditionalPanel(
                        condition = "input.usePUpolys == 0",
                        h4("Fauna Abundance Calculation Choices")
                      ),
                      conditionalPanel(
                        condition = "input.usePUpolys == 1",
                        h4("JFMP Calculation Choices")
                      ),
                      # inputs conditional on whether JFMP or "standard" analysis is been run -----                    
                      textOutput("selected_FH_name"),
                      splitLayout(
                        numericInput("startBaseline",
                                     "enter start season for abundance baseline",
                                     ""),
                        numericInput("endBaseline",
                                     "enter end season for abundance baseline",
                                     "")
                      ),
                      
                      
                      # use standard or choose custom species responses------
                      
                      fluidRow(column(2,
                                      checkboxInput(
                                        inputId = "spListChoice",
                                        label = "Use custom species list",
                                        value = FALSE,
                                        width = NULL
                                      )),
                               conditionalPanel(
                                 condition = 'input.spListChoice',
                                 
                                 column(3,
                                        shinyFilesButton(
                                          id = "customSpList",
                                          label = "select custom species list",
                                          title = "select custom species list csv file", 
                                          multiple = FALSE,
                                          buttonType = "default",
                                          class = NULL,
                                          icon = NULL,
                                          style = NULL,
                                          viewtype = "detail"
                                          
                                        )
                                 ),
                                 column(7,
                                        textOutput("customSpListName")
                                 )
                               ),
                               
                      ),
                      
                      splitLayout(checkboxInput(
                        inputId = "spResponseChoice",
                        label = "Use custom relative abundance table",
                        value = FALSE,
                        width = NULL
                      ),
                      conditionalPanel(
                        condition = 'input.spResponseChoice',
                        checkboxInput(
                          inputId = 'abundByGS',
                          label = "Relative abundance table by growth stage",
                          value = TRUE,
                          width =NULL
                        )
                      )),
                      conditionalPanel(
                        condition = 'input.spResponseChoice',
                        fluidRow(
                          column(5,
                                 shinyFilesButton(
                                   id = "customResponseFile",
                                   label = "Select user defined species response file",
                                   title = "Select user defined species response file", 
                                   multiple = FALSE,
                                   buttonType = "default",
                                   class = NULL,
                                   icon = NULL,
                                   style = NULL,
                                   viewtype = "detail",
                                   
                                 )),
                          column(7,
                                 textOutput("customResponseName")
                          )),
                      ),
                      # inputs conditional on whether JFMP or "standard" analysis is been run -----  
                      conditionalPanel(
                        condition = "input.usePUpolys == 0",
                        checkboxInput(
                          inputId = "makeRArasters",
                          label = "Make relative abundance rasters",
                          value = FALSE,
                          width = NULL
                        ),
                        # conditionalPanel(
                        #   condition = 'input.makeRArasters',
                        #   radioButtons(
                        #     "allOrSomeYears",
                        #     "Years to make rasters",
                        #     choices = c("all", "some")
                        #   ),
                        #   conditionalPanel(
                        #     condition = 'input.allOrSomeYears == "some"',
                        #     selectInput(
                        #       "yearsForRasters",
                        #       "Select one or more years for Rasters",
                        #       choices = "",
                        #       multiple = T
                        #     )
                        #   )
                        # ),
                        conditionalPanel(
                          condition = "input.usePUpolys == 0",
                          withBusyIndicatorUI(
                            actionButton(
                              "runRA",
                              label = "Run fauna relative abundance calculations"
                            )
                          )
                        ),
                        
                      ),
                      
                      conditionalPanel(
                        
                        
                        condition = "input.usePUpolys == 1",
                        splitLayout(
                          shinyFilesButton(
                            id = "targetHaFile",
                            label = "select area target file for JFMP",
                            title = "select area target file for JFMP csv file", 
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            icon = NULL,
                            style = NULL,
                            viewtype = "detail",
                            
                          ),
                          textOutput("targetHaFileName")
                        ),
                        
                        splitLayout(
                          shinyFilesButton(
                            id = "jfmpMetricWtFile",
                            label = "select file containing JFMP metric weights",
                            title = "JFMP metric weights csv file", 
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            icon = NULL,
                            style = NULL,
                            viewtype = "detail",
                            
                          ),
                          textOutput("jfmpMetricWtFileName")
                        ),
                        
                        splitLayout(
                          shinyFilesButton(
                            id = "zoneWtFile",
                            label = "select file containing JFMP zone weights",
                            title = "JFMP FMZ weights csv file", 
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            icon = NULL,
                            style = NULL,
                            viewtype = "detail",
                            
                          ),
                          
                          textOutput("zoneWtFileName")
                        ),
                        
                        withBusyIndicatorUI(
                          actionButton(
                            "runJFMP1",
                            label = "Run JFMP calculations stage 1"
                          )
                        ),
                        
                        
                      )
                  ),
                  
                  conditionalPanel(
                    condition = "input.usePUpolys == 1",
                    box(width =12,solidHeader = T,background = "light-blue",
                        title = "Compare alternative JFMPs",
                        splitLayout(
                          shinyFilesButton(
                            id = "draftJFMPFile",
                            label = "select draft JFMP input",
                            title = "select draft JFMP input csv file",
                            multiple = FALSE,
                            buttonType = "default",
                            class = NULL,
                            icon = NULL,
                            style = NULL,
                            viewtype = "detail",
                            
                          ),
                          textOutput("draftJFMPName")
                        ),
                        
                        withBusyIndicatorUI(
                          actionButton(
                            "runCompareJFMP",
                            label = "Compare Draft JFMP"
                          )
                        ),
                    )),
                  
                  conditionalPanel(
                    condition = "input.usePUpolys == 0",
                    box(title = "TFI and GS Calculations",
                        width =12,solidHeader = T,background = "light-blue",
                        
                        splitLayout(
                          checkboxInput(
                            inputId = "makeTFIrasters",
                            label =  "Make TFIstatus maps for each year",
                            value = FALSE,
                            width = NULL
                          ),
                          checkboxInput(
                            inputId = "makeBBTFIrasters",
                            label =  "Make BBTFIstatus maps for each year",
                            value = FALSE,
                            width = NULL
                          )),
                        
                        
                        splitLayout(
                          withBusyIndicatorUI(
                            actionButton("runTFI", label = "Run TFI calculations")
                          ),
                          withBusyIndicatorUI(
                            actionButton("runGS", label = "Run GS calculations")
                          )
                        )
                    )
                  )
                )
                
              ),
              
              fluidRow(
                column(6,
                       # runFH analysis action  button----
                       box(width = 12,background = "light-blue",
                           withBusyIndicatorUI(
                             actionButton("runFH", 
                                          label = "Run FH Analysis",
                                          class = "btn-warning")))),
                conditionalPanel(
                  condition = "input.usePUpolys == 0",
                  column(6,
                         box(width = 12,background = "light-blue",
                             withBusyIndicatorUI(
                               actionButton("runRA_TFI", label = "Run all calculations")
                             )
                         )
                  )
                )
              )
      ),
      # fAbund_TFI tab content------------------------------------------------------------------------------------------
      tabItem(
        tabName = "fAbund_TFI",
        
        h2("Spatial TFI and Fauna Abundance Calculations "),
        fluidRow(
          wellPanel(
            
          )
        ),
        fluidRow(
          column(6,
                 
          ),
          #TFI choices inputs-------------------------
          conditionalPanel(
            condition = "input.usePUpolys == 0",
            column(
              6,
              
            )
          )
        ),
      ),
      #Tab  TFI charts --------------------------------------------------------------------
      
      tabItem(tabName = "TFIplots",
              fluidPage(
                wellPanel(h4("TFI Plots"),
                          fluidRow(
                            column(
                              6,
                              selectInput(
                                inputId = "EFGChoices",
                                label = "TFI EFG Choices",
                                choices = NULL ,
                                selected = NULL,
                                multiple = F
                              )
                            ),
                            column(
                              6,
                              sliderInput(
                                "tfiSeasonChoices",
                                label = "Min and Max Season to plot",
                                min = 1980,
                                max = 2020,
                                value = c(1980, 2020),
                                sep = ""
                              )
                            )
                          )
                          , ),
                fluidRow(plotlyOutput("TFItrendPlot"),
                         plotlyOutput("BBTFIPlot")
                )
                
              )),
      #Tab  GS charts --------------------------------------------------------------------
      tabItem(tabName = "GSplots",
              fluidPage(
                wellPanel(h4("GS Plots"),
                          fluidRow(
                            column(
                              6,
                              selectInput(
                                "GSEFGChoices",
                                choices = NULL ,
                                label = "Choose EFG Number  (to clear use backspace)",
                                selected = NULL,
                                multiple = F
                              )
                            ),
                            column(
                              6,
                              sliderInput(
                                "GSSeasonChoices",
                                label = "Min and Max Season to plot",
                                min = 1980,
                                max = 2020,
                                value = c(1980, 2020),
                                sep = ""
                              )
                            )
                          ),
                ),
                fluidRow(plotlyOutput("GSPlot"),
                )
                
              )),
      #Tab Relative abundance plots------------------
      tabItem(tabName = "RAplots",
              fluidPage(
                wellPanel(
                  h4("Fauna Relative Abundance Plots"),
                  fluidRow(column(
                    6,
                    selectizeInput(
                      "raSpChoices",
                      choices = NULL,
                      label = "Choose 1-7 species  (to clear use backspace)",
                      selected = NULL,
                      multiple = T,
                      options = list(
                        maxItems = 7,
                        hideSelected = F,
                        placeholder = 'Select a species'
                      )
                    )
                  ),
                  column(
                    6,
                    sliderInput(
                      "raSeasonChoices",
                      label = "Min and Max Season to plot",
                      min = 1980,
                      max = 2020,
                      value = c(1980, 2020),
                      sep = ""
                    )
                  )),
                ),
                
                fluidRow(plotlyOutput("RAtrendPlot"),
                         plotlyOutput("RADeltaPlot"))
              )),
      # tab GSO processing -------------------------------------------------------------------------------------
      tabItem(tabName = "GSO",
              fluidRow(
                #h2("Aspatial GSO Input Selections"),
                column(
                  6,
                  
                  box(width =12,background = "light-blue",title ="Select and upload GSO .csv files",
                      fileInput(
                        inputId = "addGSOCSV",
                        label = "gsofiles to upload",
                        accept = c("csv", ".csv")
                      )),
                  box(width =12,background = "light-blue",title ="Select GSO input tables",
                      selectInput(
                        'spEFGLMU',
                        'Select Spp_EFG_LMU.csv file',
                        choices = c(
                          "Spp_EFG_LMU.csv",
                          list.files('./GSOInputs/', pattern = "Spp_EFG_LMU.csv$")
                        )
                      ),
                      selectInput(
                        'lmuArea',
                        'LMU_Area.csv file',
                        choice = c(
                          "LMU_Area.csv",
                          list.files('./GSOInputs/', pattern = "LMU_Area.csv$")
                        )
                      ),
                      selectInput(
                        'lmuScenarios',
                        'LMU_Scenarios.csv file',
                        choice = c(
                          "LMU_Scenarios.csv",
                          list.files('./GSOInputs/', pattern = "LMU_Scenarios.csv$")
                        )
                      ),
                      selectInput(
                        'ObsData',
                        'ObsData.csv file',
                        choice = c(
                          "ObsData.csv",
                          list.files('./GSOInputs/', pattern = "ObsData.csv$")
                        )
                      )
                  )
                ),
                column(
                  6,
                  box(width =12,background = "light-blue",title= "Analysis options",
                      h4(
                        "Please note all inputs are case sensitive, do not include any spaces"
                      ),
                      selectInput(
                        "GSOFireType",
                        choices = c("High", "Low"),
                        label = "Low or High fire type."
                      ),
                      selectInput(
                        "GSOBaseLine",
                        label = "Baseline for comparisons.'Optimisation' or select from input scenarios",
                        choices = c("Optimisation")
                      ),
                      selectInput(
                        "GSOFaunaClasses",
                        label = "Which fauna classes to use",
                        choices = c("All", "Birds", "Mammals", "Reptiles", "Frogs"),
                        selected="All",
                        #"Insects", "Fishes", "Crustaceans", "Annelids",Molluscs", "Nemerteans", "Flatworms", "Cnidarians","Echinoderms", "Zooplankton"
                        multiple = T
                      ),
                      selectInput(
                        "GSOrule",
                        "Select rule to use",
                        choices = c(
                          'Rule0',
                          'Rule1',
                          'Rule1a',
                          'Rule1b',
                          'Rule1c',
                          'Rule2',
                          'Rule2a',
                          'Rule2b',
                          'Rule2c',
                          'Rule3',
                          'Rule3a',
                          'Rule3b',
                          'Rule3c'
                        )
                      ),
                      
                      numericInput("GSOdwt", "weight for option 2", 0.75),
                      
                      numericInput("GSOnrep", "Number of iterations to run", 100),
                      numericInput("GSOnsim", "Number of simulations to generate 95% CI?", 5),
                      
                      withBusyIndicatorUI(actionButton("runGSO", "Run Aspatial GSO"))
                  )
                )
              ))
      
    ),  
    
  ),
  
  tags$head(tags$style(HTML("
    .skin- .main-sidebar {
        background-color:  green;
                            }"))),
)
