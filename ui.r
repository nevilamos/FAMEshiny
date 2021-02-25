ui <-dashboardPage(
  dashboardHeader(disable = T),
    dashboardSidebar(
    width = 300,
    sidebarMenu(id="tabs",
                color="green",
                menuItem("Home", tabName = "Intro", icon = icon("info-circle")),
                menuItem(text = "Utilities",tabName ="util",icon = icon("cloud-upload-alt")),
                menuItem("Do fire scenario analysis", tabName = "fhAnalysis", icon = icon("fire")),
                menuItem("Fauna abund or TFI for scenario", tabName = "fAbund_TFI", icon = icon("crow")),
                menuItem("TFI Plots",tabName = "TFIplots", icon = icon("chart-line")),
                menuItem("Fauna RA Plots",tabName = "RAplots", icon = icon("chart-area")),
                menuItem("Aspatial GSO Calc" ,tabName = "GSO",icon = icon("calculator"))
    ),
    absolutePanel(
      conditionalPanel(#style = "background-color:#FFFFFF",
        condition="($('html').hasClass('shiny-busy'))",
        img(src="Fire-animation.gif",width="300",height="200"),#Creative Commons Attribution-Share Alike 3.0 Unported license.https://commons.wikimedia.org/wiki/File:BurningFlame0.gif
        h6("https://commons.wikimedia.org/wiki/File:BurningFlame0.gif")
      ))
    
  ),
  dashboardBody(
    useShinyjs(),
    tags$style(appCSS),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "readable.min.css")
    # ),
    
    tabItems(
      tabItem(tabName = "Intro",
              fluidRow(
                column(8,
                       
                       img(src="FAME.png"),
                       img(src = "08732250_before_after_2014_fire.jpg"),
                       h3("Version 1.1 September 26 2019")
                       
                ),
                column(4)
              )
              
      ),
      # Upload and other utilities tab--------------------------------------------
      tabItem(tabName = "util",
              fluidRow(
                column(8,
                       wellPanel(
                         h2("Select and upload files"),
                         fileInput(inputId = "rawFH",
                                   label ="Select 4 elements of raw fire sequence shapefile\n.shp, .shx, .prj, .dbf",
                                   multiple = TRUE),
                         htmlOutput("message_text"),
                         fileInput(inputId = "adHocPoly",
                                   label ="Select 4 elements of ad hoc area shapefile\n.shp, .shx, .prj, .dbf",
                                   multiple = TRUE),
                         htmlOutput("message_text1"),
                         fileInput(inputId = "addCustomCSV",
                                   label="Add Custom Species List or FaunaAbundLUT CSV",
                                   accept = c("csv", ".csv")
                                   )
                         
                       ),
                       wellPanel(
                         h2("Download FAME ArcGIS preproccessing tool"),
                         downloadButton("downloadTool","Download tool"),
                         h2("Download manual"),
                         downloadButton("downloadManual","Download manual"),
                         useShinyjs(),
                         extendShinyjs(text = jscode, functions = c("closeWindow")),
                         h2("WARNING BUTTON BELOW WILL SHUT DOWN SERVER"),
                         h2("Make sure you have downloaded all your data first"),
                         actionButton("close", "Shut Down Server")
                       )
                       
                       
                ),
                
                column(4,
                       wellPanel(
                         h2("Create draft species lists"),
                         selectInput("spREGION_NO", "Choose an area for species list",
                                     choices = as.list(c(REG_NO))),
                         conditionalPanel(
                           condition = "input.spREGION_NO == '7'",
                           selectInput('spAdHocShape','Select AdHoc Area shapefile',
                                       choice = c("",list.files('./AdHocPolygons/',pattern=".shp$")))
                         ),
                         radioButtons("sppublic", "Restrict analysis to Public Land", c("Yes", "No") 
                         ),
                         
                         # runscript button 
                         withBusyIndicatorUI(
                           actionButton("runDSpList", label = "Run draft species list")
                         ),
                         withBusyIndicatorUI(
                           actionButton("runspEFGpList", label = "Run Spp EFG LMU for list for GSO")
                         )
                         
                       ),
                       
                       wellPanel(
                         h2("Download results"),
                         h4("Current Results Directory is:"),
                         h4(ResultsDir),
                         shinyFilesButton('files', label='Select file(s) to download', title='', multiple=T) ,
                         tableOutput('filepaths') ,
                         downloadButton("downloadFiles", "Download Files")
                       )
                       
                       
                )
              )
              
      ),
      
      # fhAnalysis tab content---------------------------------------------------------
      tabItem(tabName = "fhAnalysis",
              fluidRow(
                h2("New Fire Scenario Analysis"),
                column(6,
                       wellPanel(
                         selectInput('unionedFH','Select fire scenario shapefile',
                                     choice = c("",list.files('./rawFH/',pattern=".shp$"))),
                         selectInput("REGION_NO", "Choose a Region",
                                     choices = as.list(c(REG_NO))),
                         conditionalPanel(
                           condition = "input.REGION_NO == '7'",
                           selectInput('AdHocShape','Select AdHoc Area shapefile',
                                       choice = c("",list.files('./AdHocPolygons/',pattern=".shp$")))
                           
                         )
                         
                       )
                ),
                column(6,
                       wellPanel(
                         radioButtons("RasterRes","Select Raster Resolution",cellSizes
                         ),
                         
                         radioButtons("public", "Restrict analysis to Public Land", c("Yes", "No") 
                         ),
                         radioButtons(inputId = "otherUnknown",
                                      label = "Value for other and unknown fires",
                                      choices = c("Bushfire" = 2,
                                                  "Burn"=1,
                                                  "NA"=NA)
                                      
                         ),
                         numericInput("startTimespan",
                                      "First season for analysis output",
                                      1980,1980),
                         # runscript button 
                         withBusyIndicatorUI(
                           actionButton("runFH", label = "Run FH Analysis")
                         )
                       )  
                ))
              
              
              
              
      ),
      # fAbund_TFI tab content------------------------------------------------------------------------------------------
      tabItem(tabName = "fAbund_TFI",
              h2("Spatial TFI and Fauna Abundance Calculations "),
              fluidRow(
                wellPanel(
                  h3("Load a pre-existing FH analysis datafile?"),
                  selectInput('FHOutputLoad','FH analysis to use',
                              choice = c("use current loaded FH analysis",
                                         list.files('./FH_Outputs/',pattern=".rdata$",full.names = T)
                              )
                  ),
                  textOutput("selected_FH_name")
                  
                )
              ),
              fluidRow(
                column(8,
                       wellPanel(
                         fluidRow(
                           column(6,
                                  h3("Fauna Abundance Calculations "),
                                  # use standard or choose Custom Species List
                                  numericInput("startBaseline",
                                               "enter start season for abundance baseline",
                                               ""),
                                  numericInput("endBaseline",
                                               "enter end season for abundance baseline",
                                               "")
                           ),
                           # use standard or choose custom species responses----------------------------------------------------------
                           column(6,
                                  checkboxInput(inputId = "spListChoice",
                                               label="Use custom species table",
                                               value = FALSE,width = NULL),
                                  conditionalPanel(
                                    condition = 'input.spListChoice',
                                    selectInput("customSpList",
                                                "Select user defined species list",
                                                choice = c(list.files('./CustomCSV',pattern=".csv$",full.names=T))
                                    )
                                  ),
                                  
                                  checkboxInput(inputId = "spResponseChoice",
                                               label="Use custom relative abundance table",
                                               value = FALSE,width = NULL),
                                  conditionalPanel(
                                    condition = 'input.spResponseChoice',
                                    selectInput("customResponseFile",
                                                "Select user defined Response File",
                                                choice = c("",list.files('./CustomCSV',pattern=".csv$",full.names=T))
                                    ) 
                                  ),
                                  
                                  checkboxInput(inputId ="makeRArasters",
                                                label="Make relative abundance rasters",
                                                value = FALSE,width = NULL),
                                  conditionalPanel(
                                    condition = 'input.makeRArasters',
                                    radioButtons("allOrSomeYears","Years to make rasters",choices = c("all","some")),
                                    conditionalPanel(
                                      condition = 'input.allOrSomeYears == "some"',
                                      selectInput("yearsForRasters","Select one or more years for Rasters",choices ="",multiple=T)
                                    )
                                  )
                           )
                         )
                       )
                ),
                #TFI choices inputs-------------------------
                column(4,
                       wellPanel(
                         h3("TFI related calculations"),
                         checkboxInput("makeTFIrasters",
                                       "Make TFIstatus maps for each year",
                                       value = FALSE,
                                       width = NULL),
                         checkboxInput("makeBBTFIrasters",
                                       "Make BBTFIstatus maps for each year",
                                       value = FALSE,
                                       width = NULL)
                       )
                )),
              #TFI and RA action buttons-------------------------------------
              fluidRow(
                
                  column(4,
                         # runscript buttons
                         withBusyIndicatorUI(
                           actionButton("runRA", label = "Run fauna relative abundance calculations")
                         )
                  ),
                  column(4,
                         withBusyIndicatorUI(
                           actionButton("runTFI", label = "Run TFI calculations")
                         )
                  ),

                  column(4,
                         withBusyIndicatorUI(
                           actionButton("runGS", label = "Run GS calculations")
                         )
                  ),
                  column(4,
                         withBusyIndicatorUI(
                           actionButton("runRA_TFI",label = "Run all calculations")
                         )
                  )
                
              )
              
              
              
      ),
      #Tab Fauna TFI charts --------------------------------------------------------------------
      tabItem(tabName = "TFIplots",
              fluidPage(
                wellPanel(
                  h4("TFI Plots"),
                  fluidRow(
                    column(6,selectInput("EFGChoices",choices = NULL ,label = "Choose EFG Number  (to clear use backspace)",selected = NULL,multiple = F)),
                    column(6,sliderInput("tfiSeasonChoices",label="Min and Max Season to plot",min=1980,max=2020,value=c(1980,2020), sep="")))
                ),
                fluidRow(
                  plotlyOutput("TFItrendPlot"),
                  plotlyOutput("BBTFIPlot"),
                )
                
              )
      ),
      #Tab Relative abundance plots
      tabItem(tabName = "RAplots",
              fluidPage(
                wellPanel(
                  h4("Fauna Relative Abundance Plots"),
                  fluidRow(
                    column(6,
                           selectizeInput("raSpChoices",
                                          choices = NULL,
                                          label = "Choose 1-7 species  (to clear use backspace)",
                                          selected = NULL,
                                          multiple = T,
                                          options = list(maxItems = 7, hideSelected =F,placeholder = 'Select a species')
                           )
                    ),
                    column(6,
                           sliderInput("raSeasonChoices",
                                       label="Min and Max Season to plot",
                                       min=1980,
                                       max=2020,
                                       value=c(1980,2020),
                                       sep=""))
                  )
                ),
                
                fluidRow(
                  plotlyOutput("RAtrendPlot")
                )
              )
      ),
      # tab GSO processing -------------------------------------------------------------------------------------
      tabItem(tabName = "GSO",
              fluidRow(h2("Aspatial GSO Input Selections"),
                       column(6,
                              
                              wellPanel(
                                h3("Select and upoad GSO .csv files"),
                                fileInput(inputId = "addGSOCSV",
                                          label="gsofiles to upload",
                                          accept = c("csv", ".csv")),
                                h3("Select GSO input tables"),
                                selectInput('spEFGLMU','Select Spp_EFG_LMU.csv file',
                                            choice = c("Spp_EFG_LMU.csv",list.files('./GSOInputs/',pattern="Spp_EFG_LMU.csv$"))),
                                selectInput('lmuArea','LMU_Area.csv file',
                                            choice = c("LMU_Area.csv",list.files('./GSOInputs/',pattern="LMU_Area.csv$"))),
                                selectInput('lmuScenarios','LMU_Scenarios.csv file',
                                            choice = c("LMU_Scenarios.csv",list.files('./GSOInputs/',pattern="LMU_Scenarios.csv$"))),
                                selectInput('ObsData','ObsData.csv file',
                                            choice = c("ObsData.csv",list.files('./GSOInputs/',pattern="ObsData.csv$")))
                              )
                       ),
                       column(6,
                              wellPanel(
                                h3("Analysis options"),
                                h4("Please note all inputs are case sensitive, do not include any spaces"),
                                selectInput("GSOFireType",choices=c("High","Low"),label="Low or High fire type."),
                                selectInput("GSOBaseLine",
                                            label ="Baseline for comparisons.'Optimisation' or select from input scenarios",
                                            choices= c("Optimisation")),
                                selectInput("GSOFaunaClasses",
                                            label ="Which fauna classes to use",
                                            choices = c("All","Birds", "Mammals", "Reptiles", "Frogs" ),
                                            #"Insects", "Fishes", "Crustaceans", "Annelids",Molluscs", "Nemerteans", "Flatworms", "Cnidarians","Echinoderms", "Zooplankton"
                                            multiple =T),
                                selectInput("GSOrule","Select rule to use",
                                            choices=c('Rule0',
                                                      'Rule1','Rule1a','Rule1b','Rule1c',
                                                      'Rule2','Rule2a','Rule2b','Rule2c',
                                                      'Rule3','Rule3a','Rule3b','Rule3c')),
                                
                                numericInput("GSOdwt","weight for option 2",0.75),
                                
                                numericInput("GSOnrep","Number of iterations to run",100),
                                numericInput("GSOnsim", "Number of simulations to generate 95% CI?",5),
                                
                                withBusyIndicatorUI(
                                  actionButton("runGSO","Run Aspatial GSO")
                                )
                              )
                       )
              )
      )
    )
  )
)
