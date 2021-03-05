## This document contains details of the inputs / outputs of the EcoRes1 Module. 

Inputs/Outputs for FAME v2.0

This document regarding inputs and outputs is primarily designed for the module curator. It details the specific input files and scripts which will need to be stored and curated centrally within Policy and Planning Division. At this stage it is envisaged that MER or FFRA units will become the custodians of the module which comprises the shiny app, input data and other R functions.

Guidance around the application of the outputs from the analysis module are provided in the Final report in Appendix B *Output 5 Conceptual Framework for application of ecological values in decision making.*

## ARCGIS/ Windows pre-processing:

Pre-processing of the input fire history polygons is required in ArcGIS, this creates a file that is then loaded to the server for processing.

Hardware and software requirements**:**

Windows 10 PC with ARCGIS 10.5.1 or ARCGISPro , 16GB ram.

### Inputs

#### Code.

ArcMap v10.3 toolbox "EcoRes1.tbx" and two associated python files "makeFlattened.py"

"Add1755BushfireToFireHistory.py"

#### Data Files

Two fire sequence polygon datasets (either shapefiles or file geodatabase) in VICGRID94 projection, one giving the fire history (ie past fire events) and the other giving a future fire scenario. The Template is based on the required fields from the corporate FIRE_HISTORY dataset. In each dataset the polygons must have at least the attributes SEASON and FIRETYPE (Table1). Other attributes can be present in the attribute table, they will be deleted from the output.

Each combination of fire SEASON and FIRETYPE must be represented by a separate polygon (ie each polygon may only have one SEASON and FIRETYPE).

  Field Name   Permissible values                                            Datatype        Length
  ------------ ------------------------------------------------------------- --------------- --------
  SEASON       4 digit year value for the SEASON of the fire event \>=1755   SHORT INTEGER   
  FIRETYPE     "BURN","BUSHFIRE","OTHER","UNKNOWN"                           STRING          50

Table 1. Required attribute fields for Fire History and Fire Future input feature classes.

A polygon shapefile containing polygon(s) to be selected as the boundary of the analysis area to be clipped from FireHistory and FireScenario above.

### Outputs

Shapefile with same fields (SEASON, FIRETYPE) as the input file, combining all the fire events into a single file clipped to the boundary selected.

Server/ Locally based R processing**.**

## Hardware requirements 

### Local PC

Minimum 4 cores 16Gb RAM to run process for whole state at 225m pixel resolution. Data stored on SSD for speed. Windows 10 or linux ubuntu 18.04. Smaller areas can be run at 75m resolution with this much Ram 64Gb is recommended to run statewide analysis as 75m pixel resolution

### AWS Server

Minimum instance with 4 cores and 16gb RAM to run process for whole state at 225m pixel resolution. Linux Ubuntu 18.04. Smaller areas can be run at 75m resolution with this much Ram 64Gb is recommended to run statewide analysis as 75m pixel resolution

## Software environment Requirements

R version 4.02 or later: R-Studio 1.3 or later, (server to run on AWS server). Shinyserver to run on AWS server.

For running on AWS server, a public pre-built image (AMI) is available on AWS EC2. For full details on this image see http://www.louisaslett.com/RStudio_AMI/

The current version of the image is: AMI-name:RStudio-1.3.1073_R-4.0.2_CUDA-10.1_cuDNN-7.6.5_ubuntu-18.04-LTS-64bit, AMI ID:ami-0c48131b082d5cb01. The image is accessed from a web browser on launch with the correct security permissions

These requirements are listed primarily to enable building of the app from scratch. This should not be necessary. A user can initially all the necessary files by creating a new project in R-Studio using the correct permissions

### CRAN packages:

\"aws.s3\", \"dashboardthemes\", \"doParallel\", \"dplyr\", \"fasterize\", \"foreach\", \"gdalUtils\", \"knitr\", \"Matrix.utils\", \"plotly\", \"raster\", \"Rfast\", \"rlang\", \"sf\", \"shiny\", \"shinycssloaders\", \"shinydashboard\", \"shinyFiles\", \"shinyjs\", \"tabularaster\", \"tibble\", \"tidyr\", \"tools\"

### GitHub Package

FAMEFMR -installed from

https://github.com/nevilamos/FAMEFMR

For convenience, and to avoid the necessity of setup of Rstudio and Shinyserver on a basic Linux then the use of the pre prepared shinyserver image available from:

http://www.louisaslett.com/RStudio_AMI/

is recommended.

If it is preferred to use a sever image direct from AWS, then ubuntu 26.04 or above is recommended. Instruction on setup on AWS can be found here.

https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722

## Inputs

### Directory structure.

All files ( inputs and outputs) should be located in a single main (root)directory, and subdirectories thereof. Files are shown below with their unix "dot notation" to indicate their location in this root directory.

The subdirectories contained in this main directory (./) are :

./AdHocPolygons

./CustomCSV

./FH_Outputs

./GSO

./GSOInputs

./HDMS

./HDMS/225m/BinaryThresholded

./HDMS/225m/BinaryThresholded

./InputGeneralRasters

./rawFH

./ReferenceShapefiles

./ReferenceTables

./results/\<YYYYMMDDHHMM\>

subdirectories of the results directory are created each time the application is started, these are given the name of the numeric datetime string at their creation. Note that on AWS these times will be UTC not local time.

./www

## Files for spatial relative abundance TFI an BBTFI calculations

### Fire History Shapefile

Output File shapefile from Stage 1. Shapefile of selected polygons defining boundary for Ad Hoc study area boundary, if required. This file should be placed in the directory ./rawFH

### R script files.

./global.r

./server.r

./ui.r

These three files are the constituent files required to run the shiny app -- the global file provides setup and loads the functions and required r packages. The ui provides the user interface for shiny and the server serves data and outputs to the UI and saves results to disk

### Reference / Lookup Tables

.\\ReferenceTables\\DraftTaxonListStatewidev2.csv

List of fauna HDM rasters (577) includes VBA species \#, threat status, taxonomic divisions

.\\ReferenceTables\\EFG_EVD_TFI.csv

Look up of TFI parameters for EFGs csv copy of Lookup in CGDL "EFG_EVD_TFI"

.\\ReferenceTables\\EFG_TSF_4GScorrectedAllEFGto400yrs.csv

Growth stage to TSF lookup

.\\ReferenceTables\\HDMSums225.csv

Total \# of thresholded cells of each HDM

.\\ReferenceTables\\OrdinalExpertLong.csv

Long table format of species responses based on expert opinion

### Raster files used in calculations

.\\InputGeneralRasters\\EFG_NUM_225.tif

.\\InputGeneralRasters\\EFG_NUM_75.tif

Rasters of EFG number for the state.

.\\InputGeneralRasters\\IndexVals225.tif

.\\InputGeneralRasters\\IndexVals75.tif

Rasters providing a sequential index number for each cell in the state.

.\\InputGeneralRasters\\LF_REGION_225.tif

.\\InputGeneralRasters\\LF_REGION_75.tif

Rasters providing numbered cells (1:6) for the six DELWP fire regions in the state.

### Thresholded Rasters of HDMs at 75m and 225m pixel size and associated R sparse arrays

./HDMS/225m/BinaryThresholded/\<Common_Name\>\_SppXXXXX_Thresholded_Binary.tif

./HDMS/225m/BinaryThresholded/\<Common_Name\>\_SppXXXXX_Thresholded_Binary.tif

There are two directories of HDM files, one for each resolution stored as subdirectories of ./HDMS. The file names in each directory are identical. File names follow the format \<Common_Name\>\_SppXXXXX_Thresholded_Binary.tif where \<Common_Name\> is the Common Name of the species and XXXXX is the TAXON_ID used in the Victoria Biodiversity Atlas(VBA) as of April2016, with \_ replacing spaces between names. There are currently 577 taxa covered by these files (Appendix 1). These rasters are summarised into the sparse matrices (below), they are not used directly in the module.

./HDMS/ HDMVals225.rdata

In addition to the rasters there are two R data files (one for each resolution) these each contain a single r object -- a sparse binary matrix of 577 columns each column represents the footprint of the 577 binary HDMs thecolumn name for each column is the VBA TAXON_ID for the species. the rows of these rasters are indexed to .\\InputGeneralRasters\\IndexVals225.tif and .\\InputGeneralRasters\\IndexVals75.tif. The R script to generate these sparse matrices is ./makeHDMVals.r. These sparse arrays provide faster loading and look-up of the HDM footprints and are used instead of the HDM rasters themselves in the module.

### Input settings

In addition to the input files there are a number of settings that must be, or can optionally be chosen before running the Spatial Relative Abundance , and TFI caclautions.

+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Setting name                                    | Purpose                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                | Values                                                                                                                                                                                                                            |
+=================================================+========================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================================+===================================================================================================================================================================================================================================+
| Fire scenario shapefile                         | The fire sequence (combination of fire history and future fire scenario) to be analysed.                                                                                                                                                                                                                                                                                                                                                                                                                                                                               | Shapefile produced in the preparatory ARCGIS tool and uploaded to module                                                                                                                                                          |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Region for analysis                             | Sets the boundary of the analysis. Analysis should be restricted to only the area of interest to minimise computation time. Usually this boundary should correspond to the clipping boundary used in the ARCGIS preparatory too to create the fire sequence for analysis, however the analysis will still run if these boundaries differ ( as long as they overlap each other. Areas outside the clipping of the Fire scenario will be set to NA. If the region chosen is within the Fire Scenario area clipped, the analysis will be restricted to the region chosen. | Whole of State (Default)                                                                                                                                                                                                          |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Ad Hoc polygon (user-provided shapefile in VG94 projection of the boundary of the region of interest) or                                                                                                                          |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | One of the DELWP Fire regions                                                                                                                                                                                                     |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | \"BARWON SOUTH WEST\"=1,                                                                                                                                                                                                          |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | \"GIPPSLAND\"=2 ,                                                                                                                                                                                                                 |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | \"GRAMPIANS\"=3,                                                                                                                                                                                                                  |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | \"HUME\"=4,                                                                                                                                                                                                                       |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | \"LODDON MALLEE\"=5,                                                                                                                                                                                                              |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | \"PORT PHILLIP\"=6,                                                                                                                                                                                                               |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Raster Resolution                               | Sets the resolution used for analysis, this is important in determining memory requirements and processing speed. Use of 75m raster increases processing and memory requirements \~10x                                                                                                                                                                                                                                                                                                                                                                                 | 225 m (default)                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | 75 m                                                                                                                                                                                                                              |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Public Land Only                                | The analysis can be carried out across both public and private land, however fire history is much less complete for private land.                                                                                                                                                                                                                                                                                                                                                                                                                                      | Yes(Default)                                                                                                                                                                                                                      |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | No                                                                                                                                                                                                                                |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Other and Unknown fire value                    | Fire history may contain fires of unknown type, you need to decide how to treat these in the analysis.                                                                                                                                                                                                                                                                                                                                                                                                                                                                 | Bushfire (Default)                                                                                                                                                                                                                |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 | They may be treated as either a bushfire or a burn, or alternatively areas with an unknow fire type may be treated as "NA" values. If the latter is chosen then TFI status, and relative abundance for the cell cannot be calculated based on that fire.                                                                                                                                                                                                                                                                                                               | Burn                                                                                                                                                                                                                              |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | NA                                                                                                                                                                                                                                |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| First season for analysis output                | Start the analysis at the first season which may be of interest, this reduces processing time, particularly in the Relative abundance calculations (that loop year by year). Calculations occur for each season from the first chosen to the maximum season value in the fire sequence.                                                                                                                                                                                                                                                                                | 1980 (default)                                                                                                                                                                                                                    |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Any season after the first season in the fire sequence file provided                                                                                                                                                              |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Spatial TFI and Relative abundance calculations |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Start and end of abundance baseline period      | Set the seasons to be used to calculate the baseline relative abundance used to calculate % change from baseline. It can be a single year or a range of years.                                                                                                                                                                                                                                                                                                                                                                                                         | 1980,1980(default)                                                                                                                                                                                                                |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Any single year, or range of years after 1979 contained in the fire sequence                                                                                                                                                      |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | For a single year chose the same value for start and end                                                                                                                                                                          |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Custom species list                             |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Default: Standard species list ( all species that have RA data avaible are calculated whether or not they occur in the region of interest).                                                                                       |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Alternative values: Uploaded manually edited draft species list produced using the "create draft species list utility in the app                                                                                                  |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Make relative abundance rasters                 | Whether to output individual Species x Season relative abundance rasters. These provide the spatial view of changes in abundance for each taxon through the fire sequence, however they increase the computation time.                                                                                                                                                                                                                                                                                                                                                 | No (default for more rapid computation                                                                                                                                                                                            |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Yes (if spatial output is desired).                                                                                                                                                                                               |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Note if yes is chosen the default is to do this for each species for each year from the first year for analysis- This can result in a very large number of files being created, and require increased download and storage space. |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Make TFI status/BBTFI rasters                   | Whether to output individual season TFI status rasters. Has slight increase in computation time. And data storage/ download requirements                                                                                                                                                                                                                                                                                                                                                                                                                               | No (default)                                                                                                                                                                                                                      |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |                                                                                                                                                                                                                                   |
|                                                 |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        | Yes                                                                                                                                                                                                                               |
+-------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

## Outputs

## Preparatory ARGIS tool -- separate process on windows PC.

Shapefile (four component files .shp,.dbf, .shx, .prj) in Vicgrid94 projection. Required as precursor to all subsequent spatial RA and TFI related calculations in the module.

## Outputs created by the module.

All outputs created by the module are saved in ./Results/YYMMDDHHMM/ directory or subdirectories thereof.

## Fire scenario analysis.

The initial fire scenario analysis replaces the previous corporate "FireHAT" processing. It creates a shapefile that contains on polygon for each unique spatial sequence of fire events. The attributes table (.dbf) of the shapefile contains the following fields The file ( actually 4 files .shp,.shx,.prj.and .dbf. Collectively these are referred to form heron as the "FH_anaylsis". An R data file is also saved this contains the same data, plus metadata about the analysis and a raster with the polygon ID valuses (to allow linking of the FHanalysis vector data to further analysis in a raster environment.

The file names and locations:

./FH_analysis\_\<name_of_input_rawFH file\>.shp

./FH_analysis\_\<name_of_input_rawFH file\>..Rdata

The polygon attributes ( in the shapefile dbf and the SimpleFeatures Dataframe sotred in .rdata file are:

+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+
| Field Name(s)                                                                                  | Description of values contained                                                                                                                                                                                                                               | Example/ or possible values      |
+================================================================================================+===============================================================================================================================================================================================================================================================+==================================+
| SEAS01 ... SEASxx                                                                              | The date of sequential fire seasons for fires in the area of the polygon, SEAS01 gives the date of the first( oldest recorded) fire at each location. SEASON02 the next fire for SEASxx, xx= greatest number of sequential fires occurring in the study area. | Four digit integer fire SEASON   |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | eg 1980 or 2055.                 |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 0= No fire                       |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | NA= No fire R Sf_DataFrame       |
+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+
| FireType01 ... FiretypeXX                                                                      | The Fire type corresponding to the SEAS01 ... SEASON xx value                                                                                                                                                                                                 | Single digit integer             |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 1=Burn                           |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 2=Bushfire                       |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 3=Other                          |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 4=Unknown                        |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 0=NULL                           |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | NA=NULL in R Sf_DataFrame        |
+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+
| INT01 ... INTyy where yy=xx-1                                                                  | The inter-fire interval between sequential fires at a location. INT01 is the interval ( in years) SEAS02-SEAS01                                                                                                                                               | Integer value \>=1               |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | 0= No interval                   |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                |                                                                                                                                                                                                                                                               | NA=No interval in R Sf_DataFrame |
+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+
| YSFXXXX ... (one field for each) year including and after the First season for analysis output | The number of years (fire seasons) since the last fire at the location prior to season date XXXX                                                                                                                                                              |                                  |
+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+
| ID                                                                                             | 1 based index unique id for each polygon                                                                                                                                                                                                                      | 1:number of polygons             |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                | Present in shapefile and R SFDF                                                                                                                                                                                                                               |                                  |
+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+
| FID                                                                                            | Zero based index unique ID for                                                                                                                                                                                                                                | 0:(number of polygons-1)         |
|                                                                                                |                                                                                                                                                                                                                                                               |                                  |
|                                                                                                | each feature in shapefile, not present in SFDF                                                                                                                                                                                                                |                                  |
+------------------------------------------------------------------------------------------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+----------------------------------+

An Rdata file named "FH_analysis\_"\<name_of_input_rawFH file\>.Rdata stored in the same directory contains two R obects, each of these is a list containing further objects

+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| R - Object  | Objects listed within it | Details                                                                                                                                                                                                           |
+=============+==========================+===================================================================================================================================================================================================================+
| FHanalysis  | TimeSpan                 | Time span of fire seasons contained in the input fire scenario Min(SEASON):max(SEASON)                                                                                                                            |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | YSFNames                 | Names of the YSF fields in the FHanalysis                                                                                                                                                                         |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | LBYNames                 | Names of the YSF fields in the FHanalysis                                                                                                                                                                         |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | LFTNames                 | Names of the YSF fields in the FHanalysis                                                                                                                                                                         |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | FireScenario             | The input fire scenario shapefile analysed \"                                                                                                                                                                     |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | RasterRes                | The raster resolution output from the anaysis(75 or 225)                                                                                                                                                          |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | ClipPolygonFile          | The polygon used to clip the analysis extent if one of the standard options is used then this will be \"LF_REGIONS.shp\", if an Ad hoc polygon was selected it will be the name of the ad hoc polygons shapefile. |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | Region_No                | Integer value corresponding to the Region selected for the clipping polygon (see Inputs: Region for analysis)                                                                                                     |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | PUBLIC_ONLY              | Whether the analysis was restricted to public land only ("Yes" or No")                                                                                                                                            |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | name                     | The name of the output FHanalysis . Rdata file                                                                                                                                                                    |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | FH_IDr                   | R raster object with the extent of the clip polygon. Cell values are the values of the FHanalysis polygon ID values (Note not the FID values from the shapefile)                                                  |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | OutDF                    | The R Simple Features Dataframe containing the results of the vector FHanalysis.                                                                                                                                  |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| CropRasters | Raster                   | R raster with extent equal to the Clippolygon, positive integer value for cells within the Clippolygon (value = FHanalysis\$ Region_No) NA for all other cells.                                                   |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | Extent                   | Extent object for Raster above                                                                                                                                                                                    |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | clipIDX                  | Index values for all cells within the clip polygon from                                                                                                                                                           |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | .\\InputGeneralRasters\\IndexVals225.tif or                                                                                                                                                                       |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | .\\InputGeneralRasters\\IndexVals75.tif                                                                                                                                                                           |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | Corresponding to RasterRes,                                                                                                                                                                                       |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | Used for fast extraction of HDM values etc from corresponding rasters and arrays                                                                                                                                  |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | IDX                      | Indeces of cells of                                                                                                                                                                                               |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | .\\InputGeneralRasters\\IndexVals225.tif or                                                                                                                                                                       |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | .\\InputGeneralRasters\\IndexVals75.tif                                                                                                                                                                           |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | Corresponding to RasterRes,                                                                                                                                                                                       |
|             |                          |                                                                                                                                                                                                                   |
|             |                          | For each cell of cropRasters\$Raster                                                                                                                                                                              |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | EFG                      | Cell wise EFG_NO values for cells in the rectangular extent of cropRasters\$Raster                                                                                                                                |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | RGN                      | Cell wise Region_No values for cells in the rectangular extent of cropRasters\$Raster                                                                                                                             |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             | HDM_RASTER_PATH          | The path to the HDM raster files corresponding to the RasterRes                                                                                                                                                   |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
|             |                          |                                                                                                                                                                                                                   |
+-------------+--------------------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

The firehat analysis file.

This shapefile contains polygons each with a unique fire history. The Polygon

## Files for aspatial GSO calculator cosen in iputs

The process for running the GSO calculator from R studio was documented previously. A revised version of this file describing the process for running GSO from the shiny app is appended (appendix 2)

### R and Rmarkdown files.

./GSO/GSOAnalysisCodeShiny.R

./GSO/GSOAnalysisOutput.Rmd

### Lookup and settings files (excel and CSV files)

./GSO/VBA_FAUNA.xlsx

Common names and codes for fauna (Ideally this file would be replaced with the similar .\\ReferenceTables\\DraftTaxonListStatewidev2.csv used in the spatial relative abundance part of the module, Reconciliation of fieldnames in the GSO will be required before this can occur).

./GSO/Reference data.xlsx

sheet=\'Ordinal expert data\'

Expert opinion data for Fauna relative abundance, each EFG simplified Growth stage has a column, each data point has a row species. (Ideally this file would be replaced with the similar .\\ReferenceTables\\DraftTaxonListStatewidev2.csv used in the spatial relative abundance part of the module, Reconciliation of fieldnames in the GSO will be required before this can occur).

sheet=\'GS lookup\'

data to calculate growth stage category given the EFG and TSF . (Ideally this file would be replaced with the similar .\\ReferenceTables\\ EFG_TSF_4GScorrectedAllEFGto400yrs.csv used in the spatial relative abundance part of the module. Reconciliation of fieldnames in the GSO will be required before this can occur).

./GSO/TBL_VegetationGrowthStages.xlsx

Source of lookup for EFG full names. (Ideally this file would be replaced with the similar .\\ .\\ReferenceTables\\EFG_EVD_TFI.csv.csv used in the spatial relative abundance part of the module, Reconciliation of fieldnames in the GSO will be required before this can occur).

./GSO/ExpertEstimate.xlsx

Expert opinion data as an amount of birds, used in recalibration of expert opinion data for use in conjunction with observation data.

## Appendix 1. List of HDM Raster Files.

Agile_Antechinus_Spp11028_Thresholded_Binary.tif

Alpine_Bog_Skink_Spp12992_Thresholded_Binary.tif

Alpine_She_oak_Skink_Spp12987_Thresholded_Binary.tif

Alpine_Tree_Frog_Spp63907_Thresholded_Binary.tif

Alpine_Water_Skink_Spp12550_Thresholded_Binary.tif

Apostlebird_Spp10675_Thresholded_Binary.tif

Australasian_Bittern_Spp10197_Thresholded_Binary.tif

Australasian_Grebe_Spp10061_Thresholded_Binary.tif

Australasian_Pipit_Spp10647_Thresholded_Binary.tif

Australasian_Shoveler_Spp10212_Thresholded_Binary.tif

Australian_Bustard_Spp10176_Thresholded_Binary.tif

Australian_Hobby_Spp10235_Thresholded_Binary.tif

Australian_King_Parrot_Spp10281_Thresholded_Binary.tif

Australian_Magpie_Spp10705_Thresholded_Binary.tif

Australian_Owlet_nightjar_Spp10317_Thresholded_Binary.tif

Australian_Painted_Snipe_Spp10170_Thresholded_Binary.tif

Australian_Pelican_Spp10106_Thresholded_Binary.tif

Australian_Pratincole_Spp10173_Thresholded_Binary.tif

Australian_Raven_Spp10930_Thresholded_Binary.tif

Australian_Shelduck_Spp10207_Thresholded_Binary.tif

Australian_Spotted_Crake_Spp10049_Thresholded_Binary.tif

Australian_White_Ibis_Spp10179_Thresholded_Binary.tif

Australian_Wood_Duck_Spp10202_Thresholded_Binary.tif

Azure_Kingfisher_Spp10319_Thresholded_Binary.tif

Baillons_Crake_Spp10050_Thresholded_Binary.tif

Banded_Lapwing_Spp10135_Thresholded_Binary.tif

Banded_Stilt_Spp10147_Thresholded_Binary.tif

Bandy_Bandy_Spp12734_Thresholded_Binary.tif

Bar_shouldered_Dove_Spp10032_Thresholded_Binary.tif

Bar_tailed_Godwit_Spp10153_Thresholded_Binary.tif

Bardick_Spp12667_Thresholded_Binary.tif

Barking_Marsh_Frog_Spp13059_Thresholded_Binary.tif

Barking_Owl_Spp10246_Thresholded_Binary.tif

Bassian_Thrush_Spp10779_Thresholded_Binary.tif

Baw_Baw_Frog_Spp13106_Thresholded_Binary.tif

Beaded_Gecko_Spp12109_Thresholded_Binary.tif

Beaked_Gecko_Spp12137_Thresholded_Binary.tif

Bearded_Dragon_Spp12177_Thresholded_Binary.tif

Beautiful_Firetail_Spp10650_Thresholded_Binary.tif

Bell_Miner_Spp10633_Thresholded_Binary.tif

Black_Bittern_Spp60196_Thresholded_Binary.tif

Black_chinned_Honeyeater_Spp10580_Thresholded_Binary.tif

Black_eared_Cuckoo_Spp10341_Thresholded_Binary.tif

Black_eared_Miner_Spp10967_Thresholded_Binary.tif

Black_faced_Cormorant_Spp10098_Thresholded_Binary.tif

Black_faced_Cuckoo_shrike_Spp10424_Thresholded_Binary.tif

Black_faced_Monarch_Spp10373_Thresholded_Binary.tif

Black_faced_Woodswallow_Spp10546_Thresholded_Binary.tif

Black_Falcon_Spp10238_Thresholded_Binary.tif

Black_fronted_Dotterel_Spp10144_Thresholded_Binary.tif

Black_Honeyeater_Spp10589_Thresholded_Binary.tif

Black_Kite_Spp10229_Thresholded_Binary.tif

Black_Rock_Skink_Spp62938_Thresholded_Binary.tif

Black_shouldered_Kite_Spp10232_Thresholded_Binary.tif

Black_Swan_Spp10203_Thresholded_Binary.tif

Black_tailed_Godwit_Spp528553_Thresholded_Binary.tif

Black_tailed_Native_hen_Spp10055_Thresholded_Binary.tif

Black_Wallaby_Spp11242_Thresholded_Binary.tif

Black_winged_Stilt_Spp528555_Thresholded_Binary.tif

Blotched_Blue_tongued_Lizard_Spp12578_Thresholded_Binary.tif

Blue_billed_Duck_Spp10216_Thresholded_Binary.tif

Blue_Bonnet_Spp10297_Thresholded_Binary.tif

Blue_faced_Honeyeater_Spp10641_Thresholded_Binary.tif

Blue_Mountains_Tree_Frog_Spp13175_Thresholded_Binary.tif

Blue_winged_Parrot_Spp10306_Thresholded_Binary.tif

Booroolong_Tree_Frog_Spp13168_Thresholded_Binary.tif

Bougainvilles_Skink_Spp12475_Thresholded_Binary.tif

Boulengers_Skink_Spp12526_Thresholded_Binary.tif

Broad_toothed_Rat_Spp11438_Thresholded_Binary.tif

Brolga_Spp10177_Thresholded_Binary.tif

Brookss_Striped_Skink_Spp62933_Thresholded_Binary.tif

Brown_Cuckoo_Dove_Spp10029_Thresholded_Binary.tif

Brown_Falcon_Spp10239_Thresholded_Binary.tif

Brown_Gerygone_Spp10454_Thresholded_Binary.tif

Brown_Goshawk_Spp10221_Thresholded_Binary.tif

Brown_headed_Honeyeater_Spp10583_Thresholded_Binary.tif

Brown_Quail_Spp10010_Thresholded_Binary.tif

Brown_Songlark_Spp10508_Thresholded_Binary.tif

Brown_Thornbill_Spp10475_Thresholded_Binary.tif

Brown_Toadlet_Spp13117_Thresholded_Binary.tif

Brown_Treecreeper\_(south_eastern_ssp)\_Spp60555_Thresholded_Binary.tif

Brush_Bronzewing_Spp10035_Thresholded_Binary.tif

Brush_Cuckoo_Spp10339_Thresholded_Binary.tif

Brush_tailed_Phascogale_Spp11017_Thresholded_Binary.tif

Brush_tailed_Rock_wallaby_Spp11215_new_Thresholded_Binary.tif

Budgerigar_Spp10310_Thresholded_Binary.tif

Buff_banded_Rail_Spp10046_Thresholded_Binary.tif

Buff_rumped_Thornbill_Spp10484_Thresholded_Binary.tif

Burtons_Snake_Lizard_Spp12170_Thresholded_Binary.tif

Bush_Rat_Spp11395_Thresholded_Binary.tif

Bush_Stone_curlew_Spp10174_Thresholded_Binary.tif

Butlers_Legless_Lizard_Spp12167_Thresholded_Binary.tif

Bynoes_Gecko_Spp12105_Thresholded_Binary.tif

Cape_Barren_Goose_Spp10198_Thresholded_Binary.tif

Carnabys_Wall_Skink_Spp12326_Thresholded_Binary.tif

Carpet_Python_Spp62969_Thresholded_Binary.tif

Cattle_Egret_Spp10977_Thresholded_Binary.tif

Central_Bearded_Dragon_Spp12204_Thresholded_Binary.tif

Channel_billed_Cuckoo_Spp10348_Thresholded_Binary.tif

Chestnut_crowned_Babbler_Spp10446_Thresholded_Binary.tif

Chestnut_Quail_thrush_Spp10437_Thresholded_Binary.tif

Chestnut_rumped_Heathwren_Spp10498_Thresholded_Binary.tif

Chestnut_rumped_Thornbill_Spp10481_Thresholded_Binary.tif

Chestnut_Teal_Spp10210_Thresholded_Binary.tif

Chocolate_Wattled_Bat_Spp11351_Thresholded_Binary.tif

Clamorous_Reed_Warbler_Spp10524_Thresholded_Binary.tif

Cockatiel_Spp10274_Thresholded_Binary.tif

Collared_Sparrowhawk_Spp10222_Thresholded_Binary.tif

Common_Bent_wing_Bat\_(eastern_ssp)\_Spp61342_Thresholded_Binary.tif

Common_Bent_wing_Bat\_(sth_ssp)\_Spp61343_Thresholded_Binary.tif

Common_Blue_tongued_Lizard_Spp12580_Thresholded_Binary.tif

Common_Bronzewing_Spp10034_Thresholded_Binary.tif

Common_Brushtail_Possum_Spp11113_Thresholded_Binary.tif

Common_Cicadabird_Spp10429_Thresholded_Binary.tif

Common_Death_Adder_Spp12640_Thresholded_Binary.tif

Common_Dunnart_Spp11061_new_Thresholded_Binary.tif

Common_Froglet_Spp13134_Thresholded_Binary.tif

Common_Greenshank_Spp10158_Thresholded_Binary.tif

Common_Ringtail_Possum_Spp11129_Thresholded_Binary.tif

Common_Sandpiper_Spp10157_Thresholded_Binary.tif

Common_Scaly_foot_Spp12174_Thresholded_Binary.tif

Common_Spadefoot_Toad_Spp13086_Thresholded_Binary.tif

Common_Wombat_Spp11165_Thresholded_Binary.tif

Copper_tailed_Skink_Spp12386_Thresholded_Binary.tif

Corangamite_Water_Skink_Spp62958_Thresholded_Binary.tif

Coventrys_Skink_Spp12458_Thresholded_Binary.tif

Crescent_Honeyeater_Spp10630_Thresholded_Binary.tif

Crested_Bellbird_Spp10419_Thresholded_Binary.tif

Crested_Pigeon_Spp10043_Thresholded_Binary.tif

Crested_Shrike_tit_Spp10416_Thresholded_Binary.tif

Crested_Tern_Spp10115_Thresholded_Binary.tif

Crimson_Chat_Spp10449_Thresholded_Binary.tif

Crimson_Rosella_Spp10282_Thresholded_Binary.tif

Cunninghams_Skink_Spp12408_Thresholded_Binary.tif

Curl_Snake_Spp12722_Thresholded_Binary.tif

Curlew_Sandpiper_Spp10161_Thresholded_Binary.tif

Darter_Spp10101_Thresholded_Binary.tif

Delicate_Skink_Spp12450_Thresholded_Binary.tif

Dendys_Toadlet_Spp13120_Thresholded_Binary.tif

Desert_Skink_Spp12413_Thresholded_Binary.tif

Diamond_Dove_Spp10031_Thresholded_Binary.tif

Diamond_Firetail_Spp10652_Thresholded_Binary.tif

Diamond_Python_Spp62968_Thresholded_Binary.tif

Dollarbird_Spp10318_Thresholded_Binary.tif

Double_banded_Plover_Spp10140_Thresholded_Binary.tif

Double_barred_Finch_Spp10655_Thresholded_Binary.tif

Dusky_Antechinus_Spp11033_Thresholded_Binary.tif

Dusky_Moorhen_Spp10056_Thresholded_Binary.tif

Dusky_Woodswallow_Spp10547_Thresholded_Binary.tif

Dwyers_Snake_Spp12726_Thresholded_Binary.tif

Eastern_Bristlebird_Spp10519_Thresholded_Binary.tif

Eastern_Broad_nosed_Bat_Spp11811_Thresholded_Binary.tif

Eastern_Brown_Snake_Spp12699_Thresholded_Binary.tif

Eastern_Curlew_Spp10149_Thresholded_Binary.tif

Eastern_Dwarf_Tree_Frog_Spp13183_Thresholded_Binary.tif

Eastern_False_Pipistrelle_Spp11372_Thresholded_Binary.tif

Eastern_Freetail_Bat_Spp11839_Thresholded_Binary.tif

Eastern_Great_Egret_Spp10187_Thresholded_Binary.tif

Eastern_Grey_Kangaroo_Spp11265_Thresholded_Binary.tif

Eastern_Horseshoe_Bat_Spp11303_Thresholded_Binary.tif

Eastern_Koel_Spp10347_Thresholded_Binary.tif

Eastern_Pygmy_possum_Spp11150_Thresholded_Binary.tif

Eastern_Reef_Egret_Spp10191_Thresholded_Binary.tif

Eastern_Rosella_Spp10288_Thresholded_Binary.tif

Eastern_She_oak_Skink_Spp12988_Thresholded_Binary.tif

Eastern_Small_eyed_Snake_Spp12650_Thresholded_Binary.tif

Eastern_Spinebill_Spp10591_Thresholded_Binary.tif

Eastern_Striped_Skink_Spp12936_Thresholded_Binary.tif

Eastern_Three_lined_Skink_Spp12682_Thresholded_Binary.tif

Eastern_Wallaroo_Spp61266_Thresholded_Binary.tif

Eastern_Water_Skink_Spp12557_Thresholded_Binary.tif

Eastern_Whipbird_Spp10421_Thresholded_Binary.tif

Eastern_Yellow_Robin_Spp10392_Thresholded_Binary.tif

Egernia_PLAIN_BACK_MORPH_Spp62942_Thresholded_Binary.tif

Egernia_SPOTTED_BACK_MORPH_Spp62941_Thresholded_Binary.tif

Elegant_Parrot_Spp10307_Thresholded_Binary.tif

Emu_Spp10001_Thresholded_Binary.tif

Eurasian_Coot_Spp10059_Thresholded_Binary.tif

Fairy_Martin_Spp10360_Thresholded_Binary.tif

Fan_tailed_Cuckoo_Spp10338_Thresholded_Binary.tif

Fat_tailed_Dunnart_Spp11072_Thresholded_Binary.tif

Feathertail_Glider_Spp11147_Thresholded_Binary.tif

Flame_Robin_Spp10382_Thresholded_Binary.tif

Forest_Raven_Spp10868_Thresholded_Binary.tif

Fork_tailed_Swift_Spp10335_Thresholded_Binary.tif

Four_toed_Skink_Spp12446_Thresholded_Binary.tif

Freckled_Duck_Spp10214_Thresholded_Binary.tif

Fuscous_Honeyeater_Spp10613_Thresholded_Binary.tif

Galah_Spp10273_Thresholded_Binary.tif

Gang_gang_Cockatoo_Spp10268_Thresholded_Binary.tif

Garden_Skink_Spp12451_Thresholded_Binary.tif

Gelochelidon_nilotica_macrotarsa_Spp10111_Thresholded_Binary.tif

Giant_Bullfrog_Spp13060_Thresholded_Binary.tif

Giant_Burrowing_Frog_Spp13042_Thresholded_Binary.tif

Gilberts_Whistler_Spp10403_Thresholded_Binary.tif

Giles_Planigale_Spp11050_Thresholded_Binary.tif

Gippsland_Water_Dragon_Spp62919_Thresholded_Binary.tif

Glossy_Black_Cockatoo_Spp10265_Thresholded_Binary.tif

Glossy_Grass_Skink_Spp12683_Thresholded_Binary.tif

Glossy_Ibis_Spp10178_Thresholded_Binary.tif

Golden_headed_Cisticola_Spp10525_Thresholded_Binary.tif

Golden_Whistler_Spp10398_Thresholded_Binary.tif

Goulds_Long_eared_Bat_Spp11334_Thresholded_Binary.tif

Goulds_Wattled_Bat_Spp11349_Thresholded_Binary.tif

Grassland_Earless_Dragon_Spp12922_Thresholded_Binary.tif

Grays_Blind_Snake_Spp12599_Thresholded_Binary.tif

Great_Cormorant_Spp10096_Thresholded_Binary.tif

Great_Crested_Grebe_Spp10060_Thresholded_Binary.tif

Great_Knot_Spp10165_Thresholded_Binary.tif

Greater_Glider_Spp11133_Thresholded_Binary.tif

Greater_Long_eared_Bat_Spp61332_Thresholded_Binary.tif

Greater_Sand_Plover_Spp10141_Thresholded_Binary.tif

Green_and_Golden_Bell_Frog_Spp13166_Thresholded_Binary.tif

Green_Stream_Frog_Spp19002_Thresholded_Binary.tif

Grey_Butcherbird_Spp10702_Thresholded_Binary.tif

Grey_crowned_Babbler_Spp10443_Thresholded_Binary.tif

Grey_Currawong_Spp10697_Thresholded_Binary.tif

Grey_Falcon_Spp10236_Thresholded_Binary.tif

Grey_Fantail_Spp10361_Thresholded_Binary.tif

Grey_fronted_Honeyeater_Spp10623_new_Thresholded_Binary.tif

Grey_Goshawk_Spp10220_Thresholded_Binary.tif

Grey_headed_Flying_fox_Spp11280_Thresholded_Binary.tif

Grey_Plover_Spp10136_Thresholded_Binary.tif

Grey_Shrike_thrush_Spp10408_Thresholded_Binary.tif

Grey_tailed_Tattler_Spp10155_Thresholded_Binary.tif

Grey_Teal_Spp10211_Thresholded_Binary.tif

Greys_Skink_Spp12519_Thresholded_Binary.tif

Ground_Cuckoo_shrike_Spp10423_Thresholded_Binary.tif

Ground_Parrot_Spp10311_Thresholded_Binary.tif

Growling_Grass_Frog_Spp13207_Thresholded_Binary.tif

Gymnobelideus_leadbeateri_Spp11141_Thresholded_Binary.tif

Hardhead_Spp10215_Thresholded_Binary.tif

Haswells_Froglet_Spp13103_Thresholded_Binary.tif

Heath_Mouse_Spp11468_Thresholded_Binary.tif

Highland_Copperhead_Spp12972_Thresholded_Binary.tif

Hoary_headed_Grebe_Spp10062_Thresholded_Binary.tif

Hooded_Plover_Spp10138_Thresholded_Binary.tif

Hooded_Robin_Spp10385_Thresholded_Binary.tif

Hooded_Scaly_foot_Spp12176_Thresholded_Binary.tif

Horsfields_Bronze_Cuckoo_Spp10342_Thresholded_Binary.tif

Horsfields_Bushlark_Spp10648_Thresholded_Binary.tif

Inland_Broad_nosed_Bat_Spp11364_Thresholded_Binary.tif

Inland_Dotterel_Spp10145_Thresholded_Binary.tif

Inland_Forest_Bat_Spp11819_Thresholded_Binary.tif

Inland_Freetail_Bat_Spp11809_Thresholded_Binary.tif

Inland_Thornbill_Spp10476_Thresholded_Binary.tif

Intermediate_Egret_Spp10186_Thresholded_Binary.tif

Jacky_Winter_Spp10377_Thresholded_Binary.tif

Kefersteins_Tree_Frog_Spp528551_Thresholded_Binary.tif

King_Quail_Spp10012_new_Thresholded_Binary.tif

Koala_Spp11162_Thresholded_Binary.tif

Lace_Goanna_Spp12283_Thresholded_Binary.tif

Large_billed_Scrubwren_Spp10494_Thresholded_Binary.tif

Large_Brown_Tree_Frog_Spp13936_Thresholded_Binary.tif

Large_Forest_Bat_Spp11381_Thresholded_Binary.tif

Large_Striped_Skink_Spp12375_Thresholded_Binary.tif

Lathams_Snipe_Spp10168_Thresholded_Binary.tif

Laughing_Kookaburra_Spp10322_Thresholded_Binary.tif

Leaden_Flycatcher_Spp10365_Thresholded_Binary.tif

Lerista_timida_Spp12492_Thresholded_Binary.tif

Lesser_Long_eared_Bat_Spp11335_Thresholded_Binary.tif

Lesser_Sand_Plover_Spp10139_Thresholded_Binary.tif

Lesueurs_Frog_Spp13192_Thresholded_Binary.tif

Lewins_Honeyeater_Spp10605_Thresholded_Binary.tif

Lewins_Rail_Spp10045_Thresholded_Binary.tif

Lined_Earless_Dragon_Spp62921_Thresholded_Binary.tif

Liopholis_guthega_Spp12432_Thresholded_Binary.tif

Little_Bittern_Spp10195_Thresholded_Binary.tif

Little_Black_Cormorant_Spp10097_Thresholded_Binary.tif

Little_Broad_nosed_Bat_Spp11362_Thresholded_Binary.tif

Little_Button_quail_Spp10018_Thresholded_Binary.tif

Little_Corella_Spp10271_Thresholded_Binary.tif

Little_Crow_Spp10691_Thresholded_Binary.tif

Little_Eagle_Spp10225_Thresholded_Binary.tif

Little_Egret_Spp10185_Thresholded_Binary.tif

Little_Forest_Bat_Spp11379_Thresholded_Binary.tif

Little_Friarbird_Spp10646_Thresholded_Binary.tif

Little_Grassbird_Spp10522_Thresholded_Binary.tif

Little_Lorikeet_Spp10260_Thresholded_Binary.tif

Little_Pied_Cormorant_Spp10100_Thresholded_Binary.tif

Little_Pygmy_possum_Spp11152_Thresholded_Binary.tif

Little_Raven_Spp10954_Thresholded_Binary.tif

Little_Wattlebird_Spp10637_Thresholded_Binary.tif

Little_Whip_Snake_Spp12727_Thresholded_Binary.tif

Long_billed_Corella_Spp10272_Thresholded_Binary.tif

Long_footed_Potoroo_Spp11179_Thresholded_Binary.tif

Long_nosed_Bandicoot_Spp11097_Thresholded_Binary.tif

Long_nosed_Potoroo_Spp11175_Thresholded_Binary.tif

Long_toed_Stint_Spp10965_Thresholded_Binary.tif

Lowland_Copperhead_Spp12973_Thresholded_Binary.tif

Magpie_Goose_Spp10199_Thresholded_Binary.tif

Magpie_lark_Spp10415_Thresholded_Binary.tif

Major_Mitchells_Cockatoo_Spp10270_Thresholded_Binary.tif

Mallee_Dragon_Spp12185_Thresholded_Binary.tif

Mallee_Emu_wren_Spp10527_Thresholded_Binary.tif

Mallee_Ningaui_Spp11055_Thresholded_Binary.tif

Mallee_Ringneck_Spp60291_Thresholded_Binary.tif

Mallee_Spadefoot_Toad_Spp13085_Thresholded_Binary.tif

Mallee_Worm_Lizard_Spp12141_Thresholded_Binary.tif

Malleefowl_Spp10007_Thresholded_Binary.tif

Marbled_Gecko_Spp12126_Thresholded_Binary.tif

Marsh_Sandpiper_Spp10159_Thresholded_Binary.tif

Martins_Toadlet_Spp13930_Thresholded_Binary.tif

Masked_Lapwing_Spp10133_Thresholded_Binary.tif

Masked_Owl_Spp10250_Thresholded_Binary.tif

Masked_Woodswallow_Spp10544_Thresholded_Binary.tif

Masters_Snake_Spp12666_Thresholded_Binary.tif

McCoys_Skink_Spp12444_Thresholded_Binary.tif

Metallic_Skink_Spp12462_Thresholded_Binary.tif

Millewa_Skink_Spp12445_Thresholded_Binary.tif

Mistletoebird_Spp10564_Thresholded_Binary.tif

Mitchells_Hopping_mouse_Spp11480_Thresholded_Binary.tif

Mitchells_Short_tailed_Snake_Spp12724_Thresholded_Binary.tif

Mountain_Brushtail_Possum_Spp11115_Thresholded_Binary.tif

Mountain_Dragon_Anglesea_form_Spp63940_Thresholded_Binary.tif

Mountain_Dragon_Grampians_form_Spp63941_Thresholded_Binary.tif

Mountain_Dragon_Spp12182_Thresholded_Binary.tif

Mountain_Pygmy_possum_Spp11156_Thresholded_Binary.tif

Mountain_Skink_Spp12433_Thresholded_Binary.tif

Mulga_Parrot_Spp10296_Thresholded_Binary.tif

Murray_Striped_Skink_Spp12342_Thresholded_Binary.tif

Musk_Duck_Spp10217_Thresholded_Binary.tif

Musk_Lorikeet_Spp10258_Thresholded_Binary.tif

Nankeen_Kestrel_Spp10240_Thresholded_Binary.tif

Nankeen_Night_Heron_Spp10192_Thresholded_Binary.tif

New_Holland_Honeyeater_Spp10631_Thresholded_Binary.tif

New_Holland_Mouse_Spp11455_new_Thresholded_Binary.tif

Nobbi_Dragon_Spp19000_Thresholded_Binary.tif

Nobbi_Dragon_subsp_coggeri_Spp62917_Thresholded_Binary.tif

Nobbi_Dragon_subsp_nobbi_Spp19009_Thresholded_Binary.tif

Noisy_Friarbird_Spp10645_Thresholded_Binary.tif

Noisy_Miner_Spp10634_Thresholded_Binary.tif

Norriss_Dragon_Spp12209_Thresholded_Binary.tif

Obscure_Skink_Spp12529_Thresholded_Binary.tif

Olive_backed_Oriole_Spp10671_Thresholded_Binary.tif

Olive_Legless_Lizard_Spp12160_Thresholded_Binary.tif

Olive_Whistler_Spp10405_Thresholded_Binary.tif

Orange_bellied_Parrot_Spp10305_new_Thresholded_Binary.tif

Orange_Chat_Spp10450_Thresholded_Binary.tif

Pacific_Barn_Owl_Spp10249_Thresholded_Binary.tif

Pacific_Black_Duck_Spp10208_Thresholded_Binary.tif

Pacific_Golden_Plover_Spp10137_Thresholded_Binary.tif

Pacific_Gull_Spp60126_Thresholded_Binary.tif

Painted_Button_quail_Spp10014_Thresholded_Binary.tif

Painted_Dragon_Spp12199_Thresholded_Binary.tif

Painted_Honeyeater_Spp10598_Thresholded_Binary.tif

Pallid_Cuckoo_Spp10337_Thresholded_Binary.tif

Peaceful_Dove_Spp10030_Thresholded_Binary.tif

Pectoral_Sandpiper_Spp10978_Thresholded_Binary.tif

Peregrine_Falcon_Spp10237_Thresholded_Binary.tif

Perons_Tree_Frog_Spp13204_Thresholded_Binary.tif

Peterss_Blind_Snake_Spp12588_Thresholded_Binary.tif

Pied_Butcherbird_Spp10700_Thresholded_Binary.tif

Pied_Cormorant_Spp10099_Thresholded_Binary.tif

Pied_Currawong_Spp10694_Thresholded_Binary.tif

Pied_Oystercatcher_Spp10130_Thresholded_Binary.tif

Pilotbird_Spp10506_Thresholded_Binary.tif

Pink_eared_Duck_Spp10213_Thresholded_Binary.tif

Pink_nosed_Worm_Lizard_Spp12143_Thresholded_Binary.tif

Pink_Robin_Spp10383_Thresholded_Binary.tif

Pink_tailed_Worm_Lizard_Spp12144_Thresholded_Binary.tif

Plains_Brown_Tree_Frog_Spp13203_Thresholded_Binary.tif

Plains_Froglet_Spp13131_Thresholded_Binary.tif

Plains_wanderer_Spp10020_Thresholded_Binary.tif

Platypus_Spp5136_Thresholded_Binary.tif

Plumed_Whistling_Duck_Spp10205_Thresholded_Binary.tif

Pobblebonk_Frog_subsp_dumerilii_Spp63913_Thresholded_Binary.tif

Pobblebonk_Frog_subsp_insularis_Spp63914_Thresholded_Binary.tif

Pobblebonk_Frog_subsp_variegatus_Spp63915_Thresholded_Binary.tif

Port_Lincoln_Snake_Spp12813_Thresholded_Binary.tif

Powerful_Owl_Spp10248_Thresholded_Binary.tif

Purple_crowned_Lorikeet_Spp10259_Thresholded_Binary.tif

Purple_gaped_Honeyeater_Spp10620_Thresholded_Binary.tif

Purple_Swamphen_Spp10058_Thresholded_Binary.tif

Rainbow_Bee_eater_Spp10329_Thresholded_Binary.tif

Rainbow_Lorikeet_Spp10254_Thresholded_Binary.tif

Red_backed_Kingfisher_Spp10325_Thresholded_Binary.tif

Red_bellied_Black_Snake_Spp12693_Thresholded_Binary.tif

Red_browed_Finch_Spp10662_Thresholded_Binary.tif

Red_browed_Treecreeper_Spp10560_Thresholded_Binary.tif

Red_capped_Plover_Spp10143_Thresholded_Binary.tif

Red_capped_Robin_Spp10381_Thresholded_Binary.tif

Red_chested_Button_quail_Spp10019_Thresholded_Binary.tif

Red_Kangaroo_Spp11275_Thresholded_Binary.tif

Red_kneed_Dotterel_Spp10132_Thresholded_Binary.tif

Red_Knot_Spp10164_Thresholded_Binary.tif

Red_lored_Whistler_Spp10402_Thresholded_Binary.tif

Red_naped_Snake_Spp12669_Thresholded_Binary.tif

Red_necked_Avocet_Spp10148_Thresholded_Binary.tif

Red_necked_Stint_Spp10162_Thresholded_Binary.tif

Red_necked_Wallaby_Spp11261_Thresholded_Binary.tif

Red_rumped_Parrot_Spp10295_Thresholded_Binary.tif

Red_tailed_Black_Cockatoo_Spp10264_Thresholded_Binary.tif

Red_throated_Skink_Spp12464_Thresholded_Binary.tif

Red_Wattlebird_Spp10638_Thresholded_Binary.tif

Redthroat_Spp10497_Thresholded_Binary.tif

Regal_Striped_Skink_Spp12374_Thresholded_Binary.tif

Regent_Honeyeater_Spp10603_Thresholded_Binary.tif

Regent_Parrot_Spp10278_Thresholded_Binary.tif

Restless_Flycatcher_Spp10369_Thresholded_Binary.tif

Rose_Robin_Spp10384_Thresholded_Binary.tif

Rosenbergs_Goanna_Spp12287_Thresholded_Binary.tif

Royal_Spoonbill_Spp10181_Thresholded_Binary.tif

Ruddy_Turnstone_Spp10129_Thresholded_Binary.tif

Rufous_Bristlebird\_(coorong_subsp)\_Spp19010_Thresholded_Binary.tif

Rufous_Bristlebird\_(Otway)\_Spp19011_Thresholded_Binary.tif

Rufous_Bristlebird_Spp10521_new_Thresholded_Binary.tif

Rufous_Fantail_Spp10362_Thresholded_Binary.tif

Rufous_Fieldwren_Spp10502_Thresholded_Binary.tif

Rufous_Songlark_Spp10509_Thresholded_Binary.tif

Rufous_Whistler_Spp10401_Thresholded_Binary.tif

Rugose_Toadlet_Spp13151_Thresholded_Binary.tif

Sacred_Kingfisher_Spp10326_Thresholded_Binary.tif

Saltbush_Striped_Skink_Spp19008_Thresholded_Binary.tif

Samphire_Skink_Spp12525_Thresholded_Binary.tif

Sand_Goanna_Spp12271_Thresholded_Binary.tif

Sanderling_Spp10166_Thresholded_Binary.tif

Satin_Bowerbird_Spp10679_Thresholded_Binary.tif

Satin_Flycatcher_Spp10366_Thresholded_Binary.tif

Scaly_breasted_Lorikeet_Spp10256_Thresholded_Binary.tif

Scarlet_chested_Parrot_Spp10303_Thresholded_Binary.tif

Scarlet_Honeyeater_Spp10586_Thresholded_Binary.tif

Scarlet_Robin_Spp10380_Thresholded_Binary.tif

Sharp_tailed_Sandpiper_Spp10163_Thresholded_Binary.tif

Shining_Bronze_Cuckoo_Spp10344_Thresholded_Binary.tif

Short_beaked_Echidna_Spp11003_Thresholded_Binary.tif

Shy_Heathwren_Spp10499_Thresholded_Binary.tif

Silky_Mouse_Spp11457_Thresholded_Binary.tif

Silver_Gull_Spp10125_Thresholded_Binary.tif

Silvereye_Spp10574_Thresholded_Binary.tif

Singing_Honeyeater_Spp10608_Thresholded_Binary.tif

Slender_billed_Thornbill_Spp10482_Thresholded_Binary.tif

Sloanes_Froglet_Spp13135_Thresholded_Binary.tif

Smoky_Mouse_Spp11458_Thresholded_Binary.tif

Smooth_Toadlet_Spp13158_Thresholded_Binary.tif

Sooty_Owl_Spp10253_Thresholded_Binary.tif

Sooty_Oystercatcher_Spp10131_Thresholded_Binary.tif

Southern_Barred_Frog_Spp13073_Thresholded_Binary.tif

Southern_Boobook_Spp10242_Thresholded_Binary.tif

Southern_Brown_Bandicoot_Spp61092_Thresholded_Binary.tif

Southern_Brown_Tree_Frog_SOUTHERN_Spp63903_Thresholded_Binary.tif

Southern_Brown_Tree_Frog_Spp13182_Thresholded_Binary.tif

Southern_Bullfrog\_(ssp_unknown)\_Spp13058_Thresholded_Binary.tif

Southern_Emu_wren_Spp10526_Thresholded_Binary.tif

Southern_Forest_Bat_Spp11378_Thresholded_Binary.tif

Southern_Freetail_Bat_Spp11808_Thresholded_Binary.tif

Southern_Grass_Skink_Spp12994_Thresholded_Binary.tif

Southern_Legless_Lizard_Spp12154_Thresholded_Binary.tif

Southern_Myotis_Spp11357_Thresholded_Binary.tif

Southern_Rainbow_Skink_Spp12318_Thresholded_Binary.tif

Southern_Scrub_robin_Spp10441_Thresholded_Binary.tif

Southern_Smooth_Froglet_Spp13029_Thresholded_Binary.tif

Southern_Spiny_tailed_Gecko_Spp12059_Thresholded_Binary.tif

Southern_Toadlet_Spp13125_Thresholded_Binary.tif

Southern_Water_Skink_Spp62956_Thresholded_Binary.tif

Southern_Whiteface_Spp10466_Thresholded_Binary.tif

Spangled_Drongo_Spp10673_Thresholded_Binary.tif

Speckled_Warbler_Spp10504_Thresholded_Binary.tif

Spencers_Skink_Spp12541_Thresholded_Binary.tif

Spiny_cheeked_Honeyeater_Spp10640_Thresholded_Binary.tif

Splendid_Fairy_wren_Spp10532_Thresholded_Binary.tif

Spot_tailed_Quoll_Spp11008_Thresholded_Binary.tif

Spotless_Crake_Spp10051_Thresholded_Binary.tif

Spotted_Bowerbird_Spp10680_Thresholded_Binary.tif

Spotted_Burrowing_Skink_Spp12499_Thresholded_Binary.tif

Spotted_Harrier_Spp10218_Thresholded_Binary.tif

Spotted_Marsh_Frog\_(race_unknown)\_Spp13063_Thresholded_Binary.tif

Spotted_Marsh_Frog_NCR_Spp63917_Thresholded_Binary.tif

Spotted_Marsh_Frog_SCR_Spp63918_Thresholded_Binary.tif

Spotted_Nightjar_Spp10331_Thresholded_Binary.tif

Spotted_Pardalote_Spp10565_Thresholded_Binary.tif

Spotted_Quail_thrush_Spp10436_Thresholded_Binary.tif

Spotted_Tree_Frog_Spp13195_Thresholded_Binary.tif

Square_tailed_Kite_Spp10230_Thresholded_Binary.tif

Squirrel_Glider_Spp11137_Thresholded_Binary.tif

Sternula_albifrons_sinensis_Spp10117_Thresholded_Binary.tif

Straw_necked_Ibis_Spp10180_Thresholded_Binary.tif

Striated_Fieldwren_Spp10500_Thresholded_Binary.tif

Striated_Grasswren_Spp10513_Thresholded_Binary.tif

Striated_Heron_Spp10193_Thresholded_Binary.tif

Striated_Pardalote_Spp10976_Thresholded_Binary.tif

Striated_Thornbill_Spp10470_Thresholded_Binary.tif

Striped_Honeyeater_Spp10585_Thresholded_Binary.tif

Striped_Legless_Lizard_Spp12159_Thresholded_Binary.tif

Striped_Marsh_Frog_Spp13061_Thresholded_Binary.tif

Striped_Worm_Lizard_Spp12150_Thresholded_Binary.tif

Stubble_Quail_Spp10009_Thresholded_Binary.tif

Stumpy_tailed_Lizard_Spp12583_Thresholded_Binary.tif

Sugar_Glider_Spp11138_Thresholded_Binary.tif

Sulphur_crested_Cockatoo_Spp10269_Thresholded_Binary.tif

Superb_Fairy_wren_Spp10529_Thresholded_Binary.tif

Superb_Lyrebird_Spp10350_Thresholded_Binary.tif

Superb_Parrot_Spp10277_Thresholded_Binary.tif

Swamp_Antechinus_Spp11034_Thresholded_Binary.tif

Swamp_Harrier_Spp10219_Thresholded_Binary.tif

Swamp_Rat_Spp11398_Thresholded_Binary.tif

Swamp_Skink_Spp12407_Thresholded_Binary.tif

Swift_Parrot_Spp10309_Thresholded_Binary.tif

Tawny_crowned_Honeyeater_Spp10593_Thresholded_Binary.tif

Tawny_Frogmouth_Spp10313_Thresholded_Binary.tif

Terek_Sandpiper_Spp10160_Thresholded_Binary.tif

Tessellated_Gecko_Spp12076_Thresholded_Binary.tif

Thick_tailed_Gecko_Spp12138_Thresholded_Binary.tif

Three_toed_Skink_Spp12441_Thresholded_Binary.tif

Tiger_Snake_Spp12681_Thresholded_Binary.tif

Tree_Dragon_Spp12194_Thresholded_Binary.tif

Tree_Dtella_Spp12092_Thresholded_Binary.tif

Tree_Martin_Spp10359_Thresholded_Binary.tif

Tree_Skink_Spp12429_Thresholded_Binary.tif

Turquoise_Parrot_Spp10302_Thresholded_Binary.tif

Tussock_Skink_Spp12993_Thresholded_Binary.tif

Tylers_Toadlet_Spp13931_Thresholded_Binary.tif

Varied_Sittella_Spp10549_Thresholded_Binary.tif

Variegated_Fairy_wren_Spp10536_Thresholded_Binary.tif

Verreauxs_Frog_Spp13215_Thresholded_Binary.tif

Verreauxs_Tree_Frog_Spp63906_Thresholded_Binary.tif

Victorian_Smooth_Froglet_Spp13033_Thresholded_Binary.tif

Water_Dragon_Spp18999_Thresholded_Binary.tif

Water_Rat_Spp11415_Thresholded_Binary.tif

Weasel_Skink_Spp12452_Thresholded_Binary.tif

Wedge_tailed_Eagle_Spp10224_Thresholded_Binary.tif

Weebill_Spp10465_Thresholded_Binary.tif

Welcome_Swallow_Spp10357_Thresholded_Binary.tif

West_Australian_Dark_spined_Blind_Snake_Spp12586_Thresholded_Binary.tif

Western_Blue_tongued_Lizard_Spp12579_new_Thresholded_Binary.tif

Western_Brown_Snake_Spp12698_Thresholded_Binary.tif

Western_Gerygone_Spp10463_Thresholded_Binary.tif

Western_Grey_Kangaroo_Spp11263_Thresholded_Binary.tif

Western_Pygmy_possum_Spp11151_Thresholded_Binary.tif

Western_Whipbird\_(Mallee)\_Spp10422_Thresholded_Binary.tif

Whimbrel_Spp10150_Thresholded_Binary.tif

Whistling_Kite_Spp10228_Thresholded_Binary.tif

White_backed_Swallow_Spp10358_Thresholded_Binary.tif

White_bellied_Cuckoo_shrike_Spp10425_Thresholded_Binary.tif

White_bellied_Sea_Eagle_Spp10226_Thresholded_Binary.tif

White_breasted_Woodswallow_Spp10543_Thresholded_Binary.tif

White_browed_Babbler_Spp10445_Thresholded_Binary.tif

White_browed_Scrubwren_Spp10488_Thresholded_Binary.tif

White_browed_Treecreeper_Spp10561_Thresholded_Binary.tif

White_browed_Woodswallow_Spp10545_Thresholded_Binary.tif

White_eared_Honeyeater_Spp10617_Thresholded_Binary.tif

White_faced_Heron_Spp10188_Thresholded_Binary.tif

White_footed_Dunnart_Spp11069_Thresholded_Binary.tif

White_fronted_Chat_Spp10448_Thresholded_Binary.tif

White_fronted_Honeyeater_Spp10594_Thresholded_Binary.tif

White_headed_Pigeon_Spp10028_Thresholded_Binary.tif

White_lipped_Snake_Spp12665_Thresholded_Binary.tif

White_naped_Honeyeater_Spp10578_Thresholded_Binary.tif

White_necked_Heron_Spp10189_Thresholded_Binary.tif

White_plumed_Honeyeater_Spp10625_Thresholded_Binary.tif

White_striped_Freetail_Bat_Spp11324_Thresholded_Binary.tif

White_throated_Gerygone_Spp10453_Thresholded_Binary.tif

White_throated_Needletail_Spp10334_Thresholded_Binary.tif

White_throated_Nightjar_Spp10330_Thresholded_Binary.tif

White_throated_Treecreeper_Spp10558_Thresholded_Binary.tif

White_winged_Chough_Spp10693_Thresholded_Binary.tif

White_winged_Fairy_wren_Spp10535_Thresholded_Binary.tif

White_winged_Triller_Spp10430_Thresholded_Binary.tif

Willie_Wagtail_Spp10364_Thresholded_Binary.tif

Wonga_Pigeon_Spp10044_Thresholded_Binary.tif

Wood_Gecko_Spp12077_Thresholded_Binary.tif

Wood_Sandpiper_Spp10154_new_Thresholded_Binary.tif

Woodland_Blind_Snake_Spp12603_Thresholded_Binary.tif

Yellow_bellied_Glider_Spp11136_Thresholded_Binary.tif

Yellow_bellied_Sheathtail_Bat_Spp11321_Thresholded_Binary.tif

Yellow_bellied_Water_Skink_Spp12957_Thresholded_Binary.tif

Yellow_billed_Spoonbill_Spp10182_Thresholded_Binary.tif

Yellow_faced_Honeyeater_Spp10614_Thresholded_Binary.tif

Yellow_faced_Whip_Snake_Spp12655_Thresholded_Binary.tif

Yellow_footed_Antechinus_Spp11027_Thresholded_Binary.tif

Yellow_plumed_Honeyeater_Spp10622_Thresholded_Binary.tif

Yellow_rumped_Thornbill_Spp10486_Thresholded_Binary.tif

Yellow_tailed_Black_Cockatoo_Spp10267_Thresholded_Binary.tif

Yellow_Thornbill_Spp10471_Thresholded_Binary.tif

Yellow_throated_Miner_Spp10635_Thresholded_Binary.tif

Yellow_tufted_Honeyeater_Spp10619_Thresholded_Binary.tif

Zebra_Finch_Spp10653_Thresholded_Binary.tif

## Appendix 2

# Growth stage optimisation in *Shiny*

This document is a guide to using the growth stage optimisation (GSO) tool from the ERP1 shiny app.

## File formatting

There are several files that you need to create in Excel to run the GSO in *R*.

Currently these files are in either .csv ( comma separated value text files) or micorsoft Excel (.xlxs) formats as noted below. It is desirable that each xlsx worksheet will ultimately be replaced by a corresponding .csv file. This has not been completed in as part of ERP1.

They require that you use the same headers and name endings otherwise errors in the code may occur. (the name can be prefixed with individual details of the file ,for instance the LMU name), Please note that ***R* is case sensitive**. The files should be stored in **./GSOinputs"**. This is taken care of by uploading them to the shiny server from the app interface.

The first csvfile required is ends with"Spp_EFG_LMU.csv" which can be generated using.... The file includes the species that could be expected to be found in each EFG within the LMU and has the form,

![][1]

The second file required "LMU Area.csv" has the total area of each EFG within the LMU, with its EFG name and number.

![][2]

The file ending "LMU_Scenarios.csv" has the information about the scenarios to be compared. The "PercLandscape" column is the proportion of that EFG in that GS. Therefore, they need to sum to 1 for an EFG within each scenario. For instance, in EFG 6 in the 2017 (current) scenario the proportions are 0.04, 0.06, 0.42 and 0.48, which add up to 1 (or 100%).

![][3]

The next required is "ObsData.csv". This contains the observational data, with each row containing the observations for one species at one survey site.

![][4]

## Options for GSO in Selected in shiny app

The shiny app provides a single screen GUI to select the four.csv file required and select all the settings required for t GSO to be run ( these were previously handled by editing the text in the R file). The options are given in the table below.

Shiny GSO GUI

![][5]

GSO Options

  Option                                                                                                                                                                           Name in *R*   Options
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  Most recent fire type                                                                                                                                                            FireType      "Low" or "High"
  The scenario to use for comparisons                                                                                                                                              Comparison    This will depend on which scenario you want to set for comparisons, and what you called your scenarios. If you want to use the optimised solution, then type "Optimisation".
  Which combination of data to use. Options range from exclusive use of expert opinion or observational data to various combinations of both. See ?? for what each option means.   Rule          "Rule0", "Rule1", "Rule1a", "Rule1b", "Rule1c", "Rule2", "Rule2a", "Rule2b", "Rule2c", "Rule3", "Rule3a", "Rule3b" or "Rule3c"
  The weight to use when combining expert opinion and observational data if using "Rule2".                                                                                         dWt           A number between 0 and 1, with 0 meaning no weight goes to the survey data (effectively "Rule0") and 1 meaning all weight goes to survey data (where available, effectively "Rule1").
  The number of times we resample from the data to estimate the abundance index.                                                                                                   nrep          Number greater than 0. Default is 100.
  The number of times we simulate the process, used to generate 95% confidence intervals.                                                                                          nsim          Number greater than 0.

## Running the GSO

Once the data files are saved in the folder "./GSOInputs" and the model options are selected in the second coloured box the GSO is ready to run. To run the model, you just need to click the "Run GSO button at the bottom left of the GDSO shiny app window.

**Note: this process may take some time** depending on the amount of observational data, number of simulations required and the speed of the computer.

Once the analysis has run two files will be created. "GSO_Analysis_Output.docx which can be used as the basis of a report. It documents the options used, including model choices, EFGs and species used and produces some tables, plots and comparisons. A file "GSO Species Changes.csv" is also created to store the change in abundance index for each species and scenario. **Note** these file will be overwritten if the "Run GSO" button is pushed again.

  [1]: media/image1.png {width="4.748837489063867in" height="1.8270024059492564in"}
  [2]: media/image2.png {width="2.7052285651793526in" height="0.6558136482939633in"}
  [3]: media/image3.png {width="5.169318678915135in" height="2.4418602362204727in"}
  [4]: media/image4.png {width="5.544186351706037in" height="2.9573567366579177in"}
  [5]: media/image5.png {width="3.1875in" height="3.557597331583552in"}
