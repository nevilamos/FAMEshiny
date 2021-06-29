# library(aws.s3)
newDirPaths<-c("AdHocPolygons",
               "CustomCSV",
               "FH_Outputs",
               "HDMS",
               "InputGeneralRasters",
               "PUPolygons",
               "rawFH",
               "ReferenceShapefiles",
               "ReferenceTables",
               "results",
               "www"# 
)
# 
# 
# Sys.setenv("AWS_DEFAULT_REGION" = "ap-southeast-2")
# 
# 
for(mypath in newDirPaths){

  if (file.exists(mypath)){

  } else {
    dir.create(mypath,recursive = T)
#     
#     aws.s3::s3sync(path=paste0("./",mypath),
#                    bucket = "ecological-risk-analysis",
#                    prefix = paste0("FAME_FMR","/",mypath),
#                    check_region = F,
#                    direction = "download"
#                    
#     )
#     
#     
   }
#   
#   
}
if(!file.exists("FAME_inputfiles.zip")){
  download.file("https://ecological-risk-analysis.s3-ap-southeast-2.amazonaws.com/FAME_FMR/FAME_inputfiles.zip","FAME_inputfiles.zip")
  unzip("./FAME_inputfiles.zip",exdir = ".")
  
}





