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

   }
#   
#   
}
if(!file.exists("FAME_inputfiles.zip")){
  download.file("https://ecological-risk-analysis.s3-ap-southeast-2.amazonaws.com/FAME_FMR/FAME4_inputfiles.zip","FAME_inputfiles.zip")
  
  if (.Platform$OS.type == "unix"){
    system("unzip -o ./FAME_inputfiles.zip && cp -rl ./FAME_inputfiles ./  && rm -r ./FAME_inputfiles")
  }else{
    unzip("./FAME_inputfiles.zip",exdir = ".")
    system("xcopy .\\FAME_inputfiles\ . /E/H/y")
    unlink("FAME_inputfiles",recursive=TRUE)
  }
  
}





