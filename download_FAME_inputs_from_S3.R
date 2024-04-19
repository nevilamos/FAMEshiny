
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
               "results"
)

for(mypath in newDirPaths){
  
  if (file.exists(mypath)){
    
  } else {
    dir.create(mypath,recursive = T)
    
  }
  #   
  #   
  
}
if(!file.exists("FAME4_inputfiles.zip")){
  download.file("https://ecological-risk-analysis.s3-ap-southeast-2.amazonaws.com/FAME_FMR/FAME4_inputfiles.zip","FAME4_inputfiles.zip")
  
  if (.Platform$OS.type == "unix"){
    system("unzip -o ./FAME4_inputfiles.zip && cp -r FAME4_inputfiles/. . ")
  }else{
    unzip("./FAME_inputfiles.zip",exdir = ".")
    system("xcopy .\\FAME4_inputfiles\ . /E/H/y")
    
  }
  unlink("FAME4_inputfiles",recursive=TRUE)
}



