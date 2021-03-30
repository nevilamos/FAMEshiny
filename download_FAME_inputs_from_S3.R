library(aws.s3)
newDirPaths<-c("AdHocPolygons",
               "CustomCSV",
               "FAMEPreProcessing",
               "FH_Outputs",
               "HDMS",
               "InputGeneralRasters",
               "Manual",
               "rawFH",
               "ReferenceShapefiles",
               "ReferenceTables",
               "results",
               "dummyname"# this name is a workaround for the shinyapp a directory and files ./www is required but s3sync will not open the directory so this name is used instead and renamed at the end of the process
)


Sys.setenv("AWS_DEFAULT_REGION" = "ap-southeast-2")


for(mypath in newDirPaths){
  
  if (file.exists(mypath)){
    
  } else {
    dir.create(mypath,recursive = T)
    
    aws.s3::s3sync(path=paste0("./",mypath),
                   bucket = "ecological-risk-analysis",
                   prefix = paste0("FAME_FMR","/",mypath),
                   check_region = F,
                   direction = "download"
                   
    )
    
    
  }
  
  
}


try(file.rename("dummyname","www"))
try(dir.create("dummyname"))
#rm(AWS_ACCESS_KEY_ID,AWS_SECRET_ACCESS_KEY)

