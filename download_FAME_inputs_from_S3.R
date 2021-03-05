library(aws.s3)
newDirPaths<-c("AdHocPolygons",
               "CustomCSV",
               "FAMEPreProcessing",
               "FH_Outputs",
               "HDMS",
               "InputGeneralRasters",
               "rawFH",
               "ReferenceShapefiles",
               "ReferenceTables",
               "results",
               "www"
)               
for(mypath in newDirPaths){
  if (file.exists(mypath)){} else{
    
    dir.create(mypath,recursive = T)
    
    if(!exists("AWS_ACCESS_KEY_ID")){
      AWS_ACCESS_KEY_ID <- readline(prompt = "enter AWS_ACCESS_KEY_ID:")
    }
    if(!exists("AWS_SECRET_ACCESS_KEY")){  
      AWS_SECRET_ACCESS_KEY <- readline(prompt = "enter AWS_SECRET_ACCESS_KEY:")
    }
    
    
    aws.s3::s3sync(path=paste0("./",mypath),
                   bucket = "ecological-risk-analysis",
                   prefix = paste0("FAME_FMR","/",mypath,"/"),
                   check_region = F,
                   direction = "download",
                   key = AWS_ACCESS_KEY_ID,
                   secret = AWS_SECRET_ACCESS_KEY,
                   region = "ap-southeast-2")
  }
}
rm(AWS_ACCESS_KEY_ID,AWS_SECRET_ACCESS_KEY)
