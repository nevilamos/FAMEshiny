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



Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIASBSJ6EYMARPJLDLD",
           "AWS_SECRET_ACCESS_KEY" ="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" ,
           "AWS_DEFAULT_REGION" = "ap-southeast-2")


aws.s3::s3sync(path=paste0("./",mypath),bucket = "ecological-risk-analysis",prefix = paste0("FAME_FMR","/",mypath), check_region = F,direction = "download")}
}
