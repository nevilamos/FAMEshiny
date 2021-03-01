library(aws.s3)
newDirPaths<-c("AdHocPolygons",
               "CustomCSV",
               "FH_Outputs",
               "HDMS",
               "InputGeneralRasters",
               "rawFH",
               "ReferenceShapeFiles",
               "ReferenceTables",
               "results"
               )               
for(mypath in newDirPaths){
if (file.exists(mypath)){} else{

dir.create(mypath,recursive = T)



Sys.setenv("AWS_ACCESS_KEY_ID" = ,
           "AWS_SECRET_ACCESS_KEY" = ,
           "AWS_DEFAULT_REGION" = "ap-southeast-2")


aws.s3::s3sync(path=paste0("./",mypath),bucket = "ecological-risk-analysis",prefix = paste0("FAME_FMR","/",mypath), check_region = F,direction = "download")}
}
