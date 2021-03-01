library(aws.s3)
newDirPaths<-c("CustomCSV","FH_Outputs","HDMS","InputGeneralRasters","ReferenceShapeFiles","ReferenceTables","Results","rawFH","CustomCSV","AdHocPolygons")
for(mypath in newDirPaths){
if (file.exists(mypath)){} else{

dir.create(mypath,recursive = T)


Sys.setenv("AWS_ACCESS_KEY_ID" = ,
           "AWS_SECRET_ACCESS_KEY" = ,
           "AWS_DEFAULT_REGION" = "ap-southeast-2")


aws.s3::s3sync(path=paste0("./",mypath),bucket = "ecological-risk-analysis",prefix = paste0("FAME_FMR","/",path), check_region = F,direction = "download")}
}
