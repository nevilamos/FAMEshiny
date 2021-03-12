library(raster)
library(foreach)
library(doParallel)
library(Matrix)
library(FAMEFMR)
library(qs)
for (Res in c(75,225))
Res=225
myDir<-paste0("./HDMS/",Res,"m/BinaryThresholded")
RasterPaths<-dir(myDir,full.names = T,pattern=".tif$")
outFile<-paste0("./HDMS/HDMVals",Res,"list.qs")


cl<-makeCluster(30)
registerDoParallel(cl, cores=30)
myHDMVals<-foreach(i=iter(RasterPaths),.combine='c',.packages=c("raster","Matrix"), .multicombine=TRUE)%dopar%{
  myVals<-raster::values(raster::raster(i))
  myVals[is.na(myVals)]<-0
  myVals<-as(as(myVals,"CsparseMatrix"),"ngCMatrix")
  myVals}

stopCluster(cl)
x<-NULL
names(myHDMVals)<-as.character(get_Spp_No(RasterPaths))


