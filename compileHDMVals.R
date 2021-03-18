#script to compile compact .qs serial files of sparse matrices of HDM Raster
#values from directories of correctly sized HDM rasters where raster name
#contains VBA_TAXON_ID.  Needs to be run on machine with ~64gb RAM for 75m resolution


library(raster)
library(foreach)
library(doParallel)
library(Matrix)
library(FAMEFMR)
library(qs)
for (Res in c(75, 225)) {
  myDir <-
    paste0("./", Res, "m/BinaryThresholded")##this line needs to point at directory containing HDM tiffs at each Res
  RasterPaths <- dir(myDir, full.names = T, pattern = ".tif$")
  outFile <- paste0("./HDMS/HDMVals", Res, "list.qs")
  myNames <- names(myHDMVals) <- as.character(get_Spp_No(RasterPaths))
  
  
  cl <- makeCluster(30)
  registerDoParallel(cl, cores = 30)
  
  myHDMVals <-
    foreach(
      i = iter(RasterPaths),
      .combine = 'c',
      .packages = c("raster", "Matrix"),
      .multicombine = TRUE
    ) %dopar% {
      myVals <- raster::values(raster::raster(i))
      myVals[is.na(myVals)] <- 0
      myVals <- as(as(myVals, "CsparseMatrix"), "ngCMatrix")
      myVals
    }
  names(myHDMVals) <- myNames
  qsave(myHDMVals, outFile)
  
  if (Res == 225) {
    myM <-
      myHDMVals <-
      foreach(
        i = iter(RasterPaths),
        .combine = 'c',
        .packages = c("raster", "Matrix"),
        .multicombine = TRUE
      ) %dopar% {
        myVals <- raster::values(raster::raster(i))
        myVals[is.na(myVals)] <- 0
        myVals
      }
    mySM <- as(as(myM, "CsparseMatrix"), "ngCMatrix")
    names(mySM) <- myNames
    qsave(mySM, gsub("list", "", outFile))
    
    
  }
  
 
  
}
stopCluster(cl)
