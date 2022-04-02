#file to assemble all Species RA data from numerous fame output qs files.   
#looks for all suciatbaly named files in a subdirectory of "fame-obm" set this subdirectory as a filter in 
#filter(x =="results") ( currently set to directory "results")


library(tidyverse)
library(stringr)
library(qs)
library(data.table)
#library(parallel)
library(foreach)
#library(doParallel)


system(
  "s3fs fame-obm -o use_cache=/tmp -o allow_other -o uid=1001 -o mp_umask=002 -o multireq_max=5 /home/rstudio/ShinyApps/FAME/fame-obm"
)
#mydb <- dbConnect(RSQLite::SQLite(), "OBRM-db.sqlite")
myqs<-list.files("fame-obm",recursive =T,pattern = "preWrangleData.qs$")
myqs<-data.frame(myqs)
myqs1<-myqs %>% 
  mutate(fName = basename(myqs),Path = dirname(myqs)) %>% 
  tidyr::separate(Path,sep ="/|\\\\", c("x","AbundType")) %>% 
  filter(!(grepl("Demo",fName))) %>% 
  filter(!(grepl("test",fName))) %>% 
  filter(x =="results") %>% 
  filter(!is.na(AbundType)) %>% 
  mutate(xx = (regmatches(fName,gregexpr("[[:digit:]]+", fName)))) %>% 
  mutate(Percent= unlist(map(xx,1)),Replicate = unlist(map(xx,5))) %>% 
  select(-xx) %>% 
  mutate(SppSumFile=paste0("fame-obm/results/SppSumTables/SppSumDistrictTab_",Percent,"pc_",Replicate,"_",AbundType, ".csv")) %>% 
  mutate(SppFileExists = file.exists(SppSumFile))

write_csv(myqs1,"Finished.csv")
myqs2<-myqs1 %>% filter(SppFileExists==FALSE)




#cl <- makeCluster(4)
#registerDoParallel(cl, cores = 4)

for(i in 1:nrow(myqs2)){
  myDetails<-myqs2[i,]          
  rv<-qread(file.path("fame-obm/results",myDetails$myqs))
  print(paste( "read",myDetails$myqs))
  grpSpYearSumm<-rv$SpYearSumm$grpSpYearSumm
  allCombs<-rv$allCombs
  
  
  
  rm(rv)
  gc()
  grpSpYearSumm<-grpSpYearSumm %>% dplyr::rename(Index_AllCombs = `myAllCombs$Index_AllCombs`)
  
  PC<-myDetails$Percent
  AT<-myDetails$AbundType
  REP<-myDetails$Replicate
  #This next section is a cludge to get around 2 billion row problem for pivot longer here at the moment splits it into individual seasons - potentially could do more and then parralellise
  N<-dim(grpSpYearSumm)[2]-1
  EFG<-allCombs$U_AllCombs_TFI$EFG[grpSpYearSumm$Index_AllCombs]
  DISTRICT_NO<-allCombs$U_AllCombs_TFI$PU[grpSpYearSumm$Index_AllCombs]
  PUBLIC<-is.na(allCombs$U_AllCombs_TFI$PLM[grpSpYearSumm$Index_AllCombs])
  
  
  print(paste("processing",myDetails$myqs))
  library(tictoc)
  tic()
  resTab<-foreach(
    i = 2:N,
    .combine = 'rbind',
    .packages = c("dplyr","tidyr"),
    .multicombine = TRUE
  ) %do% {
    myTab<-grpSpYearSumm[,c(1,73,i)] %>%
      tidyr::pivot_longer(
        -tidyr::one_of("TAXON_ID", "Index_AllCombs"),
        names_to = "SEASON",
        values_to = "sumRA") %>%
      mutate(EFG = EFG ,DISTRICT_NO =DISTRICT_NO, PUBLIC =PUBLIC)%>%
      dplyr::group_by(TAXON_ID, PUBLIC,EFG,DISTRICT_NO, SEASON) %>%
      dplyr::summarise(sumRA = sum(sumRA)) %>%
      dplyr::mutate(TAXON_ID = as.integer(TAXON_ID))%>% 
      dplyr::mutate(AbundType= AT,Percent =PC,Replicate = REP)
    gc()
    myTab
    
  }
  toc()
  #close(pb)
  write_csv(resTab,paste0("fame-obm/results/SppSumTables/SppSumDistrictTab_",PC,"pc_",REP,"_",AT, ".csv"))
  rm(resTab)
  gc()
}

myqs1<-myqs1 %>%
  mutate(SppFileExists = file.exists(SppSumFile))

write_csv(myqs1,"Finished.csv")
mySppFiles <- myqs1%>% filter(SppFileExists==TRUE)

#assemble all data into one table


out = rbindlist(lapply(mySppFiles$SppSumFile, function(file) {
  dt = fread(file)
  # further processing/filtering
}))
qsave(out,"fame-obm/results/AllSppSumTabNew.qs")
gc()
#drop rows with 0 sumRA
myTabNoZero<-out[sumRA>0]
qsave(myTabNoZero,"fame-obm/results/AllSppSumTabNoZeroNew.qs")

#disable four lines below if you do not want server to stop 
unlink("index.html")
system("wget http://169.254.169.254/latest/meta-data/instance-id/")
myID<-readLines("index.html")[1]
system(paste("aws ec2 stop-instances --region ap-southeast-2 --instance-ids",myID))
