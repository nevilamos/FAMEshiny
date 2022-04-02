# need to either copy files out of results directlry or change all relevant paths from results to new direcotry name to stop overwriting including in   filter(x =="results")

library(tidyverse)
library(stringr)
library(qs)
library(data.table)
library(zoo)
library(broom)
system(
  "s3fs fame-obm -o use_cache=/tmp -o allow_other -o uid=1001 -o mp_umask=002 -o multireq_max=5 /home/rstudio/ShinyApps/FAME/fame-obm"
)

myqs<-list.files("fame-obm",recursive =T,pattern = "preWrangleData.qs$")
myqs<-data.frame(myqs)
myqs1<-myqs %>% 
  mutate(fName = basename(myqs),Path = dirname(myqs)) %>% 
  tidyr::separate(Path,sep ="/|\\\\", c("x","AbundType")) %>% 
  filter(!(grepl("Demo",fName))) %>% 
  filter(!(grepl("test",fName))) %>% 
  filter(x =="results") %>% 
  filter(!is.na(AbundType)) %>%
  filter(AbundType = "") %>% 
  mutate(xx = (regmatches(fName,gregexpr("[[:digit:]]+", fName)))) %>% 
  mutate(Percent= unlist(map(xx,1)),Replicate = unlist(map(xx,5))) %>% 
  select(-xx) %>% 
  mutate(SppSumFile=paste0("fame-obm/results/SppSumTables/SppSumDistrictTab_",Percent,"pc_",Replicate,"_",AbundType, ".csv")) %>% 
  mutate(SppFileExists = file.exists(SppSumFile))

fwrite(myqs1,"Finished.csv")

# TFI_LONG_ALL<-NULL
# GS_LONG_ALL<-NULL
# BBTFI_LONG_ALL<-NULL
for (i in 1:nrow(myqs1)){
  print(paste0("doing",i))
  myDetails<-myqs1[i,]
  R<-myDetails$Replicate
  A<-myDetails$AbundType
  P<-myDetails$Percent
  rv<-qread(file.path("fame-obm",myDetails$myqs))
  
  GS_Summary_wide<-rv$GS_Summary$GS_Summary_wide
  GS_Summary_PU_LONG<-GS_Summary_wide %>% 
    ungroup() %>% 
    select(-any_of(c("EFG", "MIN_LO_TFI", "MIN_HI_TFI", "MAX_TFI",  "Index", 
                     "nPixel", "FH_ID", "FIRE_REG", "FIREFMZ",  "DELWP", 
                     "FIRE_FMZ_NAME", "FIRE_FMZ_SHORT_NAME", "FIRE_REGION_NAME",
                     "DELWP_REGION" ))) %>% 
    rename("DISTRICT_NO" = "PU") %>% 
    pivot_longer(cols=starts_with("GS_"),
                 names_to = "SEASON",
                 names_prefix="GS_" ,
                 values_to = "GS4_NO",
                 values_drop_na = TRUE) %>% 
    mutate(AbundType = A,Replicate =R,Percent=P)
  
  qsave(GS_Summary_PU_LONG,paste0("fame-obm/results/GSSumTables/GS_LONG_",A,"_",P,"_",R,".qs"))
  #GS_LONG_ALL<-rbind(GS_LONG_ALL,GS_Summary_PU_LONG)
  
  BBTFI_LONG<-rv$BBTFI$BBTFI_LONG %>% 
    ungroup() %>% 
    select(any_of(c("PLM", "EFG_NAME",  "SEAS", "FireType", "TBTFI", "PU", "Hectares"))) %>% 
    rename("SEASON" = "SEAS","DISTRICT_NO" = "PU") %>% 
    mutate(AbundType = A,Replicate =R,Percent=P)  %>% 
    rename("DISTRICT_NO" = "PU") %>% 
    qsave(BBTFI_LONG,paste0("fame-obm/results/BBTFISumTables/BBTFI_LONG_",A,"_",P,"_",R,".qs"))
  #BBTFI_LONG_ALL<-rbind(BBTFI_LONG_ALL,BBTFI_LONG)
  TFI_LONG<-rv$TFI %>% 
    ungroup() %>% 
    select(-any_of(c( "FIRE_FMZ_NAME", "FIRE_FMZ_SHORT_NAME", "FIRE_REGION_NAME", 
                      "DELWP_REGION", "EFG", "FIRE_REG", "FIREFMZ",  "DELWP" ))) %>% 
    rename("DISTRICT_NO" = "PU") %>% 
    mutate(AbundType = A,Replicate =R,Percent=P)
  qsave(TFI_LONG,paste0("fame-obm/results/TFISumTables/TFI_LONG_",A,"_",P,"_",R,".qs"))
  #TFI_LONG_ALL<-rbind(TFI_LONG_ALL,TFI_LONG)
  
  
}

gc()
TFISumAll = rbindlist(lapply(dir("fame-obm/results/TFISumTables",full.names=T), function(file) {
  dt = qread(file)
  # further processing/filtering
}))
qsave(TFISumAll,"fame-obm/results/TFISumAll.qs")
rm(TFISumAll)
gc()
BBTFISumAll = rbindlist(lapply(dir("fame-obm/results/BBTFISumTables",full.names=T), function(file) {
  dt = qread(file)
  # further processing/filtering
}))
qsave(BBTFISumAll,"fame-obm/results/BBTFISumAll.qs")
rm(BBTFISumAll)
gc()


GSSumAll = rbindlist(lapply(dir("fame-obm/results/GSSumTables",full.names=T), function(file) {
  dt = qread(file)
  # further processing/filtering
}))
GSSumAll
qsave(GSSumAll,"fame-obm/results/GSSumAll.qs")
rm(GSSumAll)
gc()

unlink("index.html")
system("wget http://169.254.169.254/latest/meta-data/instance-id/")
myID<-readLines("index.html")[1]
system(paste("aws ec2 stop-instances --region ap-southeast-2 --instance-ids",myID))
