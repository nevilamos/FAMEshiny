library(tidyverse)
library(stringr)
library(qs)
library(data.table)
library(zoo)
library(broom)
system(
  "s3fs fame-obm -o use_cache=/tmp -o allow_other -o uid=1001 -o mp_umask=002 -o multireq_max=5 /home/rstudio/ShinyApps/FAME/fame-obm"
)

#reference tables for names
taxonList<-fread("CustomCSV/FAME_TAXON_LIST_FFG_update2.csv")


districtTab<-foreign::read.dbf("ReferenceShapefiles/LF_DISTRICT_with_PU_field.dbf") %>% select(2,3,7)
names(districtTab)<-c("FIRE_DISTRICT","FIRE_REGION","DISTRICT_NO")
efgTab<-read_csv("ReferenceTables/EFG_EVD_TFI.csv")%>% select(2,3)
names(efgTab)[1]<-"EFG"
#DISRTICT GROUPING --------
AllSppSumTabNoZero<-qread("fame-obm/results/AllSppSumTabNoZero.qs")

DISTRICT_RA <-AllSppSumTabNoZero[,list(sumRA = sum(sumRA)),by=list(TAXON_ID,DISTRICT_NO,SEASON,AbundType,Percent,Replicate)]
DISTRICT_RA<-DISTRICT_RA %>% 
  left_join(districtTab) %>% 
  left_join(taxonList %>% select(TAXON_ID,COMMON_NAME))
fwrite(DISTRICT_RA,"fame-obm/results/DISTRICT_RA.csv")


DISTRICT_LM<-DISTRICT_RA%>% 
  group_by(COMMON_NAME,FIRE_DISTRICT,AbundType,Percent,Replicate) %>% 
  do(tidy(lm(sumRA ~ SEASON,.))) 
fwrite(DISTRICT_LM,"fame-obm/results/DISTRICT_LM.csv")

DISTRICT_Trends<-DISTRICT_LM %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

DISTRICT_Trends_Summ<-DISTRICT_Trends %>% 
  group_by(AbundType,FIRE_DISTRICT,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(DISTRICT_Trends_Summ,"DISTRICT_Trends_Summ.csv")

districtTrendPlot<-DISTRICT_Trends_Summ %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,FIRE_DISTRICT,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining1980-2050 \n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")


DISTRICT_LM_2022_2050<-DISTRICT_RA %>% 
  filter(SEASON > 2021) %>% 
  group_by(COMMON_NAME,FIRE_DISTRICT,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(DISTRICT_LM_2022_2050,"fame-obm/results/DISTRICT_LM_2022_2050.csv")


DISTRICT_Trends_2022_2050<-DISTRICT_LM_2022_2050 %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

DISTRICT_Trends_Summ_2022_2050<-DISTRICT_Trends_2022_2050 %>% 
  group_by(AbundType,FIRE_DISTRICT,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(DISTRICT_Trends_Summ_2022_2050,"DISTRICT_Trends_Summ_2022_2050.csv")

districtTrendPlot_2022_2050<-DISTRICT_Trends_Summ_2022_2050 %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,FIRE_DISTRICT,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining 2022 - 2050\n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")



DISTRICT_LM_1980_2021<-DISTRICT_RA %>% 
  filter(SEASON < 2022) %>% 
  group_by(COMMON_NAME,FIRE_DISTRICT,AbundType) %>%
  distinct()
do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(DISTRICT_LM_1980_2021,"fame-obm/results/DISTRICT_LM_1980_2021.csv")


DISTRICT_Trends_1980_2021<-DISTRICT_LM_1980_2021 %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

DISTRICT_Trends_Summ_1980_2021<-DISTRICT_Trends_1980_2021 %>% 
  group_by(AbundType,FIRE_DISTRICT,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(DISTRICT_Trends_Summ_1980_2021,"DISTRICT_Trends_Summ_1980_2021.csv")

districtTrendPlot_1980_2021<-DISTRICT_Trends_Summ_1980_2021 %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,FIRE_DISTRICT,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining 1980 - 2021\n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")

gridExtra::grid.arrange(districtTrendPlot_1980_2021,districtTrendPlot_2022_2050,nrow=2)

DISTRICT_RAChange<-DISTRICT_RA %>%
  left_join((taxonList %>% select(COMMON_NAME,OBRM_CombThreshold))) %>% 
  group_by(COMMON_NAME,FIRE_DISTRICT,AbundType,Percent,Replicate,OBRM_CombThreshold) %>% 
  mutate(sumRA_lag1= lag(sumRA ,n= 1),
         sumRA_lag5= lag(sumRA,n = 5),
         Prop1980 = sumRA/first(sumRA), 
         YearOnYear = sumRA/sumRA_lag1,
         YearOn5yearBefore = sumRA/sumRA_lag5
  ) %>%
  mutate(Baseline = mean(sumRA[SEASON%in%1980:1999]),#change years for baseline here------,
         YearOnBaseline = sumRA/Baseline) %>%   mutate(YearOnBaseline_Threshold_Decline = ifelse(YearOn5yearBefore < OBRM_CombThreshold, 1,0)) %>% 
  # mutate(meanRA_5yrs = zoo::rollapply(data =sumRA_lag1,
  #                                     width = 5, 
  #                                     FUN = mean, 
  #                                     align = "right", 
  #                                     fill = NA, 
  #                                     na.rm = T)) %>% 
  # mutate(Year_mean_Prev_5 = sumRA/lag(meanRA_5yrs)) %>% 
  mutate(YearOnYear15pcDecline = ifelse(YearOnYear < 0.85 , 1 , 0 )) %>% 
  mutate(YearOn5yearBefore_Threshold_Decline = ifelse(YearOn5yearBefore < OBRM_CombThreshold, 1,0))
write_csv(DISTRICT_RAChange,"fame-obm/results/DISTRICT_RAChange.csv")
#REGION GROUPING --------

REGION_RA<-DISTRICT_RA[,list(sumRA = sum(sumRA)),by=list(COMMON_NAME,FIRE_REGION,AbundType,Percent,Replicate,SEASON)] 
  
write_csv(REGION_RA,"fame-obm/results/REGION_RA.csv")

REGION_LM<-REGION_RA %>% 
  group_by(COMMON_NAME,FIRE_REGION,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(REGION_LM,"fame-obm/results/REGION_LM.csv")

REGION_Trends<-REGION_LM %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

REGION_Trends_Summ<-REGION_Trends %>% 
  group_by(AbundType,FIRE_REGION,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(REGION_Trends_Summ,"REGION_Trends_Summ.csv")

RegionTrendPlot<-REGION_Trends_Summ %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,FIRE_REGION,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining1980-2050 \n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")
# 2022-2050----

REGION_LM_2022_2050<-REGION_RA %>% 
  filter(SEASON > 2021) %>% 
  group_by(COMMON_NAME,FIRE_REGION,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(REGION_LM_2022_2050,"fame-obm/results/REGION_LM_2022_2050.csv")


REGION_Trends_2022_2050<-REGION_LM_2022_2050 %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

REGION_Trends_Summ_2022_2050<-REGION_Trends_2022_2050 %>% 
  group_by(AbundType,FIRE_REGION,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(REGION_Trends_Summ_2022_2050,"REGION_Trends_Summ_2022_2050.csv")

RegionTrendPlot_2022_2050<-REGION_Trends_Summ_2022_2050 %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,FIRE_REGION,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining 2022 - 2050\n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")

#1980-2021-------------

REGION_LM_1980_2021<-REGION_RA %>% 
  filter(SEASON < 2022) %>% 
  group_by(COMMON_NAME,FIRE_REGION,AbundType) %>%
  distinct()
do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(REGION_LM_1980_2021,"fame-obm/results/REGION_LM_1980_2021.csv")


REGION_Trends_1980_2021<-REGION_LM_1980_2021 %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

REGION_Trends_Summ_1980_2021<-REGION_Trends_1980_2021 %>% 
  group_by(AbundType,FIRE_REGION,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(REGION_Trends_Summ_1980_2021,"REGION_Trends_Summ_1980_2021.csv")

RegionTrendPlot_1980_2021<-REGION_Trends_Summ_1980_2021 %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,FIRE_REGION,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining 1980 - 2021\n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")

gridExtra::grid.arrange(RegionTrendPlot_1980_2021,RegionTrendPlot_2022_2050,nrow=2)



REGION_RAChange<-REGION_RA %>%
  left_join((taxonList %>% select(COMMON_NAME,OBRM_CombThreshold))) %>% 
  group_by(COMMON_NAME,FIRE_REGION,AbundType,Percent,Replicate,OBRM_CombThreshold) %>% 
  mutate(sumRA_lag1= lag(sumRA ,n= 1),
         sumRA_lag5= lag(sumRA,n = 5),
         Prop1980 = sumRA/first(sumRA), 
         YearOnYear = sumRA/sumRA_lag1,
         YearOn5yearBefore = sumRA/sumRA_lag5
  ) %>%
 mutate(Baseline = mean(sumRA[SEASON%in%1980:1999]),#change years for baseline here------,
         YearOnBaseline = sumRA/Baseline) %>%   mutate(YearOnBaseline_Threshold_Decline = ifelse(YearOn5yearBefore < OBRM_CombThreshold, 1,0)) %>% 
  # mutate(meanRA_5yrs = zoo::rollapply(data =sumRA_lag1,
  #                                     width = 5, 
  #                                     FUN = mean, 
  #                                     align = "right", 
  #                                     fill = NA, 
  #                                     na.rm = T)) %>% 
  # mutate(Year_mean_Prev_5 = sumRA/lag(meanRA_5yrs)) %>% 
  mutate(YearOnYear15pcDecline = ifelse(YearOnYear < 0.85 , 1 , 0 )) %>% 
  mutate(YearOn5yearBefore_Threshold_Decline = ifelse(YearOn5yearBefore < OBRM_CombThreshold, 1,0))
write_csv(REGION_RAChange,"fame-obm/results/REGION_RAChange.csv")


# STATEWIDE GROUPING --------------------

State_RA<-DISTRICT_RA[,list(sumRA = sum(sumRA)),by=list(COMMON_NAME,AbundType,Percent,Replicate,SEASON)] 
  
fwrite(State_RA,"fame-obm/results/State_RA.csv")

State_LM<-State_RA %>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(State_LM,"fame-obm/results/State_LM.csv")

State_Trends<-State_LM %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

State_Trends_Summ<-State_Trends %>% 
  group_by(AbundType,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(State_Trends_Summ,"State_Trends_Summ.csv")

StateTrendPlot<-State_Trends_Summ %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining1980-2050 \n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")

# 2022-2050----
State_LM_2022_2050<-State_RA %>% 
  filter(SEASON > 2021) %>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(State_LM_2022_2050,"fame-obm/results/State_LM_2022_2050.csv")


State_Trends_2022_2050<-State_LM_2022_2050 %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

State_Trends_Summ_2022_2050<-State_Trends_2022_2050 %>% 
  group_by(AbundType,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(State_Trends_Summ_2022_2050,"State_Trends_Summ_2022_2050.csv")

StateTrendPlot_2022_2050<-State_Trends_Summ_2022_2050 %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining 2022 - 2050\n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")

# 1980-2021----

State_LM_1980_2021<-State_RA %>% 
  filter(SEASON < 2022) %>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(State_LM_1980_2021,"fame-obm/results/State_LM_1980_2021.csv")


State_Trends_1980_2021<-State_LM_1980_2021 %>% 
  filter(term == "SEASON") %>% 
  mutate(Trend = ifelse(p.value>=0.05,"N.S.",ifelse(estimate>0,"Increase","Decrease")))

State_Trends_Summ_1980_2021<-State_Trends_1980_2021 %>% 
  group_by(AbundType,Percent,Replicate,Trend) %>%
  tally() %>% 
  mutate(Freq = n/sum(n))
write.csv(State_Trends_Summ_1980_2021,"State_Trends_Summ_1980_2021.csv")

StateTrendPlot_1980_2021<-State_Trends_Summ_1980_2021 %>% 
  filter(Trend == "Decrease") %>% 
  group_by(AbundType,Percent) %>% 
  ggplot2::ggplot(aes(x=Percent,y=Freq,col=AbundType)) +
  geom_point(size=2, shape=23)+ 
  ggtitle("Proportion of species declining 1980 - 2021\n by percent burn scenario") +
  xlab("Percent Burn") + 
  ylab("Proportion Species Declining")
  
gridExtra::grid.arrange(StateTrendPlot_1980_2021,StateTrendPlot_2022_2050,nrow=2)

# calculates the lagged change in abundance for Year on Year ()
State_RAChange<-State_RA %>%
  left_join((taxonList %>% select(COMMON_NAME,OBRM_CombThreshold))) %>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate,OBRM_CombThreshold) %>% 
  mutate(sumRA_lag1= lag(sumRA ,n= 1),
         sumRA_lag5= lag(sumRA,n = 5),
         Prop1980 = sumRA/first(sumRA), 
         YearOnYear = sumRA/sumRA_lag1,
         YearOn5yearBefore = sumRA/sumRA_lag5
         ) %>%
  mutate(Baseline = mean(sumRA[SEASON%in%1980:1999]),#change years for baseline here------
         YearOnBaseline = sumRA/Baseline) %>%   mutate(YearOnBaseline_Threshold_Decline = ifelse(YearOn5yearBefore < OBRM_CombThreshold, 1,0)) %>% 
  # mutate(meanRA_5yrs = zoo::rollapply(data =sumRA_lag1,
  #                                     width = 5, 
  #                                     FUN = mean, 
  #                                     align = "right", 
  #                                     fill = NA, 
  #                                     na.rm = T)) %>% 
  # mutate(Year_mean_Prev_5 = sumRA/lag(meanRA_5yrs)) %>% 
  mutate(YearOnYear15pcDecline = ifelse(YearOnYear < 0.85 , 1 , 0 )) %>% 
    mutate(YearOn5yearBefore_Threshold_Decline = ifelse(YearOn5yearBefore < OBRM_CombThreshold, 1,0))
write_csv(State_RAChange,"fame-obm/results/State_RAChange.csv")

StateYearOnYearSummary <- State_RAChange %>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate,YearOnYear15pcDecline) %>% 
  tally()



