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
taxonList<-read_csv("ReferenceTables/FAME_TAXON_LIST.csv")
VIC_ADVISORY_STATUS<-unique(taxonList$VIC_ADVISORY_STATUS)
VAS<-data.table(VIC_ADVISORY_STATUS)

unique(taxonList$VIC_ADVISORY_STATUS)
districtTab<-foreign::read.dbf("ReferenceShapefiles/LF_DISTRICT_with_PU_field.dbf") %>% select(2,3,7)
names(districtTab)<-c("FIRE_DISTRICT","FIRE_REGION","DISTRICT_NO")
efgTab<-read_csv("ReferenceTables/EFG_EVD_TFI.csv")%>% select(2,3)
names(efgTab)[1]<-"EFG"



district_RAs<- myTabNoZero%>% 
  group_by(TAXON_ID,DISTRICT_NO,SEASON,AbundType,Percent,Replicate) %>% 
  summarise(sumRA = sum(sumRA))%>% 
  left_join(districtTab) %>% 
  left_join(taxonList %>% select(TAXON_ID,COMMON_NAME))


district_LM<-district_RAs%>% 
  group_by(COMMON_NAME,FIRE_DISTRICT,AbundType,Percent,Replicate) %>% 
  do(tidy(lm(sumRA ~ SEASON,.))) 
write_csv(district_LM,"fame-obm/results/District_LM.csv")

Region_RA<-district_RAs%>% 
  group_by(COMMON_NAME,FIRE_REGION,AbundType,Percent,Replicate,SEASON) %>% 
  summarise(sumRA = sum(sumRA))

Region_LM<-Region_RA %>% 
  group_by(COMMON_NAME,FIRE_REGION,AbundType,Percent,Replicate) %>%
  do(tidy(lm(sumRA ~ SEASON,.)))
write_csv(Region_LM,"fame-obm/results/Region_LM.csv")

State_RA<-district_RAs%>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate,SEASON) %>% 
  summarise(sumRA = sum(sumRA))

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
  
gridExtra::grid.arrange(StateTrendPlot,StateTrendPlot_2022_2050,nrow=2)

# calculates the lagged change in abundance for Year on Year ()
State_RAChange<-State_RA %>%
  group_by(COMMON_NAME,AbundType,Percent,Replicate) %>% 
  mutate(sumRA_lag1= lag(sumRA ,n= 1),
         sumRA_lag5= lag(sumRA,n = 5),
         Prop1980 = sumRA/first(sumRA), 
         YearOnYear = sumRA/sumRA_lag1,
         YearOn5yearBefore = sumRA/sumRA_lag5) %>%
  # mutate(meanRA_5yrs = zoo::rollapply(data =sumRA_lag1,
  #                                     width = 5, 
  #                                     FUN = mean, 
  #                                     align = "right", 
  #                                     fill = NA, 
  #                                     na.rm = T)) %>% 
  # mutate(Year_mean_Prev_5 = sumRA/lag(meanRA_5yrs)) %>% 
  mutate(YearOnYear15pcDecline = ifelse(YearOnYear < 0.85 , 1 , 0 ))
write_csv(State_RAChange,"fame-obm/results/State_RAChange.csv")

StateYearOnYearSummary <- State_RAChange %>% 
  group_by(COMMON_NAME,AbundType,Percent,Replicate,YearOnYear15pcDecline) %>% 
  tally()



