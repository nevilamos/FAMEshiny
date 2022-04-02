library(tidyverse)
#library(stringr)
library(qs)
library(data.table)
#library(zoo)
library(broom)
#library(DBI)
#library(RSQLite)
system(
  "s3fs fame-obm -o use_cache=/tmp -o allow_other -o uid=1001 -o mp_umask=002 -o multireq_max=5 /home/rstudio/ShinyApps/FAME/fame-obm"
)
# read in lookup tables and compiled data tables-----

OBRM_GS_LUT<-fread("CustomCSV/OBRM_GS_LUT.csv")
OBRM_TFI_LUT<-fread("CustomCSV/OBRM_TFI_LUT_v2.csv")

##This could be fited up in the file you read in:
OBRM_TFI_LUT<-OBRM_TFI_LUT %>% 
  select(EFG_NAME,OBRM_CombThreshold_belowTFI,OBRM_CombThreshold_aboveTFI,OBRM_CombThreshold_BBTFI) %>%
  pivot_longer(!EFG_NAME,names_to="THRESHOLD_TYPE",values_to="THRESHOLD_VALUE" ) %>%
  mutate(TFI_STATUS= ifelse(THRESHOLD_TYPE=="OBRM_CombThreshold_belowTFI",
                            "BELOW_MIN_TFI",
                            ifelse(THRESHOLD_TYPE =="OBRM_CombThreshold_aboveTFI",
                                   "ABOVE_MAX_TFI",
                                   "BBTFI")))
EFG_DISTRICT_AREA<-read_csv("fame-obm/LUTS/EFG_DISTRICT_AREA.csv")
districtTab<-foreign::read.dbf("ReferenceShapefiles/LF_DISTRICT_with_PU_field.dbf") %>% select(2,3,7)
names(districtTab)<-c("FIRE_DISTRICT","FIRE_REGION","DISTRICT_NO")
efgTab<-read_csv("ReferenceTables/EFG_EVD_TFI.csv")%>% select(2,3)
names(efgTab)[1]<-"EFG"

#GS Results------------
# district level ------------------
#These are the big data tables only read in if required
GSSumAll<-qread("fame-obm/results/GSSumAll.qs")

names(GSSumAll)[3]<-"DISTRICT_NO"

#using Data.table here because of size of Table
GS_DISTRICT_SUM<-GSSumAll[,list(Hectares =sum(Hectares)),by=list(DISTRICT_NO,AbundType,Percent,Replicate,EFG_NAME,GS4_NO,SEASON)]

GS_DISTRICT_SUM <-GS_DISTRICT_SUM %>% left_join(districtTab)
fwrite(GS_DISTRICT_SUM,"fame-obm/results/GS_DISTRICT_SUM.csv")
#GS_DISTRICT_SUM<-fread("fame-obm/results/GS_DISTRICT_SUM.csv")

rm(GSSumAll)
gc()

#EXPAND to incude all GS4_NO for each EFG in all CASES 
#assumes that all GS4_NO are present for each EFG somewhere in the state.
GS_DISTRICT_SUM_EXPANDED<-GS_DISTRICT_SUM %>%
  expand(nesting(AbundType,Percent,Replicate,FIRE_DISTRICT,EFG_NAME),SEASON,GS4_NO) %>%
  left_join(GS_DISTRICT_SUM) %>% 
  mutate(Hectares = ifelse(is.na(Hectares),0,Hectares))#assigns zeros areas to GS$_NO that do not exist in data

GS_DISTRICT_SUM_FREQ<-GS_DISTRICT_SUM_EXPANDED%>% 
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,EFG_NAME,SEASON,GS4_NO) %>% 
  summarise(Hectares = sum(Hectares)) %>% 
  mutate(Freq = Hectares/sum(Hectares))%>% 
  left_join(OBRM_GS_LUT %>%
              select(EFG_NAME,GS4_NO,OBRMCombThrGSProp)) %>%
  mutate(Below_GS_Threshold = ifelse (Freq<OBRMCombThrGSProp,1,0))



fwrite(GS_DISTRICT_SUM_FREQ,"fame-obm/results/GS_DISTRICT_SUM_FREQ.csv")
gc()


GS_DISTRICT_EFG_Breach<-GS_DISTRICT_SUM_FREQ %>%
  ungroup() %>% 
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,SEASON,EFG_NAME) %>% 
  summarise(EFG_Breach= sum(Below_GS_Threshold),EFG_HA_IN_UNIT = sum(Hectares))
fwrite(GS_DISTRICT_EFG_Breach,"fame-obm/results/GS_DISTRICT_EFG_Breach.csv")

#this is where the minimum area of EFG per district needs to be changed see commented value and line below-------------------
#you do it by editing table EFG_DISTRICT_AREA whcih can be remade ( with all values at 0) as in the 4rows below.
# EFG_DISTRICT_AREA<-GS_DISTRICT_EFG_Breach %>%
#   ungroup() %>% 
#   select(FIRE_DISTRICT,EFG_NAME) %>% 
#   distinct() %>% 
#   mutate(EFG_MIN_AREA = 0)%>% right_join(districtTab)
# fwrite(EFG_DISTRICT_AREA,"fame-obm/LUTS/EFG_DISTRICT_AREA.csv")
EFG_DISTRICT_AREA<-read_csv("fame-obm/LUTS/EFG_DISTRICT_AREA.csv")

GS_DISTRICT_EFG_Breach_Sum<-GS_DISTRICT_EFG_Breach %>% 
  left_join(EFG_DISTRICT_AREA) %>%
  filter( EFG_HA_IN_UNIT > EFG_MIN_AREA ) %>% 
  summarise(Number_of_EFG_Breaching = sum(EFG_Breach>0), TotEFG = n())
fwrite(GS_DISTRICT_EFG_Breach_Sum,"fame-obm/GS_DISTRICT_EFG_Breach_Sum.csv")

#  Region level ----------------
GS_REGION_SUM<-GS_DISTRICT_SUM[,list(Hectares =sum(Hectares)),by=list(FIRE_REGION,AbundType,Percent,Replicate,EFG_NAME,GS4_NO,SEASON)]
fwrite(GS_REGION_SUM,"fame-obm/results/GS_REGION_SUM.csv")
#GS_REGION_SUM<-fread("fame-obm/results/GS_REGION_SUM.csv")


#EXPAND to include all GS4_NO for each EFG in all CASES 
#assumes that all GS4_NO are present for each EFG somewhere in the state.
GS_REGION_SUM_EXPANDED<-GS_REGION_SUM %>%
  expand(nesting(AbundType,Percent,Replicate,FIRE_REGION,EFG_NAME),SEASON,GS4_NO) %>%
  left_join(GS_REGION_SUM) %>% 
  mutate(Hectares = ifelse(is.na(Hectares),0,Hectares))#assigns zeros areas to GS$_NO that do not exist in data

GS_REGION_SUM_FREQ<-GS_REGION_SUM_EXPANDED%>% 
  group_by(FIRE_REGION,AbundType,Percent,Replicate,EFG_NAME,SEASON,GS4_NO) %>% 
  summarise(Hectares = sum(Hectares)) %>% 
  mutate(Freq = Hectares/sum(Hectares))%>% 
  left_join(OBRM_GS_LUT %>%
              select(EFG_NAME,GS4_NO,OBRMCombThrGSProp)) %>%
  mutate(Below_GS_Threshold = ifelse (Freq<OBRMCombThrGSProp,1,0))

fwrite(GS_REGION_SUM_FREQ,"fame-obm/results/GS_REGION_SUM_FREQ.csv")
gc()


GS_REGION_EFG_Breach<-GS_REGION_SUM_FREQ %>%ungroup()  %>%
  group_by(FIRE_REGION,AbundType,Percent,Replicate,SEASON,EFG_NAME) %>% 
  summarise(EFG_Breach= sum(Below_GS_Threshold),EFG_HA_IN_UNIT = sum(Hectares))
fwrite(GS_REGION_EFG_Breach,"fame-obm/results/GS_REGION_EFG_Breach.csv")

#this is where the minimum area of EFG per district needs to be changed see commented value and line below-------------------
#you do it by editing table EFG_DISTRICT_AREA whcih can be remade ( with all values at 0) as in the 4rows below.
# EFG_DISTRICT_AREA<-GS_DISTRICT_EFG_Breach %>%
#   ungroup() %>% 
#   select(FIRE_DISTRICT,EFG_NAME) %>% 
#   distinct() %>% 
#   mutate(EFG_MIN_AREA = 0)
# fwrite(EFG_DISTRICT_AREA,"fame-obm/LUTS/EFG_DISTRICT_AREA.csv")
EFG_DISTRICT_AREA<-read_csv("fame-obm/LUTS/EFG_DISTRICT_AREA.csv")

GS_REGION_EFG_Breach_Sum<-GS_REGION_EFG_Breach %>% 
  left_join(EFG_DISTRICT_AREA) %>%
  filter(EFG_HA_IN_UNIT>EFG_MIN_AREA) %>% 
  summarise(Number_of_EFG_Breaching = sum(EFG_Breach>0), TotEFG = n())
fwrite(GS_REGION_EFG_Breach_Sum,"fame-obm/GS_REGION_EFG_Breach_Sum.csv")

# State Level---------------------------
GS_STATE_SUM<-GS_REGION_SUM[,list(Hectares =sum(Hectares)),by=list(AbundType,Percent,Replicate,EFG_NAME,GS4_NO,SEASON)]
fwrite(GS_STATE_SUM,"fame-obm/results/GS_STATE_SUM.csv")
#GS_STATE_SUM<-fread("fame-obm/results/GS_STATE_SUM.csv")


#EXPAND to incude all GS4_NO for each EFG in all CASES 
#assumes that all GS4_NO are present for each EFG somewhere in the state.
GS_STATE_SUM_EXPANDED<-GS_STATE_SUM %>%
  expand(nesting(AbundType,Percent,Replicate,EFG_NAME),SEASON,GS4_NO) %>%
  left_join(GS_STATE_SUM) %>% 
  mutate(Hectares = ifelse(is.na(Hectares),0,Hectares))#assigns zeros areas to GS$_NO that do not exist in data

GS_STATE_SUM_FREQ<-GS_STATE_SUM_EXPANDED%>% 
  group_by(AbundType,Percent,Replicate,EFG_NAME,SEASON,GS4_NO) %>% 
  summarise(Hectares = sum(Hectares)) %>% 
  mutate(Freq = Hectares/sum(Hectares))%>% 
  left_join(OBRM_GS_LUT %>%
              select(EFG_NAME,GS4_NO,OBRMCombThrGSProp)) %>%
  mutate(Below_GS_Threshold = ifelse (Freq<OBRMCombThrGSProp,1,0))

fwrite(GS_STATE_SUM_FREQ,"fame-obm/results/GS_STATE_SUM_FREQ.csv")
gc()


GS_STATE_EFG_Breach<-GS_STATE_SUM_FREQ %>%ungroup()  %>%
  group_by(AbundType,Percent,Replicate,SEASON,EFG_NAME) %>% 
  summarise(EFG_Breach= sum(Below_GS_Threshold),EFG_HA_IN_UNIT = sum(Hectares))
fwrite(GS_STATE_EFG_Breach,"fame-obm/results/GS_STATE_EFG_Breach.csv")



GS_STATE_EFG_Breach_Sum<-GS_STATE_EFG_Breach %>% 
  summarise(Number_of_EFG_Breaching = sum(EFG_Breach>0), TotEFG = n())
fwrite(GS_STATE_EFG_Breach_Sum,"fame-obm/GS_STATE_EFG_Breach_Sum.csv")

# TFI results----------------
TFISumAll<-qread("fame-obm/results/TFISumAll.qs")
# names(TFISumAll)
#District level --------------------------

TFI_DISTRICT_SUM<- TFISumAll%>%
  filter(!is.na(DISTRICT_NO)) %>%
  filter(EFG_NAME!="") %>% 
  filter(TFI_STATUS != "NONE") %>% 
  filter(TFI_STATUS != "") %>% 
  group_by(DISTRICT_NO,AbundType,Percent,Replicate,EFG_NAME,TFI_STATUS,SEASON) %>% 
  summarise(Hectares =sum(Hectares)) %>% left_join(districtTab)


fwrite(TFI_DISTRICT_SUM,"fame-obm/results/TFI_DISTRICT_SUM.csv")
#TFI_DISTRICT_SUM<-fread("fame-obm/results/TFI_DISTRICT_SUM.csv")




TFI_DISTRICT_SUM_FREQ<-TFI_DISTRICT_SUM%>% 
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,EFG_NAME,SEASON,TFI_STATUS) %>% 
  summarise(Hectares = sum(Hectares)) %>% 
  mutate(Freq = Hectares/sum(Hectares)) %>% 
  left_join(OBRM_TFI_LUT[,c(1,3,4)]) %>% 
  mutate(Above_TFI_Threshold = ifelse (Freq>THRESHOLD_VALUE,1,0))
fwrite(TFI_DISTRICT_SUM_FREQ,"fame-obm/results/TFI_DISTRICT_SUM_FREQ.csv")


TFI_DISTRICT_EFG_Breach<-TFI_DISTRICT_SUM_FREQ %>%
  ungroup()  %>%
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,SEASON,EFG_NAME) %>% 
  summarise(EFG_Breach= sum(Above_TFI_Threshold,na.rm=T),EFG_HA_IN_UNIT = sum(Hectares,na.rm=T))
fwrite(TFI_DISTRICT_EFG_Breach,"fame-obm/results/TFI_DISTRICT_EFG_Breach.csv")

#this is where the minimum area of EFG per district needs to be changed see commented value and line below-------------------
#EFG_DISTRICT_AREA<-read_csv("fame-obm/LUTS/EFG_DISTRICT_AREA.csv")

TFI_DISTRICT_EFG_Breach_Sum<-TFI_DISTRICT_EFG_Breach %>% 
  left_join(EFG_DISTRICT_AREA) %>%
  filter(EFG_HA_IN_UNIT>EFG_MIN_AREA) %>% 
  summarise(Number_of_EFG_Breaching = sum(EFG_Breach>0), TotEFG = n())
fwrite(TFI_DISTRICT_EFG_Breach_Sum,"fame-obm/TFI_DISTRICT_EFG_Breach_Sum.csv")


# Region Level --------
TFI_REGION_SUM<- TFI_DISTRICT_SUM%>%
  filter(!is.na(FIRE_REGION)) %>%
  filter(EFG_NAME!="") %>% 
  filter(TFI_STATUS != "NONE") %>% 
  filter(TFI_STATUS != "") %>% 
  group_by(FIRE_REGION,AbundType,Percent,Replicate,EFG_NAME,TFI_STATUS,SEASON) %>% 
  summarise(Hectares =sum(Hectares))


fwrite(TFI_REGION_SUM,"fame-obm/results/TFI_REGION_SUM.csv")
#TFI_REGION_SUM<-fread("fame-obm/results/TFI_REGION_SUM.csv")




TFI_REGION_SUM_FREQ<-TFI_REGION_SUM%>% 
  group_by(FIRE_REGION,AbundType,Percent,Replicate,EFG_NAME,SEASON,TFI_STATUS) %>% 
  summarise(Hectares = sum(Hectares)) %>% 
  mutate(Freq = Hectares/sum(Hectares)) %>% 
  left_join(OBRM_TFI_LUT[,c(1,3,4)]) %>% 
  mutate(Above_TFI_Threshold = ifelse (Freq>THRESHOLD_VALUE,1,0))
fwrite(TFI_REGION_SUM_FREQ,"fame-obm/results/TFI_REGION_SUM_FREQ.csv")


TFI_REGION_EFG_Breach<-TFI_REGION_SUM_FREQ %>%ungroup()  %>%
  group_by(FIRE_REGION,AbundType,Percent,Replicate,SEASON,EFG_NAME) %>% 
  summarise(EFG_Breach= sum(Above_TFI_Threshold,na.rm=T),EFG_HA_IN_UNIT = sum(Hectares,na.rm=T))
fwrite(TFI_REGION_EFG_Breach,"fame-obm/results/TFI_REGION_EFG_Breach.csv")

#this is where the minimum area of EFG per district needs to be changed see commented value and line below-------------------
#EFG_DISTRICT_AREA<-read_csv("fame-obm/LUTS/EFG_DISTRICT_AREA.csv")
# EFG_REGION_AREA<-EFG_DISTRICT_AREA %>%
#   select(-DISTRICT_NO,-FIRE_DISTRICT) %>% 
#   distinct()
#fwrite(EFG_REGION_AREA,"fame-obm/LUTS/EFG_REGION_AREA.csv")
EFG_REGION_AREA<-fread("fame-obm/LUTS/EFG_REGION_AREA.csv")

TFI_REGION_EFG_Breach_Sum<-TFI_REGION_EFG_Breach %>% 
  left_join(EFG_REGION_AREA) %>%
  filter(EFG_HA_IN_UNIT>EFG_MIN_AREA) %>% 
  summarise(Number_of_EFG_Breaching = sum(EFG_Breach>0), TotEFG = n())
fwrite(TFI_REGION_EFG_Breach_Sum,"fame-obm/TFI_REGION_EFG_Breach_Sum.csv")

# State level ------

TFI_STATE_SUM<- TFI_DISTRICT_SUM%>%
  filter(!is.na(FIRE_DISTRICT)) %>%
  filter(EFG_NAME!="") %>% 
  filter(TFI_STATUS != "NONE") %>% 
  filter(TFI_STATUS != "") %>% 
  group_by(AbundType,Percent,Replicate,EFG_NAME,TFI_STATUS,SEASON) %>% 
  summarise(Hectares =sum(Hectares))


fwrite(TFI_STATE_SUM,"fame-obm/results/TFI_STATE_SUM.csv")
#TFI_STATE_SUM<-fread("fame-obm/results/TFI_STATE_SUM.csv")




TFI_STATE_SUM_FREQ<-TFI_STATE_SUM%>% 
  group_by(AbundType,Percent,Replicate,EFG_NAME,SEASON,TFI_STATUS) %>% 
  summarise(Hectares = sum(Hectares)) %>% 
  mutate(Freq = Hectares/sum(Hectares)) %>% 
  left_join(OBRM_TFI_LUT[,c(1,3,4)]) %>% 
  mutate(Above_TFI_Threshold = ifelse (Freq>THRESHOLD_VALUE,1,0))
fwrite(TFI_STATE_SUM_FREQ,"fame-obm/results/TFI_STATE_SUM_FREQ.csv")


TFI_STATE_EFG_Breach<-TFI_STATE_SUM_FREQ %>%ungroup()  %>%
  group_by(AbundType,Percent,Replicate,SEASON,EFG_NAME) %>% 
  summarise(EFG_Breach= sum(Above_TFI_Threshold,na.rm=T),EFG_HA_IN_UNIT = sum(Hectares,na.rm=T))
fwrite(TFI_STATE_EFG_Breach,"fame-obm/results/TFI_STATE_EFG_Breach.csv")


TFI_STATE_EFG_Breach_Sum<-TFI_STATE_EFG_Breach %>% 
  summarise(Number_of_EFG_Breaching = sum(EFG_Breach>0), TotEFG = n())
fwrite(TFI_STATE_EFG_Breach_Sum,"fame-obm/TFI_STATE_EFG_Breach_Sum.csv")

#BBTFI results -------
# STATE level ----------

# getting areas of EFG at State, region and local from previous queries because cannot be calculated from BBTFI figures------
DISTRICT_EFG_AREA<-GS_DISTRICT_EFG_Breach %>%
  ungroup() %>%
  select(FIRE_DISTRICT,EFG_NAME,EFG_HA_IN_UNIT,EFG_HA_IN_UNIT)%>%
  distinct()

DISTRICT_TOT_EFG<-TFI_DISTRICT_EFG_Breach_Sum %>%
  ungroup() %>%
  select(FIRE_DISTRICT,TotEFG) %>%
  distinct()

REGION_EFG_AREA<-TFI_REGION_EFG_Breach %>%
  ungroup() %>%
  select(FIRE_REGION,EFG_HA_IN_UNIT,EFG_NAME)%>%
  distinct()

REGION_TOT_EFG<-TFI_REGION_EFG_Breach_Sum %>%
  ungroup() %>%
  select(FIRE_REGION,TotEFG) %>%
  distinct()

STATE_EFG_AREA<-TFI_STATE_EFG_Breach %>%
  ungroup() %>%
  select(EFG_HA_IN_UNIT,EFG_NAME)%>%
  distinct()

# read in big BBTFI table#


BBTFISumAll<-qread("fame-obm/results/BBTFISumAll.qs")
names(BBTFISumAll)

BBTFI_DISTRICT_SUM<-BBTFISumAll[,list(Hectares =sum(Hectares)),by=list(DISTRICT_NO,AbundType,Percent,Replicate,EFG_NAME,TBTFI,SEASON)]

BBTFI_DISTRICT_SUM<-BBTFI_DISTRICT_SUM %>%
  filter(!is.na(TBTFI)) %>%
  filter(TBTFI == 1) %>%
  left_join(districtTab)


fwrite(BBTFI_DISTRICT_SUM,"fame-obm/results/BBTFI_DISTRICT_SUM.csv")

BBTFI_DISTRICT_SUM_FREQ<-BBTFI_DISTRICT_SUM %>% 
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,EFG_NAME,SEASON) %>% 
  summarise(Hectares_BBTFI_in_SEASON = sum(Hectares)) %>% 
  ungroup() %>% 
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,EFG_NAME)%>% 
  left_join(DISTRICT_EFG_AREA)%>% 
  arrange(FIRE_DISTRICT,AbundType,Percent,Replicate,EFG_NAME,EFG_HA_IN_UNIT,SEASON) %>% 
  mutate(Hectares_BBTFI_cumulative_to_SEASON = cumsum(Hectares_BBTFI_in_SEASON))  %>% 
  mutate(Prop_BBTFI_in_Season = Hectares_BBTFI_in_SEASON/EFG_HA_IN_UNIT ) %>% 
  mutate(Prop_BBTFI_cumulative_to_Season = Hectares_BBTFI_cumulative_to_SEASON/EFG_HA_IN_UNIT ) %>% 
  left_join(OBRM_TFI_LUT %>%
              filter(TFI_STATUS == "BBTFI")%>%
              select(EFG_NAME,THRESHOLD_VALUE))%>% 
  mutate(Above_BBTFI_Threshold_in_Season = ifelse (Prop_BBTFI_in_Season>THRESHOLD_VALUE,1,0)) %>% 
  mutate(Above_BBTFI_Threshold_cumulative_to_Season = ifelse (Prop_BBTFI_cumulative_to_Season > THRESHOLD_VALUE,1,0))

fwrite(BBTFI_DISTRICT_SUM_FREQ,"fame-obm/results/BBTFI_DISTRICT_SUM_FREQ.csv")


BBTFI_DISTRICT_EFG_Breach<-BBTFI_DISTRICT_SUM_FREQ %>%
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,SEASON,EFG_NAME,EFG_HA_IN_UNIT) %>% 
  summarise(EFG_Breach_Area_BBTFI_in_Season= sum(Above_BBTFI_Threshold_in_Season,na.rm=T),
            EFG_Breach_Area_BBTFI_cumulative_to_Season = sum(Above_BBTFI_Threshold_cumulative_to_Season,na.rm=T)) %>% 
  arrange(FIRE_DISTRICT,AbundType,Percent,Replicate,EFG_NAME,SEASON) 
fwrite(BBTFI_DISTRICT_EFG_Breach,"fame-obm/results/BBTFI_DISTRICT_EFG_BreachAll_TBTFI.csv")

#this is where the minimum area of EFG per district needs to be changed see commented value and line below-------------------
#EFG_DISTRICT_AREA<-read_csv("fame-obm/LUTS/EFG_DISTRICT_AREA.csv")

BBTFI_DISTRICT_EFG_Breach_Sum<-BBTFI_DISTRICT_EFG_Breach %>% 
  ungroup() %>% 
  group_by(FIRE_DISTRICT,AbundType,Percent,Replicate,SEASON) %>% 
  left_join(EFG_DISTRICT_AREA) %>%
  filter(EFG_HA_IN_UNIT>EFG_MIN_AREA) %>% 
  summarise(Number_of_EFG_Breaching_Area_BBTFI_in_Season = sum(EFG_Breach_Area_BBTFI_in_Season > 0),
            Number_of_EFG_Breaching_Area_BBTFI__cumulative_to_Season = sum(EFG_Breach_Area_BBTFI_cumulative_to_Season > 0)) %>% 
  left_join(DISTRICT_TOT_EFG) %>% 
  arrange(FIRE_DISTRICT,AbundType,Percent,Replicate,SEASON)
fwrite(BBTFI_DISTRICT_EFG_Breach_Sum,"fame-obm/BBTFI_DISTRICT_EFG_Breach_Sum.csv")
