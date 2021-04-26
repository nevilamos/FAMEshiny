##########################################################################################################
######################### R script for running GSO in R ##################################################
##########################################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(nloptr)
library(ggplot2)

source("GSO_Functions.R")
#### Pre-written text ####
## Pre-written text to be selected, depending on users choices in markdown document.

RuleText <-
  data.frame(
    Rule = c(
      'Rule0',
      'Rule1',
      'Rule1a',
      'Rule1b',
      'Rule1c',
      'Rule2',
      'Rule2a',
      'Rule2b',
      'Rule2c',
      'Rule3',
      'Rule3a',
      'Rule3b',
      'Rule3c'
    ),
    Text = c(
      'expert opinion and no observational data.',
      'mean of observed data across surveys in that *EFG* growth stage where survey data is available for that species across all *EFG*s of interest, otherwise use the expert opinion.',
      'maximum of observed data across surveys in that *EFG* growth stage where survey data is available for that species across all *EFG*s of interest, otherwise use the expert opinion.',
      'median of observed data across surveys in that *EFG* growth stage where survey data is available for that species across all *EFG*s of interest, otherwise use the expert opinion.',
      'upper quartile of observed data across surveys in that *EFG* growth stage where survey data is available for that species across all *EFG*s of interest, otherwise use the expert opinion.',
      paste0(
        'weighted mean of the mean of the survey data and the expert opinion, with ',
        100 * dWt,
        '% of the weight going to the survey data, otherwise use the expert opinion.'
      ),
      paste0(
        'weighted mean of the maximum of the survey data and the expert opinion, with ',
        100 * dWt,
        '% of the weight going to the survey data, otherwise use the expert opinion.'
      ),
      paste0(
        'weighted mean of the median of the survey data and the expert opinion, with ',
        100 * dWt,
        '% of the weight going to the survey data, otherwise use the expert opinion.'
      ),
      paste0(
        'weighted mean of the upper quartile of the survey data and the expert opinion, with ',
        100 * dWt,
        '% of the weight going to the survey data, otherwise use the expert opinion.'
      ),
      'mean of observed data across surveys in that *EFG* growth stage and no expert opinion.',
      'maximum of observed data across surveys in that *EFG* growth stage and no expert opinion.',
      'median of observed data across surveys in that *EFG* growth stage and no expert opinion.',
      'upper quartile of observed data across surveys in that *EFG* growth stage and no expert opinion.'
    )
  )

#### Data loading ####
## Common names and codes for fauna
FaunaCodes = read_csv("../ReferenceTables/FAME_TAXON_LIST.csv")

## Species to EFG to LMU data
#SpEFGLMU <- read_csv(input$spEFGLMU,na='NA')
#SpEFGLMU <- read_excel(file.path("GSO",'Spp_EFG_LMU.xlsx'),sheet=1 ,na='NA')
names(SpEFGLMU)[1] <- 'COMMON_NAME'
SpEFGLMU <- SpEFGLMU %>%
  mutate(EFG_GS = paste0('EFG', str_sub(as.character(EFG_NO + 100), 2, 3)),
         EFG_GSsp = paste(EFG_GS, TAXON_ID, sep = '_'))

## EFGs areas and scenarios
#GSOScen<-read_csv(input$lmuScenarios,na='NA')
#GSOScen <- read_excel('../GSO/ScenariosForGSO.xlsx',sheet='LMU Scenarios', na='NA')
GSOScen$EFG <- paste0('EFG', str_sub(100 + GSOScen$EFG_NO, -2, -1))
GSOScen$EFG_GS <-
  paste(GSOScen$EFG, str_sub(GSOScen$GS_NAME, 1, 1), sep = '_')
#GSOArea<-read_csv(input$lmuArea)
#GSOArea <- read_excel('../GSO/ScenariosForGSO.xlsx',sheet='LMU Area', na='NA')

# ## EFG full names
# GSONames <-
#   read_csv("../ReferenceTables/TBL_VegetationGrowthStages.csv",
#              na = 'NA')
# GSONames <-  GSONames %>% 
#   group_by(EFG_NAME) %>% 
#   summarise(EFG_GS = paste0('EFG', str_sub(100 + first(EFG_NO), -2, -1)))


#### Construct data into the required format ####-----------------------
## Add the GS_Name to the survey data
SurveyData$TSF <- floor(SurveyData$TSF)
GS_NAMES=c("Juvenile","Adolescent","Mature","Old")

SurveyData<-left_join(SurveyData,EFG_TSF_4GS%>%
                        rename(TSF=YSF,GS_NAME=GS4_NO))
SurveyData$GS_NAME<-GS_NAMES[SurveyData$GS_NAME]

SurveyData$EFG_GS <-
  paste0(
    'EFG',
    ifelse(SurveyData$EFG_NO > 9, '', '0'),
    SurveyData$EFG_NO,
    '_',
    str_sub(SurveyData$GS_NAME, 1, 1)
  )
EFGData <-SurveyData %>%
  filter(!is.na(GS_NAME)) %>% 
  group_by(EFG_GS) %>% 
  summarise(EFG =  first(EFG_NO), GS = first(GS_NAME))

## Reformatting of OrdinalExpertLong Reference table so it can be used in Aspatial GSO ( rather than previous versions separate tables)
OrdinalExpertLong <-
  read_csv("../ReferenceTables/OrdinalExpertLong.csv")[,c("COMMON_NAME",
                                                          "TAXON_ID",
                                                          "FireType",
                                                          "EFG_GS",
                                                          "Abund")]%>%
  rename(Response=Abund)%>%
  filter(TAXON_ID %in% unique(SpEFGLMU$TAXON_ID))

# Read revised Expert score values to be used in conversion by ConvertExpert()
ExpertScore <-read_csv('../ReferenceTables/ExpertEstimate.csv', na = '') 

#conversion via none little some most all scale after pivot long
#ideally the ConvertExpert() could be vectorised to avoid this loop
for (i in 1:nrow(OrdinalExpertLong)) {
    OrdinalExpertLong$Response[i]<-ConvertExpert(as.character(OrdinalExpertLong$TAXON_ID[i]), OrdinalExpertLong$Response[i])
  }
StageNames <- c("J", "A", "M", "O")
## Combine the survey and expert data frames together to be used in conversion and GSO
WorkData <-  rbind(
    data.frame(Source = 'Expert', SurvID = 'Expert', OrdinalExpertLong[, c(1:2, 4, 3, 5)]),
    data.frame(Source = 'Survey', SurvID = SurveyData$SurvID, SurveyData[, c(4, 3, 11, 7, 8)])
  )
WorkData <-  WorkData %>% 
  separate(EFG_GS, c('EFG', 'GS'), sep = '_', remove = FALSE) %>%
  filter(paste(EFG, TAXON_ID, sep = '_') %in% 
           unique(SpEFGLMU$EFG_GSsp[which(SpEFGLMU$EFG_NO %in% GSOArea$EFG_NO)]))

if (!Classes == "All") {
  WorkData <- WorkData %>% 
    left_join(FaunaCodes[, c("DIVNAME","TAXON_ID")], by = 'TAXON_ID') %>% 
    filter(DIVNAME %in% Classes)
}

## get UsedEFGs - there must be a better way to do this next bit to get usedEFGnames at the moment relying on TBL_VegetationGrowthStages.csv which is only used for this purpose - should be able to get wome other way from EFG in one of the other tables
# GSONames <-
#   read_csv("../ReferenceTables/TBL_VegetationGrowthStages.csv",
#              na = 'NA')
# GSONames <-  GSONames %>%
#   group_by(EFG_NAME) %>%
#   summarise(EFG_GS = paste0('EFG', str_sub(100 + first(EFG_NO), -2, -1)))
# 
# Usedefgs <-
#   GSONames$EFG_GS[which(as.numeric(str_sub(GSONames$EFG_GS, -2, -1)) %in% GSOArea$EFG_NO)]
Usedefgs <-paste0("EFG",str_pad(GSOArea$EFG_NO, 2, pad = "0"))


NoData <-  SpEFGLMU %>% 
  filter(EFG_NO %in% GSOArea$EFG_NO &
           !(EFG_GSsp %in% 
               unique( paste(WorkData$EFG[which(WorkData$FireType == FireType)],
                             WorkData$TAXON_ID[which(WorkData$FireType == FireType)], sep = '_')
                        ))) %>%
  group_by(COMMON_NAME, TAXON_ID) %>% 
  summarise(n = n()) %>% 
  left_join(FaunaCodes[, c("SCIENTIFIC_NAME","TAXON_ID","DIVNAME")], by = 'TAXON_ID')

write_csv(NoData[, c(4, 1:2,5)], file.path(GSOResultsDir, ('List of species without suitable data.csv')))



#### Optimisation ####-------------------------
## This conducts "bootstrapping" so that we get not only the optimal solution,
## but the 95% credible interval as well
OptCI <-
  OptRunCI(
    data=WorkData,
    efgs=Usedefgs,
    area = GSOArea,
    rule = Rule,
    FiTy = FireType,
    Weight = dWt,
    N = nsim,
    NRep = nrep
  )

OptCIS <- OptCI[[1]]
OptCI0 <- OptCI[[2]]
OptCIC <- OptCI[[3]]
OptCIR <- OptCI[[4]]
OptCISp <- OptCI[[5]]
OptCIS$GS <- factor(OptCIS$GS, levels = StageNames)

## Comparisons to other scenarios
OptSpp <-
  OptCISp[, c(ncol(OptCISp) - 1,
              ncol(OptCISp) - 2,
              1,
              ncol(OptCISp),
              2:(ncol(OptCISp) - 3))]
names(OptSpp)[-1:-4] <- row.names(OptCIC)
OptSpp[, -1:-4] <-
  round(100 * (OptSpp[, -1:-4] / OptSpp[, which(names(OptSpp) == Comparison)] -
                 1), 1)
OptSpp <- OptSpp[, -which(names(OptSpp) == Comparison)]
write_csv(OptSpp, file.path(GSOResultsDir, ('GSO Species Changes.csv')))

SppDec <-
  data.frame(
    SpecDec = c('More than 20%', 'More than 50%', '100% (local extinction)'),
    
    rbind(
      colSums(as.matrix(OptSpp[, -1:-4] < -20)),
      colSums(as.matrix(OptSpp[, -1:-4] < -50)),
      colSums(as.matrix(OptSpp[, -1:-4] == -100))
    )
  )
names(SppDec) <- c('Species declining by', names(OptSpp)[-1:-4])

