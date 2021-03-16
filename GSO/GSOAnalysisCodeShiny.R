##########################################################################################################
######################### R script for running GSO in R ##################################################
##########################################################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(nloptr)
library(ggplot2)

source("./GSO_Functions.R")
#source(file.path("GSO","tempsettings.r"))
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
FaunaCodes = read_excel("VBA_FAUNA.xlsx", sheet = 1)
#%>%
#mutate(across(c(SPEC_NO,VBA_CODE),as.integer))


## Species to EFG to LMU data
#SpEFGLMU <- read.csv(input$spEFGLMU,na='NA')
#SpEFGLMU <- read_excel(file.path("GSO",'Spp_EFG_LMU.xlsx'),sheet=1 ,na='NA')
names(SpEFGLMU)[1] <- 'COMMON_NAME'
SpEFGLMU <- left_join(SpEFGLMU, FaunaCodes, by = 'COMMON_NAME') %>%
  mutate(efgID = paste0('EFG', str_sub(as.character(EFG_NO + 100), 2, 3)),
         efgIDsp = paste(efgID, VBA_CODE, sep = '_'))

## EFGs areas and scenarios
#GSOScen<-read.csv(input$lmuScenarios,na='NA')
#GSOScen <- read_excel('../GSO/ScenariosForGSO.xlsx',sheet='LMU Scenarios', na='NA')
GSOScen$EFG <- paste0('EFG', str_sub(100 + GSOScen$EFG_NO, -2, -1))
GSOScen$efgID <-
  paste(GSOScen$EFG, str_sub(GSOScen$GS_NAME, 1, 1), sep = '_')
#GSOArea<-read.csv(input$lmuArea)
#GSOArea <- read_excel('../GSO/ScenariosForGSO.xlsx',sheet='LMU Area', na='NA')

## EFG full names
GSONames <-
  read_excel('TBL_VegetationGrowthStages.xlsx',
             sheet = 1,
             na = 'NA')
GSONames <-
  GSONames %>% group_by(EFG_NAME) %>% summarise(efgID = paste0('EFG', str_sub(100 +
                                                                                first(EFG_NO), -2, -1)))

## Suvey data, long format. Each species and data point has a row.

#SurveyData <- read_csv('../GSO/ObsData.xlsx',sheet='ObsData' ,na='NA')
#summary(SurveyData)

## Expert opinion data, each efg period has a column, each data point has a row species
ExpertData = read_excel('Reference data.xlsx', sheet = 'Ordinal expert data', na =
                          '')
StageNames <- str_sub(unique(GSOScen$GS_NAME), 1, 1)
for (i in 3:ncol(ExpertData)) {
  names(ExpertData)[i] <-
    paste(str_sub(names(ExpertData)[i], 1, 5), StageNames[((i + 1) %% length(StageNames) + 1)], sep =
            '_')
}
names(ExpertData)[1] <- 'COMMON_NAME'
# summary(ExpertData)

## Expert opinion data as an amount of birds
ExpertScore <-
  read_excel('ExpertEstimate.xlsx', sheet = 1, na = '') %>% arrange(Code) %>% left_join(FaunaCodes, by =
                                                                                          'COMMON_NAME') %>%
  dplyr::select(Code, COMMON_NAME, `0`, `1`, `2`, `3`, VBA_CODE)
ExpertScore$Code <- toupper(ExpertScore$Code)
names(ExpertScore) <-  c('SPECIES_CODE',
                         'COMMON_NAME',
                         'None',
                         'Few',
                         'Some',
                         'Lots',
                         'VBA_CODE')


#### Construct data into the required format ####
## Add the GS_Name to the survey data
SurveyData$GS_NAME <- NA
for (i in 1:nrow(SurveyData)) {
  SurveyData$GS_NAME[i] <-
    GSfn(SurveyData$EFG_NO[i], SurveyData$TSF[i])
}
SurveyData$efgID <-
  paste0(
    'EFG',
    ifelse(SurveyData$EFG_NO > 9, '', '0'),
    SurveyData$EFG_NO,
    '_',
    str_sub(SurveyData$GS_NAME, 1, 1)
  )
EFGData <-SurveyData %>%
  filter(!is.na(GS_NAME)) %>% 
  group_by(efgID) %>% 
  summarise(EFG =  first(EFG_NO), GS = first(GS_NAME))

## Convert the expert data to longer form, each row is an observation of species at a particular EFG GS
ExpertData2 <-
  left_join(ExpertData,
            dplyr::select(FaunaCodes, COMMON_NAME, VBA_CODE),
            by = 'COMMON_NAME') %>%
  filter(VBA_CODE %in% ExpertScore$VBA_CODE)

for (i in 1:nrow(ExpertData2)) {
  for (j in 3:(ncol(ExpertData2) - 1)) {
    ExpertData2[i, j] <-
      ConvertExpert(ExpertData2$VBA_CODE[i], ExpertData2[i, j])
  }
}
ExpertData2 <-
  ExpertData2[which(ExpertData2$VBA_CODE %in% unique(SpEFGLMU$VBA_CODE)),
              c(1, ncol(ExpertData2), 2:(ncol(ExpertData2) - 1))]
ExpertData2 <-  ExpertData2 %>% 
  gather('efgID', 'Response', 4:ncol(ExpertData2))

## Combine the survey and expert data frames together to be used in conversion and GSO
WorkData <-  rbind(
    data.frame(Source = 'Expert', SurvID = 'Expert', ExpertData2[, c(1:2, 4, 3, 5)]),
    data.frame(Source = 'Survey', SurvID = SurveyData$SurvID, SurveyData[, c(4, 3, 11, 7, 8)])
  )
WorkData <-  WorkData %>% 
  separate(efgID, c('EFG', 'GS'), sep = '_', remove = FALSE) %>%
  filter(paste(EFG, VBA_CODE, sep = '_') %in% 
           unique(SpEFGLMU$efgIDsp[which(SpEFGLMU$EFG_NO %in% GSOArea$EFG_NO)]))

if (!Classes == "All") {
  WorkData <- WorkData %>% 
    left_join(FaunaCodes[, 4:5], by = 'VBA_CODE') %>% 
    filter(DIVNAME %in% Classes)
}
Usedefgs <-
  GSONames$efgID[which(as.numeric(str_sub(GSONames$efgID, -2, -1)) %in% GSOArea$EFG_NO)]

NoData <-  SpEFGLMU %>% 
  filter(EFG_NO %in% GSOArea$EFG_NO &
           !(efgIDsp %in% 
               unique( paste(WorkData$EFG[which(WorkData$FireType == FireType)],
                             WorkData$VBA_CODE[which(WorkData$FireType == FireType)], sep = '_')
                        ))) %>%
  group_by(COMMON_NAME, VBA_CODE) %>% 
  summarise(n = n()) %>% 
  left_join(FaunaCodes[, 3:5], by = 'VBA_CODE')
head(NoData)
write.csv(NoData[, c(4, 1:2, 5)], file.path(GSOResultsDir, ('List of species without suitable data.csv')))


#### Model to use ####
## Inequality constraints.
## Since the objective is monotone, we can replace the proportion
## equality constraint with <=.
## Per-species limits should be do-able in the same form.

eval_cs <- function(x, spp) {
  n <- length(StageNames)
  Res <- rep(NA, length(x) / n)
  for (i in 1:length(Res)) {
    Res[i] <- sum(x[1:n + (i - 1) * n]) - 1
  }
  return(Res)
}

## Jacobian (partial derivative matrix) for the inequalities
## Since each constraint is linear, the Jacobian is just the
## coefficients of each varible in eval_cs.
eval_cs_jac <- function(x, spp) {
  n <- length(StageNames)
  return(matrix(
    rep(c(rep(1, n), rep(0, length(
      x
    ))), length(x) / n),
    ncol = length(x),
    nrow = length(x) / n,
    byrow = TRUE
  ))
}



gso <- function(spp) {
  x0 <-
    const_vector(ncol(spp), 1 / ncol(spp)) ## *** Corresponds to the number of columns in the input ***
  run <-
    nloptr(
      x0 = x0,
      eval_f = geomean.obj,
      eval_grad_f = geomean.grad,
      lb = const_vector(length(x0), 0),
      ub = const_vector(length(x0), 1),
      eval_g_ineq = eval_cs,
      eval_jac_g_ineq = eval_cs_jac,
      opts = list(
        ## Algorithm to use.
        "algorithm" = "NLOPT_LD_MMA",
        ## Termination conditions
        "maxeval" = 1000,
        "ftol_rel" = 1.0e-15,
        "xtol_rel" = 1.0e-8,
        ## Suppress output during optimization
        "print_level" = 0
      ),
      spp = spp
    )
  ## Find the objective value corresponding to the optimal solution
  res <- rbind(c(run$solution, geomean.fun(run$solution, spp)))
  colnames(res) <- c(names(spp), "geom")
  res <- data.frame(res)
  return (res)
}


## Function to run repeated samlpes and calculate 95% CIs
OptRunCI <-
  function(data,
           efgs,
           Scen = GSOScen ,
           area = GSOArea,
           rule = 'Rule0',
           FiTy = 'High',
           Weight = 0.5,
           NRep = 20,
           N = 10,
           Comp = Comparison) {
    OptData <-
      DataGen(
        data,
        efgs = efgs,
        rule = rule,
        FT = FiTy,
        Wt = Weight,
        Rand = TRUE,
        NRep = NRep
      )[[1]]
    resultn <- gso(spp = OptData[, -1])
    resultS <- Scenarios(OptData, Scen = Scen, efg = efgs)
    resultS2 <- as.data.frame(t(c(resultS$GMA, resultn$geom)))
    names(resultS2) <-
      c(levels(resultS$Scenario)[resultS$Scenario], 'Optimisation')
    SpecRes <- SppRes(OptData, resultn, Scen = Scen, TArea = area)
    SpList <- array(NA, dim = c(nrow(SpecRes), ncol(SpecRes), N))
    SpList[, , 1] <- as.matrix(SpecRes)
    for (i in 2:N) {
      OptData <-
        DataGen(
          data,
          efgs = efgs,
          rule = rule,
          FT = FiTy,
          Wt = Weight,
          Rand = TRUE,
          NRep = NRep
        )[[1]]
      resultn[i, ] <- gso(spp = OptData[, -1])
      resultS <- Scenarios(OptData, Scen = Scen, efg = efgs)
      resultS2[i, ] <- t(c(resultS$GMA, resultn$geom[i]))
      SpList[, , i] <-
        as.matrix(SppRes(OptData, resultn, Scen = Scen, TArea = area))
    }
    res <- as.data.frame(t(apply(resultn, 2, CIM)))
    row.names(res)[nrow(res)] <- c('Optimisation')
    Scens <-
      as.data.frame(t(apply(resultS2 / resultS2[, which(names(resultS2) == Comp)], 2, CIM)))
    Scens0 <- as.data.frame(t(apply(resultS2, 2, CIM)))
    names(res) <- c('Prop', 'LB', 'UB')
    names(Scens) <- c('Prop', 'LB', 'UB')
    resultn2 <-
      data.frame(Scenario = rule, efgID = colnames(resultn), res)[-length(resultn), ]
    resultn2$EFG <- as.numeric(str_sub(resultn2$efgID, 4, 5))
    resultn2$GS <- StageNames
    resultsSp <- data.frame(apply(SpList, c(1, 2), mean))
    colnames(resultsSp) <- names(SpecRes)
    resultsSp <- left_join(resultsSp, FaunaCodes[, -1])
    return(list(resultn2, res[nrow(res), ], Scens, Scens0, resultsSp))
    # return(resultn)
  }

#### Optimisation ####
## This conducts "bootstrapping" so that we get not only the optimal solution,
## but the 95% credible interval as well
OptCI <-
  OptRunCI(
    WorkData,
    Usedefgs,
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
write.csv(OptSpp, file.path(GSOResultsDir, ('GSO Species Changes.csv')), row.names = FALSE)

SppDec <-
  data.frame(
    SpecDec = c('More than 20%', 'More than 50%', '100% (local extinction)'),
    rbind(
      colSums(OptSpp[, -1:-4] < -20),
      colSums(OptSpp[, -1:-4] < -50),
      colSums(OptSpp[, -1:-4] == -100)
    )
  )
names(SppDec) <- c('Species declining by', names(OptSpp)[-1:-4])
