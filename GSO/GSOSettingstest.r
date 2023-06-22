FireType ='High'
Comparison = 'Optimisation'
Classes = 'All'
Rule = 'Rule0'
dWt = 0.75
nrep = 100
nsim = 5
SpEFGLMU = read_csv('../GSOInputs/Demo_Spp_EFG_LMU.csv',na='NA')
GSOScen<-read_csv('../GSOInputs/1LMU_Scenarios.csv',na='NA')
GSOArea<-read_csv('../GSOInputs/Demo_LMU_Area.csv',na='NA')
SurveyData<-read_csv('../GSOInputs/Demo_ObsData.csv',na='NA')
EFG_TSF_4GS <-
  read.csv("E:/FAMEshiny/ReferenceTables/EFG_TSF_4GScorrectedAllEFGto400yrsV2.csv")[, c('EFG_NO', 'GS4_NO', "YSF")]
GSOResultsDir ="../results/gsotest"
if(!dir.exists(GSOResultsDir)){dir.create(GSOResultsDir)}
