GSOResultsDir ='E:/FAMEshiny/./results/20210315_1222'
FireType ='High'
Comparison = 'Optimisation'
Classes = 'All'
Rule = 'Rule0'
dWt = 0.75
nrep = 100
nsim = 5
SpEFGLMU = read.csv('E:/FAMEshiny/GSOInputs/Spp_EFG_LMU.csv',na='NA')
GSOScen<-read.csv('E:/FAMEshiny/GSOInputs/LMU_Scenarios.csv',na='NA')
GSOArea<-read.csv('E:/FAMEshiny/GSOInputs/LMU_Area.csv',na='NA')
SurveyData<-read.csv('E:/FAMEshiny/GSOInputs/ObsData.csv',na='NA')
