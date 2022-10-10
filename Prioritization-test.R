##Output prioritized SSS
##M Tarjan
##Oct 10, 2022

library(readxl)
library(tidyverse)

sss<-read_excel(path = "Data/Prioritization_Tool_11Aug2022.xlsx", sheet = "Data")
sss$Percent_EOs_BLM<-as.numeric(sss$Percent_EOs_BLM)
sss$Percent_Model_Area_BLM<-as.numeric(sss$Percent_Model_Area_BLM)

##source code for prioritization function
source('Prioritization-function.R', local = T)

results<-prioritize(species = sss$Scientific_Name)

##write to excel file
#library(xlsx)
#write.xlsx(results, file="prioritization-results-20221010.xlsx", sheetName="all_species", row.names=FALSE)
#write.xlsx(subset(results, Tier=="Tier 1"), file="prioritization-results-20221010.xlsx", sheetName="Tier1", append=TRUE, row.names=FALSE)