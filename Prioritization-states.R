## Prioritization by state
## Oct 25, 2022
## M Tarjan

library(readxl)
library(tidyverse)

##source code for prioritization function
source('Prioritization-function.R', local = T)

## Get input data
sss <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/Random Test Species - Oct 2022/NatureServe - Random Species Run in Prioritization Tool_States_One Sheet_25Oct2022.xlsx", sheet = "Sheet1")

sss <- rename(sss, Practical_Cons_BLM_Score = `Practical Conservation`, `Keystone/Multispecies_Benefit_BLM_Score` = `Keystone/Multispecies benefit`, Partnering_Opps_BLM_Score = `Partnering Opportunities 1-5 Low High`) %>% mutate(Ave_Model_Review_Score = NA, Rank_Review_Year = NA, Percent_Model_Area_BLM = NA)

results<-dim(0)
## Loop through each state
states<-unique(sss$State)
for (j in 1:length(states)) {
  sss.temp <- subset(sss, State == states[j])
  sss.temp <- mutate(sss.temp, Percent_EOs_BLM = as.numeric(Percent_EOs_BLM_Statewide))
  
  results.temp<-prioritize(data = sss.temp) %>% rename(State_Tier = Tier) %>% select(-Percent_EOs_BLM)
  results <- rbind(results, results.temp)
}

write.csv(results, file=paste0("output/state-prioritization-results-",Sys.Date(),".csv"), row.names=FALSE)

#write.csv(results, file=paste0("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/Random Test Species - Oct 2022/state-prioritization-results-",Sys.Date(),".csv"), row.names=FALSE)
