##Output prioritized SSS
##M Tarjan
##Oct 10, 2022

library(readxl)
library(tidyverse)
library(googlesheets4)
library(openxlsx)

#sss<-read_excel(path = "Data/Prioritization_Tool_11Aug2022.xlsx", sheet = "Data")
#sss<-read_excel(path = "Data/NatureServe - Random Test Species - National Data_19 Oct 2022.xlsx", sheet= "Sheet1")
sss<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_2023-02-02")
sss$USFWS_Recovery_Priority_Num<-as.character(sss$USFWS_Recovery_Priority_Num)
sss$USFWS_Recovery_Priority_Num[which(sss$USFWS_Recovery_Priority_Num == "NULL")] <- NA

#sss$Percent_EOs_BLM<-as.numeric(sss$Percent_EOs_BLM)
#sss$Percent_Model_Area_BLM<-as.numeric(sss$Percent_Model_Area_BLM)

##source code for prioritization function
source('shiny/SSS-Prioritization-Tool/Prioritization-function.R', local = T)

results<-prioritize(data = sss)

sheet_write(data = results, ss = "https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet = paste0("ESA_spp_", Sys.Date()))

# write.csv(results, file=paste0("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/prioritization-results-",Sys.Date(),".csv"), row.names=FALSE)

## Format results for BLM
results_by_tier <- data.frame(matrix(NA, nrow=max(table(results$Tier)), ncol=5))
names(results_by_tier) <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Data deficient")
for (j in 1:ncol(results_by_tier)) {
  tier.temp <- names(results_by_tier)[j]
  spp.temp <- subset(results, Tier == tier.temp)$NatureServe_Common_Name
  results_by_tier[1:length(spp.temp),j] <- spp.temp
}
head(results_by_tier)

metadata <- read.xlsx(xlsxFile = "C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/Prioritization-metadata.xlsx") %>% filter(!Name %in% c("Management_Responsibility_EOs", "Management_Responsibility_Models"))
metadata$Description[which(metadata$Name == "Date")] <- format(Sys.Date(), "%m-%d-%Y")

results_out <- results %>% select(subset(metadata, Type=="Field names")$Name)

results_file_name <- paste0("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/NatureServe - ESA Species - Prioritization Results-", Sys.Date(), ".xlsx")
wb <- openxlsx::createWorkbook()
addWorksheet(wb, "Metadata")
writeData(wb, "Metadata", metadata, headerStyle = createStyle(textDecoration = "Bold"))
addWorksheet(wb, "Ruleset")
insertImage(wb, "Ruleset", "shiny/SSS-Prioritization-Scores/www/decision_tree.png", width = 6, height = 8)
addWorksheet(wb, "Results")
writeData(wb, "Results", results_by_tier, headerStyle = createStyle(textDecoration = "Bold"))
addWorksheet(wb, "Data Abbreviated")
writeData(wb, "Data Abbreviated", results_out %>% select(Tier, Lower_Level_Informal_Group, Scientific_Name, NatureServe_Common_Name, States_of_Occurrence, A_Management_Responsibility, B_Imperilment, C_Practicability, NS_Range_Restricted, E_Multispecies_Benefits, F_Partnering_Ops), headerStyle = createStyle(textDecoration = "Bold"))
addWorksheet(wb, "Data")
writeData(wb, "Data", results_out, headerStyle = createStyle(textDecoration = "Bold"))
openxlsx::saveWorkbook(wb, results_file_name)


##Plot results
library(RColorBrewer)

##Plot number of species in each tier
data.plot<-data.frame(table(results$Tier))
##get the label positions
data.plot <- data.plot %>%
  #arrange(desc(Var1)) %>%
  mutate(Var1 = factor(Var1, levels = c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Data deficient"))) %>%
  arrange(desc(Var1)) %>%
  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq) %>%
  data.frame()

fig <- ggplot(data.plot, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = Freq), color = "black", size=8)+
  scale_fill_manual(values = c(rev(brewer.pal(4, "Greens")), "grey"), name="") +
  #scale_fill_brewer(palette = "Greens", name="", direction = -1) +
  theme_void() +
  xlim(.9, 2.5) +
  theme(text = element_text(size = 20), legend.position="right")
fig

png(filename = paste0("Output/fig.tiers.png"), width = 1200*1.5, height = 1200, res=150*1.5)
print(fig)
dev.off()

##plot taxonomic composition of each tier
#data.plot <- data.frame(table(results$Tier, results$Informal_Group)) %>% subset(Freq>0)

###NEED TO FIRST CALCULATE PROPORTION OF GROUP AND NOT FREQUENCY

##get the label positions
#data.plot <- data.plot %>%
 # group_by(Var1) %>%
#  arrange(desc(Var2)) %>%
#  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq) %>%
#  data.frame()

#fig <- ggplot(data.plot, aes(x = 2, y = Freq, fill = Var2)) +
#  geom_bar(stat = "identity", color = "white") +
 # coord_polar(theta = "y", start = 0)+
  #facet_wrap(.~Var1)+
  #geom_text(aes(y = lab.ypos, label = Freq), color = "black", size=8)+
  ##geom_text(aes(y = 1, x = 1, label = paste0(round(label*100,0), "%")), color = c("black"), size = 6) +
  #scale_fill_brewer(palette = "Greens", name="", direction = -1) +
  #theme_void() +
  #xlim(.9, 2.5) +
  #theme(text = element_text(size = 20), legend.position="right")
#fig

##Show data completeness and outcomes for each prioritization step
completeness <- subset(results, select = c(`A_Management_Responsibility`, `B_Imperilment`, C_Practicability, E_Multispecies_Benefits, F_Partnering_Ops)) %>% gather(key = "Criteria", value = "TF") %>% group_by(Criteria, TF) %>% summarise(n.spp = n()) %>% spread(key = "TF", value = "n.spp") %>% mutate(Percent_data_completeness = round((`FALSE`+`TRUE`)/nrow(results)*100, 1)) %>% subset(select = -`<NA>`) %>% rename("TRUE (No.Spp)" = `TRUE`, "FALSE (No.Spp)" = `FALSE`)

## Identify species that changed tier due to updates from suggested scores
old.results <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_2023-01-05") %>% rename(old.Tier = Tier)

tier.compare <- subset(results, select=c(NatureServe_Common_Name, Tier, BLM_Scores_Reviewed)) %>% left_join(subset(old.results, select =c(NatureServe_Common_Name, old.Tier)))

tier.compare %>% filter(Tier != old.Tier) %>% data.frame()
