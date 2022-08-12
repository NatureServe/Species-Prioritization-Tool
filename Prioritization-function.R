##function to run species through a prioritization ruleset given parameters
##Max Tarjan
##Aug 12, 2022

prioritize <- function (species, threshold.eo, threshold.model, threshold.practical, threshold.partner) {
  
  ##set default values if they are not given
  if(!hasArg(species)) {species<-sss$Scientific_Name}
  if(!hasArg(threshold.eo)) {threshold.eo<-30}
  if(!hasArg(threshold.model)) {threshold.model<-30}
  if(!hasArg(threshold.practical)) {threshold.practical<-3}
  if(!hasArg(threshold.partner)) {threshold.partner<-3}
  
  ##subset backend input data to select species
  sss.temp<-subset(sss, Scientific_Name %in% species)
  
  ##create a dataframe for results
  results <- data.frame(Species = sss.temp$BLM_Common_Name, Common_Name = sss.temp$BLM_Common_Name, Tier = NA, Inventory.Priority = F, Monitoring.Priority = F)
  
  ##Tier 1
  results$Tier[which(
    ##Management responsibility: >30% EOs on BLM land, >30% Modeled habitat on BLM land
    (sss.temp$Percent_EOs_BLM > threshold.eo | sss.temp$Percent_Model_Area_BLM > threshold.model)
    ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
    & (!is.na(sss.temp$USESA_Status) | (sss.temp$Rounded_Global_Rank %in% c("G1", "G2", "T1", "T2") & sss.temp$No_Known_Threats == 0))
    ##Conservation Practicability: BLM Assessment derived from USFWS recovery priority numbers
    & sss.temp$Practical_Cons_BLM_Score > threshold.practical
    ##Multispecies benefits: 6a. Species occurs in special habitats (wetland/riparian, scrub/shrubland, grassland/steppe/prairie); 6b. BLM Assessment
    & (sss.temp$`Habitat_Wetland/riparian` | sss.temp$`Habitat_scrub/shrubland` | sss.temp$`Habitat_grassland/steppe/prairie` | sss.temp$`Keystone/Multispecies_Benefit_BLM_Score` > 3)
    ##Partnering Opportunities: BLM Assessment
    & sss.temp$Partnering_Opps_BLM_Score > threshold.partner
  )] <- "Tier 1"
  
  ##Tier 2
  results$Tier[which(
    is.na(results$Tier)
    ##Management responsibility: >30% EOs on BLM land, >30% Modeled habitat on BLM land
    & (sss.temp$Percent_EOs_BLM > threshold.eo | sss.temp$Percent_Model_Area_BLM > threshold.model)
    ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
    & (!is.na(sss.temp$USESA_Status) | sss.temp$Rounded_Global_Rank %in% c("G1", "G2", "T1", "T2"))
    ##Conservation Practicability: BLM Assessment derived from USFWS recovery priority numbers
    & sss.temp$Practical_Cons_BLM_Score > threshold.practical
  )] <- "Tier 2"
  
  ##Tier 3
  results$Tier[which(
    is.na(results$Tier)
    ##Management responsibility: >30% EOs on BLM land, >30% Modeled habitat on BLM land
    & (sss.temp$Percent_EOs_BLM > threshold.eo | sss.temp$Percent_Model_Area_BLM > threshold.model)
    ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
    & (!is.na(sss.temp$USESA_Status) | sss.temp$Rounded_Global_Rank %in% c("G1", "G2", "T1", "T2"))
  )] <- "Tier 3"
  
  ##Tier 4
  results$Tier[which(
    is.na(results$Tier)
  )] <- "Tier 4"
  
  ##Management responsibility: >30% EOs on BLM land, >30% Modeled habitat on BLM land
  ##Inventory priority: species has a habitat model that experts reviewed as poor
  ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
  ##Monitoring Priority: Species has an unknown short-term trend and its rank was reviewed in the past 10 years
  ##Conservation Practicability: BLM Assessment derived from USFWS recovery priority numbers
  ##Multispecies benefits: 6a. Species occurs in special habitats (wetland/riparian, scrub/shrubland, grassland/steppe/prairie); 6b. BLM Assessment
  ##Partnering Opportunities: BLM Assessment
  
  ##Inventory priority: species has a habitat model that experts reviewed as poor
  results$Inventory.Priority[which(
    sss.temp$Ave_Model_Review_Score < 3
  )] <- T
  
  ##Monitoring Priority: Species has an unknown short-term trend and its rank was reviewed in the past 10 years
  results$Monitoring.Priority[which(
    sss.temp$`Short-Term_Trend` == "U = Unknown" & sss.temp$Rank_Review_Year > 2011
  )] <- T
  
  results
}