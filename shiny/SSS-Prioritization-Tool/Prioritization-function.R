##function to run species through a prioritization ruleset given parameters
##Max Tarjan
##Aug 12, 2022

prioritize <- function (data, species, threshold.eo, threshold.model, threshold.practical, threshold.partner) {
  
  ## Check if all input parameters exist. If not, print an error
  required.inputs<-c("Scientific_Name", "Percent_EOs_BLM", "Percent_Model_Area_BLM", "ESA_Status", "Rounded_Global_Rank", "NO_KNOWN_THREATS", "BLM_Practicability_Score", "Habitat_Wetland/riparian", "Habitat_scrub/shrubland", "Habitat_grassland/steppe/prairie", "BLM_Mutispecies_Score", "BLM_Partnering_Score", "S_TREND", "Rank_Review_Year", "Endemic", "BLM_Threats")
  missing.inputs<-required.inputs[which(!(required.inputs %in% names(data)))]
  if (length(missing.inputs)>0){
    stop(paste0("the following inputs are missing from the input dataset: ", paste0(missing.inputs, collapse = ", ")))
  }
  
  ##set default values if they are not given
  if(!hasArg(species)) {species<-data$Scientific_Name}
  if(!hasArg(threshold.eo)) {threshold.eo<-30}
  if(!hasArg(threshold.model)) {threshold.model<-30}
  if(!hasArg(threshold.practical)) {threshold.practical<-3}
  if(!hasArg(threshold.partner)) {threshold.partner<-3}
  
  ##subset backend input data to select species
  results <- subset(data, Scientific_Name %in% species)
  results$Tier <- NA
  
  ##DID EACH SPECIES PASS OR FAIL EACH CRITERION?
  ##Management responsibility: >30% EOs on BLM land, >30% Modeled habitat on BLM land
  results$Management.responsibility.EO<-ifelse(results$Percent_EOs_BLM > threshold.eo/100, T, F)
  results$Management.responsibility.model<-ifelse(results$Percent_Model_Area_BLM > threshold.model/100, T, F)
  #results$Management.responsibility<-ifelse(results$Management.responsibility.EO | results$Management.responsibility.model, T, F)
  results <- results %>% mutate(Management.responsibility = case_when(Management.responsibility.EO ~ T,
                                                                      Management.responsibility.model ~ T,
                                                                      !Management.responsibility.EO ~ F,
                                                                      !Management.responsibility.model ~ F))
  
  ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
  results$Imperiled<-ifelse((!is.na(results$ESA_Status) & results$ESA_Status!="DL" & results$ESA_Status != 0) | (results$Rounded_Global_Rank %in% c("G1", "G2", "T1", "T2") & results$NO_KNOWN_THREATS == 0), T, F)
  
  ##Conservation Practicability: BLM Assessment derived from USFWS recovery priority numbers
  results$Conservation.practicability <- ifelse(results$BLM_Practicability_Score > threshold.practical, T, F)
  
  ##Multispecies benefits: 6a. Species occurs in special habitats (wetland/riparian, scrub/shrubland, grassland/steppe/prairie); 6b. BLM Assessment
  results$Multispecies <- ifelse(results$`Habitat_Wetland/riparian` | results$`Habitat_scrub/shrubland` | results$`Habitat_grassland/steppe/prairie` | replace_na(results$`BLM_Mutispecies_Score`, 0) > 3, T, F)
  
  ##Partnering Opportunities: BLM Assessment
  results$Partnering <- ifelse(results$BLM_Partnering_Score > threshold.partner, T, F)
  
  ##Inventory priority: species has a habitat model that experts reviewed as poor
  #results$Inventory.Priority <- ifelse(results$Ave_Model_Review_Score < 3, T, F)
  
  ##Monitoring Priority: Species has an unknown short-term trend and its rank was reviewed in the past 10 years
  results$Monitoring.Priority <- ifelse(results$`S_TREND` == "U = Unknown" & results$Rank_Review_Year > (as.numeric(format(Sys.Date(), "%Y"))-10), T, F)
  
  ##ASSIGN TO TIERS
  ##Tier 1
  results$Tier[which(results$Management.responsibility & results$Imperiled & results$Conservation.practicability & results$Multispecies & results$Partnering)] <- "Tier 1"
  
  ##Tier 2
  results$Tier[which(is.na(results$Tier) & results$Management.responsibility & results$Imperiled & results$Conservation.practicability)] <- "Tier 2"
  
  ##Tier 3
  results$Tier[which(is.na(results$Tier) & results$Management.responsibility & results$Imperiled)] <- "Tier 3"
  
  ##Tier 4
  results$Tier[which(is.na(results$Tier))] <- "Tier 4"
  
  ##Overwrite the tiers for endemics if applicable
  ##Endemic: Tier 1
  results$Tier[which(results$Management.responsibility & results$Imperiled & results$Conservation.practicability & results$Endemic & results$BLM_Threats)] <- "Tier 1"
  
  ##Endemic: Tier 2
  results$Tier[which(results$Management.responsibility & results$Imperiled & results$Conservation.practicability & results$Endemic & !results$BLM_Threats)] <- "Tier 2"
  
  ## Data deficient as a tier (unknown management responsibility)
  results$Tier[which(is.na(results$Management.responsibility))] <- "Data deficient"
  
  ## Data deficient = data deficiency influences the tier
  ## Flagged if the lack of data brings the species into another Tier
  ## eg ##tier 2 and NA multispecies or partnering
  results$Data.deficient <- ifelse(
    ##sp is in data deficient tier due to NA in management responsibility
    is.na(results$Management.responsibility) |
      ##species is in tier 2 due to NA in partnering or multispp
      (results$Tier =="Tier 2" & (is.na(results$Partnering) | is.na(results$Multispecies))) |
      ##species is in tier 3 due to NA in cons
      (results$Tier =="Tier 3" & is.na(results$Conservation.practicability)) |
      ## In tier 4 due to NA in imperilment
      (!is.na(results$Management.responsibility) & is.na(results$Imperiled)), 
    T, F)
  
  results
}
