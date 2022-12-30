##function to run species through a prioritization ruleset given parameters
##Max Tarjan
##Aug 12, 2022

prioritize <- function (data, species, threshold.eo, threshold.model, threshold.practical, threshold.partner) {
  
  ## Check if all input parameters exist. If not, print an error
  required.inputs<-c("Scientific_Name", "Percent_EOs_BLM_2019", "Percent_Model_Area_BLM", "ESA_Status", "Rounded_Global_Rank", "NO_KNOWN_THREATS", "BLM_Practicability_Score", "Habitat_Wetland/riparian", "Habitat_scrub/shrubland", "Habitat_grassland/steppe/prairie", "BLM_Multispecies_Score", "BLM_Partnering_Score", "S_TREND", "Rank_Review_Year", "NS_Range_Restricted", "BLM_Threats", "Riparian")
  missing.inputs<-required.inputs[which(!(required.inputs %in% names(data)))]
  if (length(missing.inputs)>0){
    stop(paste0("the following inputs are missing from the input dataset: ", paste0(missing.inputs, collapse = ", ")))
  }
  
  ##set default values if they are not given
  if(!hasArg(species)) {species<-data$Scientific_Name}
  if(!hasArg(threshold.eo)) {threshold.eo<-30}
  if(!hasArg(threshold.eo.ab)) {threshold.eo.ab<-50}
  if(!hasArg(threshold.model)) {threshold.model<-30}
  if(!hasArg(threshold.practical)) {threshold.practical<-3}
  if(!hasArg(threshold.partner)) {threshold.partner<-3}
  
  ##subset backend input data to select species
  results <- subset(data, Scientific_Name %in% species)
  results$Tier <- NA
  
  ## DID EACH SPECIES PASS OR FAIL EACH CRITERION?
  ## Management responsibility: 
    ## Riparian: eos or modeled > 15% on blm land
    ## Not riparian: >30% EOs on BLM land, >30% Modeled habitat on BLM land OR 10% EOs and 50% A/B rank eos on blm land
  results$Management.responsibility <- ifelse(results$Riparian,
                                              ifelse(results$Percent_EOs_BLM_2019 > 15 & !is.na(results$Percent_EOs_BLM_2019), T, 
                                                     ifelse(results$Percent_Model_Area_BLM > 15 & !is.na(results$Percent_Model_Area_BLM), T, 
                                                            ifelse(is.na(results$Percent_EOs_BLM_2019) & is.na(results$Percent_Model_Area_BLM), NA, F)
                                                            )
                                                     ),
                                              ifelse(results$Percent_EOs_BLM_2019 > threshold.eo & !is.na(results$Percent_EOs_BLM_2019), T,
                                                     ifelse(results$Percent_Model_Area_BLM > threshold.model & !is.na(results$Percent_Model_Area_BLM), T,
                                                            ifelse(results$Percent_EOs_BLM_2019 > 10 & results$Percent_eo_AB_BLM >= threshold.eo.ab & !is.na(results$Percent_EOs_BLM_2019) & !is.na(results$Percent_eo_AB_BLM), T, 
                                                                   ifelse(is.na(results$Percent_EOs_BLM_2019) & is.na(results$Percent_Model_Area_BLM) & is.na(results$Percent_eo_AB_BLM), NA, F)
                                                                   )
                                                            )
                                                     )
                                              )
  
  ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
  results$Imperiled<-ifelse((!is.na(results$ESA_Status) & results$ESA_Status!="DL" & results$ESA_Status != 0) | (results$Rounded_Global_Rank %in% c("G1", "G2", "T1", "T2") & results$NO_KNOWN_THREATS == 0), T, F)
  
  ##Conservation Practicability: BLM Assessment derived from USFWS recovery priority numbers
  results$Conservation.practicability <- ifelse(results$BLM_Practicability_Score > threshold.practical, T, F)
  
  ##Multispecies benefits: 6a. Species occurs in special habitats (wetland/riparian, scrub/shrubland, grassland/steppe/prairie); 6b. BLM Assessment
  results$Multispecies <- ifelse(results$`Habitat_Wetland/riparian` | results$`Habitat_scrub/shrubland` | results$`Habitat_grassland/steppe/prairie` | replace_na(results$`BLM_Multispecies_Score`, 0) > 3, T, F)
  
  ##Partnering Opportunities: BLM Assessment
  results$Partnering <- ifelse(results$BLM_Partnering_Score > threshold.partner, T, F)
  
  ##Inventory priority: species has a habitat model that experts reviewed as poor
  #results$Inventory.Priority <- ifelse(results$Ave_Model_Review_Score < 3, T, F)
  
  ##Monitoring Priority: Species has an unknown short-term trend and its rank was reviewed in the past 10 years
  results$Monitoring.Priority <- ifelse(results$`S_TREND` == "U = Unknown" & results$Rank_Review_Year > (as.numeric(format(Sys.Date(), "%Y"))-10), T, F)
  
  ##ASSIGN TO TIERS
  results$Tier <- ifelse(is.na(results$Management.responsibility), "Data deficient",
                               ifelse(results$Management.responsibility, no = "Tier 4", 
                                      ifelse(results$Imperiled, no = "Tier 4",
                                             ifelse(results$Conservation.practicability, no = "Tier 3",
                                                    ifelse(results$NS_Range_Restricted,
                                                           ifelse(results$BLM_Threats, "Tier 1", "Tier 2"),
                                                           ifelse(results$Multispecies, no = "Tier 2",
                                                                  ifelse(is.na(results$Partnering), "Tier 2", 
                                                                         ifelse(results$Partnering,"Tier 1","Tier 2"))))))))
  ## Description of tier evaluation
  results$Evaluation<-NA
  results$Evaluation <- ifelse(is.na(results$Management.responsibility), paste0("BLM's stewardship responsibility to the ", results$NatureServe_Common_Name," is unknown due to a lack of spatial data."),
                               ifelse(results$Management.responsibility, no = paste0("The ", results$NatureServe_Common_Name," has low or no occurence on BLM-managed lands."), 
                                      ifelse(results$Imperiled, no = paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is not imperiled."),
                                             ifelse(results$Conservation.practicability, no = paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). It would be difficult to implement conservation actions for this taxon."),
                                                    ifelse(results$NS_Range_Restricted,
                                                           ifelse(results$BLM_Threats, paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has a restricted range. It is affected by threats that BLM can mitigate."), paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has a restricted range. It is not affected by threats that BLM can mitigate.")),
                                                           ifelse(results$Multispecies, no = paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has an unrestricted range. There are not sufficient multispecies benefits to its conservation."),
                                                                  ifelse(is.na(results$Partnering), paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has an unrestricted range. There are multispecies benefits to its conservation. Partnering opportunities are unknown."),
                                                                         ifelse(results$Partnering, paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has an unrestricted range. There are multispecies benefits to its conservation. There are partnering opportunities for its conservation."), paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has an unrestricted range. There are multispecies benefits to its conservation. There are insufficient partnering opportunities for its conservation.")))))))))
  
  ## Data deficient = data deficiency influences the tier
  ## Flagged if the lack of data brings the species into another Tier
  results$Data.deficient <- ifelse(is.na(results$Management.responsibility) | is.na(results$Imperiled), T,
                                   ifelse(results$Tier == "Tier 3" & is.na(results$Conservation.practicability), T,
                                          ifelse(results$Tier == "Tier 2" & (is.na(results$Multispecies) | is.na(results$Partnering)), T, F
                                                 )
                                          )
                                   )
  
  results
}
