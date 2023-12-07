##function to run species through a prioritization ruleset given parameters
##Max Tarjan
##Aug 12, 2022

prioritize <- function (data, species, threshold.eo, threshold.model, threshold.practical, threshold.partner) {
  
  ## Check if all input parameters exist. If not, print an error
  required.inputs<-c("Scientific_Name", "Percent_EOs_BLM_2019", "Percent_Model_Area_BLM", "USESA_Status", "Rounded_Global_Rank", "No_Known_Threats", "Practical_Cons_BLM_Score", "Habitat_wetland/riparian", "Habitat_grassland/steppe/prairie", "Multispecies_Benefit_BLM_Score", "Partnering_Opps_BLM_Score", "Short-Term_Trend", "Rank_Review_Year", "NS_Range_Restricted", "BLM_Threats", "Riparian", "Percent_AB_EOs_BLM", "Average_Model_Review_Score")
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
  results$A_Management_Responsibility <- ifelse(results$Riparian,
                                              ifelse(results$Percent_EOs_BLM_2019 > 15 & !is.na(results$Percent_EOs_BLM_2019), T, 
                                                     ifelse(results$Percent_Model_Area_BLM > 15 & !is.na(results$Percent_Model_Area_BLM), T, 
                                                            ifelse(is.na(results$Percent_EOs_BLM_2019) & is.na(results$Percent_Model_Area_BLM), NA, F)
                                                            )
                                                     ),
                                              ifelse(results$Percent_EOs_BLM_2019 > threshold.eo & !is.na(results$Percent_EOs_BLM_2019), T,
                                                     ifelse(results$Percent_Model_Area_BLM > threshold.model & !is.na(results$Percent_Model_Area_BLM), T,
                                                            ifelse(results$Percent_EOs_BLM_2019 > 10 & results$Percent_AB_EOs_BLM >= threshold.eo.ab & !is.na(results$Percent_EOs_BLM_2019) & !is.na(results$Percent_AB_EOs_BLM), T, 
                                                                   ifelse(is.na(results$Percent_EOs_BLM_2019) & is.na(results$Percent_Model_Area_BLM) & is.na(results$Percent_AB_EOs_BLM), NA, F)
                                                                   )
                                                            )
                                                     )
                                              )
  
  ##Imperilment: Species is ESA listed, proposed, candidate' Species ranked G1, G2, T1, T2
  results$`B_Imperilment`<-ifelse((!is.na(results$USESA_Status) & results$USESA_Status!="DL" & results$USESA_Status != 0) | (results$Rounded_Global_Rank %in% c("G1", "G2", "T1", "T2") & results$No_Known_Threats == 0), T, F)
  
  ##Conservation Practicability: BLM Assessment derived from USFWS recovery priority numbers
  results$C_Practicability <- ifelse(results$Practical_Cons_BLM_Score > threshold.practical, T, F)
  
  ##Multispecies benefits: 6a. Species occurs in special habitats (wetland/riparian, scrub/shrubland, grassland/steppe/prairie); 6b. BLM Assessment
  results$Multispecies_Benefits_Special_Habitats <- ifelse(results$`Habitat_wetland/riparian` | results$`Habitat_grassland/steppe/prairie`, T, F)
  results$Multispecies_Benefits_BLM_Score <- ifelse(replace_na(results$`Multispecies_Benefit_BLM_Score`, 0) > 3, T, F)
  results$E_Multispecies_Benefits <- ifelse(results$Multispecies_Benefits_Special_Habitats | results$Multispecies_Benefits_BLM_Score, T, F)
  
  ##Partnering Opportunities: BLM Assessment
  results$F_Partnering_Ops <- ifelse(results$Partnering_Opps_BLM_Score > threshold.partner, T, F)
  
  ##Inventory priority: species has a habitat model that experts reviewed as poor
  results$A1_Inventory_Priority <- ifelse(!results$A_Management_Responsibility & results$Average_Model_Review_Score < 3, T, F)
  
  ##Monitoring Priority: Species has an unknown short-term trend and its rank was reviewed in the past 10 years
  results$B1_Monitoring_Priority <- ifelse(results$A_Management_Responsibility & !results$B_Imperilment & results$`Short-Term_Trend` == "U = Unknown" & results$Rank_Review_Year > (as.numeric(format(Sys.Date(), "%Y"))-10), T, F)
  
  ##ASSIGN TO TIERS
  results$Tier <- ifelse(is.na(results$`A_Management_Responsibility`), "NatureServe data deficient",
                               ifelse(results$`A_Management_Responsibility`, no = "Tier 4", 
                                      ifelse(results$`B_Imperilment`, no = "Tier 4",
                                             ifelse(results$C_Practicability & !is.na(results$C_Practicability), no = "Tier 3",
                                                    ifelse(results$NS_Range_Restricted,
                                                           ifelse(results$BLM_Threats, "Tier 1", "Tier 2"),
                                                           ifelse(results$E_Multispecies_Benefits, no = "Tier 2",
                                                                  ifelse(is.na(results$F_Partnering_Ops), "Tier 2", 
                                                                         ifelse(results$F_Partnering_Ops,"Tier 1","Tier 2"))))))))
  ## Description of tier evaluation
  results$Evaluation<-NA
  results$Evaluation <- ifelse(is.na(results$`A_Management_Responsibility`), paste0("BLM's stewardship responsibility to the ", results$NatureServe_Common_Name," is unknown due to a lack of available spatial data."),
                               ifelse(results$`A_Management_Responsibility`, no = paste0("The ", results$NatureServe_Common_Name," has low or no occurence on BLM-managed lands."), 
                                      ifelse(results$`B_Imperilment`, no = paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is not imperiled."),
                                             ifelse(results$C_Practicability & !is.na(results$C_Practicability), no = paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). Implementing conservation actions for this taxon would have low or unknown practicability."),
                                                    ifelse(results$NS_Range_Restricted,
                                                           ifelse(results$BLM_Threats, paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has a restricted range. It is affected by threats that BLM can mitigate."), paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It has a restricted range. It is not affected by threats that BLM can mitigate.")),
                                                           ifelse(results$E_Multispecies_Benefits, no = paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It does not have a restricted range. There are not sufficient multispecies benefits to its conservation."),
                                                                  ifelse(is.na(results$F_Partnering_Ops), paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It does not have a restricted range. There are multispecies benefits to its conservation. Partnering opportunities are unknown."),
                                                                         ifelse(results$F_Partnering_Ops, paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It does not have a restricted range. There are multispecies benefits to its conservation. There are partnering opportunities for its conservation."), paste0("The ", results$NatureServe_Common_Name," has high occurence on BLM-managed lands. It is imperiled (ESA listed, under review, or G1/G2). There is sufficient conservation practicability. It does not have a restricted range. There are multispecies benefits to its conservation. There are limited partnering opportunities for its conservation.")))))))))
  
  results$Tier_path <- NA
  results$Tier_path_fig <- NA

  # Tier path: Tier 4 Riparian Step A1
  if (results$Riparian & !results$`A_Management_Responsibility` & results$Percent_EOs_BLM_2019 <= 15){
    results$Tier_path <- paste0("This is a riparian taxon with less than 15% of element occurrences overlapping BLM lands: this taxon is Tier 4 Step A1")
    results$Tier_path_fig <- "Tier 4 Riparian Step A1"
  }
  # Tier path: Tier 4 Non-Riparian Step A1 (EO-based)
  if (!results$Riparian & !results$`A_Management_Responsibility`& results$Percent_EOs_BLM_2019 <= 10){
    results$Tier_path <- paste0("This is a non-riparian taxon with less than 10% of element occurrences overlapping BLM lands: this taxon is Tier 4 Step A1")
    results$Tier_path_fig <- "Tier 4 Non-Riparian Step A1"
  }
  # Tier path: Tier 4 Non-Riparian Step A1 (model-based)
  if (!results$Riparian & !results$`A_Management_Responsibility` & results$Percent_Model_Area_BLM < threshold.model){
    results$Tier_path <- paste0("This is a non-riparian taxon with less than", threshold.model, "% of element occurrences overlapping BLM lands: this taxon is Tier 4 Step A1")
    results$Tier_path_fig <- "Tier 4 Non-Riparian Step A1"
  }
  # Tier path: Tier 4 Non-Riparian Step A1 (AB EO-based)
  if (!results$Riparian & !results$`A_Management_Responsibility` & results$Percent_AB_EOs_BLM < threshold.eo.ab){
    results$Tier_path <- paste0("This is a non-riparian taxon with less than", threshold.eo.ab, "% of AB element occurrences overlapping BLM lands: this taxon is Tier 4 Step A1")
    results$Tier_path_fig <- "Tier 4 Non-Riparian Step A1"
  }
  # Tier path: Tier 4 Riparian Step B1
  if (results$Riparian & results$`A_Management_Responsibility` & !results$`B_Imperilment`){
    results$Tier_path <- paste0("This is a riparian taxon with high BLM stewardship responsibility but it is not imperiled: this taxon is Tier 4 Step B1")
    results$Tier_path_fig <- "Tier 4 Riparian Step A1"
  }
  # Tier path: Tier 4 Riparian Step B1
  if (!results$Riparian & results$`A_Management_Responsibility` & !results$`B_Imperilment`){
    results$Tier_path <- paste0("This is a non-riparian taxon with high BLM stewardship responsibility but it is not imperiled: this taxon is Tier 4 Step B1")
    results$Tier_path_fig <- "Tier 4 Non-Riparian Step B1"
  }
  # Tier path: Tier 3 Riparian Step C
  if (results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & !results$C_Practicability){
    results$Tier_path <- paste0("This taxon is riparian, has high BLM stewardship responsibility, and is imperiled, but it has low practicability for conservation actions: this taxon is Tier 3 Step C")
    results$Tier_path_fig <- "Tier 3 Riparian Step C"
  }
  # Tier path: Tier 3 Non-Riparian Step C
  if (!results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & !results$C_Practicability){
    results$Tier_path <- paste0("This taxon is non-riparian, has high BLM stewardship responsibility, and is imperiled, but it has low practicability for conservation actions: this taxon is Tier 3 Step C")
    results$Tier_path_fig <- "Tier 3 Non-Riparian Step C"
  }
  # Tier path: Tier 3 Tier 1 Riparian Step D1
  if (results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & results$NS_Range_Restricted & results$BLM_Threats){
    results$Tier_path <- paste0("This taxon is riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is range-restricted, and its primary threats are under BLM control: this taxon is Tier 1 Step D1")
    results$Tier_path_fig <- "Tier 1 Riparian Step D1"
  }
  # Tier path: Tier 3 Riparian Step C
  if (!results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & results$NS_Range_Restricted & results$BLM_Threats){
    results$Tier_path <- paste0("This taxon is non-riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is range-restricted, and its primary threats are under BLM control: this taxon is Tier 1 Step D1")
    results$Tier_path_fig <- "Tier 1 Non-Riparian Step D1"
  }
  # Tier path: Tier 2 Riparian Step D1
  if (results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & results$NS_Range_Restricted & !results$BLM_Threats){
    results$Tier_path <- paste0("This taxon is riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is range-restricted, but its primary threats are not under BLM control: this taxon is Tier 2 Step D1")
    results$Tier_path_fig <- "Tier 2 Riparian Step D1"
  }
  # Tier path: Tier 2 Non-Riparian Step D1
  if (!results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & results$NS_Range_Restricted & !results$BLM_Threats){
    results$Tier_path <- paste0("This taxon is non-riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is range-restricted, but its primary threats are not under BLM control: this taxon is Tier 2 Step D1")
    results$Tier_path_fig <- "Tier 2 Non-Riparian Step D1"
  }
  # Tier path: Tier 3 Tier 2 Riparian Step E
  if (results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & !results$NS_Range_Restricted & !results$E_Multispecies_Benefits){
    results$Tier_path <- paste0("This taxon is riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is not range-restricted, but there are no sufficient multispecies benefits to its conservation: this taxon is Tier 2 Step E")
    results$Tier_path_fig <- "Tier 2 Riparian Step E"
  }
  # Tier path:Tier 2 Non-Riparian Step E
  if (!results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & !results$NS_Range_Restricted & !results$E_Multispecies_Benefits){
    results$Tier_path <- paste0("This taxon is non-riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is not range-restricted, but there are no sufficient multispecies benefits to its conservation: this taxon is Tier 2 Step E")
    results$Tier_path_fig <- "Tier 2 Non-Riparian Step E"
  }
  # Tier path: Tier 2 Riparian Step F
  if (results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & !results$NS_Range_Restricted & results$E_Multispecies_Benefits & !results$F_Partnering_Ops){
    results$Tier_path <- paste0("This taxon is riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is not range-restricted, there are sufficient multispecies benefits to its conservation, but partnering opportunities are low or unknown: this taxon is Tier 2 Step F")
    results$Tier_path_fig <- "Tier 2 Riparian Step F"
  }
  # Tier path:Tier 2 Non-Riparian Step F
  if (!results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & !results$NS_Range_Restricted & results$E_Multispecies_Benefits & !results$F_Partnering_Ops){
    results$Tier_path <- paste0("This taxon is non-riparian, has high BLM stewardship responsibility, is imperiled, and has high practicability for conservation actions, is not range-restricted, there are sufficient multispecies benefits to its conservation, but partnering opportunities are low or unknown: this taxon is Tier 2 Step F")
    results$Tier_path_fig <- "Tier 2 Non-Riparian Step F"
  }
  # Tier path: Tier 1 Riparian Step F
  if (results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & !results$NS_Range_Restricted & results$E_Multispecies_Benefits & results$F_Partnering_Ops){
    results$Tier_path <- paste0("This taxon is riparian, has high BLM stewardship responsibility, is imperiled, has high practicability for conservation actions, is not range-restricted, there are sufficient multispecies benefits to its conservation, and there are sufficient partnering opportunities for its conservation: this taxon is Tier 2 Step F")
    results$Tier_path_fig <- "Tier 1 Riparian Step F"
  }
  # Tier path: Tier 1 Non-Riparian Step F
  if (!results$Riparian & results$`A_Management_Responsibility` & results$`B_Imperilment` & results$C_Practicability & !results$NS_Range_Restricted & results$E_Multispecies_Benefits & results$F_Partnering_Ops){
    results$Tier_path <- paste0("This taxon is non-riparian, has high BLM stewardship responsibility, is imperiled, has high practicability for conservation actions, is not range-restricted, there are sufficient multispecies benefits to its conservation, and there are sufficient partnering opportunities for its conservation: this taxon is Tier 1 Step F")
    results$Tier_path_fig <- "Tier 1 Non-Riparian Step F"
  }

  ## Data deficient = data deficiency influences the tier
  ## Flagged if the lack of data brings the species into another Tier
  results$Data_Deficient <- ifelse(is.na(results$`A_Management_Responsibility`) | is.na(results$`B_Imperilment`), T,
                                   ifelse(results$Tier == "Tier 3" & is.na(results$C_Practicability), T,
                                          ifelse(results$Tier == "Tier 2" & (is.na(results$E_Multispecies_Benefits) | is.na(results$F_Partnering_Ops)), T, F
                                                 )
                                          )
                                   )
  
  # Tier path: NatureServe Data Deficient
  if (results$Data_Deficient) results$Tier_path <- paste0("BLM priority status could not be determined due to a lack of available spatial data: this taxon is NatureServe Data Deficient.")
  
  results
}
