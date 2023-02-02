## Wrangle data
## M Tarjan
## Nov 9, 2022

## Load packages
library(readxl)
library(tidyverse)
library(RODBC)
library(googlesheets4)
library(googledrive)

## Check which species have EOs in the spatial snapshot for jurisdictional analysis
## Load sss from esa batch
# sss.esa <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/ESA listed BLM SSS to score for Prioritization Tool_7Nov2022.xlsx")

## all SSS
sss <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Task 2-List interoperability/BLM - Compiled SSS List Information - September 2021.xlsx", sheet = "1b. BLM-NS SSS Data Summary", skip = 1)
## remove any without NS ID
sss <- subset(sss, !is.na(`Element Global ID`))

## Get available EOs
#eos <-  read_excel("C:/Users/max_tarjan/OneDrive - NatureServe/Documents/Species-Select/Data/Biotics_EO_Summary.xlsx", sheet = "EO_Summary_202207")
#eos <- eos %>% select(c(ELEMENT_GLOBAL_ID, SUBNATION_CODE, NUM_CURRENT_EOS)) %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(NUM_CURRENT_EOS = sum(NUM_CURRENT_EOS))

#sss.esa <- left_join(sss.esa, eos, by = c(`NatureServe Element ID` = "ELEMENT_GLOBAL_ID"))

#write.csv(sss.esa, "C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/EOs for ESA listed BLM SSS.csv", row.names= F)

## CREATE BACKEND DATASHEETS FOR SHINY APPLICATIONS

## define species ids for selection
# ids <- sss.esa$`NatureServe Element ID`
ids <- sss$`Element Global ID`
# ids <- 101258

## Connect to central biotics to pull out most recent data on sss
con<-odbcConnect("centralbiotics", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

## Select all records from biotics that have a BLM_SSS_States entry
qry <- "SELECT
 egt.element_global_id
 , 'ELEMENT_GLOBAL.'||egt.element_global_ou_uid||'.'||egt.element_global_seq_uid eguid
 , GETNAMECATDESC(egt.gname_id) Name_Category
 , informal_grp((informal_tax(egt.element_global_id))) informal_grp
 , informal_tax(egt.element_global_id) Informal_Tax
 , sn.SCIENTIFIC_NAME gname
 , egt.g_primary_common_name
 , egt.G_RANK Global_Rank
 , egt.ROUNDED_G_RANK rnd
 , egt.g_rank_review_date
 , d_usesa.display_value usesa
 , tg.interpreted_usesa
 , d_short_term_trend.display_value S_Trend
 , d_range_extent.range_extent_cd
 , d_range_extent.range_extent_desc

,(case when sn.d_name_category_id in (1, 2, 3)  /******ANIMAL*******/ then 
(DelimList('SELECT terrestrial_habitat_desc FROM d_terrestrial_habitat dth, animal_cag_terr_hab acth WHERE 
        acth.d_terrestrial_habitat_id = dth.d_terrestrial_habitat_id and acth.element_global_id = '||egt.element_global_id, '; '))
when sn.d_name_category_id in (4, 5, 6, 7, 19)  /******PLANT*******/  then 
(DelimList('SELECT terrestrial_habitat_desc FROM d_terrestrial_habitat dth, plant_cag_terr_hab pcth WHERE 
        pcth.d_terrestrial_habitat_id = dth.d_terrestrial_habitat_id and pcth.element_global_id = '||egt.element_global_id, '; '))  
else '' end)    Terrestrial_Habitats, 

(case when sn.d_name_category_id in (1, 2, 3)  /******ANIMAL*******/ then 
(DelimList('SELECT lacustrine_habitat_desc FROM d_lacustrine_habitat dlh, animal_cag_lacus_hab aclh WHERE 
        aclh.d_lacustrine_habitat_id = dlh.d_lacustrine_habitat_id and aclh.element_global_id = '||egt.element_global_id, '; '))
when sn.d_name_category_id in (4, 5, 6, 7, 19)  /******PLANT*******/  then 
(DelimList('SELECT lacustrine_habitat_desc FROM d_lacustrine_habitat dlh, plant_cag_lacus_hab pclh WHERE 
        pclh.d_lacustrine_habitat_id = dlh.d_lacustrine_habitat_id and pclh.element_global_id = '||egt.element_global_id, '; '))  
else '' end)    lacustrine_habitats, 

(case when sn.d_name_category_id in (1, 2, 3)  /******ANIMAL*******/ then 
(DelimList('SELECT palustrine_habitat_desc FROM d_palustrine_habitat dph, animal_cag_palus_hab acph WHERE 
        acph.d_palustrine_habitat_id = dph.d_palustrine_habitat_id and acph.element_global_id = '||egt.element_global_id, '; '))
when sn.d_name_category_id in (4, 5, 6, 7, 19)  /******PLANT*******/  then 
(DelimList('SELECT palustrine_habitat_desc FROM d_palustrine_habitat dph, plant_cag_palus_hab pcph WHERE 
        pcph.d_palustrine_habitat_id = dph.d_palustrine_habitat_id and pcph.element_global_id = '||egt.element_global_id, '; '))  
else '' end)    palustrine_habitats, 

(case when sn.d_name_category_id in (1, 2, 3)  /******ANIMAL*******/ then 
(DelimList('SELECT riverine_habitat_desc FROM d_riverine_habitat drh, animal_cag_riverine_hab acrh WHERE 
        acrh.d_riverine_habitat_id = drh.d_riverine_habitat_id and acrh.element_global_id = '||egt.element_global_id, '; '))
when sn.d_name_category_id in (4, 5, 6, 7, 19)  /******PLANT*******/  then 
(DelimList('SELECT riverine_habitat_desc FROM d_riverine_habitat drh, plant_cag_riverine_hab pcrh WHERE 
        pcrh.d_riverine_habitat_id = drh.d_riverine_habitat_id and pcrh.element_global_id = '||egt.element_global_id, '; '))  
else '' end)    riverine_habitats 

/* selects threat category if it is No Known Threats */
  ,(SELECT d_iucn_threat_category.display_value FROM d_iucn_threat_category, el_global_threats_assess 
       WHERE el_global_threats_assess.D_IUCN_THREAT_CATEGORY_ID = d_iucn_threat_category.D_IUCN_THREAT_CATEGORY_ID 
       and el_global_threats_assess.d_iucn_threat_category_id = 89 /*No Known threats */
       and el_global_threats_assess.element_global_id =  egt.element_global_id )   as No_Known_Threats

, DelimList('SELECT DECODE(s.nation_id,139,''MX'','''')
    || s.subnation_code ||   ''''  AS subnatl_dist '
    || ' FROM element_subnational, subnation s, taxon_subnatl_sprot_ext ts_ext
         WHERE element_subnational.subnation_id = s.subnation_id and element_subnational.element_subnational_id = ts_ext.element_subnational_id
         and ts_ext.blm_sss_2019 = ''Y''
         and element_subnational.element_national_id IN ' 
    || '(SELECT element_national_id FROM element_national WHERE element_global_id='  
    || egt.element_global_id || ') ORDER BY s.nation_id desc, SUBNATL_DIST ', ', ') 
  AS BLM_SSS_STATES

, DelimList('SELECT DECODE(s.nation_id,139,''MX'','''')
    || s.subnation_code ||   ''''  AS subnatl_dist '
    || ' FROM element_subnational, subnation s
         WHERE element_subnational.subnation_id = s.subnation_id
         and element_subnational.element_national_id IN ' 
    || '(SELECT element_national_id FROM element_national WHERE element_global_id='  
    || egt.element_global_id || ') ORDER BY s.nation_id desc, SUBNATL_DIST ', ', ') 
  AS STATES

, DelimList('SELECT iucn_threat_category_desc FROM el_global_threats_assess, d_iucn_threat_category WHERE 
        el_global_threats_assess.d_iucn_threat_category_id = d_iucn_threat_category.d_iucn_threat_category_id and el_global_threats_assess.element_global_id = '||egt.element_global_id || 'ORDER BY iucn_threat_category_desc', '; ') AS Threats_Description

, DelimList('SELECT iucn_threat_category_cd FROM el_global_threats_assess, d_iucn_threat_category WHERE 
        el_global_threats_assess.d_iucn_threat_category_id = d_iucn_threat_category.d_iucn_threat_category_id and el_global_threats_assess.element_global_id = '||egt.element_global_id || 'ORDER BY iucn_threat_category_cd', '; ') AS Threats_Codes

FROM 
ELEMENT_GLOBAL egt 
 , taxon_global tg
 , SCIENTIFIC_NAME sn
 , D_CLASSIFICATION_STATUS dcs
 , element_global_rank egr
 , PLANT_CAG
 , ANIMAL_CAG
 , element_national ent
 , el_natl_agency_status nas
 , d_usesa
 , d_short_term_trend
 , d_range_extent
 
WHERE 
 egt.element_global_id = tg.element_global_id
and egt.GNAME_ID =  sn.SCIENTIFIC_NAME_ID
and egt.element_global_id = egr.element_global_id (+) 
and egt.D_CLASSIFICATION_STATUS_ID = dcs.D_CLASSIFICATION_STATUS_ID
and egt.element_global_id = PLANT_CAG.element_global_id (+) 
and egt.element_global_id = ANIMAL_CAG.element_global_id (+)
and egt.element_global_id = ent.element_global_id
and ent.element_national_id = nas.element_national_id
 and tg.d_usesa_id = d_usesa.d_usesa_id (+)
 and egr.d_short_term_trend_id = d_short_term_trend.d_short_term_trend_id (+)
and egr.d_range_extent_id = d_range_extent.d_range_extent_id (+)
and nas.agency_name like 'BLM West 2019-11%'
/*and ent.nation_id = 225 
and egt.inactive_ind = 'N'*/

ORDER BY 
sn.d_name_category_id,name_category, informal_grp, informal_tax, scientific_name
;"

dat <- sqlQuery(con, qry)

# When finished, close the connection
odbcClose(con)

## Compare sss in biotics (dat) to sss in workbook (sss)
#sss %>% filter(`Element Global ID` %in% setdiff(y = dat$ELEMENT_GLOBAL_ID, x = sss$`Element Global ID`)) %>% select(`Common Name`) %>% data.frame()
#dat %>% filter(ELEMENT_GLOBAL_ID %in% setdiff(x = dat$ELEMENT_GLOBAL_ID, y = sss$`Element Global ID`)) %>% select(G_PRIMARY_COMMON_NAME) %>% data.frame()

sss.data <- dat %>% 
  filter(!duplicated(ELEMENT_GLOBAL_ID)) %>%
  unique() %>%
  mutate(Explorer_url = paste0("https://explorer.natureserve.org/Taxon/", EGUID, "/", sub(x=GNAME, pattern = " ", replacement = "_")),
         ExplorerPro_url = paste0("https://explorer.natureserve.org/pro/Map/?taxonUniqueId=", EGUID),
         Riparian = ifelse((!is.na(RIVERINE_HABITATS) & !RIVERINE_HABITATS=="(?i)Aerial") | grepl(PALUSTRINE_HABITATS, pattern = "(?i)riparian"), T, F),
         `Habitat_wetland/riparian` =  ifelse((!is.na(RIVERINE_HABITATS) & RIVERINE_HABITATS!="(?i)Aerial") | !is.na(PALUSTRINE_HABITATS), T, F),
         `Habitat_scrub/shrubland` = ifelse(grepl(TERRESTRIAL_HABITATS, pattern = "(?i)scrub|(?i)shrub|(?i)savanna"), T, F),
         `Habitat_grassland/steppe/prairie` = ifelse(grepl(TERRESTRIAL_HABITATS, pattern = "(?i)grassland|(?i)steppe|(?i)prairie|(?i)Old Field|(?i)barrens"), T, F),
         NS_Range_Restricted = ifelse(grepl(RANGE_EXTENT_CD, pattern = "A|B|C|D|E") & !grepl(RANGE_EXTENT_CD, pattern = "G|H"), T, F),
         BLM_Threats = ifelse(grepl(strsplit(THREATS_CODES, split = "; "), pattern = "(?<!\\d|\\.)1.2|(?<!\\d|\\.)1.3|(?<!\\d|\\.)2.3|(?<!\\d|\\.)3.1|(?<!\\d|\\.)3.2|(?<!\\d|\\.)3.3|(?<!\\d|\\.)6.1|(?<!\\d|\\.)3", perl=T), T, F),
         Rank_Review_Year = format(G_RANK_REVIEW_DATE, "%Y"),
         USESA_Status = ifelse(is.na(USESA), yes = INTERPRETED_USESA, no = sub(x = USESA, pattern = "\\:.*", replacement = ""))) %>%
  # select(ELEMENT_GLOBAL_ID, NAME_CATEGORY, INFORMAL_TAX, GNAME, G_PRIMARY_COMMON_NAME, RND, USESA, BLM_SSS_STATES, RANGE_EXTENT_DESC, RANGE_EXTENT_CD, Explorer.url, ExplorerPro.url) %>%
 rename(NatureServe_Element_ID = ELEMENT_GLOBAL_ID, Major_Group= NAME_CATEGORY, Higher_Level_Informal_Group = INFORMAL_GRP, Lower_Level_Informal_Group = INFORMAL_TAX, Scientific_Name = GNAME, NatureServe_Common_Name = G_PRIMARY_COMMON_NAME, Rounded_Global_Rank = RND, BLM_SSS_States = BLM_SSS_STATES, `Short-Term_Trend`=S_TREND, Global_Rank = GLOBAL_RANK, No_Known_Threats = NO_KNOWN_THREATS, Threats_Description = THREATS_DESCRIPTION, Threats_Codes = THREATS_CODES, States_of_Occurrence = STATES)

## Add data from BLM
BLM.scores.hq <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="BLM_scores_ESA_spp") %>%
  rename_with(~gsub(.x, pattern = " ", replacement = "_"))

sss.data <- sss.data %>% 
  left_join(y = subset(BLM.scores.hq, select = c("NatureServe_Element_ID", "USFWS_Recovery_Priority_Num", "BLM_Practicability_Score", "BLM_Multispecies_Score", "BLM_Partnering_Score", "Endemic", "HQ_Notes")) %>% rename(Range_Restricted_BLM_Score = Endemic, Practical_Cons_BLM_Score
= BLM_Practicability_Score, `Multispecies_Benefit_BLM_Score` = BLM_Multispecies_Score, Partnering_Opps_BLM_Score = BLM_Partnering_Score))

## Overwrite BLM scores with suggested scores from scores application
## take the most recent scores if someone submitted multiple scores for a single taxon
BLM.scores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="suggested_scores") %>%
  rename_with(~gsub(.x, pattern = " ", replacement = "_")) %>% unique() %>% group_by(Reviewer_Email, Scientific_Name) %>% summarise(Practical_Cons_BLM_Score = last(Practical_Cons_BLM_Score), Multispecies_Benefit_BLM_Score = last(Multispecies_Benefit_BLM_Score), Partnering_Opps_BLM_Score = last(Partnering_Opps_BLM_Score))
## get average score per species
BLM.scores <- BLM.scores %>% group_by(Scientific_Name) %>% 
  summarise(Practical_Cons_BLM_Score_av = mean(Practical_Cons_BLM_Score, na.rm=T), Multispecies_Benefit_BLM_Score_av = mean(Multispecies_Benefit_BLM_Score, na.rm=T), Partnering_Opps_BLM_Score_av = mean(Partnering_Opps_BLM_Score, na.rm=T))

sss.data <- left_join(sss.data, BLM.scores) %>% mutate(
  Practical_Cons_BLM_Score = ifelse(is.na(Practical_Cons_BLM_Score_av), Practical_Cons_BLM_Score, Practical_Cons_BLM_Score_av), 
  Partnering_Opps_BLM_Score = ifelse(is.na(Partnering_Opps_BLM_Score_av), Partnering_Opps_BLM_Score, Partnering_Opps_BLM_Score_av), 
  Multispecies_Benefit_BLM_Score = ifelse(is.na(Multispecies_Benefit_BLM_Score_av), Multispecies_Benefit_BLM_Score, Multispecies_Benefit_BLM_Score_av),
  BLM_Scores_Reviewed = ifelse(Scientific_Name %in% BLM.scores$Scientific_Name, T, F)) %>% 
  select(-c(Practical_Cons_BLM_Score_av, Partnering_Opps_BLM_Score_av, Multispecies_Benefit_BLM_Score_av))

## Add results from jurisdictional analysis
ja <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Provided to BLM/BLM - Information for T & E Strategic Decision-Making - October 2022.xlsx", sheet = "BLM SSS Information by State", skip = 1) %>% 
  mutate(Percent_EOs_BLM_2019 = ifelse(`Total Occurrences on BLM Lands (West)`==0,0, as.numeric(`Occurrences on BLM Lands (West) / Total Occurrences Rangewide`)*100),
         Percent_Model_Area_BLM = `Percent Suitable Habitat on BLM Lands (West)`*100) %>%
  rename("NatureServe_Element_ID" = "Element Global ID")

sss.data <- sss.data %>% left_join(y=subset(ja, select = c("NatureServe_Element_ID", "Percent_EOs_BLM_2019", "Percent_Model_Area_BLM", "Total Occurrences Rangewide", "Total Occurrences on BLM Lands (West)")) %>% rename(Total_EOs_Rangewide_2019 = `Total Occurrences Rangewide`, Total_EOs_BLM_2019 = `Total Occurrences on BLM Lands (West)`))

##add more recent JA results
ja2023<- read.csv("C:/Users/max_tarjan/OneDrive - NatureServe/Documents/Partners-In-Conservation/Output-2023-02-02/blm_eo_jurisdiction_results-2023-02-02.csv") %>% 
  select(EGT_ID, Total_EOs_Rangewide_2023, Total_EOs_BLM_2023, Percent_EOs_BLM_2023, Total_AB_EOs, Total_AB_EOs_BLM, Percent_AB_EOs_BLM) %>%
  rename("NatureServe_Element_ID" = "EGT_ID")
sss.data <- sss.data %>% left_join(ja2023)

##plot JA results for 2023 versus 2019
plot(data = sss.data, Percent_EOs_BLM_2023 ~ Percent_EOs_BLM_2019)
#missing.ja<-subset(sss.data, is.na(Percent_EOs_BLM_2023) & !is.na(Percent_EOs_BLM_2019))$NatureServe_Element_ID
## show species that were past the 30% threshold in 2019 but not in 2023 results
subset(sss.data, (Percent_EOs_BLM_2023<30 | is.na(Percent_EOs_BLM_2023)) & Percent_EOs_BLM_2019>30) %>% select(c(NatureServe_Common_Name, Riparian, Percent_EOs_BLM_2019, Percent_EOs_BLM_2023, Total_EOs_Rangewide_2019, Total_EOs_Rangewide_2023, BLM_SSS_States))

## show species with biggest difference in 2019 and 2023 results
sss.data %>% mutate(results.diff = abs(Percent_EOs_BLM_2019 - Percent_EOs_BLM_2023)) %>% arrange(desc(results.diff)) %>% select(c(NatureServe_Common_Name, Riparian, Percent_EOs_BLM_2019, Percent_EOs_BLM_2023, Total_EOs_Rangewide_2019, Total_EOs_Rangewide_2023, BLM_SSS_States, results.diff)) %>% 
  # filter(BLM_SSS_States != "AZ" & Total_EOs_Rangewide_2019>Total_EOs_Rangewide_2023) %>%
  head(20) 

##Subset data to the group that should appear in the applications (ESA listed)
#sss.listed <- sss.data %>% filter(!is.na(`ESA Status`) & `ESA Status` != "DL: Delisted")

##Add model review score so can assess inventory priority
mobimodels <- read_excel("C:/Users/max_tarjan/NatureServe/Map of Biodiversity Importance - Summary Tables/MoBI Modeling Summary by Species January 2021.xlsx", sheet = "MoBI_Model_Assessment", skip = 3) %>% mutate(NatureServe_Element_ID = ELEMENT_GLOBAL_ID...3, Average_Model_Review_Score = as.numeric(`Average Review Score`)) %>% select(NatureServe_Element_ID, Average_Model_Review_Score)
sss.data <- sss.data %>% left_join(mobimodels)

##Subset to ESA listed spp
sss.esa.data <- subset(sss.data, USESA_Status != "DL" & !is.na(USESA_Status))

##Write data to googlesheet
sheet_write(data = sss.esa.data, ss = "https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet = paste0("ESA_spp_", Sys.Date()))

## Show which scores were changed by app users
hq <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_2023-01-05")
suggest <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="suggested_scores")
changes <- left_join(suggest, 
                     hq %>% 
                       select('Scientific_Name', 'NatureServe_Common_Name', 'Practical_Cons_BLM_Score', 'Multispecies_Benefit_BLM_Score', 'Partnering_Opps_BLM_Score') %>% 
                       rename_with(.fn = gsub, pattern = "_", replacement = " ") %>%
                       rename_with(.fn = gsub, pattern = "BLM Score", replacement = "BLM Provisional Score")
                     ) %>% 
  mutate(`Score Changes` = "",
         `Score Changes` = ifelse(`Practical Cons BLM Provisional Score` != `Practical Cons BLM Score` & !is.na(`Practical Cons BLM Score`)| (is.na(`Practical Cons BLM Provisional Score`) & !is.na(`Practical Cons BLM Score`)), no = `Score Changes`, yes = paste(`Score Changes`, "Practical Cons;")),
         `Score Changes` = ifelse(`Multispecies Benefit BLM Provisional Score` != `Multispecies Benefit BLM Score` & !is.na(`Multispecies Benefit BLM Score`)| (is.na(`Multispecies Benefit BLM Provisional Score`) & !is.na(`Multispecies Benefit BLM Score`)), no = `Score Changes`, yes = paste(`Score Changes`, "Multispecies Benefit;")),
         `Score Changes` = ifelse(`Partnering Opps BLM Provisional Score` != `Partnering Opps BLM Score` & !is.na(`Partnering Opps BLM Score`)| (is.na(`Partnering Opps BLM Provisional Score`) & !is.na(`Partnering Opps BLM Score`)), no = `Score Changes`, yes = paste(`Score Changes`, "Partnering Opps;")),
         `Score Changes` = gsub(pattern = ";$", replacement = "", x = `Score Changes`)) %>%
  select('Reviewer Name', 'Reviewer Email', 'Reviewer Affiliation', 'Scientific Name', 'NatureServe Common Name', 'Provisional Tier', 'Practical Cons BLM Provisional Score', 'Multispecies Benefit BLM Provisional Score', 'Partnering Opps BLM Provisional Score', 'Practical Cons BLM Score', 'Multispecies Benefit BLM Score', 'Partnering Opps BLM Score', 'Score Changes', 'Notes') %>%
  rename_with(.fn = gsub, pattern = "BLM Score", replacement = "BLM Suggested Score")

metadata<-data.frame(Field = names(changes), Description = c("Name", "Email", "Affiliation", "Scientific Name", "Common Name from Biotics central", "Tier based on HQ scores", "Practical Cons BLM Score from HQ", "Multispecies Benefit BLM Score from HQ", "Partnering Opps BLM Score from HQ", "Practical Cons BLM Score approved or suggested by reviewer", "Multispecies Benefit BLM Score approved or suggested by reviewer", "Partnering Opps BLM Score approved or suggested by reviewer", "Names of scores from HQ that were adjusted by the reviewer", "Notes from reviewer"))

library(openxlsx)

results_file_name <- paste0("NatureServe - ESA Species - Suggest Scores -", Sys.Date(), ".xlsx")
wb <- openxlsx::createWorkbook()
addWorksheet(wb, "Metadata")
writeData(wb, "Metadata", metadata, headerStyle = createStyle(textDecoration = "Bold"))
addWorksheet(wb, "Suggested scores")
writeData(wb, "Suggested scores", changes, headerStyle = createStyle(textDecoration = "Bold"))
openxlsx::saveWorkbook(wb, results_file_name)