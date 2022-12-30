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
sss.esa <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/ESA listed BLM SSS to score for Prioritization Tool_7Nov2022.xlsx")

## Get available EOs
#eos <-  read_excel("C:/Users/max_tarjan/OneDrive - NatureServe/Documents/Species-Select/Data/Biotics_EO_Summary.xlsx", sheet = "EO_Summary_202207")
#eos <- eos %>% select(c(ELEMENT_GLOBAL_ID, SUBNATION_CODE, NUM_CURRENT_EOS)) %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(NUM_CURRENT_EOS = sum(NUM_CURRENT_EOS))

#sss.esa <- left_join(sss.esa, eos, by = c(`NatureServe Element ID` = "ELEMENT_GLOBAL_ID"))

#write.csv(sss.esa, "C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/EOs for ESA listed BLM SSS.csv", row.names= F)

## CREATE BACKEND DATASHEETS FOR SHINY APPLICATIONS

## define species ids for selection
ids <- sss.esa$`NatureServe Element ID`

## Connect to central biotics to pull out most recent data on sss
con<-odbcConnect("centralbiotics", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

qry <- paste0("SELECT
 egt.element_global_id
 , 'ELEMENT_GLOBAL.'||egt.element_global_ou_uid||'.'||egt.element_global_seq_uid eguid
 , GETNAMECATDESC(egt.gname_id) Name_Category
 , informal_grp((informal_tax(egt.element_global_id))) informal_grp
 , informal_tax(egt.element_global_id) Informal_Tax
 , sn.SCIENTIFIC_NAME gname
 , egt.g_primary_common_name
 , egt.G_RANK grank
 , egt.ROUNDED_G_RANK rnd
 , egt.g_rank_review_date
 , d_usesa.display_value usesa
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
       and el_global_threats_assess.element_global_id =  egt.element_global_id )   as NO_KNOWN_THREATS

, DelimList('SELECT DECODE(s.nation_id,139,''MX'','''')
    || s.subnation_code ||   ''''  AS subnatl_dist '
    || ' FROM element_subnational, subnation s, taxon_subnatl_sprot_ext ts_ext
         WHERE element_subnational.subnation_id = s.subnation_id and element_subnational.element_subnational_id = ts_ext.element_subnational_id
         and ts_ext.blm_sss_2019 = ''Y''
         and element_subnational.element_national_id IN ' 
    || '(SELECT element_national_id FROM element_national WHERE element_global_id='  
    || egt.element_global_id || ') ORDER BY s.nation_id desc, SUBNATL_DIST ', ', ') 
  AS BLM_SSS_states

, DelimList('SELECT iucn_threat_category_desc FROM el_global_threats_assess, d_iucn_threat_category WHERE 
        el_global_threats_assess.d_iucn_threat_category_id = d_iucn_threat_category.d_iucn_threat_category_id and el_global_threats_assess.element_global_id = '||egt.element_global_id || 'ORDER BY iucn_threat_category_desc', '; ') AS THREATS_DESC

, DelimList('SELECT iucn_threat_category_cd FROM el_global_threats_assess, d_iucn_threat_category WHERE 
        el_global_threats_assess.d_iucn_threat_category_id = d_iucn_threat_category.d_iucn_threat_category_id and el_global_threats_assess.element_global_id = '||egt.element_global_id || 'ORDER BY iucn_threat_category_cd', '; ') AS THREATS

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
/*and nas.agency_name like 'BLM West 2019-11%'
and ent.nation_id = 225 
and egt.inactive_ind = 'N'*/
and egt.element_global_id IN (", paste0(ids, collapse = ", "),")

ORDER BY 
sn.d_name_category_id,name_category, informal_grp, informal_tax, scientific_name
;")

##SELECT iucn_threat_category_desc FROM el_global_threats_assess, d_iucn_threat_category WHERE egt.element_global_id = el_global_threats_assess.element_global_id and el_global_threats_assess.d_iucn_threat_category_id = d_iucn_threat_category.d_iucn_threat_category_id') AS THREATS

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

# When finished, close the connection
odbcClose(con)

sss.data <- dat %>% 
  filter(!duplicated(ELEMENT_GLOBAL_ID)) %>%
  unique() %>%
  mutate(Explorer.url = paste0("https://explorer.natureserve.org/Taxon/", EGUID, "/", sub(x=GNAME, pattern = " ", replacement = "_")),
         ExplorerPro.url = paste0("https://explorer.natureserve.org/pro/Map/?taxonUniqueId=", EGUID),
         Riparian = ifelse((!is.na(RIVERINE_HABITATS) & !RIVERINE_HABITATS=="(?i)Aerial") | grepl(PALUSTRINE_HABITATS, pattern = "(?i)riparian"), T, F),
         `Habitat_Wetland/riparian` =  ifelse((!is.na(RIVERINE_HABITATS) & RIVERINE_HABITATS!="(?i)Aerial") | !is.na(PALUSTRINE_HABITATS), T, F),
         `Habitat_scrub/shrubland` = ifelse(grepl(TERRESTRIAL_HABITATS, pattern = "(?i)scrub|(?i)shrub|(?i)savanna"), T, F),
         `Habitat_grassland/steppe/prairie` = ifelse(grepl(TERRESTRIAL_HABITATS, pattern = "(?i)grassland|(?i)steppe|(?i)prairie|(?i)Old Field|(?i)barrens"), T, F),
         NS_Range_Restricted = ifelse(grepl(RANGE_EXTENT_CD, pattern = "A|B|C|D|E") & !grepl(RANGE_EXTENT_CD, pattern = "G|H"), T, F),
         BLM_Threats = ifelse(grepl(strsplit(THREATS, split = "; "), pattern = "(?<!\\d|\\.)1.2|(?<!\\d|\\.)1.3|(?<!\\d|\\.)2.3|(?<!\\d|\\.)3.1|(?<!\\d|\\.)3.2|(?<!\\d|\\.)3.3|(?<!\\d|\\.)6.1|(?<!\\d|\\.)3", perl=T), T, F),
         Rank_Review_Year = format(G_RANK_REVIEW_DATE, "%Y"),
         USESA = sub(x = USESA, pattern = "\\:.*", replacement = "")) %>%
  # select(ELEMENT_GLOBAL_ID, NAME_CATEGORY, INFORMAL_TAX, GNAME, G_PRIMARY_COMMON_NAME, RND, USESA, BLM_SSS_STATES, RANGE_EXTENT_DESC, RANGE_EXTENT_CD, Explorer.url, ExplorerPro.url) %>%
 rename(NatureServe_Element_ID = ELEMENT_GLOBAL_ID, Major_Group= NAME_CATEGORY, Higher_Level_Informal_Group = INFORMAL_GRP, Lower_Level_Informal_Group = INFORMAL_TAX, Scientific_Name = GNAME, NatureServe_Common_Name = G_PRIMARY_COMMON_NAME, Rounded_Global_Rank = RND, ESA_Status = USESA, BLM_SSS_States = BLM_SSS_STATES)

## Add data from BLM
BLM.scores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="BLM_scores_ESA_spp") %>%
  rename_with(~gsub(.x, pattern = " ", replacement = "_"))

sss.data <- sss.data %>% 
  left_join(y = subset(BLM.scores, select = c("NatureServe_Element_ID", "USFWS_Recovery_Priority_Num", "BLM_Practicability_Score", "BLM_Multispecies_Score", "BLM_Partnering_Score", "Endemic", "HQ_Notes")) %>% rename(BLM_Range_Restricted = Endemic))

## Add results from jurisdictional analysis
ja <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Provided to BLM/BLM - Information for T & E Strategic Decision-Making - October 2022.xlsx", sheet = "BLM SSS Information by State", skip = 1) %>% 
  mutate(Percent_EOs_BLM_2019 = as.numeric(`Occurrences on BLM Lands (West) / Total Occurrences Rangewide`)*100,
         Percent_Model_Area_BLM = `Percent Suitable Habitat on BLM Lands (West)`*100) %>%
  rename("NatureServe_Element_ID" = "Element Global ID")

sss.data <- sss.data %>% left_join(y=subset(ja, select = c("NatureServe_Element_ID", "Percent_EOs_BLM_2019", "Percent_Model_Area_BLM", "Total Occurrences Rangewide", "Total Occurrences on BLM Lands (West)")))

##add more recent JA results
ja2022<- read.csv("C:/Users/max_tarjan/OneDrive - NatureServe/Documents/Partners-In-Conservation/Output-2022-12-28/blm_eo_jurisdiction_results-2022-12-28.csv") %>% 
  select(EGT_ID, eos_total, Percent_eo_BLM, Percent_eo_AB_BLM, eos_total, BLM) %>%
  rename("NatureServe_Element_ID" = "EGT_ID",
         "Percent_EOs_BLM_2022" = "Percent_eo_BLM",
         "eos_total_2022" = "eos_total",
         "eos_BLM" = "BLM")
sss.data <- sss.data %>% left_join(ja2022)

##plot JA results for 2022 versus 2019
plot(data = subset(sss.data, `Total Occurrences on BLM Lands (West)`<50), eos_BLM ~ `Total Occurrences on BLM Lands (West)`)

##Subset data to the group that should appear in the applications (ESA listed)
#sss.listed <- sss.data %>% filter(!is.na(`ESA Status`) & `ESA Status` != "DL: Delisted")

##Write data to googlesheet
sheet_write(data = sss.data, ss = "https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet = paste0("ESA_spp_", Sys.Date()))