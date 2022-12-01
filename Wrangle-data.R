## Wrangle data
## M Tarjan
## Nov 9, 2022

## Load packages
library(readxl)
library(tidyverse)
library(RODBC)

## Check which species have EOs in the spatial snapshot for jurisdictional analysis
## Load sss from esa batch
sss.esa <- read_excel("C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/ESA listed BLM SSS to score for Prioritization Tool_7Nov2022.xlsx")

## Get available EOs
eos <-  read_excel("C:/Users/max_tarjan/OneDrive - NatureServe/Documents/Species-Select/Data/Biotics_EO_Summary.xlsx", sheet = "EO_Summary_202207")
eos <- eos %>% select(c(ELEMENT_GLOBAL_ID, SUBNATION_CODE, NUM_CURRENT_EOS)) %>% group_by(ELEMENT_GLOBAL_ID) %>% summarise(NUM_CURRENT_EOS = sum(NUM_CURRENT_EOS))

sss.esa <- left_join(sss.esa, eos, by = c(`NatureServe Element ID` = "ELEMENT_GLOBAL_ID"))

#write.csv(sss.esa, "C:/Users/max_tarjan/NatureServe/BLM - BLM SSS Distributions and Rankings Project-FY21/Species Prioritization Tool/ESA Species/EOs for ESA listed BLM SSS.csv", row.names= F)

## Connect to central biotics to pull out most recent data on sss
con<-odbcConnect("centralbiotics", uid="biotics_report", pwd=rstudioapi::askForPassword("Password"))

qry <- "SELECT 
 egt.element_global_id
 ,GETNAMECATDESC(egt.gname_id) Name_Category
 , informal_grp((informal_tax(egt.element_global_id))) informal_grp
 , informal_tax(egt.element_global_id) Informal_Tax
 , sn.SCIENTIFIC_NAME gname
 , egt.g_primary_common_name
 , egt.G_RANK grank
 , egt.ROUNDED_G_RANK rnd
 , d_usesa.display_value usesa
 , d_short_term_trend.display_value S_Trend

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

, DelimList('SELECT DECODE(s.nation_id,139,''MX'','''')
    || s.subnation_code ||   ''''  AS subnatl_dist '
    || ' FROM element_subnational, subnation s, taxon_subnatl_sprot_ext ts_ext
         WHERE element_subnational.subnation_id = s.subnation_id and element_subnational.element_subnational_id = ts_ext.element_subnational_id
         and ts_ext.blm_sss = ''Y''
         and element_subnational.element_national_id IN ' 
    || '(SELECT element_national_id FROM element_national WHERE element_global_id='  
    || egt.element_global_id || ') ORDER BY s.nation_id desc, SUBNATL_DIST ', ', ') 
  AS BLM_SSS_states

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
and nas.agency_name like 'BLM West 2019-11%'
and ent.nation_id = 225 
and egt.inactive_ind = 'N'

ORDER BY 
sn.d_name_category_id,name_category, informal_grp, informal_tax, scientific_name
;"

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

# When finished, it's a good idea to close the connection
odbcClose(con)