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

qry <- "SELECT * FROM"

dat<-sqlQuery(con, qry); head(dat) ##import the queried table

# When finished, it's a good idea to close the connection
odbcClose(con)