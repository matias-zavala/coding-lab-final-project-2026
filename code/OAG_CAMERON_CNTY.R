library(tidyverse)
library(tinytex)
library(readr)
library(readxl)
library(dplyr)


OAG_Cam_Cnty <- read_csv("TX_OAG_Cameron_Cnty_48_Categories.csv")

AM_University_Cam_Cnty <- read_csv("AM_Cam_County_style.csv")

###############################################
# A&M had staff and volunteers cross check data manually with OAG map

# Project will inspect human errors rather than importing data with R
  # data fields to crossref:
      # name matches id, 2015 color class, Alternative Name, est. pop, incorp
      # water service, wastewater service, trash collection, # prop lots, plat

# A&M prioritized observable data: Flooding (FLUJOS), plat, incorp, 
  # and street lights, but not recorded in data
# A&M did not strictly abide by water access, paved roads, trash coll, and WW

# Project will identify vectors with diff data sources:
# Current color class, flood risk, 

# ? What benefit / diff of A&M recording OAG prop lots, #Prop CAD, and 
# residential prop ?

# A&M would benefit from recording roads paved, roads passable, in floodplain,
# streetlights
########################################################

Filtered_OAG <- OAG_Cam_Cnty %>%
  select(COMM_NM, MNUMBER, ALTERNATEN, COUNTYNAME, LOCATION, 
         IS_PLATTED...9, PLATDATERE, PLATVOLUME, PLATPAGE, COLOR_CLAS, 
         EST_POP, NEARESTCIT, INCORPORAT, NUMLOTS, NUMOCCUPIE, SUBDNOTES,  
         TWDB_ID, NUMOCCUPIE, TRASH_COLL, WW_COLLECT, INADEQUATE, 
         RAINFALL_F, IN_FLOODPL, RDS_PASSAB, RDS_PAVED, INFRASTRUCTURE)

OAG_AM_MNumberdf <- Filtered_OAG %>% full_join(
  AM_University_Cam_Cnty, by = c("MNUMBER" = "Colonia ID")) 

OAG_AM_Name <- Filtered_OAG %>% full_join(
  AM_University_Cam_Cnty, by = c("COMM_NM" = "Colonia Name"))

AM_University_Cam_Cnty %>%
  ggplot(aes(x = `Health Risk Indicator`)) +
  geom_bar()

AM_University_Cam_Cnty %>%
  ggplot(aes(x = `2022 Needs Assessment Observations Health-Risk Color Classification`))+
  geom_histogram(binwidth = 1)






AM_University_Cam_Cnty %>%
  ggplot(aes(x = Precinct, color = `Health Risk Indicator`)) +
  geom_line(aes(y = )) +
theme_minimal() +
  labs(x = "Precinct",
       title = "Colonia Health Risk Indicators" ,
       subtitle = "Source: Texas A&M Colonias Program",
       color = "") +
  theme(legend.position = "bottom")


