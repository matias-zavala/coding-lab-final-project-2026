library(tidyverse)
library(tinytex)
library(readr)
library(readxl)
library(dplyr)

AM_University_Cam_Cnty <- read_csv("AM_Cam_County_style.csv")

community_count <- AM_University_Cam_Cnty %>% 
  count(Precinct, `Health Risk Indicator`, name = "Sorted Communities")

community_count %>% ggplot(aes(x = factor(Precinct), y = `Sorted Communities`, fill = `Health Risk Indicator`)) + geom_col(position = "dodge", color = "black") + labs(title = "", x = "", y = "", fill = "")


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

# OAG - A&M equivalent column accuracy comparison:
# COMM_NM == Colonia Name
# MNumber == Colonia ID
# ALTERNATEN == Alternative Name
# COMMISSIONER PCT == Precinct #
# IS_PLATTED == Plat Available & Recorded
# COLOR_CLAS == 2015 SB 827 Report
# EST_POP == Est. Household Pop. (OAG)
# INCORPORAT == Community Incorporated (OAG)
# WATERCOMMU == Water Service Provider (OAG)
# WWCOMMUNIT == Wastewater Community Service (OAG)
# TRASH_COLL == Trash Collection Available (OAG)
# NUMLOTS == # of Property Lots (OAG)

# Cross column analysis - possible crosswalk table
# LOCATION == Cross Streets / Borders
# Color_Clas == 2022 Needs Assessment Color Class Observations 
# RAINFALL_F, IN_FLOODPL == Flooding Risk FLUJOS Flood Map March 2025 Storm

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

