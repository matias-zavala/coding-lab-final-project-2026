library(tidyverse)
library(tinytex)
library(readr)
library(dplyr)

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

getwd()
list.files("/Users/matiaszavala/Documents/GitHub/coding lab final project 2026")

AM_University_Cam_Cnty <- read_csv("AM_Cam_County_style.csv")

community_count <- AM_University_Cam_Cnty %>% 
  count(color, `Health Risk Indicator`, name = "Sorted Communities")

#what is the health risk across colonias

community_count %>% ggplot(aes(x = "TAMU NAO 2022", y = ` `, 
  fill = "Health Risk Classification")) + 
  geom_col(position = "dodge", color = "black") + 
  labs(title = "Colonia Health Risks", x = "TAMU NAO 2022", 
       y = "Number of Colonias", fill = "Health Risk")

# logistic regression

# separate out indicator from classification: colors from comparsion 
# relation within unit across: another graph/table

summary_table <-
  AM_University_Cam_Cnty %>%
  group_by(Precinct) %>%
  summarize(`2022 Needs Assessment Observations Health-Risk Color Classification` 
            = sum(`2022 Needs Assessment Observations Health-Risk Color Classification`, na.rm = TRUE),
            total_additional_deaths = sum(diff, na.rm = TRUE),
            percent_diff = 100 * total_additional_deaths / expected_deaths)






