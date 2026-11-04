library(tidyverse)
library(tinytex)
library(readr)
library(readxl)
library(dplyr)

# R code: save in your R script and run in RStudio, not in the Terminal.
# Install tidyverse once in the Console if needed:
# install.packages("tidyverse")
getwd()
list.files("/Users/matiaszavala/Documents/GitHub/coding lab final project 2026")

AM_University_Cam_Cnty <- read_csv("AM_Cam_County_style.csv")

community_count <- AM_University_Cam_Cnty %>% 
  count(Precinct, `Health Risk Indicator`, name = "Sorted Communities")

community_count %>% ggplot(aes(x = factor(Precinct), y = `Sorted Communities`, 
  fill = `Health Risk Indicator`)) + geom_col(position = "dodge", color = "black") + 
  labs(title = "Colonia Health Risks by precinct", x = "Precinct", 
       y = "Number of Colonias", fill = "Health Risk Indicator"
       )


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



