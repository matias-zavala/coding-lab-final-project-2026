library(tidyverse)
library(tinytex)
library(readr)
library(readxl)
library(dplyr)

AM_University_Cam_Cnty <- read_csv("AM_Cam_County_style.csv")

risk_count <- AM_University_Cam_Cnty %>% count(`2022 Needs Assessment Observations Health-Risk Color Classifications`, name = "risk_count")

risk_count %>% ggplot(aes(x = ))

#community_count <- AM_University_Cam_Cnty %>% count(`Health Risk Indicator`, 
#                                                    name = "Number of Colonias")
community_count <- AM_University_Cam_Cnty %>% 
  count(Precinct, `Health Risk Indicator`, name = "Sorted Precincts")

community_count %>% ggplot(aes(x = factor(Precinct), y = `Sorted Precincts`, 
  fill = `Health Risk Indicator`)) + 
  geom_col(position = "dodge", color = "black") + 
  labs(title = "Colonia Precinct Health Risk Indicators", x = "Precinct", 
       y = "Number of Colonias")

#community_count %>%
#  ggplot(aes(x = `Health Risk Indicator`, y = `Number of Colonias`)) +
#  geom_col(color = "black")
