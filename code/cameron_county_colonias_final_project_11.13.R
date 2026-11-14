library(tidyverse)
library(tinytex)
library(readr)
library(readxl)
library(dplyr)
library(gt)
library(janitor)

setwd("~/Documents/GitHub/coding lab final project 2026/Final Project")
OAG_Cam_Cnty <- read_csv("TX_OAG_Cameron_Cnty_48_Categories.csv")

AM_University_Cam_Cnty <- read_csv("AM_Cam_County_style.csv")

savec <- read_csv("savec.csv")
savec <- savec |> clean_names()
names(savec)
savec <- savec |> mutate(across(where(is.character), str_trim))
View(savec)
savec <- savec |>
  mutate(health_risk_color_classification_based_on_2022_needs_assessment_observations =
           factor(str_trim(health_risk_color_classification_based_on_2022_needs_assessment_observations),
                  levels = c("low risk", "intermediate risk", "high risk")))

#mutate set risk order

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

ggplot(savec, aes(x = health_risk_color_classification_based_on_2022_needs_assessment_observations, 
                  fill = health_risk_color_classification_based_on_2022_needs_assessment_observations)) + 
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.05) +
  scale_fill_manual(values = c("low risk" = "#00FF00", 
                               "intermediate risk" = "#FFFF00",
                               "high risk" = "#FF0000")) +
  scale_x_discrete(labels = c("Low", "Intermediate", "High")) +
  labs(title = "Colonias by Health Risk Level", x = "Health risk indicator",
       y = "Number of colonias") +
  theme_minimal() + theme(legend.position = "none")

# geom_text puts the number of colonias above each bar
# fill colors the bars according to same variable as x

my_table <- savec |>
  count(health_risk_color_classification_based_on_2022_needs_assessment_observations, name = "colonias") |>
  mutate(percent = round(colonias / sum(colonias) * 100, 1))

#colonias is the count

my_table |>
  gt() |>
  tab_header(title = "Colonias by Health Risk Level") |>
  cols_label(health_risk_color_classification_based_on_2022_needs_assessment_observations = "Risk level",
             colonias = "Colonias",
             percent = "Percent")

#  risk = c("Low", "Intermediate", "High"),  count = c(9, 105, 58))

#gt takes the dataframe and creates a table

