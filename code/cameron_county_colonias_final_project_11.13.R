library(tidyverse)
library(tinytex)
library(readr)
library(readxl)
library(dplyr)
library(gt)
library(janitor)

setwd("~/Documents/GitHub/coding lab final project 2026/data")
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


#colonias is the count

my_table |>
  gt() |>
  tab_header(title = "Cameron County Health Risk") |>
  cols_label(
    health_risk_color_classification_based_on_2022_needs_assessment_observations = "Risk level",
             colonias = "Colonias",
             percent = "Percent")

#  risk = c("Low", "Intermediate", "High"),  count = c(9, 105, 58))

#gt takes the dataframe and creates a table

yes <- "\u2611"   # ☑ checked box
no  <- "\u2612"   # ☒ boxed X

colonia_tbl <- tribble(
  ~service,                 ~green, ~yellow, ~red, 
  "Drinkable Water",        yes,    yes,     no,   
  "Waste Water Disposal",   yes,    yes,     no,   
  "Legal Plats",            yes,    yes,     no,   
  "Paved Roads",            yes,    no,      na,   
  "Adequate Drainage",      yes,    no,      na,   
  "Solid Waste Disposal",   yes,    no,      na,   
)

risk_pct <- savec |>
  count(health_risk_color_classification_based_on_2022_needs_assessment_observations,
        name = "colonias") |>
  mutate(percent = colonias / sum(colonias) * 100)

ggplot(risk_pct, aes(x = health_risk_color_classification_based_on_2022_needs_assessment_observations,
                     y = percent,
                     fill = health_risk_color_classification_based_on_2022_needs_assessment_observations)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percent, 1), "%")), vjust = -0.5) +
  scale_fill_manual(values = c("low risk" = "#00FF00",
                               "intermediate risk" = "#FFFF00",
                               "high risk" = "#FF0000")) +
  scale_x_discrete(labels = c("Low", "Intermediate", "High")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Colonias by Health Risk Level",
       x = "Health risk indicator",
       y = "Percent of colonias") +
  theme_minimal() +
  theme(legend.position = "none")

savec <- savec |>
  rename(risk = health_risk_color_classification_based_on_2022_needs_assessment_observations) |>
  mutate(risk_num = as.numeric(risk))   # low = 1, intermediate = 2, high = 3

savec <- savec |>
  mutate(
    text = str_to_lower(paste(summary, infrastructure_major_needs_utilities_paved_roads_street_lights_etc )),
    needs_water    = str_detect(text, "water"),
    needs_sewer    = str_detect(text, "wastewater|sewer|septic"),
    needs_drainage = str_detect(text, "drainage|flood"),
    needs_roads    = str_detect(text, "road|pav|street"),
    needs_trash    = str_detect(text, "trash|solid waste|garbage"),
    needs_plat     = str_detect(text, "plat")
  )

heat <- savec |>
  pivot_longer(starts_with("needs_"), names_to = "need", values_to = "flagged") |>
  group_by(risk, need) |>
  summarise(pct = mean(flagged, na.rm = TRUE) * 100, .groups = "drop") |>
  mutate(need = str_remove(need, "needs_"))

ggplot(heat, aes(x = risk, y = need, fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(pct), "%"))) +
  scale_fill_gradient(low = "#F7FBFF", high = "#B30000", name = "% of colonias") +
  scale_x_discrete(labels = c("Low", "Intermediate", "High")) +
  labs(title = "Share of colonias mentioning each need, by risk level",
       x = "Health risk level", y = NULL) +
  theme_minimal()

need_cols <- c("needs_water", "needs_sewer", "needs_drainage",
              "needs_roads", "needs_trash", "needs_plat")

results <- map_dfr(need_cols, \(v) {
  tibble(
    need     = v,
    rho      = cor(savec$risk_num, as.numeric(savec[[v]]), method = "spearman",
                   use = "complete.obs"),
    p_fisher = fisher.test(table(savec$risk, savec[[v]]))$p.value
  )
})

results |> arrange(p_fisher)