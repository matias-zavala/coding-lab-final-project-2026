library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(tidyverse)

savec <- read_csv("savec.csv")
savec <- savec |> clean_names()
names(savec)
savec <- savec |> mutate(across(where(is.character), str_trim))
View(savec)

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
