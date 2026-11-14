getwd()
read_csv("Public_Health_Statistics.csv")
# R code: save in your R script and run in RStudio, not in the Terminal.
# Install tidyverse once in the Console if needed:
# install.packages("tidyverse")
library(tidyverse)
getwd()
list.files("data")

data_file <- file.path("~/Documents/GitHub/coding-lab-project/data/Public_Health_Statistics.csv")
raw_data <- read_csv(
  data_file,
  col_types = cols(.default = col_character())
)
head(raw_data)
names(raw_data)
# R code: save in your R script and run in RStudio, not in the Terminal.
dim(raw_data)   # Rows, then columns
nrow(raw_data)
ncol(raw_data)
names(raw_data)
head(raw_data)
glimpse(raw_data)
# Optional in RStudio: View(raw_data)

clean_data <- raw_data
colSums(is.na(clean_data))
#2 Childhood Blood Lead Level Screening 
#3 Childhood Lead Poisoning 
#13 Gonorrhea in Females 
#comm area = 0, all else = 1
sum(duplicated(clean_data)). #0


# to look at unincorprated need new data set outside of chicago

# R code: save in your R script and run in RStudio, not in the Terminal.
class(clean_data[[".   "]])
head(clean_data[[".   "]])
sum(is.na(clean_data[[".    "]]))

# Convert the column in your working table.
clean_data[["your_column"]] <-
  parse_number(raw_data[["your_column"]])

# Check the result and compare with the original above.
class(clean_data[["Community Area Name"]])
head(clean_data[["Community Area Name"]])
sum(is.na(clean_data[["Community Area Name"]]))
