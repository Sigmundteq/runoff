# Load the tidyverse package
library(tidyverse)
library(tidyr)

# Read the pr.csv, tas.csv, and discharge.csv datasets
discharge_data <- subset(read.csv("discharge.csv"), year >= 1979 & year <= 2015)
tas_data <- subset(read.csv("tas.csv"), year >= 1979 & year <= 2015)
pr_data <- subset(read.csv("pr.csv"), year >= 1979 & year <= 2015)

# Reshape pr_data and tas_data into a long format
pr_data_long <- pr_data %>%
  pivot_longer(cols = -c(year, month), names_to = "grid_id", values_to = "pr") %>%
  mutate(grid_id = gsub("pr", "", grid_id)) # Extract grid_id

tas_data_long <- tas_data %>%
  pivot_longer(cols = -c(year, month), names_to = "grid_id", values_to = "tas") %>%
  mutate(grid_id = gsub("tas", "", grid_id)) # Extract grid_id

# Combine pr_data_long, tas_data_long, and discharge_data
combine_data <- cbind(pr_data_long, tas_data_long)
combine_data <- select(combine_data,-c(5,6,7))

# Print the first few rows of the combined dataset
head(combine_data)

# Calculate the mean for each group of 313 rows
mean_data <- combine_data %>%
  mutate(group_id = rep(1:(n() %/% 313), each = 313)) %>%
  group_by(group_id) %>%
  summarise(mean_pr = mean(pr),
            mean_tas = mean(tas))
mean_data <- cbind(mean_data, discharge_data)
mean_data <- select(mean_data,-c(1,4,5))

# Save the combined dataset to a new CSV file
write.csv(mean_data, "mean_data.csv", row.names = FALSE)

