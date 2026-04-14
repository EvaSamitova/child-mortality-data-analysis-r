##################################
# DATA Project Assignment
# Eva Samitova
##################################

##################################
# Load Required Packages
##################################

library(tidyverse)
library(readxl)
library(RJSONIO)
library(ggplot2)

##################################
# Step 1: Check files
##################################

cat("\nFiles in working directory:\n")
print(list.files())

##################################
# Step 2: Load Excel Data
##################################

mortality_data <- read_excel("child_mortality_rates.xlsx", sheet = 1)

cat("\n--- Mortality Data Preview ---\n")
print(head(mortality_data))

##################################
# Step 3: Load JSON Data (Shots)
##################################

json_url <- "https://www.murach.com/python_analysis/shots.json"

if (!file.exists("shots.json")) {
  download.file(json_url, "shots.json", mode = "wb")
}

json_data <- fromJSON("shots.json")

column_names <- json_data[["resultSets"]][[1]][["headers"]]
rows <- json_data[["resultSets"]][[1]][["rowSet"]]

shots <- data.frame()
for (row in rows) {
  shots <- rbind(shots, row)
}

names(shots) <- column_names
shots <- as_tibble(shots)

cat("\n--- Shots Data Preview ---\n")
print(head(shots))

##################################
# Step 4: Create plots folder
##################################

if (!dir.exists("plots")) {
  dir.create("plots")
}

##################################
# Step 5: Plot 1 - Ages 1–4 Trend
##################################

p1 <- ggplot(mortality_data, aes(x = Year, y = `01-04 Years`)) +
  geom_line() +
  geom_point(size = 1) +
  labs(
    title = "Child Mortality Trend: Ages 1-4",
    x = "Year",
    y = "Death Rate"
  ) +
  theme_minimal()

ggsave("plots/plot1_mortality_age_1_4.png", plot = p1, width = 8, height = 5)

##################################
# Step 6: Plot 2 - All Age Groups
##################################

mortality_long <- mortality_data %>%
  pivot_longer(
    cols = -Year,
    names_to = "Age_Group",
    values_to = "Death_Rate"
  )

p2 <- ggplot(mortality_long, aes(x = Year, y = Death_Rate, linetype = Age_Group)) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Child Mortality Trends by Age Group",
    x = "Year",
    y = "Death Rate",
    linetype = "Age Group"
  ) +
  theme_minimal()

ggsave("plots/plot2_mortality_all_groups.png", plot = p2, width = 9, height = 5)

##################################
# Step 7: Plot 3 - Latest Year
##################################

latest_year_data <- mortality_long %>%
  filter(Year == max(Year))

p3 <- ggplot(latest_year_data, aes(x = Age_Group, y = Death_Rate)) +
  geom_col() +
  labs(
    title = paste("Child Mortality by Age Group in", max(mortality_long$Year)),
    x = "Age Group",
    y = "Death Rate"
  ) +
  theme_minimal()

ggsave("plots/plot3_mortality_latest_year.png", plot = p3, width = 8, height = 5)

##################################
# Step 8: Plot 4 - Shots by Period
##################################

shots_plot_data <- shots %>%
  filter(!is.na(PERIOD)) %>%
  count(PERIOD, sort = FALSE)

p4 <- ggplot(shots_plot_data, aes(x = as.factor(PERIOD), y = n)) +
  geom_col() +
  labs(
    title = "Shot Attempts by Period",
    x = "Period",
    y = "Count"
  ) +
  theme_minimal()

ggsave("plots/plot4_shots_by_period.png", plot = p4, width = 8, height = 5)

##################################
# Done
##################################

cat("\nAll plots saved in 'plots' folder.\n")
