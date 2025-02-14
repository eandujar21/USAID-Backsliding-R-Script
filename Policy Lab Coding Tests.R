rm(list=ls())
library(tidyverse)
library(haven)
library(readxl)
library(ggplot2)
library(dplyr)
options(scipen = 999) 
options(digits = 10)

# Install the development version of the vdemdata package 
# (the package is an ongoing project, 
# keep checking for updates)

# First, you need to have the devtools package installed
install.packages("devtools")
# now, install the vdemdata package directly from GitHub
devtools::install_github("vdeminstitute/vdemdata")

# NOTE: make sure you have an updated R version and
# - since the package is a development version - 
# an updated version of rlang, xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to contact@v-dem.net at the V-Dem Institute.

library(vdemdata)

v_dem <- vdem

#filter to relevant years 
#filter to international variables
#keep liberal democracy variable 

international_factors_dataset <- v_dem %>% 
  filter(year >= 2005)  %>%
  select(country_name, country_text_id, country_id, year, v2x_libdem, v2svdomaut, v2svinlaut,
         v2elintmon, v2elmonden,v2svindep, v2regsupgroups_13) %>%
  rename(liberal_democracy = v2x_libdem, domestic_policy_autonomy = v2svdomaut,
         international_policy_autonomy = v2svinlaut, international_election_monitors = v2elintmon, 
         election_monitor_denied = v2elmonden, independent_state = v2svindep, 
         foreign_regime_support = v2regsupgroups_13)

write.csv(international_factors_dataset, "international_factor.csv", row.names = FALSE)


#liberal democracy graph

# Ensure the year column is treated as numeric
international_factors_dataset$year <- as.numeric(international_factors_dataset$year)

# Get unique country names
countries <- unique(international_factors_dataset$country_name)

# Define number of countries per plot
num_countries_per_plot <- 10

# Split countries into groups of 10
country_groups <- split(countries, ceiling(seq_along(countries) / num_countries_per_plot))

# Loop through each group and create a line plot
for (i in seq_along(country_groups)) {
  group <- country_groups[[i]]
  
  # Filter dataset for selected countries
  df_subset <- international_factors_dataset %>% filter(country_name %in% group)
  
  # Create the plot
  plot <- ggplot(df_subset, aes(x = year, y = liberal_democracy, color = country_name, group = country_name)) +
    geom_line(size = 1, linetype = "dashed") +
    geom_point(size = 2) +
    labs(title = paste("Liberal Democracy Trends (Group", i, ")"),
         x = "Year",
         y = "Liberal Democracy Index",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Display the plot
  print(plot)
}

#change LDI from absolute change to % change and then plot in relationship to independent variable
# Compute LDI percentage change
mutated_data <- mutated_data %>%
  group_by(country_id) %>%  # Ensure calculation is done within each country
  arrange(year, .by_group = TRUE) %>%  # Ensure time order
  mutate(ldi_pct_change = (liberal_democracy - lag(liberal_democracy)) / abs(lag(liberal_democracy)) * 100) %>%
  ungroup() %>%
  mutate(ldi_pct_change = replace(ldi_pct_change, is.infinite(ldi_pct_change), NA))  # Replace Inf with NA

# Scatter plot: LDI % Change vs Independent Variables
selected_vars_plot <- c("domestic_policy_autonomy", "international_policy_autonomy",
                        "international_election_monitors", "election_monitor_denied",
                        "election_monitor_refused", "independent_state", "foreign_regime_support")

# Loop through variables and create plots
plot_list <- lapply(selected_vars_plot, function(var) {
  ggplot(mutated_data, aes_string(x = var, y = "ldi_pct_change")) +
    geom_point(alpha = 0.5) +  # Scatter plot
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear trend
    labs(title = paste("LDI % Change vs", var),
         x = var,
         y = "LDI % Change") +
    theme_minimal()
})

# Display the plots
print(plot_list)



#normalize index using z score - bigger score is better for democracy 

international_factors_dataset <- v_dem %>% 
  filter(year >= 2005) %>%
  select(country_name, country_text_id, country_id, year, v2x_libdem, v2svdomaut, v2svinlaut,
         v2elintmon, v2elmonden, v2elmonref, v2svindep, v2regsupgroups_13) %>%
  rename(liberal_democracy = v2x_libdem, 
         domestic_policy_autonomy = v2svdomaut,
         international_policy_autonomy = v2svinlaut, 
         international_election_monitors = v2elintmon, 
         election_monitor_denied = v2elmonden, 
         election_monitor_refused = v2elmonref, 
         independent_state = v2svindep, 
         foreign_regime_support = v2regsupgroups_13)

mutated_data <- international_factors_dataset %>%
  mutate(across(c(election_monitor_denied, election_monitor_refused, foreign_regime_support), ~ - .x))

selected_vars <- c("domestic_policy_autonomy", 
                   "international_policy_autonomy", 
                   "international_election_monitors", 
                   "election_monitor_denied", 
                   "election_monitor_refused", 
                   "independent_state", 
                   "foreign_regime_support")

# Replace NAs with 0
mutated_data[selected_vars] <- mutated_data[selected_vars] %>% replace(is.na(.), 0)

# Standardize & Compute Index
mutated_data <- mutated_data %>%
  mutate(across(all_of(selected_vars), ~ as.numeric(scale(.x)))) %>%
  rowwise() %>%
  mutate(index = mean(c_across(all_of(selected_vars))))


#regression of variables in international factors on LDI

#regression of index on LDI






