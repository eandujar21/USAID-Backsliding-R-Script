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


#### POLITICAL ECONOMY INDEX


#getwd()
#setwd("C:/Users/eddie/Downloads/V-Dem-CY-FullOthers-v14_csv_YyKfizl")

# CSV file path (commented out since it is a local file):
# vdem <- read.csv("V-Dem-CY-Full+Others-v14.csv")

library(tidyverse)

subset_data <- vdem %>%
  select(
    e_miinflat,        # Inflation
    e_total_fuel_income_pc, # Petroleum, coal, and natural gas production per capita
    e_gdppc,
    e_cow_imports,     # Imports
    v2clstown,         # State ownership of economy
    e_miurbpop,        # Urban population
    e_miurbani,        # Urbanization
    v2x_libdem,         # Liberal democracy
    v2peapsecon,
    v2xpe_exlecon,
    e_gdp,
    year,
    country_name,
    v2x_libdem
  )

library(dplyr)

# Filter the dataset to include only years from 2005 onwards
vdem_cleaner <- subset_data %>%
  filter(year >= 2005)







library(dplyr)

vdem_cleaner <- vdem_cleaner %>%
  mutate(
    z_gdp = scale(e_gdp),
    z_gdppc = scale(e_gdppc),
    z_inflation = -scale(e_miinflat),
    z_imports = scale(e_cow_imports),
    z_fuel_income_pc = scale(e_total_fuel_income_pc),
    z_urban_pop = scale(e_miurbpop),
    z_urbanization = scale(e_miurbani),
    z_v2peapsecon = scale(v2peapsecon),  # New variable for economic equality of the population
    z_v2xpe_exlecon = -scale(v2xpe_exlecon)  # New variable for economic equality of the executive
  )

# Compute the PEI index with equal weighting
vdem_cleaner <- vdem_cleaner %>%
  rowwise() %>%
  mutate(PEI = mean(c_across(starts_with("z_")), na.rm = TRUE)) %>%
  ungroup()

# Display the first few rows with country names and PEI
vdem_cleaner %>%
  select(country_name, year, PEI) %>%
  arrange(desc(PEI)) %>%
  head(10)  # View top 10 highest PEI countries



# Assigning higher weights to z_gdp and z_v2peapsecon (e.g., 2x weight for these two)
weights <- c(
  z_gdp = 2, 
  z_v2peapsecon = 2, 
  z_gdppc = 1, 
  z_inflation = 1, 
  z_imports = 1, 
  z_fuel_income_pc = 1, 
  z_urban_pop = 1, 
  z_urbanization = 1,
  z_v2xpe_exlecon = 1
)


library(dplyr)
library(purrr)

# Compute the weighted PEI index correctly
vdem_cleaner <- vdem_cleaner %>%
  rowwise() %>%
  mutate(
    PEI_weighted = sum(
      map2_dbl(
        c_across(starts_with("z_")),
        weights[names(weights) %in% colnames(select(., starts_with("z_")))],
        `*`
      ),
      na.rm = TRUE
    ) / sum(weights[names(weights) %in% colnames(select(., starts_with("z_")))])
  ) %>%
  ungroup()

summary(vdem_cleaner$PEI_weighted)


####### END OF POLITICAL ECONOMY INDEX