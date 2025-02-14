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

# Filter the dataset to include only years from 1990 onwards
vdem_cleaner <- subset_data %>%
  filter(year >= 1990)







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

####### Poltical Leadership #################
# Loading V-Dem dataset
library(vdemdata)

v_dem <- vdem

# Keeping only the rows for year 2005 and afterwards
v_dem_filtered <- v_dem %>% 
  filter(year >= 2005)

# Select only the required columns

v_dem_filtered <- v_dem_filtered %>%
  select(year,country_name,v2exrescon,v2xlg_legcon,v2x_jucon,v2mecenefm,v2excrptps, v2exbribe,v2exembez, v2exhoshog, 
         v2exdfcbhs, v2exdfvths, v2exdfdmhs, v2exdfpphs, v2x_libdem)

# Analyzing distribution
print (summary(v_dem_filtered))

# Create a summary of total values, NA counts, and percentage of missing values
na_summary <- data.frame(
  Column = names(v_dem_filtered),
  Total_Values = nrow(v_dem_filtered),
  NA_Counts = sapply(v_dem_filtered, function(x) sum(is.na(x))),
  NA_Percentage = sapply(v_dem_filtered, function(x) mean(is.na(x)) * 100)  # Convert to percentage
)

# Print the summary
print(na_summary)

# Creating Correlation Plot
library(corrplot)

# Select only numeric columns
numeric_vars <- v_dem_filtered %>%
  select(-year, -country_name)

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
# Increase plot size
options(repr.plot.width = 15, repr.plot.height = 10)
# Plot the heatmap
corrplot(cor_matrix, method = "color", type = "full", tl.cex = 0.8, cl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.9, font.labels = 2, tl.col = "black")

# Setting image size
options(repr.plot.width = 10, repr.plot.height = 10)

# Define the variable name mapping
var_names <- c(
  "v2exrescon" = "Exec_Respect_Constitution",
  "v2xlg_legcon" = "Legislative_Constraints",
  "v2x_jucon" = "Judicial_Independence",
  "v2mecenefm" = "Media_Freedom",
  "v2excrptps" = "Exec_Corruption_Perception",
  "v2exbribe" = "Exec_Bribery",
  "v2exembez" = "Exec_Embezzlement",
  "v2x_execorr" = "Exec_Overall_Corruption",
  "v2exhoshog" = "HeadOfState_HeadOfGov",
  "v2exdfcbhs" = "HeadOfState_Cabinet_Power",
  "v2exdfvths" = "HeadOfState_Veto_Power",
  "v2exdfdmhs" = "HeadOfState_Dismiss_Ministers",
  "v2exdfpphs" = "HeadOfState_Propose_Laws"
)

# Adjust margins (bottom, left, top, right)
par(mar = c(5, 12, 6, 2))  # Increased top margin

# Select only numeric columns
numeric_vars <- v_dem_filtered %>%
  select(-year, -country_name)

# Compute correlation matrix
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Extract correlations with v2x_libdem
cor_with_libdem <- cor_matrix["v2x_libdem", ]

# Remove self-correlation (v2x_libdem with itself)
cor_with_libdem <- cor_with_libdem[names(cor_with_libdem) != "v2x_libdem"]

# Rename variables using the mapping
names(cor_with_libdem) <- var_names[names(cor_with_libdem)]

# Sort correlations in descending order
cor_with_libdem <- sort(cor_with_libdem, decreasing = TRUE)

# Assign colors based on correlation values
bar_colors <- ifelse(cor_with_libdem > 0.5, "green",
                     ifelse(cor_with_libdem < -0.5, "red", "blue"))

# Create a horizontal bar plot with more space for the title
bar_positions <- barplot(cor_with_libdem, horiz = TRUE, col = bar_colors,
                         las = 1, xlab = "Correlation Coefficient", cex.names = 0.9,
                         ylim = c(0, length(cor_with_libdem) + 2))  # More space for title

# **Properly add title** without overlapping bars
title(main = "Correlation with LDI", font.main = 2, cex.main = 1.2, line = 4)

# Add correlation values inside the bars
text_x_pos <- cor_with_libdem * 0.5  # Position text inside bars (centered)
text(x = text_x_pos, y = bar_positions, labels = round(cor_with_libdem, 2),
     col = "black", font = 2, cex = 0.9)


### Reversing the Coding Scheme so that All Variables Align Positively with LDI
# Create a new dataframe with direction reversed for specific variables
v_dem_reversed <- v_dem_filtered

# Reverse the direction of the specified variables
v_dem_reversed$v2exdfpphs <- v_dem_reversed$v2exdfpphs * -1 # HOS can propse new laws
v_dem_reversed$v2excrptps <- v_dem_reversed$v2excrptps * -1 # Perception of corruption
v_dem_reversed$v2exbribe <- v_dem_reversed$v2exbribe * -1 # Extent of Bribery
v_dem_reversed$v2exembez <- v_dem_reversed$v2exembez * -1 # Misuse of state resources
# v_dem_reversed$v2mecenefm <- v_dem_reversed$v2mecenefm * -1 # Media censorship 

# Setting image size
options(repr.plot.width = 10, repr.plot.height = 10)

# Define the variable name mapping
var_names <- c(
  "v2exrescon" = "Exec_Respect_Constitution",
  "v2xlg_legcon" = "Legislative_Constraints",
  "v2x_jucon" = "Judicial_Independence",
  "v2mecenefm" = "Media_Freedom",
  "v2excrptps" = "Exec_Corruption_Perception",
  "v2exbribe" = "Exec_Bribery",
  "v2exembez" = "Exec_Embezzlement",
  "v2x_execorr" = "Exec_Overall_Corruption",
  "v2exhoshog" = "HeadOfState_HeadOfGov",
  "v2exdfcbhs" = "HeadOfState_Cabinet_Power",
  "v2exdfvths" = "HeadOfState_Veto_Power",
  "v2exdfdmhs" = "HeadOfState_Dismiss_Ministers",
  "v2exdfpphs" = "HeadOfState_Propose_Laws",
  "v2x_libdem" = "LDI"
)

# Adjust margins (bottom, left, top, right)
par(mar = c(5, 12, 6, 2))  # Increased top margin

# Select only numeric columns
numeric_vars_rev <- v_dem_reversed %>%
  select(-year, -country_name)

# Compute correlation matrix
cor_matrix_rev <- cor(numeric_vars_rev, use = "pairwise.complete.obs")

# Extract correlations with v2x_libdem
cor_with_libdem_rev <- cor_matrix_rev["v2x_libdem", ]

# Remove self-correlation (v2x_libdem with itself)
cor_with_libdem_rev <- cor_with_libdem_rev[names(cor_with_libdem) != "v2x_libdem"]

# Rename variables using the mapping
names(cor_with_libdem_rev) <- var_names[names(cor_with_libdem_rev)]

# Sort correlations in descending order
cor_with_libdem_rev <- sort(cor_with_libdem_rev, decreasing = TRUE)

# Assign colors based on correlation values
bar_colors <- ifelse(cor_with_libdem_rev > 0.5, "green",
                     ifelse(cor_with_libdem_rev < -0.5, "red", "blue"))

# Create a horizontal bar plot with more space for the title
bar_positions <- barplot(cor_with_libdem_rev, horiz = TRUE, col = bar_colors,
                         las = 1, xlab = "Correlation Coefficient", cex.names = 0.9,
                         ylim = c(0, length(cor_with_libdem_rev) + 2))  # More space for title

# **Properly add title** without overlapping bars
title(main = "Correlation with LDI", font.main = 2, cex.main = 1.2, line = 4)

# Add correlation values inside the bars
text_x_pos <- cor_with_libdem_rev * 0.5  # Position text inside bars (centered)
text(x = text_x_pos, y = bar_positions, labels = round(cor_with_libdem_rev, 2),
     col = "black", font = 2, cex = 0.9)

### Grouping Base Variables into Broad Dimensions

# Compute new variables
v_dem_grouped <- v_dem_reversed %>%
  mutate(
    Executive_Accountability = rowMeans(select(., v2exrescon, v2xlg_legcon, v2x_jucon, v2mecenefm), na.rm = TRUE),
    Corruption_Misuse = rowMeans(select(., v2excrptps, v2exbribe, v2exembez), na.rm = TRUE),
    Executive_Power = rowMeans(select(., v2exhoshog, v2exdfcbhs, v2exdfvths, v2exdfdmhs, v2exdfpphs), na.rm = TRUE)
  )

# Aggregate data by year and country_name
v_dem_yearly <- v_dem_grouped %>%
  group_by(year, country_name) %>%  # Group by both year and country
  summarise(
    Executive_Accountability = mean(Executive_Accountability, na.rm = TRUE),
    Corruption_Misuse = mean(Corruption_Misuse, na.rm = TRUE),
    Executive_Power = mean(Executive_Power, na.rm = TRUE),
    .groups = "drop"  # Ungroup after summarization
  )

### Creation of a Single Index
# Perform PCA on the scaled variables
pca_model <- prcomp(v_dem_yearly[, c("Executive_Accountability", "Corruption_Misuse", "Executive_Power")], 
                     center = TRUE, scale. = FALSE)

# Add the first principal component as the new index
v_dem_yearly <- v_dem_yearly %>%
  mutate(Governance_Index = pca_model$x[, 1])

# Select relevant columns for correlation
correlation_matrix <- cor(v_dem_yearly[, c("Executive_Accountability", "Corruption_Misuse", "Executive_Power", "Governance_Index")], 
                          use = "complete.obs")  # Exclude missing values
# Generate the correlation plot
corrplot(correlation_matrix, method = "color", type = "full", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex = 0.8)


###### End of Political Leadership ############# 
