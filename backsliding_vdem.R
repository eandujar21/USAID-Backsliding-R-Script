# First, you need to have the devtools package installed
#install.packages("devtools")
# now, install the vdemdata package directly from GitHub
#devtools::install_github("vdeminstitute/vdemdata")

# NOTE: make sure you have an updated R version and
# - since the package is a development version - 
# an updated version of rlang, xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to contact@v-dem.net at the V-Dem Institute.

library(vdemdata)

v_dem <- vdem


#### POLITICAL ECONOMY INDEX ###################


getwd()
setwd("C:/Users/eddie/Downloads/V-Dem-CY-FullOthers-v14_csv_YyKfizl")

# CSV file path (commented out since it is a local file):
vdem <- read.csv("V-Dem-CY-Full+Others-v14.csv")

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
  z_v2peapsecon = 0.5, 
  z_gdppc = 1, 
  z_inflation = 1, 
  z_imports = 1, 
  z_fuel_income_pc = 1, 
  z_urban_pop = 1, 
  z_urbanization = 1,
  z_v2xpe_exlecon = 2
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


####### END OF POLITICAL ECONOMY INDEX #############

####### Poltical Leadership #################

# Loading V-Dem dataset
library(vdemdata)

v_dem <- vdem

# Keeping only the rows for year 2005 and afterwards
v_dem_filtered <- v_dem %>% 
  filter(year >= 2005)

# Select only the required columns

v_dem_filtered <- v_dem_filtered %>%
  select(year,country_name,v2exrescon,v2xlg_legcon,v2x_jucon,v2mecenefm,v2excrptps, v2exbribe,v2exembez,  
         v2exdfcbhs, v2exdfvths, v2exdfdmhs, v2exdfpphs, v2x_libdem)

# Analyzing distribution
#print (summary(v_dem_filtered))

# Create a summary of total values, NA counts, and percentage of missing values
#na_summary <- data.frame(
#  Column = names(v_dem_filtered),
#  Total_Values = nrow(v_dem_filtered),
#  NA_Counts = sapply(v_dem_filtered, function(x) sum(is.na(x))),
#  NA_Percentage = sapply(v_dem_filtered, function(x) mean(is.na(x)) * 100)  # Convert to percentage
#)

# Print the summary
#print(na_summary)

# Calculate the mean or median
mean_value <- mean(v_dem_filtered$v2xlg_legcon, na.rm = TRUE)
# median_value <- median(v_dem_filtered$v2xlg_legcon, na.rm = TRUE)

# Fill missing values with mean or median
v_dem_filtered$v2xlg_legcon[is.na(v_dem_filtered$v2xlg_legcon)] <- mean_value
# v_dem_filtered$v2xlg_legcon[is.na(v_dem_filtered$v2xlg_legcon)] <- median_value

# Create a summary of total values, NA counts, and percentage of missing values
na_summary <- data.frame(
  Column = names(v_dem_filtered),
  Total_Values = nrow(v_dem_filtered),
  NA_Counts = sapply(v_dem_filtered, function(x) sum(is.na(x))),
  NA_Percentage = sapply(v_dem_filtered, function(x) mean(is.na(x)) * 100)  # Convert to percentage
)

# Print the summary
print(na_summary)

# Yearly Trends of Each Variable

# Remove 'country_name' before aggregation
#yearly_avg <- aggregate(. ~ year, data = v_dem_filtered[, !names(v_dem_filtered) %in% "country_name"], FUN = mean, na.rm = TRUE)

# Number of variables (excluding 'year')
#num_vars <- ncol(yearly_avg) - 1  
# Setting image size
#options(repr.plot.width = 25, repr.plot.height = 20)
# Adjust layout and margins
#par(mfrow = c(ceiling(num_vars / 2), 2), mar = c(4, 4, 2, 1))  

# Loop through each variable and create a separate plot
#for (var in colnames(yearly_avg)[-1]) {
#  plot(yearly_avg$year, yearly_avg[[var]], type = "l", col = "blue", lwd = 2,
#       xlab = "Year", ylab = "Average Value", main = var)
#}



# Creating Correlation Plot
#library(corrplot)

# Select only numeric columns
#numeric_vars <- v_dem_filtered %>%
  #select(-year, -country_name)

# Compute correlation matrix
#cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
# Increase plot size
#options(repr.plot.width = 15, repr.plot.height = 10)
# Plot the heatmap
#corrplot(cor_matrix, method = "color", type = "full", tl.cex = 0.8, cl.cex = 0.8,
#         addCoef.col = "black", number.cex = 0.9, font.labels = 2, tl.col = "black")

# Setting image size
#options(repr.plot.width = 10, repr.plot.height = 10)

# Define the variable name mapping
#var_names <- c(
#  "v2exrescon" = "Exec_Respect_Constitution",
#  "v2xlg_legcon" = "Legislative_Constraints",
#  "v2x_jucon" = "Judicial_Independence",
#  "v2mecenefm" = "Media_Freedom",
#  "v2excrptps" = "Exec_Corruption_Perception",
#  "v2exbribe" = "Exec_Bribery",
#  "v2exembez" = "Exec_Embezzlement",
#  "v2x_execorr" = "Exec_Overall_Corruption",
#  "v2exdfcbhs" = "HeadOfState_Cabinet_Power",
#  "v2exdfvths" = "HeadOfState_Veto_Power",
#  "v2exdfdmhs" = "HeadOfState_Dismiss_Ministers",
#  "v2exdfpphs" = "HeadOfState_Propose_Laws"
#)

# Adjust margins (bottom, left, top, right)
#par(mar = c(5, 12, 6, 2))  # Increased top margin

# Select only numeric columns
#numeric_vars <- v_dem_filtered %>%
 # select(-year, -country_name)

# Compute correlation matrix
#cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Extract correlations with v2x_libdem
#cor_with_libdem <- cor_matrix["v2x_libdem", ]

# Remove self-correlation (v2x_libdem with itself)
#cor_with_libdem <- cor_with_libdem[names(cor_with_libdem) != "v2x_libdem"]

# Rename variables using the mapping
#names(cor_with_libdem) <- var_names[names(cor_with_libdem)]

# Sort correlations in descending order
#cor_with_libdem <- sort(cor_with_libdem, decreasing = TRUE)

# Assign colors based on correlation values
#bar_colors <- ifelse(cor_with_libdem > 0.5, "green",
#                     ifelse(cor_with_libdem < -0.5, "red", "blue"))

# Create a horizontal bar plot with more space for the title
#bar_positions <- barplot(cor_with_libdem, horiz = TRUE, col = bar_colors,
#                         las = 1, xlab = "Correlation Coefficient", cex.names = 0.9,
#                         ylim = c(0, length(cor_with_libdem) + 2))  # More space for title

# **Properly add title** without overlapping bars
#title(main = "Correlation with LDI", font.main = 2, cex.main = 1.2, line = 4)

# Add correlation values inside the bars
#text_x_pos <- cor_with_libdem * 0.5  # Position text inside bars (centered)
#text(x = text_x_pos, y = bar_positions, labels = round(cor_with_libdem, 2),
#     col = "black", font = 2, cex = 0.9)


### Reversing the Coding Scheme so that variables so that higher values mean more democracy
# Create a new dataframe with direction reversed for specific variables
v_dem_reversed <- v_dem_filtered

# Reverse the direction of the specified variables

v_dem_reversed$v2exdfcbhs <- v_dem_reversed$v2exdfcbhs * -1 # HOS can appoint cabinet ministers
v_dem_reversed$v2exdfvths <- v_dem_reversed$v2exdfvths * -1 # HOS has veto power
v_dem_reversed$v2exdfdmhs <- v_dem_reversed$v2exdfdmhs * -1 # HOS can dismiss ministers

# Setting image size
#options(repr.plot.width = 10, repr.plot.height = 10)

# Define the variable name mapping
#var_names <- c(
#  "v2exrescon" = "Exec_Respect_Constitution",
#  "v2xlg_legcon" = "Legislative_Constraints",
#  "v2x_jucon" = "Judicial_Independence",
#  "v2mecenefm" = "Media_Freedom",
#  "v2excrptps" = "Exec_Corruption_Perception",
#  "v2exbribe" = "Exec_Bribery",
#  "v2exembez" = "Exec_Embezzlement",
#  "v2x_execorr" = "Exec_Overall_Corruption",
#  "v2exdfcbhs" = "HeadOfState_Cabinet_Power",
#  "v2exdfvths" = "HeadOfState_Veto_Power",
#  "v2exdfdmhs" = "HeadOfState_Dismiss_Ministers",
#  "v2exdfpphs" = "HeadOfState_Propose_Laws",
#  "v2x_libdem" = "LDI"
#)

# Adjust margins (bottom, left, top, right)
#par(mar = c(5, 12, 6, 2))  # Increased top margin

# Select only numeric columns
#numeric_vars_rev <- v_dem_reversed %>%
#  select(-year, -country_name)

# Compute correlation matrix
#cor_matrix_rev <- cor(numeric_vars_rev, use = "pairwise.complete.obs")

# Extract correlations with v2x_libdem
#cor_with_libdem_rev <- cor_matrix_rev["v2x_libdem", ]

# Remove self-correlation (v2x_libdem with itself)
#cor_with_libdem_rev <- cor_with_libdem_rev[names(cor_with_libdem) != "v2x_libdem"]

# Rename variables using the mapping
#names(cor_with_libdem_rev) <- var_names[names(cor_with_libdem_rev)]

# Sort correlations in descending order
#cor_with_libdem_rev <- sort(cor_with_libdem_rev, decreasing = TRUE)

# Assign colors based on correlation values
#bar_colors <- ifelse(cor_with_libdem_rev > 0.5, "green",
#                     ifelse(cor_with_libdem_rev < -0.5, "red", "blue"))

# Create a horizontal bar plot with more space for the title
#bar_positions <- barplot(cor_with_libdem_rev, horiz = TRUE, col = bar_colors,
#                         las = 1, xlab = "Correlation Coefficient", cex.names = 0.9,
#                         ylim = c(0, length(cor_with_libdem_rev) + 2))  # More space for title

# **Properly add title** without overlapping bars
#title(main = "Correlation with LDI", font.main = 2, cex.main = 1.2, line = 4)

# Add correlation values inside the bars
#text_x_pos <- cor_with_libdem_rev * 0.5  # Position text inside bars (centered)
#text(x = text_x_pos, y = bar_positions, labels = round(cor_with_libdem_rev, 2),
#     col = "black", font = 2, cex = 0.9)

### Grouping Base Variables into Broad Dimensions

# Compute new variables
v_dem_grouped <- v_dem_reversed %>%
  mutate(
    Executive_Accountability = rowMeans(select(., v2exrescon, v2xlg_legcon, v2x_jucon, v2mecenefm), na.rm = TRUE),
    Corruption_Misuse = rowMeans(select(., v2excrptps, v2exbribe, v2exembez), na.rm = TRUE),
    Executive_Power = rowMeans(select(., v2exdfcbhs, v2exdfvths, v2exdfdmhs, v2exdfpphs), na.rm = TRUE)
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
#correlation_matrix <- cor(v_dem_yearly[, c("Executive_Accountability", "Corruption_Misuse", "Executive_Power", "Governance_Index")], 
 #                         use = "complete.obs")  # Exclude missing values
# Generate the correlation plot
#corrplot(correlation_matrix, method = "color", type = "full", 
#         addCoef.col = "black", tl.col = "black", tl.srt = 45, number.cex = 0.8)

# historical Trend Plotting

# Ensure numeric columns (excluding 'year' and 'country_name')
#numeric_cols <- setdiff(names(v_dem_yearly), c("year", "country_name"))
#v_dem_yearly[, numeric_cols] <- lapply(v_dem_yearly[, numeric_cols], as.numeric)

# Aggregate by year (ignoring 'country_name')
#agg_data <- aggregate(. ~ year, data = v_dem_yearly[, c("year", numeric_cols)], FUN = mean, na.rm = TRUE)

# Setting image size
#options(repr.plot.width = 15, repr.plot.height = 10)

# Setting image size and adjusting margins to create space below the graph
#par(mar = c(7, 5, 5, 2))  # Increase bottom margin

# Plot all columns on one graph using matplot (without title)
#matplot(agg_data$year, agg_data[, -1], type = "o", pch = 16, lty = 1, 
#        col = c("blue", "red", "green", "purple"),
#        xlab = "Year", ylab = "Index Value", main = "")

# Add a horizontal legend outside the top border
#legend("top", legend = colnames(agg_data)[-1], col = c("blue", "red", "green", "purple"), 
#       pch = 16, lty = 1, horiz = TRUE, xpd = TRUE, inset = -0.1)

# Add the title below the graph
#mtext("Governance Index and Its Constituents Indicators Over Time", 
#      side = 1, line = 5, font = 2, cex = 1.2)

###### End of Political Leadership ############# 





####### POLITICAL INSTITUTIONS ###########

# Subset

subset_data2 <- vdem %>%
  select(
    v2x_jucon,
    v2x_corr,
    v2xps_party,
    year,
    country_name,
    v2x_libdem
  )

library(dplyr)

# Filter the dataset to include only years from 2005 onwards
vdem_institution <- subset_data2 %>%
  filter(year >= 2005)



library(dplyr)

# Create PII (Political Institutional Index) and add it to the dataset
vdem_institution <- subset_data2 %>%
  filter(year >= 2004) %>%
  mutate(
    z_jucon = as.numeric(scale(v2x_jucon)),  
    z_corr = as.numeric(scale(v2x_corr)),
    z_party = as.numeric(scale(v2xps_party)),
    PII = (z_jucon + z_corr + z_party) / 3  # Final Political Institutional Index
  )

# View the first few rows
head(vdem_institution)

view(vdem_institution)



######## END OF POLITICAL INSTITUTIONS ################



### Transformation of LibDem to Percent Change ######

vdem_cleanest <- vdem_cleaner %>%
  group_by(country_name) %>%  # Group by country to calculate change within each country
  arrange(year) %>%  # Ensure data is in chronological order
  mutate(LDI_pct_change = (v2x_libdem - lag(v2x_libdem)) / lag(v2x_libdem) * 100) %>%
  ungroup()

view(vdem_cleanest)

vdem_cleanest %>%
  group_by(year) %>%
  summarise(
    mean_LDI = mean(LDI_pct_change, na.rm = TRUE),
    median_LDI = median(LDI_pct_change, na.rm = TRUE),
    min_LDI = min(LDI_pct_change, na.rm = TRUE),
    min_country = country_name[which.min(LDI_pct_change)],
    max_LDI = max(LDI_pct_change, na.rm = TRUE),
    max_country = country_name[which.max(LDI_pct_change)]
  ) %>%
  arrange(year) %>%
  walk(~ cat(
    "Year:", .x$year, "\n",
    "  Mean LDI % Change:", round(.x$mean_LDI, 2), "\n",
    "  Median LDI % Change:", round(.x$median_LDI, 2), "\n",
    "  Min LDI % Change:", round(.x$min_LDI, 2), "(", .x$min_country, ")", "\n",
    "  Max LDI % Change:", round(.x$max_LDI, 2), "(", .x$max_country, ")", "\n\n"
  ))

###############################################

## SOCIAL STRUCTURE AND POLITICAL COALITIONS ##
subset_data <- v_dem %>%
  select(
    year,
    country_name,
    histname, # time-specific country name
    country_id,
    e_mipopula, # population total in thousands
    e_wb_pop, # total population from World Bank
    e_miurbani, # urbanization rate
    e_miurbpop, # total urban population
    e_regiongeo, #geographic region
    e_regionpol, #politico-geographic region
    e_fh_status, # Freedom House status
    e_wbgi_pve, # World Bank political stability
    e_p_polity, # Polity score
    v2x_polyarchy,     # Electoral democracy index 
    v2x_libdem,        # Liberal democracy
    v2x_egaldem,       # Egalitarian democracy
    v2x_partipdem,     # Participatory democracy
    v2x_delibdem,      # Deliberative democracy
    v2regsupgroupssize,        # Regime support groups size
    v2regoppgroupssize, # Regime opposition groups size
    v2regsuploc, # Regime support location
    v2clacjust, #social class equality in civil liberties
    v2clsocgrp,     # social group equality
    v2clrgunev,     # sub national civil lib unevenness      
    v2pepwrses, # power dist by SES
    v2pepwrsoc, # power dist by social group
    v2clpolcl, # political group equality civil lib
    v2peapsecon, # access to public services by SES
    v2peapssoc, # access to public services by social group
    v2clgencl, # gender equality civil liberties
    v2peapsgen,  # access to public services by gender
    v2pepwrgen, # power dist by gender
    v2pepwrgeo, # power dist by urban-rural
    v2peapsgeo, # access to public services by urban-rural
    v2clgeocl #urban-rural equality civil liberties
  )

# Filter the data set for years including and after 2005 
subset_data_yr <- subset_data %>%
  filter(year >= 2005)                    

# Create new data set containing only variables that will be included in index
social_index_data <- subset_data_yr %>%
  select(
    v2regsupgroupssize,        # Regime support groups size
    v2clacjust, #social class equality in civil liberties
    v2clsocgrp,     # social group equality
    v2pepwrses, # power dist by SES
    v2pepwrsoc, # power dist by social group
    v2clpolcl, # political group equality civil lib
    v2peapsecon, # access to public services by SES
    v2clgencl, # gender equality civil liberties
    v2pepwrgen, # power dist by gender
  )
                     
# Aggregate variables into a single summary index by calculating the simple mean
# Add summary index as new column to data set
subset_data_yr$social_index <- rowMeans(social_index_data, na.rm = TRUE)    
                     ###  END OF SOCIAL STRUCTURE ###

view(subset_data_yr)


#######  INTERNATIONAL FACTORS #########
#International Factors Dataset with LibDemIndex
international_factors_dataset <- v_dem %>% 
  filter(year >= 2005)  %>%
  select(country_name, country_text_id, country_id, year, v2x_libdem, v2svdomaut, v2svinlaut,
         v2elintmon, v2elmonden,v2svindep, v2regsupgroups_13) %>%
  rename(liberal_democracy = v2x_libdem, domestic_policy_autonomy = v2svdomaut,
         international_policy_autonomy = v2svinlaut, international_election_monitors = v2elintmon, 
         election_monitor_denied = v2elmonden, independent_state = v2svindep, 
         foreign_regime_support = v2regsupgroups_13)

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
                     
#Change the Directon of Selected Vatiables to better reflect imapact on LDI
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
  mutate(democracy_index = mean(c_across(all_of(selected_vars)), na.rm = TRUE)) %>%
  ungroup()

view(mutated_data)

#regression of variables in international factors on LDI
#make sure it is % change 
mutated_data <- mutated_data %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(ldi_pct_change = (liberal_democracy - lag(liberal_democracy)) / abs(lag(liberal_democracy)) * 100) %>%
  ungroup()

# Run the regression
model_pct_change <- lm(ldi_pct_change ~ ., data = mutated_data %>% select(ldi_pct_change, all_of(selected_vars)))

# Display regression results
summary(model_pct_change)

#Regression of the Index on % Change in LDI
index_model <- lm(ldi_pct_change ~ index, data = mutated_data)
summary(index_model)



###### END OF INT FACTORS ########




######### POLITICAL CULTURE ##########

# Filter data for years and select relevant variables
vdem_filtered <- vdem %>%
  filter(year >= 2004) %>%
  select(year, country_name, v2x_libdem, 
         v2capolit, v2canonpol, v2caassemb, 
         v2elirreg, v2psnatpar, v2pscomprg, 
         v2caautmob, v2mecorrpt, v2castate)

# Standardize all selected variables (except year & country_name)
vdem_standardized <- vdem_filtered %>%
  mutate(across(-c(year, country_name), ~ scale(.)[,1]))

# Create summary indices
vdem_standardized <- vdem_standardized %>%
  rowwise() %>%
  mutate(
    Citizen_Engagement_Index = mean(c(v2capolit, v2canonpol, v2caassemb), na.rm = TRUE),
    Electoral_Integrity_Index = mean(c(v2elirreg, v2psnatpar, v2pscomprg), na.rm = TRUE),
    State_Control_Index = mean(c(v2caautmob, v2mecorrpt, v2castate), na.rm = TRUE)
  ) %>%
  ungroup()

# Reverse State Control Index so that higher values = better democracy
vdem_standardized <- vdem_standardized %>%
  mutate(State_Control_Index_Reversed = -State_Control_Index)

# Create a single "Political Culture Index"
vdem_standardized <- vdem_standardized %>%
  rowwise() %>%
  mutate(
    Political_Culture_Index = mean(c(Citizen_Engagement_Index, 
                                     Electoral_Integrity_Index, 
                                     State_Control_Index_Reversed), na.rm = TRUE)
  ) %>%
  ungroup()

# View summary statistics for Political Culture Index
summary(vdem_standardized$Political_Culture_Index)



####### END OF POLITICAL CULTURE ##################










####### COMBINING ALL DATA INTO ONE FRAME #############


# Select only the required columns from mutated_data
new_dataset <- mutated_data %>%
  select(country_name, year, democracy_index) 

# Join with subset_data_yr to add the social_index column
new_dataset <- new_dataset %>%
  left_join(select(subset_data_yr, country_name, year, social_index), 
            by = c("country_name", "year"))

# View the result
print(new_dataset)

view(new_dataset)




# Select required columns from mutated_data
new_dataset <- mutated_data %>%
  select(country_name, year, democracy_index, liberal_democracy) 

# Join with subset_data_yr to add social_index
new_dataset <- new_dataset %>%
  left_join(select(subset_data_yr, country_name, year, social_index), 
            by = c("country_name", "year"))

# View the result
print(new_dataset)






# Select required columns from mutated_data
new_dataset <- mutated_data %>%
  select(country_name, year, democracy_index, liberal_democracy) 

# Join with subset_data_yr to add social_index
new_dataset <- new_dataset %>%
  left_join(select(subset_data_yr, country_name, year, social_index), 
            by = c("country_name", "year")) %>%
  
  # Join with v_dem_yearly to add Governance_Index
  left_join(select(v_dem_yearly, country_name, year, Governance_Index), 
            by = c("country_name", "year"))

# View the result
print(new_dataset)
view(new_dataset)




# Add Political_Culture_Index from vdem_standardized to new_dataset
new_dataset <- new_dataset %>%
  left_join(select(vdem_standardized, country_name, year, Political_Culture_Index), 
            by = c("country_name", "year"))

# View the updated dataset
print(head(new_dataset))




# Select required columns from mutated_data
new_dataset <- mutated_data %>%
  select(country_name, year, democracy_index, liberal_democracy) 

# Join with subset_data_yr to add social_index
new_dataset <- new_dataset %>%
  left_join(select(subset_data_yr, country_name, year, social_index), 
            by = c("country_name", "year")) %>%
  
  # Join with v_dem_yearly to add Governance_Index
  left_join(select(v_dem_yearly, country_name, year, Governance_Index), 
            by = c("country_name", "year")) %>%
  
  # Join with vdem_institution to add PII
  left_join(select(vdem_institution, country_name, year, PII), 
            by = c("country_name", "year"))

# View the result
print(new_dataset)


# Select required columns from mutated_data
new_dataset <- mutated_data %>%
  select(country_name, year, democracy_index, liberal_democracy) 

# Sequentially join datasets to add more variables
new_dataset <- new_dataset %>%
  left_join(select(subset_data_yr, country_name, year, social_index), 
            by = c("country_name", "year")) %>%
  left_join(select(v_dem_yearly, country_name, year, Governance_Index), 
            by = c("country_name", "year")) %>%
  left_join(select(vdem_institution, country_name, year, PII), 
            by = c("country_name", "year")) %>%
  left_join(select(vdem_cleaner, country_name, year, PEI_weighted), 
            by = c("country_name", "year"))

# View the final dataset
print(new_dataset)


view(new_dataset)
view(vdem_cleanest)






# Assuming new_dataset has been created as per previous steps

# Join with vdem_cleanest to add LDI_pct_change
new_dataset <- new_dataset %>%
  left_join(select(vdem_cleanest, country_name, year, LDI_pct_change), 
            by = c("country_name", "year"))

# View the updated dataset
print(new_dataset)



view(new_dataset)



#### REGRESSION #########


# Rename the columns in new_dataset
new_dataset <- new_dataset %>%
  rename(
    international_index = democracy_index,
    institutions_index = PII,
    economy_index = PEI_weighted
  )

# View the result
print(new_dataset)



# Run linear regression
model <- lm(LDI_pct_change ~ ., data = new_dataset %>% select(-country_name, -year, -liberal_democracy))

# View summary of regression results
summary(model)


# Run linear regression
model2 <- lm(liberal_democracy ~ ., data = new_dataset %>% select(-country_name, -year, -LDI_pct_change))

summary(model2)







### TIME LAG MODEL (WORK IN PROGRESS) #####



# Step 1: Compute the difference in LDI_pct_change from 2006 to 2011
ldi_change <- new_dataset %>%
  filter(year %in% c(2006, 2011)) %>%
  select(country_name, year, LDI_pct_change) %>%
  spread(key = year, value = LDI_pct_change) %>%
  mutate(ldi_change = `2011` - `2006`) %>%
  select(country_name, ldi_change)

# Step 2: Extract independent variables from 2006, keeping only `_index` variables
independent_vars_2006 <- new_dataset %>%
  filter(year == 2006) %>%
  select(country_name, ends_with("_index"))  # Keep only variables ending in "_index"

# Step 3: Merge the computed difference with independent variables from 2006
regression_data <- independent_vars_2006 %>%
  left_join(ldi_change, by = "country_name") %>%
  select(-country_name)  # Drop country_name to avoid categorical encoding

# Step 4: Run the regression
model <- lm(ldi_change ~ ., data = regression_data)

# View the regression results
summary(model)











library(dplyr)
library(tidyr)

# Step 1: Compute the difference in LDI_pct_change from 2006 to 2011
ldi_change <- new_dataset %>%
  filter(year %in% c(2006, 2011)) %>%
  select(country_name, year, LDI_pct_change) %>%
  pivot_wider(names_from = year, values_from = LDI_pct_change) %>%
  mutate(ldi_change = `2011` - `2006`) %>%
  select(country_name, ldi_change)

# Step 2: Extract independent variables from 2006, keeping only `_index` variables
independent_vars_2006 <- new_dataset %>%
  filter(year == 2006) %>%
  select(country_name, ends_with("_index"))  

# Step 3: Merge the computed difference with independent variables from 2006
regression_data <- independent_vars_2006 %>%
  left_join(ldi_change, by = "country_name") %>%
  drop_na()  # Remove missing values

# Step 4: Run six separate regressions, one for each independent variable
independent_vars <- names(regression_data)[names(regression_data) != "ldi_change"]

models <- lapply(independent_vars, function(var) {
  formula <- as.formula(paste("ldi_change ~", var))
  lm(formula, data = regression_data)
})

# Step 5: Display regression results
lapply(models, summary)














##### LDI Percent Change Panel Data ###########



library(dplyr)
library(plm)

# Step 1: Create lagged independent variables (by one time period, e.g., 5 years)
panel_data <- new_dataset %>%
  arrange(country_name, year) %>%  # Ensure correct order
  group_by(country_name) %>%
  mutate(across(ends_with("_index"), lag, n = 5)) %>%  # Lag all _index variables by 5 years
  ungroup()

# Step 2: Filter out the first few years where lagged data is missing
panel_data <- panel_data %>%
  filter(!is.na(liberal_democracy) & year >= 2011)  # Ensure lagged vars exist

# Step 3: Convert to panel data format
panel_data <- pdata.frame(panel_data, index = c("country_name", "year"))

# Step 4: Run a fixed effects model with all indices, including LDI_pct_change as the dependent variable
model_fe_ldi <- plm(LDI_pct_change ~ economy_index + Governance_Index + 
                      social_index + international_index + institutions_index, 
                    data = panel_data, effect = "individual", model = "within")

# Step 5: Run a random effects model for comparison
model_re_ldi <- plm(LDI_pct_change ~ economy_index + Governance_Index + 
                      social_index + international_index + institutions_index, 
                    data = panel_data, model = "random")

# Step 6: Compare models
summary(model_fe_ldi)
summary(model_re_ldi)

# Step 7: Hausman test
phtest(model_fe_ldi, model_re_ldi)





######## Liberal Democracy Panel Data (with lag) ######

# Create a lag for liberal_democracy (shift by 5 years)
panel_data_lagged <- panel_data %>%
  arrange(country_name, year) %>%
  group_by(country_name) %>%
  mutate(liberal_democracy_lagged = lag(liberal_democracy, 5)) %>%
  ungroup()


# Run a fixed effects model with the current year's indices and the lagged liberal democracy
model_fe_ldi_lagged <- plm(liberal_democracy_lagged ~ economy_index + Governance_Index + 
                             social_index + international_index + institutions_index, 
                           data = panel_data_lagged, effect = "individual", model = "within")

# Run a random effects model for comparison
model_re_ldi_lagged <- plm(liberal_democracy_lagged ~ economy_index + Governance_Index + 
                             social_index + international_index + institutions_index, 
                           data = panel_data_lagged, model = "random")

# Step 3: Compare models
summary(model_fe_ldi_lagged)
summary(model_re_ldi_lagged)

# Step 4: Hausman test
phtest(model_fe_ldi_lagged, model_re_ldi_lagged)



view(panel_data_lagged)





summary(new_dataset$LDI_pct_change)