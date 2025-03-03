# Specify the file path
#file_path <- "C:/Users/eddie/OneDrive/Documents/GitHub/USAID-Backsliding-R-Script/new_dataset.csv"

# Read the dataset into R
#new_dataset <- read.csv(file_path)


old_dataset <- read.csv("C:/Users/Eddie Andujar/Downloads/all_indices.csv")


# View the first few rows of the dataset
head(new_dataset)

library(tidyverse)

view(new_dataset)





# Get the column names of the dataset
colnames(new_dataset)


colnames(new_dataset) <- c(
  "country_name", 
  "year", 
  "international_index",  # Rename democracy_index to international_index
  "liberal_democracy", 
  "social_index", 
  "Governance_Index",     # Rename Political_Leadership to Governance_Index
  "institutions_index",   # Rename PII to institutions_index
  "economy_index",        # Rename PEI_weighted to economy_index
  "Political_Culture_Index"
)


view(new_dataset)


library(ggplot2)

# Scatterplot comparing economy_index and liberal_democracy
ggplot(new_dataset, aes(x = economy_index, y = liberal_democracy)) +
  geom_point(alpha = 0.5) +  # Scatter points with transparency
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Linear regression with confidence interval
  labs(
    title = "Economy Index vs. Liberal Democracy",
    x = "Economy Index",
    y = "Liberal Democracy Index"
  ) +
  theme_minimal()





# Scatterplot comparing international_index and liberal_democracy
ggplot(new_dataset, aes(x = international_index, y = liberal_democracy)) +
  geom_point(alpha = 0.5) +  # Scatter points with transparency
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Linear regression with confidence interval
  labs(
    title = "International Index vs. Liberal Democracy",
    x = "International Index",
    y = "Liberal Democracy Index"
  ) +
  theme_minimal()












####### REGRESSION ###########

# Load the plm package for panel data regression
library(plm)

# Convert the dataset to a pdata.frame for panel data
panel_data <- pdata.frame(new_dataset, index = c("country_name", "year"))

# Check the structure of the panel data
str(panel_data)









# Load the plm package
library(plm)

# Run the regression with liberal_democracy as the dependent variable
regression_model <- plm(
  liberal_democracy ~ international_index + social_index + Governance_Index + 
    institutions_index + economy_index + Political_Culture_Index, 
  data = panel_data, 
  model = "within"  # This specifies the fixed effects model
)

# Summary of the regression model
summary(regression_model)











########## CHANGES AND LAGS #############


# Create the dependent variable (difference in liberal democracy) with NA for the first row
panel_data$liberal_democracy_diff <- c(NA, diff(panel_data$liberal_democracy))

# Lag the independent variables (shift by 1 year)
panel_data$international_index_lag <- lag(panel_data$international_index, 1)
panel_data$social_index_lag <- lag(panel_data$social_index, 1)
panel_data$Governance_Index_lag <- lag(panel_data$Governance_Index, 1)
panel_data$institutions_index_lag <- lag(panel_data$institutions_index, 1)
panel_data$economy_index_lag <- lag(panel_data$economy_index, 1)
panel_data$Political_Culture_Index_lag <- lag(panel_data$Political_Culture_Index, 1)

# Remove missing values due to lag or diff
panel_data <- na.omit(panel_data)

# Check if the number of rows matches now
str(panel_data)  # Ensure that the rows align properly

# Run the regression with liberal_democracy_diff as the dependent variable
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_lag + social_index_lag + Governance_Index_lag + 
    institutions_index_lag + economy_index_lag + Political_Culture_Index_lag, 
  data = panel_data, 
  model = "within"  # Country-level fixed effects
)

# Summary of the regression model
summary(regression_model_backsliding)












# Load dplyr package if it's not already loaded
library(dplyr)

# Create the difference in liberal democracy by grouping by country
panel_data <- panel_data %>%
  arrange(country_name, year) %>%  # Make sure the data is sorted by country and year
  group_by(country_name) %>%  # Group by country_name to calculate differences for each country
  mutate(liberal_democracy_diff = liberal_democracy - lag(liberal_democracy)) %>%  # Calculate the difference
  ungroup()  # Ungroup after the mutation




# Check for missing values in the new liberal_democracy_diff column
table(is.na(panel_data$liberal_democracy_diff))






# Create lagged variables for each independent variable
panel_data$international_index_lag <- lag(panel_data$international_index, 1)
panel_data$social_index_lag <- lag(panel_data$social_index, 1)
panel_data$Governance_Index_lag <- lag(panel_data$Governance_Index, 1)
panel_data$institutions_index_lag <- lag(panel_data$institutions_index, 1)
panel_data$economy_index_lag <- lag(panel_data$economy_index, 1)
panel_data$Political_Culture_Index_lag <- lag(panel_data$Political_Culture_Index, 1)

# Now, run the regression with the lagged variables
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_lag + social_index_lag + Governance_Index_lag + 
    institutions_index_lag + economy_index_lag + Political_Culture_Index_lag, 
  data = panel_data, 
  model = "within"
)

# Display the regression summary
summary(regression_model_backsliding)






##### POTENTIALLY INTERESTING #######



# Step 1: Load the new dataset (assuming it's already loaded as 'new_dataset')
# Create the dependent variable: Difference in liberal democracy (t+1 - t)
new_dataset$liberal_democracy_diff <- c(NA, diff(new_dataset$liberal_democracy))

# Step 2: Lag the independent variables at time t
new_dataset$international_index_lag <- lag(new_dataset$international_index, 1)
new_dataset$social_index_lag <- lag(new_dataset$social_index, 1)
new_dataset$Governance_Index_lag <- lag(new_dataset$Governance_Index, 1)
new_dataset$institutions_index_lag <- lag(new_dataset$institutions_index, 1)
new_dataset$economy_index_lag <- lag(new_dataset$economy_index, 1)
new_dataset$Political_Culture_Index_lag <- lag(new_dataset$Political_Culture_Index, 1)

# Step 3: Drop rows with NA values (because of lag and diff)
new_dataset_clean <- na.omit(new_dataset)

# Step 4: Run the regression with country-level fixed effects
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_lag + social_index_lag + Governance_Index_lag + 
    institutions_index_lag + economy_index_lag + Political_Culture_Index_lag, 
  data = new_dataset_clean, 
  model = "within"
)

# Step 5: Display the regression summary
summary(regression_model_backsliding)




####### END OF POTENTIALLY INTERESTING #####

view(new_dataset)

######### POTENTIALLY EVEN MORE INTERESTING ######




# Step 1: Load necessary library
library(dplyr)

# Step 2: Create lagged variables and dependent variable
panel_data_lagged <- panel_data  # Use the original dataset

# Group by country_name and create lagged variables within each country
panel_data_lagged <- panel_data_lagged %>%
  group_by(country_name) %>%
  arrange(country_name, year) %>%
  mutate(
    international_index_lag = lag(international_index, k = 1),
    social_index_lag = lag(social_index, k = 1),
    Governance_Index_lag = lag(Governance_Index, k = 1),
    institutions_index_lag = lag(institutions_index, k = 1),
    economy_index_lag = lag(economy_index, k = 1),
    Political_Culture_Index_lag = lag(Political_Culture_Index, k = 1),
    liberal_democracy_diff = liberal_democracy - lag(liberal_democracy, k = 1)
  ) %>%
  ungroup()  # Ungroup after operation

# Step 3: Remove rows with missing values in key columns
panel_data_lagged_clean <- na.omit(panel_data_lagged)  # Remove rows with any missing values

# Step 4: Filter for years 2006 to 2020
panel_data_lagged_clean <- panel_data_lagged_clean[panel_data_lagged_clean$year >= 2006 & panel_data_lagged_clean$year <= 2020, ]

# Step 5: Convert to pdata.frame to define panel structure
panel_data_lagged_clean <- pdata.frame(panel_data_lagged_clean, index = c("country_name", "year"))

# Step 6: Run the regression with t-1 independent variables
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_lag + social_index_lag + Governance_Index_lag + 
    institutions_index_lag + economy_index_lag + Political_Culture_Index_lag, 
  data = panel_data_lagged_clean, 
  model = "within"
)

# Step 7: Display the regression summary
summary(regression_model_backsliding)




view(panel_data_lagged)










# Step 1: Load necessary library
library(dplyr)

# Step 2: Group by country_name and arrange by year
panel_data_lagged <- panel_data %>%
  group_by(country_name) %>%
  arrange(country_name, year) %>%
  mutate(
    liberal_democracy_minus_one = lag(liberal_democracy, k = 1)  # Create lagged variable
  ) %>%
  ungroup()  # Ungroup after operation

# Step 3: View the result
head(panel_data_lagged)



view(panel_data_lagged)










# Step 1: Load necessary library
library(dplyr)

# Step 2: Group by country_name and arrange by year, then shift the liberal_democracy column
panel_data_lagged <- panel_data %>%
  group_by(country_name) %>%
  arrange(country_name, year) %>%
  mutate(
    liberal_democracy_minus_one = c(NA, head(liberal_democracy, -1))  # Shift values down by 1, first row becomes NA
  ) %>%
  ungroup()

# Step 3: View the result
head(panel_data_lagged)


view(panel_data_lagged)








# Step 1: Create liberal_democracy_diff by subtracting liberal_democracy from liberal_democracy_minus_one
panel_data_lagged <- panel_data_lagged %>%
  mutate(liberal_democracy_diff = liberal_democracy_minus_one - liberal_democracy)

# Step 2: View the result
head(panel_data_lagged)



view(panel_data_lagged)











# Step 1: Convert to pdata.frame to define panel structure
panel_data_lagged_clean <- pdata.frame(panel_data_lagged, index = c("country_name", "year"))

# Step 2: Run the regression with liberal_democracy_diff as the dependent variable
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_lag + social_index_lag + Governance_Index_lag + 
    institutions_index_lag + economy_index_lag + Political_Culture_Index_lag, 
  data = panel_data_lagged_clean, 
  model = "within"
)

# Step 3: Display the regression summary
summary(regression_model_backsliding)










# Shift all index variables by one row
panel_data_lagged$international_index_minus_one <- c(NA, head(panel_data_lagged$international_index, -1))
panel_data_lagged$social_index_minus_one <- c(NA, head(panel_data_lagged$social_index, -1))
panel_data_lagged$Governance_Index_minus_one <- c(NA, head(panel_data_lagged$Governance_Index, -1))
panel_data_lagged$institutions_index_minus_one <- c(NA, head(panel_data_lagged$institutions_index, -1))
panel_data_lagged$economy_index_minus_one <- c(NA, head(panel_data_lagged$economy_index, -1))
panel_data_lagged$Political_Culture_Index_minus_one <- c(NA, head(panel_data_lagged$Political_Culture_Index, -1))

# Create the new dependent variable (liberal_democracy_diff)
panel_data_lagged$liberal_democracy_diff <- panel_data_lagged$liberal_democracy_minus_one - panel_data_lagged$liberal_democracy

# Convert to pdata.frame to define panel structure
panel_data_lagged_clean <- pdata.frame(panel_data_lagged, index = c("country_name", "year"))

# Run the regression with the new lagged index variables
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_minus_one + social_index_minus_one + Governance_Index_minus_one + 
    institutions_index_minus_one + economy_index_minus_one + Political_Culture_Index_minus_one, 
  data = panel_data_lagged_clean, 
  model = "within"
)

# Display the regression summary
summary(regression_model_backsliding)




view(panel_data_lagged)









### SOMETHING RESEMBLING ACCEPTABLE ##########


# Shift the lag variables by one row
panel_data_lagged$international_index_lag <- c(NA, head(panel_data_lagged$international_index, -1))
panel_data_lagged$social_index_lag <- c(NA, head(panel_data_lagged$social_index, -1))
panel_data_lagged$Governance_Index_lag <- c(NA, head(panel_data_lagged$Governance_Index, -1))
panel_data_lagged$institutions_index_lag <- c(NA, head(panel_data_lagged$institutions_index, -1))
panel_data_lagged$economy_index_lag <- c(NA, head(panel_data_lagged$economy_index, -1))
panel_data_lagged$Political_Culture_Index_lag <- c(NA, head(panel_data_lagged$Political_Culture_Index, -1))

# Now create the dependent variable (liberal_democracy_diff)
panel_data_lagged$liberal_democracy_diff <- panel_data_lagged$liberal_democracy_minus_one - panel_data_lagged$liberal_democracy

# Convert to pdata.frame to define panel structure
panel_data_lagged_clean <- pdata.frame(panel_data_lagged, index = c("country_name", "year"))

# Run the regression with lagged variables
regression_model_backsliding <- plm(
  liberal_democracy_diff ~ international_index_lag + social_index_lag + Governance_Index_lag + 
    institutions_index_lag + economy_index_lag + Political_Culture_Index_lag, 
  data = panel_data_lagged_clean, 
  model = "within"
)

# Display the regression summary
summary(regression_model_backsliding)



library(tidyverse)
vieW(panel_data_lagged)



# Define the file path
file_path <- "C:/Users/eddie/OneDrive/Documents/GitHub/USAID-Backsliding-R-Script/panel_data_lagged.csv"

# Save the dataframe as a CSV file
write.csv(panel_data_lagged, file = file_path, row.names = FALSE)














##### FINDINGS ####




colnames(panel_data_lagged)


# Load necessary libraries
library(plm)
library(dplyr)
library(stargazer)

# Load your dataset (assuming it's already in R as panel_data_lagged)

# Select relevant columns (dependent + independent variables)
df <- panel_data_lagged %>%
  select(country_name, year, liberal_democracy, ends_with("_lag"))

# Ensure country_name is a factor for fixed effects
df$country_name <- as.factor(df$country_name)

# Convert to a panel data structure
panel_df <- pdata.frame(df, index = c("country_name", "year"))

# Run the fixed effects regression
model <- plm(liberal_democracy ~ international_index_lag + social_index_lag +
               Governance_Index_lag + institutions_index_lag + economy_index_lag +
               Political_Culture_Index_lag,
             data = panel_df, model = "within", effect = "individual")

# Display results
summary(model)

# Optional: Export results in a readable format
stargazer(model, type = "text")











### LAGS ON DIFFED LIBDEM ####


# Load necessary libraries
library(plm)
library(dplyr)
library(stargazer)

# Load your dataset (assuming it's already in R as panel_data_lagged)

# Select relevant columns (dependent + independent variables)
df <- panel_data_lagged %>%
  select(country_name, year, liberal_democracy_diff, ends_with("_lag"))

# Ensure country_name is a factor for fixed effects
df$country_name <- as.factor(df$country_name)

# Convert to a panel data structure
panel_df <- pdata.frame(df, index = c("country_name", "year"))

# Run the fixed effects regression
model <- plm(liberal_democracy_diff ~ international_index_lag + social_index_lag +
               Governance_Index_lag + institutions_index_lag + economy_index_lag +
               Political_Culture_Index_lag,
             data = panel_df, model = "within", effect = "individual")

# Display results
summary(model)









library(dplyr)

# Create second-lagged variables (_lag2) by shifting them down by one additional row within each country
panel_data_lagged <- panel_data_lagged %>%
  group_by(country_name) %>% 
  mutate(
    international_index_lag2 = lag(international_index_lag, 1),
    social_index_lag2 = lag(social_index_lag, 1),
    Governance_Index_lag2 = lag(Governance_Index_lag, 1),
    institutions_index_lag2 = lag(institutions_index_lag, 1),
    economy_index_lag2 = lag(economy_index_lag, 1),
    Political_Culture_Index_lag2 = lag(Political_Culture_Index_lag, 1)
  ) %>%
  ungroup()


view(panel_data_lagged)







library(dplyr)

# Create third-lagged variables (_lag3) by shifting the original variables down by 3 periods within each country
panel_data_lagged <- panel_data_lagged %>%
  group_by(country_name) %>%
  mutate(
    international_index_lag3 = lag(international_index, 3),
    social_index_lag3 = lag(social_index, 3),
    Governance_Index_lag3 = lag(Governance_Index, 3),
    institutions_index_lag3 = lag(institutions_index, 3),
    economy_index_lag3 = lag(economy_index, 3),
    Political_Culture_Index_lag3 = lag(Political_Culture_Index, 3)
  ) %>%
  ungroup()







library(dplyr)

# Create fourth-lagged variables (_lag4) by shifting the original variables down by 4 periods within each country
panel_data_lagged <- panel_data_lagged %>%
  group_by(country_name) %>%
  mutate(
    international_index_lag4 = lag(international_index, 4),
    social_index_lag4 = lag(social_index, 4),
    Governance_Index_lag4 = lag(Governance_Index, 4),
    institutions_index_lag4 = lag(institutions_index, 4),
    economy_index_lag4 = lag(economy_index, 4),
    Political_Culture_Index_lag4 = lag(Political_Culture_Index, 4)
  ) %>%
  ungroup()






view(panel_data_lagged)





library(dplyr)

# Create full_panel_data without LDI_pct_change and _minus_one variables
full_panel_data <- panel_data_lagged %>%
  select(-LDI_pct_change, -ends_with("_minus_one"))

# Check the updated column names
colnames(full_panel_data)


view(full_panel_data)





write.csv(full_panel_data, "C:/Users/eddie/Downloads/V-Dem-CY-FullOthers-v14_csv_YyKfizl/full_panel_data.csv", row.names = FALSE)















####### LAG OF TWO YEARS ##########


library(plm)

# Define the regression formula
formula <- liberal_democracy ~ international_index_lag2 + social_index_lag2 + 
  Governance_Index_lag2 + institutions_index_lag2 + 
  economy_index_lag2 + Political_Culture_Index_lag2

# Run the fixed-effects panel regression
model_lag2 <- plm(formula, data = final_panel_data, 
                  index = c("country_name", "year"), 
                  model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_lag2)






####### LAG OF ONE YEAR ##########




library(plm)

# Define the regression formula with one-year lagged variables
formula <- liberal_democracy ~ international_index_lag + social_index_lag + 
  Governance_Index_lag + institutions_index_lag + 
  economy_index_lag + Political_Culture_Index_lag

# Run the fixed-effects panel regression
model_lag1 <- plm(formula, data = final_panel_data, 
                  index = c("country_name", "year"), 
                  model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_lag1)




##### LAG OF THREE YEARS #######


library(plm)

# Define the regression formula with three-year lagged variables
formula <- liberal_democracy ~ international_index_lag3 + social_index_lag3 + 
  Governance_Index_lag3 + institutions_index_lag3 + 
  economy_index_lag3 + Political_Culture_Index_lag3

# Run the fixed-effects panel regression
model_lag3 <- plm(formula, data = final_panel_data, 
                  index = c("country_name", "year"), 
                  model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_lag3)






##### LAG OF FOUR YEARS ######


library(plm)

# Define the regression formula with four-year lagged variables
formula <- liberal_democracy ~ international_index_lag4 + social_index_lag4 + 
  Governance_Index_lag4 + institutions_index_lag4 + 
  economy_index_lag4 + Political_Culture_Index_lag4

# Run the fixed-effects panel regression
model_lag4 <- plm(formula, data = final_panel_data, 
                  index = c("country_name", "year"), 
                  model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_lag4)








####### LAG MIXING TIME HORIZONS #####


library(plm)

# Define the regression formula with specified lags
formula <- liberal_democracy ~ 
  international_index_lag2 + social_index_lag4 + 
  Governance_Index_lag2 + institutions_index_lag2 + 
  economy_index_lag4 + Political_Culture_Index_lag4

# Run the fixed-effects panel regression
model_custom_lags <- plm(formula, data = full_panel_data, 
                         index = c("country_name", "year"), 
                         model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_custom_lags)







library(plm)

# Define the regression formula with governance at 1-year lag and all other indices at 4-year lag
formula <- liberal_democracy ~ 
  international_index_lag4 + social_index_lag4 + 
  Governance_Index_lag + institutions_index_lag4 + 
  economy_index_lag4 + Political_Culture_Index_lag4

# Run the fixed-effects panel regression
model_custom_lags_2 <- plm(formula, data = full_panel_data, 
                           index = c("country_name", "year"), 
                           model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_custom_lags_2)







####### MACRO INDICES #########

# Load necessary library
library(dplyr)

# Create the new indices by summing their respective components
final_panel_data <- final_panel_data %>%
  mutate(
    Vertical_Accountability = economy_index + Political_Culture_Index + social_index,
    Horizontal_Accountability = institutions_index + international_index + Governance_Index
  )

# Re-standardize each index to ensure mean = 0 and SD = 1
final_panel_data <- final_panel_data %>%
  mutate(
    Vertical_Accountability = scale(Vertical_Accountability)[,1],
    Horizontal_Accountability = scale(Horizontal_Accountability)[,1]
  )

# Check summary statistics to confirm standardization
summary(final_panel_data$Vertical_Accountability)
summary(final_panel_data$Horizontal_Accountability)

# View the final dataset
view(final_panel_data)




######## MACRO INDEX REGRESSION #####


library(plm)

# Define the regression formula
formula <- liberal_democracy ~ Vertical_Accountability + Horizontal_Accountability

# Run the fixed-effects panel regression
model_accountability <- plm(formula, data = final_panel_data, 
                            index = c("country_name", "year"), 
                            model = "within", effect = "individual")  # Country-level fixed effects

# Display results
summary(model_accountability)





##### LAGGED MACRO INDEX #######

# Create the lagged indices by summing their respective lagged components
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged = economy_index_lag + Political_Culture_Index_lag + social_index_lag,
    Horizontal_Accountability_lagged = institutions_index_lag + international_index_lag + Governance_Index_lag
  )

# Re-standardize each lagged index to ensure mean = 0 and SD = 1
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged = scale(Vertical_Accountability_lagged)[,1],
    Horizontal_Accountability_lagged = scale(Horizontal_Accountability_lagged)[,1]
  )

# Check summary statistics to confirm standardization
summary(full_panel_data$Vertical_Accountability_lagged)
summary(full_panel_data$Horizontal_Accountability_lagged)



######## LAGGED MACRO INDEX REGRESSION #####

# Define the regression formula
formula_lagged <- liberal_democracy ~ Vertical_Accountability_lag + Horizontal_Accountability_lag

# Run the fixed-effects panel regression (country-level fixed effects)
model_accountability_lagged <- plm(formula_lagged, data = final_panel_data, 
                                   index = c("country_name", "year"), 
                                   model = "within", effect = "individual")  

# Display results
summary(model_accountability_lagged)




###### TWO YEAR MACRO INDEX PLUS REGRESSION ######

# Create the 2-year lagged indices by summing their respective 2-year lagged components
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged2 = economy_index_lag2 + Political_Culture_Index_lag2 + social_index_lag2,
    Horizontal_Accountability_lagged2 = institutions_index_lag2 + international_index_lag2 + Governance_Index_lag2
  )

# Re-standardize each 2-year lagged index to ensure mean = 0 and SD = 1
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged2 = scale(Vertical_Accountability_lagged2)[,1],
    Horizontal_Accountability_lagged2 = scale(Horizontal_Accountability_lagged2)[,1]
  )

# Check summary statistics to confirm standardization
summary(full_panel_data$Vertical_Accountability_lagged2)
summary(full_panel_data$Horizontal_Accountability_lagged2)



# Define the regression formula
formula_lagged2 <- liberal_democracy ~ Vertical_Accountability_lag2 + Horizontal_Accountability_lag2

# Run the fixed-effects panel regression (country-level fixed effects)
model_accountability_lagged2 <- plm(formula_lagged2, data = final_panel_data, 
                                    index = c("country_name", "year"), 
                                    model = "within", effect = "individual")  

# Display results
summary(model_accountability_lagged2)



####### FOUR YEAR MACROS PLUS REGRESSION ######

# Create the 4-year lagged indices by summing their respective 4-year lagged components
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged4 = economy_index_lag4 + Political_Culture_Index_lag4 + social_index_lag4,
    Horizontal_Accountability_lagged4 = institutions_index_lag4 + international_index_lag4 + Governance_Index_lag4
  )

# Re-standardize each 4-year lagged index to ensure mean = 0 and SD = 1
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged4 = scale(Vertical_Accountability_lagged4)[,1],
    Horizontal_Accountability_lagged4 = scale(Horizontal_Accountability_lagged4)[,1]
  )

# Check summary statistics to confirm standardization
summary(full_panel_data$Vertical_Accountability_lagged4)
summary(full_panel_data$Horizontal_Accountability_lagged4)




# Define the regression formula
formula_lagged4 <- liberal_democracy ~ Vertical_Accountability_lag4 + Horizontal_Accountability_lag4

# Run the fixed-effects panel regression (country-level fixed effects)
model_accountability_lagged4 <- plm(formula_lagged4, data = final_panel_data, 
                                    index = c("country_name", "year"), 
                                    model = "within", effect = "individual")  

# Display results
summary(model_accountability_lagged4)






############ THREE YEAR MACRO PLUS REGRESSION ######


# Create the 3-year lagged indices by summing their respective 3-year lagged components
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged3 = economy_index_lag3 + Political_Culture_Index_lag3 + social_index_lag3,
    Horizontal_Accountability_lagged3 = institutions_index_lag3 + international_index_lag3 + Governance_Index_lag3
  )

# Re-standardize each 3-year lagged index to ensure mean = 0 and SD = 1
full_panel_data <- full_panel_data %>%
  mutate(
    Vertical_Accountability_lagged3 = scale(Vertical_Accountability_lagged3)[,1],
    Horizontal_Accountability_lagged3 = scale(Horizontal_Accountability_lagged3)[,1]
  )

# Check summary statistics to confirm standardization
summary(full_panel_data$Vertical_Accountability_lagged3)
summary(full_panel_data$Horizontal_Accountability_lagged3)



# Define the regression formula
formula_lagged3 <- liberal_democracy ~ Vertical_Accountability_lag3 + Horizontal_Accountability_lag3

# Run the fixed-effects panel regression (country-level fixed effects)
model_accountability_lagged3 <- plm(formula_lagged3, data = final_panel_data, 
                                    index = c("country_name", "year"), 
                                    model = "within", effect = "individual")  

# Display results
summary(model_accountability_lagged3)











###### CONTROLS ####

# Load necessary library
library(readr)

# Define the file path
file_path <- "C:/Users/eddie/Downloads/V-Dem-CY-FullOthers-v14_csv_YyKfizl/vdem_cleaner.csv"


file_path2 <- "C:/Users/eddie/Downloads/V-Dem-CY-FullOthers-v14_csv_YyKfizl/full_panel_data.csv"


full_panel_data <- file_path2

# Read the CSV file into R
vdem_cleaner <- read_csv(file_path)

full_panel_data <- read_csv(file_path2)

# Print the first few rows to check
head(vdem_cleaner)


view(vdem_cleaner)






# Load necessary library
library(dplyr)

# Merge selected variables into full_panel_data by country_name and year
full_panel_data <- full_panel_data %>%
  left_join(vdem_cleaner %>% select(country_name, year, e_pelifeex, e_regiongeo, e_wb_pop),
            by = c("country_name", "year"))

# Print first few rows to check the result
head(full_panel_data)


view(full_panel_data)











# Define the regression formula with control variables
formula_lagged4_controls <- liberal_democracy ~ Vertical_Accountability_lagged4 + 
  Horizontal_Accountability_lagged4 + 
  e_wb_pop + e_pelifeex

# Run the fixed-effects panel regression (country-level fixed effects)
model_accountability_lagged4_controls <- plm(formula_lagged4_controls, data = full_panel_data, 
                                             index = c("country_name", "year"), 
                                             model = "within", effect = "individual")  

# Display results
summary(model_accountability_lagged4_controls)







# Define the regression formula with individual indices and control variables
formula_lagged4_controls <- liberal_democracy ~ economy_index_lag4 + 
  Political_Culture_Index_lag4 + 
  social_index_lag4 + 
  Governance_Index_lag4 + 
  institutions_index_lag4 + 
  international_index_lag4 + 
  e_wb_pop + e_pelifeex

# Run the fixed-effects panel regression (country-level fixed effects)
model_lagged4_controls <- plm(formula_lagged4_controls, data = full_panel_data, 
                              index = c("country_name", "year"), 
                              model = "within", effect = "individual")  

# Display results
summary(model_lagged4_controls)



view(full_panel_data)









# Merge e_wb_pop and e_pelifeex from vdem_cleaner into full_panel_data
full_panel_withcontrols <- full_panel_data %>%
  left_join(select(vdem_cleaner, country_name, year, e_wb_pop, e_pelifeex), by = c("country_name", "year"))

# Check the new dataset to confirm the merge
summary(full_panel_withcontrols)



view(full_panel_withcontrols)











# Define the regression formula with lag4 indices and control variables
formula_lagged4_controls <- liberal_democracy ~ economy_index_lag4 + 
  Political_Culture_Index_lag4 + 
  social_index_lag4 + 
  Governance_Index_lag4 + 
  institutions_index_lag4 + 
  international_index_lag4 + 
  e_wb_pop + e_pelifeex

# Run the fixed-effects panel regression (country-level fixed effects)
model_lagged4_controls <- plm(formula_lagged4_controls, data = full_panel_withcontrols, 
                              index = c("country_name", "year"), 
                              model = "within", effect = "individual")

# Display results
summary(model_lagged4_controls)








# Define the regression formula without lags, using original indices and control variables
formula_no_lags_controls <- liberal_democracy ~ economy_index + 
  Political_Culture_Index + 
  social_index + 
  Governance_Index + 
  institutions_index + 
  international_index + 
  e_wb_pop.y + e_pelifeex.y

# Run the fixed-effects panel regression (country-level fixed effects)
model_no_lags_controls <- plm(formula_no_lags_controls, data = full_panel_withcontrols, 
                              index = c("country_name", "year"), 
                              model = "within", effect = "individual")

# Display results
summary(model_no_lags_controls)








# Check the correlations between the independent variables and control variables
correlation_matrix <- cor(full_panel_withcontrols[, c("economy_index", "Political_Culture_Index", "social_index", 
                                                      "Governance_Index", "institutions_index", "international_index", 
                                                      "e_wb_pop.y", "e_pelifeex.y")], use = "complete.obs")

# Display the correlation matrix
print(correlation_matrix)






# Standardize the control variables
full_panel_withcontrols$e_wb_pop_standardized <- scale(full_panel_withcontrols$e_wb_pop.y)
full_panel_withcontrols$e_pelifeex_standardized <- scale(full_panel_withcontrols$e_pelifeex.y)

# Check the first few rows to verify the changes
head(full_panel_withcontrols[, c("e_wb_pop.y", "e_pelifeex.y", "e_wb_pop_standardized", "e_pelifeex_standardized")])









###### CONTROL REGRESSION ON 4 YEAR LAG #######

# Load the plm package for panel data regressions
library(plm)

# Run regression with the standardized control variables using within estimator for country-level fixed effects
model_with_controls_standardized_plm <- plm(
  liberal_democracy ~ economy_index_lag4 + Political_Culture_Index_lag4 + social_index_lag4 +
    Governance_Index_lag4 + institutions_index_lag4 + international_index_lag4 +
    e_wb_pop_standardized + e_pelifeex_standardized,
  data = full_panel_withcontrols,
  index = c("country_name", "year"),
  model = "within"
)

# Display the summary of the model
summary(model_with_controls_standardized_plm)







# Run regression with vertical and horizontal 4-year lags and standardized control variables using within estimator for country-level fixed effects
model_vertical_horizontal_with_controls_standardized_plm <- plm(
  liberal_democracy ~ Vertical_Accountability_lagged4 + Horizontal_Accountability_lagged4 +
    e_wb_pop_standardized + e_pelifeex_standardized,
  data = full_panel_withcontrols,
  index = c("country_name", "year"),
  model = "within"
)

# Display the summary of the model
summary(model_vertical_horizontal_with_controls_standardized_plm)























########### REDOING LAGS ##########


library(dplyr)

# Create lagged variables with NA for the first row within each country
new_dataset <- new_dataset %>%
  arrange(country_name, year) %>%  # Sort by country_name and year
  group_by(country_name) %>%  # Group by country to apply lag per country
  mutate(across(ends_with("index"), 
                ~c(NA, head(., -1)),  # Shift down by 1 row, first becomes NA
                .names = "{.col}_lag")) %>%  # Name the new lagged variables
  mutate(across(ends_with("index"), 
                ~c(NA, NA, head(., -2)),  # Shift down by 2 rows
                .names = "{.col}_lag2")) %>%
  mutate(across(ends_with("index"), 
                ~c(NA, NA, NA, head(., -3)),  # Shift down by 3 rows
                .names = "{.col}_lag3")) %>%
  mutate(across(ends_with("index"), 
                ~c(NA, NA, NA, NA, head(., -4)),  # Shift down by 4 rows
                .names = "{.col}_lag4")) %>%
  ungroup()


view(new_dataset)

real_panel_data <- new_dataset


view(real_panel_data)








###### FIXING INDEX ########

full_panel <- read.csv("C:/Users/Eddie Andujar/Documents/GitHub/USAID-Backsliding-R-Script/full_panel_data.csv")

view(full_panel)








# Assuming both datasets have 'country_name' and 'year' as identifiers
new_dataset <- new_dataset %>%
  left_join(full_panel %>% select(country_name, year, Governance_Index), 
            by = c("country_name", "year")) %>%
  mutate(Governance_Index = Governance_Index.y) %>%
  select(-Governance_Index.y)  # Remove the extra column from join


view(new_dataset)


# Assuming both datasets have 'country_name' and 'year' as identifiers
new_dataset <- new_dataset %>%
  left_join(full_panel %>% select(country_name, year, Governance_Index), 
            by = c("country_name", "year")) %>%
  mutate(Governance_Index = Governance_Index.y) %>%
  select(-Governance_Index.x, -Governance_Index.y)  # Remove both .x and .y columns





realest_data <- new_dataset %>%
  select(country_name, year, 
         liberal_democracy, international_index, 
         social_index, institutions_index, 
         economy_index, Political_Culture_Index, 
         Governance_Index)


library(dplyr)

# Create lagged variables with NA for the first row within each country
realest_data <- realest_data %>%
  arrange(country_name, year) %>%  # Sort by country_name and year
  group_by(country_name) %>%  # Group by country to apply lag per country
  mutate(across(ends_with("index"), 
                ~c(NA, head(., -1)),  # Shift down by 1 row, first becomes NA
                .names = "{.col}_lag")) %>%  # Name the new lagged variables
  mutate(across(ends_with("index"), 
                ~c(NA, NA, head(., -2)),  # Shift down by 2 rows
                .names = "{.col}_lag2")) %>%
  mutate(across(ends_with("index"), 
                ~c(NA, NA, NA, head(., -3)),  # Shift down by 3 rows
                .names = "{.col}_lag3")) %>%
  mutate(across(ends_with("index"), 
                ~c(NA, NA, NA, NA, head(., -4)),  # Shift down by 4 rows
                .names = "{.col}_lag4")) %>%
  ungroup()


view(realest_data)


final_panel_data <- realest_data

write.csv(final_panel_data, "C:/Users/Eddie Andujar/Downloads/final_panel_data.csv", row.names = FALSE)




###### RETESTING MACRO LAGS ########


####### MACRO INDICES (LAGGED) #########

# Load necessary library
library(dplyr)

# Create the new lagged indices by summing their respective lagged components
final_panel_data <- final_panel_data %>%
  mutate(
    Vertical_Accountability_lag = economy_index_lag + Political_Culture_Index_lag + social_index_lag,
    Horizontal_Accountability_lag = institutions_index_lag + international_index_lag + Governance_Index_lag
  )

# Re-standardize each lagged index to ensure mean = 0 and SD = 1
final_panel_data <- final_panel_data %>%
  mutate(
    Vertical_Accountability_lag = scale(Vertical_Accountability_lag)[,1],
    Horizontal_Accountability_lag = scale(Horizontal_Accountability_lag)[,1]
  )

# Check summary statistics to confirm standardization
summary(final_panel_data$Vertical_Accountability_lag)
summary(final_panel_data$Horizontal_Accountability_lag)

# View the final dataset
view(final_panel_data)





####### MACRO INDICES (LAGGED 2, 3, 4) #########

# Load necessary library
library(dplyr)

# Create the new lagged indices (lag2, lag3, lag4) by summing their respective components
final_panel_data <- final_panel_data %>%
  mutate(
    Vertical_Accountability_lag2 = economy_index_lag2 + Political_Culture_Index_lag2 + social_index_lag2,
    Horizontal_Accountability_lag2 = institutions_index_lag2 + international_index_lag2 + Governance_Index_lag2,
    Vertical_Accountability_lag3 = economy_index_lag3 + Political_Culture_Index_lag3 + social_index_lag3,
    Horizontal_Accountability_lag3 = institutions_index_lag3 + international_index_lag3 + Governance_Index_lag3,
    Vertical_Accountability_lag4 = economy_index_lag4 + Political_Culture_Index_lag4 + social_index_lag4,
    Horizontal_Accountability_lag4 = institutions_index_lag4 + international_index_lag4 + Governance_Index_lag4
  )

# Re-standardize each lagged index to ensure mean = 0 and SD = 1
final_panel_data <- final_panel_data %>%
  mutate(
    Vertical_Accountability_lag2 = scale(Vertical_Accountability_lag2)[,1],
    Horizontal_Accountability_lag2 = scale(Horizontal_Accountability_lag2)[,1],
    Vertical_Accountability_lag3 = scale(Vertical_Accountability_lag3)[,1],
    Horizontal_Accountability_lag3 = scale(Horizontal_Accountability_lag3)[,1],
    Vertical_Accountability_lag4 = scale(Vertical_Accountability_lag4)[,1],
    Horizontal_Accountability_lag4 = scale(Horizontal_Accountability_lag4)[,1]
  )

# Check summary statistics to confirm standardization
summary(final_panel_data$Vertical_Accountability_lag2)
summary(final_panel_data$Horizontal_Accountability_lag2)
summary(final_panel_data$Vertical_Accountability_lag3)
summary(final_panel_data$Horizontal_Accountability_lag3)
summary(final_panel_data$Vertical_Accountability_lag4)
summary(final_panel_data$Horizontal_Accountability_lag4)

# View the final dataset
view(final_panel_data)


