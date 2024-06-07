setwd('C:/Users/koshy/OneDrive/Desktop/Joel Folder')

library(dplyr)
library(ggplot2)
install.packages('ggcorrplot')
library(ggcorrplot)

df <- read.csv('Electric_Vehicle_Population_Data.csv')
View(df)

str(df)

head(df)

glimpse(df)

names (df)

summary(df)

sum(is.na(df))

sum(duplicated(df))

numeric_data <- df %>% select_if(is.numeric)

# basic statistics (mean, median, standard deviation, etc.) 
# for each numerical column in the dataset?
basic_stats_summary <- summary(numeric_data)
print(basic_stats_summary)

detailed_stats <- numeric_data %>%
  summarise_all(list(
    mean = ~ mean(., na.rm = TRUE),
    median = ~ median(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    var = ~ var(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    range = ~ max(., na.rm = TRUE) - min(., na.rm = TRUE),
    IQR = ~ IQR(., na.rm = TRUE)
  ))

View(detailed_stats)

# distribution of electric vehicle models in the dataset? 
# Are there any models that are more popular than others?
model_distribution <- df %>%
  group_by(Model) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Select the top 30 models
top_30_models <- head(model_distribution, 30)

# Print the top 30 models
print(top_30_models)

# Visualize the distribution of the top 30 vehicle models
ggplot(top_30_models, aes(x = reorder(Model, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 30 Electric Vehicle Models", x = "Model", y = "Count")

# City with Most Electric Cars
city_distribution <- df %>%
  group_by(City) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the city distribution
print(city_distribution)

# Identify the city with the most electric cars
top_city <- city_distribution %>%
  slice(1)
print(top_city)



# Regions have the highest number of electric vehicles? 
# Are there any geographic trends or patterns?
region_data <- df %>%
  group_by(City) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the region distribution
print(region_data)

# Select the top 20 regions
top_20_regions <- head(region_data, 20)

# Print the top 20 regions
print(top_20_regions)

# Visualize the distribution of the top 20 regions
ggplot(top_20_regions, aes(x = reorder(City, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Top 20 Regions with the Highest Number of Electric Vehicles", x = "Region", y = "Count")

# make of electric vehicles common in the dataset? 
# How does this distribution vary by region?
type_distribution <- df %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the type distribution
print(type_distribution)

top_10_make <- head(type_distribution, 10)
print(top_10_make)

# Visualize the overall distribution of vehicle types
ggplot(top_10_make, aes(x = reorder(Make, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Electric Vehicle Types", x = "Type", y = "Count")

# Group by vehicle type and region and count the number of each type in each region
type_region_distribution <- df %>%
  group_by(Make, City) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Print the type distribution by region
print(type_region_distribution)

top_10_region <- head(type_region_distribution, 10)

# Visualize the distribution of vehicle types by region
ggplot(top_10_region, aes(x = reorder(City, -count), y = count, fill = Make)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Distribution of Electric Vehicle Make by Region", x = "Region", y = "Count")

# Assuming the year of manufacture column is named 'Year_of_Manufacture' and 
# the range column is named 'Range' Adjust the column names if necessary

# Select relevant numeric variables for correlation analysis
numeric_data <- df %>%
  select(Model.Year, Electric.Range, everything()) %>%  # Add other numeric columns if necessary
  select_if(is.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# Visualize the correlation matrix
ggcorrplot(correlation_matrix, lab = TRUE, type = "upper", tl.cex = 0.8, tl.srt = 45)

#Scatter Plot on Year of Manufacture vs Range
ggplot(df, aes(x = Model.Year, y = Electric.Range)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of Year of Manufacture vs. Range", x = "Year", y = "Range")

# Histogram of a numeric variable (e.g., Range)
ggplot(df, aes(x = Electric.Range)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Range", x = "Range", y = "Frequency")

# Group by vehicle type and summarize the dataset
type_summary <- df %>%
  group_by(Electric.Vehicle.Type) %>%
  summarise(count = n())

# Print the summary
print(type_summary)

# Identify the model with the highest range
max_range_model <- df %>%
  filter(Electric.Range == max(Electric.Range, na.rm = TRUE))

# Print the model with the highest range
print(max_range_model)

# Visualize the model with the highest range
ggplot(max_range_model, aes(x = Model, y = Electric.Range)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Model with the Highest Range", x = "Model", y = "Range")


# Identify the model with the highest MSRP
max_msrp_model <- df %>%
  filter(Base.MSRP == max(Base.MSRP, na.rm = TRUE))

# Print the model with the highest MSRP
print(max_msrp_model)

# Visualize the model with the highest MSRP
ggplot(max_msrp_model, aes(x = Model, y = Base.MSRP)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Model with the Highest MSRP", x = "Model", y = "MSRP")



# Assuming the make column is named 'Make', the model column is named 'Model', 
# and the clean alternative fuel eligibility column is 
# named 'Clean.Alternative.Fuel.Eligibility'
# Adjust the column names if necessary

# Filter the data for clean alternative fuel eligibility
clean_fuel_vehicles <- df %>%
  filter(Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility == "Clean Alternative Fuel Vehicle Eligible")

# Identify the models with clean alternative fuel eligibility
models_with_clean_fuel <- clean_fuel_vehicles %>%
  select(Make, Model) %>%
  distinct() %>%
  arrange(Make, Model)

# Print the models with clean alternative fuel eligibility
print(models_with_clean_fuel)


# Group by make and count the number of eligible vehicles
make_counts <- clean_fuel_vehicles %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Identify the make with the maximum number of clean alternative fuel eligible vehicles
top_make <- make_counts %>%
  slice(1)

# Print the make with the maximum number of clean alternative fuel eligible vehicles
cat("Make with the maximum number of clean alternative fuel eligible vehicles:\n")
print(top_make)

# Print the full list of makes with their counts
cat("\nAll makes with their counts of clean alternative fuel eligible vehicles:\n")
print(make_counts)
View(make_counts)
