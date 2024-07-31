if (!require("corrplot")) install.packages("corrplot")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")

library(dplyr)
library(corrplot)
library(lubridate)
library(tidyr)

data_dir = '/Users/aggarwal.mohit/Documents/project/Diksha16/data/'
weather_data = read.csv(paste(data_dir, 'weather_data/pnb_weather.csv', sep=""))

### Filter weather according to season.
# Function to check if a given date falls within the specified range
is_within_date_range <- function(month, day, start_month, start_day, end_month, end_day) {
  # print(month)
  # print(date)
  year = format(Sys.Date(), "%Y")
  # Create date objects
  date_to_check <- as.Date(paste(year, month, day, sep = "-"))
  start_date <- as.Date(paste(year, start_month, start_day, sep = "-"))
  end_date <- as.Date(paste(year, end_month, end_day, sep = "-"))
  
  # Handle the case where the date range spans the end of the year
  if (start_month > end_month || (start_month == end_month && start_day > end_day)) {
    end_date <- end_date %m+% years(1)
    # Adjust date_to_check if it's before start_date and the range spans the end of the year
    if (date_to_check < start_date) {
      date_to_check <- date_to_check %m+% years(1)
    }
  }
  # Check if the date falls within the range
  valid <- (date_to_check >= start_date & date_to_check <= end_date)
  return(valid)
}

# print(is_within_date_range(1, 30, 11, 15, 4, 10))
#Rabi
weather_rabi <- weather_data %>%
  rowwise() %>%
  filter(is_within_date_range(month, date, 11, 7, 4, 15))
# Kharif
weather_kharif <- weather_data %>%
  rowwise() %>%
  filter(is_within_date_range(month, date, 6, 10, 10, 10))


# Create the time series plot
ggplot(monthly_data, aes(x=as.Date(paste(2024, month, 1, sep="-")), y=Average_Temperature, color=year, group=year)) +
  geom_line(size=1) +
  labs(title="Daily Temperature Readings (1990-2000)",
       x="Date",
       y="Temperature (°C)",
       color="Year") +
  theme_minimal()

monthly_data <- weather_data %>%
  group_by(year, month) %>%
  summarise(Average_Temperature = mean(`X2m_temperature._min`, na.rm=TRUE))

####################
### Bucketize weather for temperature
# Define a function to bucketize temperature
bucketize_temperature <- function(df) {
  df %>%
    mutate(Temp_Bracket = cut(X2m_temperature._min, breaks = seq(floor(min(X2m_temperature._min)/5)*5, ceiling(max(X2m_temperature._min)/5)*5, by = 5))) %>%
    group_by(year, Temp_Bracket) %>%
    summarise(Frequency = n()) %>%
    ungroup()
}

# Apply the function to the data
bucketed_data <- bucketize_temperature(weather_kharif)

# Calculate the percentage of each temperature bracket within each year
data <- bucketed_data %>%
  group_by(year) %>%
  mutate(Total_Frequency = sum(Frequency),
         Percentage = (Frequency / Total_Frequency) * 100) %>%
  ungroup()

# Plot the distribution of temperature as a percentage of total in a single bar for each year
ggplot(data, aes(x = factor(year), y = Percentage, fill = Temp_Bracket)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Temperature as a Percentage of Total by Year",
       x = "Year",
       y = "Percentage",
       fill = "Temperature Bracket") +
  theme_minimal()

# Plot the time series for frequency for all Temp_Bracket values separately
ggplot(bucketed_data, aes(x = year, y = Frequency, color = Temp_Bracket, group = Temp_Bracket)) +
  geom_line() +
  geom_point() +
  labs(title = "Time Series of Temperature Bracket Frequencies",
       x = "Year",
       y = "Frequency",
       color = "Temperature Bracket") +
  theme_minimal()


bucketed_data <- bucketize_temperature(df_filtered)

### Calculate annual precipitation for each year.
annual_precipitation <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = sum(total_precipitation._sum, na.rm = TRUE))

monthly_precipitation <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = sum(total_precipitation._sum, na.rm = TRUE))

monthly_precipitation <- monthly_precipitation %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

precipitation_distribition <- merge(monthly_precipitation, annual_precipitation, by = "year")

# Create the time series plot
ggplot(summarized_df, aes(x=year, y=total_value)) +
  geom_line(size=1) +
  labs(title="Daily Temperature Readings (1990-2000)",
       x="Date",
       y="Precipitation in season across years",
       color="Year") +
  theme_minimal()


buckets <- bucketed_data %>%
  pivot_wider(
    names_from = Temp_Bracket,  # Column to become new column names
    values_from = Frequency,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

get_temperature_correlation <- function(df) {
  yields <- df %>% ungroup() %>% select(c("Crop_Year", "Adjusted_Yield", "Yield"))
  combined_temp_yield <- merge(buckets, yields, by.x = "year", , by.y = "Crop_Year")
  cor_matrix <- cor(subset(combined_temp_yield, select = -year))
  return(cor_matrix["Adjusted_Yield", ])
}

season_name <- "Kharif     "
region_name <- "Punjab"

pnb_crop_data <- crop_data %>% filter(Season == season_name) %>% filter(State == region_name)

### Plot correlation matrix of temperature buckets with crop yields for all crops.
crops <- unique(pnb_crop_data$Crop)
results_list <- list()
for (crop in crops) {
  cor <- get_temperature_correlation(pnb_crop_data %>% filter(Crop == crop))
  cor_df <- as.data.frame(cor)
  cor_df$Variable <- rownames(cor_df)
  cor_df$Crop <- crop
  results_list[[crop]] <- cor_df
}
# Combine all results into a single data frame
temperature_correlation <- do.call(rbind, results_list)

ggplot(final_df, aes(x = Variable, y = Crop, fill = cor)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heat Map of Correlation for different temperature brackets and Crop",
       x = "Temperature Brackets",
       y = "Crop",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Plot correlation matrix of precipitation distribution with crop yields for all crops.
get_precipitation_correlation <- function(df) {
  yields <- df %>% ungroup() %>% select(c("Crop_Year", "Adjusted_Yield", "Yield"))
  combined_temp_yield <- merge(precipitation_distribition, yields, by.x = "year", , by.y = "Crop_Year")
  cor_matrix <- cor(subset(combined_temp_yield, select = -year))
  return(cor_matrix["Adjusted_Yield", ])
}

crops <- unique(pnb_crop_data$Crop)
results_list <- list()
for (crop in crops) {
  cor <- get_precipitation_correlation(pnb_crop_data %>% filter(Crop == crop))
  cor_df <- as.data.frame(cor)
  cor_df$Variable <- rownames(cor_df)
  cor_df$Crop <- crop
  results_list[[crop]] <- cor_df
}
# Combine all results into a single data frame
precipitation_correlation <- do.call(rbind, results_list)
precipitation_correlation <- precipitation_correlation %>% filter(!Variable %in% c("Adjusted_Yield", "Yield"))

ggplot(precipitation_correlation, aes(x = Variable, y = Crop, fill = cor)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heat Map of Correlation by Variable and Crop",
       x = "Precipitation in different months",
       y = "Crop",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


