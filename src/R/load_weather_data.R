if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")

state = 'mp'

data_dir = '/Users/aggarwal.mohit/Documents/projects/Diksha16/data/'
weather_data = read.csv(paste(data_dir, 'weather_data/', state, '_weather.csv', sep=""))

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

#Rabi
weather_rabi <- weather_data %>%
  rowwise() %>%
  filter(is_within_date_range(month, date, 11, 7, 4, 15))
weather_rabi$year <- ifelse(weather_rabi$month < 5, weather_rabi$year - 1, weather_rabi$year)

# Kharif
weather_kharif <- weather_data %>%
  rowwise() %>%
  filter(is_within_date_range(month, date, 6, 10, 10, 10))

weather_kharif <- weather_rabi

# Monthly Average Minimum Temperature
monthly_data <- weather_data %>%
  group_by(year, month) %>%
  summarise(Average_Temperature = mean(`X2m_temperature._min`, na.rm=TRUE))

# Temperature buckets.
bucketize_temperature <- function(df) {
  df %>%
    mutate(Temp_Bracket = cut(X2m_temperature._min, breaks = seq(floor(min(X2m_temperature._min)/5)*5, ceiling(max(X2m_temperature._min)/5)*5, by = 5))) %>%
    group_by(year, Temp_Bracket) %>%
    summarise(Frequency = n()) %>%
    ungroup()
}

bucketed_data <- bucketize_temperature(weather_kharif)

buckets <- bucketed_data %>%
  pivot_wider(
    names_from = Temp_Bracket,  # Column to become new column names
    values_from = Frequency,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

# Monthly Minimum Temperature
annual_min_temperature <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = min(X2m_temperature._min, na.rm = TRUE))

monthly_min_temperature <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = min(X2m_temperature._min, na.rm = TRUE))

monthly_min_temperature <- monthly_min_temperature %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

temperature_min_distribition <- merge(monthly_min_temperature, annual_min_temperature, by = "year")

# Monthly Maximum Temperature
annual_max_temperature <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = max(X2m_temperature._max, na.rm = TRUE))

monthly_max_temperature <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = max(X2m_temperature._max, na.rm = TRUE))

monthly_max_temperature <- monthly_max_temperature %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

temperature_max_distribition <- merge(monthly_max_temperature, annual_max_temperature, by = "year")

# Monthly Average Temperature
annual_avg_temperature <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = mean(X2m_temperature._min, na.rm = TRUE))

monthly_avg_temperature <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = mean(X2m_temperature._min, na.rm = TRUE))

monthly_avg_temperature <- monthly_avg_temperature %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

temperature_avg_distribition <- merge(monthly_avg_temperature, annual_avg_temperature, by = "year")

### Total precipitation for each year.
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

### Maximum precipitation for each year.
annual_max_precipitation <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = max(total_precipitation._sum, na.rm = TRUE))

monthly_max_precipitation <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = max(total_precipitation._sum, na.rm = TRUE))

monthly_max_precipitation <- monthly_max_precipitation %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

precipitation_max_distribition <- merge(monthly_max_precipitation, annual_max_precipitation, by = "year")

### Total Radiation
annual_radiation <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = sum(surface_net_solar_radiation._sum, na.rm = TRUE))

monthly_radiation <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = sum(surface_net_solar_radiation._sum, na.rm = TRUE))

monthly_radiation <- monthly_radiation %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

radiation_distribition <- merge(monthly_radiation, annual_radiation, by = "year")

### Max Radiation
annual_max_radiation <- weather_kharif %>%
  group_by(year) %>%
  summarise(total_value = max(surface_net_solar_radiation._sum, na.rm = TRUE))

monthly_max_radiation <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(total_value = max(surface_net_solar_radiation._sum, na.rm = TRUE))

monthly_max_radiation <- monthly_max_radiation %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = total_value,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

add_prefix <- function(df, prefix) {
  return(setNames(df, c("year", paste0(prefix, names(df)[-1]))))
}


temperature_min_distribition <- add_prefix(temperature_min_distribition, "tmn_")
temperature_max_distribition <- add_prefix(temperature_max_distribition, "tmx_")
temperature_avg_distribition <- add_prefix(temperature_avg_distribition, "tavg_")
precipitation_distribition <- add_prefix(precipitation_distribition, "pt_")
precipitation_max_distribition <- add_prefix(precipitation_max_distribition, "pmx_")
radiation_distribition <- add_prefix(radiation_distribition, "rt_")
monthly_max_radiation <- add_prefix(monthly_max_radiation, "rmx_")

combine_data_frames <- function(list) {
  return(Reduce(function(x, y) merge(x, y, by = "year"), list))
}

combined_weather_data <- combine_data_frames(list(temperature_min_distribition, 
                         temperature_max_distribition, 
                         temperature_avg_distribition, 
                         precipitation_distribition,
                         precipitation_max_distribition,
                         radiation_distribition,
                         monthly_max_radiation))

relevant_weather_data <- combined_weather_data[, c("year", 
                                                   "tmn_9", 
                                                   "tavg_7", "tavg_8", "tavg_total_value",
                                                   "pt_7", "pt_8", "pt_total_value", 
                                                   "pmx_7", "pmx_8", 
                                                   "rt_7", "rt_8", "rt_total_value",
                                                   "rmx_7", "rmx_8")]

                    