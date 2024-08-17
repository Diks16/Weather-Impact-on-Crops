
data_dir = '/Users/aggarwal.mohit/Documents/projects/Diksha16/data/'
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

#Rabi
weather_rabi <- weather_data %>%
  rowwise() %>%
  filter(is_within_date_range(month, date, 11, 7, 4, 15))

# Kharif
weather_kharif <- weather_data %>%
  rowwise() %>%
  filter(is_within_date_range(month, date, 6, 10, 10, 10))

monthly_data <- weather_data %>%
  group_by(year, month) %>%
  summarise(Average_Temperature = mean(`X2m_temperature._min`, na.rm=TRUE))
