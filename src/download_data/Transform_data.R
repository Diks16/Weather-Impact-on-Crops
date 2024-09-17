# Install necessary packages if not already installed
if (!require("raster")) install.packages("raster")
#if (!require("rgdal")) install.packages("rgdal")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

# Load the packages
library(raster) 
#library(rgdal)
library(dplyr)
library(stringr)


# List of GRIB files
grib_files <- list.files(path = "/Users/aggarwal.mohit/Documents/projects/Diksha16/src/download_data/gj", pattern = "\\.grib$", full.names = TRUE)

# Function to read a GRIB file into a data frame
read_grib_to_df <- function(file) {
  # grib_data <- readGDAL(file)
  # df <- as.data.frame(grib_data)
  GRIB<-brick(file) 
  GRIB<-as.array(GRIB)
  GRIB <- c(GRIB)
  matrix_data <- matrix(GRIB, nrow = 24, ncol = 9, byrow = TRUE)
  ans <- as.data.frame(matrix_data)
  return(ans)
}

# Initialize an empty list to store data frames
df_list <- data.frame(matrix(nrow = 0, ncol = 10))

# Iterate over the list of GRIB files and read them into data frames
for (file in grib_files) {
  # Convert the matrix into a data frame
  # Example code where you want to ignore errors
  result <- try({
    ans <- read_grib_to_df(file)
  })
  
  # Check if there was an error
  if (inherits(result, "try-error")) {
    cat("An error occurred but was ignored.\n")
    next
  }
  column_names <- c('10m_u_component_of_wind', '10m_v_component_of_wind', 'evaporation',
  '2m_temperature', 'soil_type', 'surface_net_solar_radiation',
  'surface_pressure', 'total_cloud_cover', 'total_precipitation')
  colnames(ans) <- column_names
  
  summarized_array <- data.frame(matrix(nrow = 1, ncol = 0))
  parsed_integers <- as.integer(unlist(str_extract_all(file, "\\d+")))
  summarized_array[, 'year'] <- parsed_integers[2]
  summarized_array[, 'month'] <- parsed_integers[3]
  summarized_array[, 'date'] <- parsed_integers[4]
  
  for (column in column_names) {
    arr <- ans[[column]]
    max_list <- c('2m_temperature')
    if (column %in% max_list) {
      value <- max(arr)
      new_column_name <- paste(column, "_max", collapse = "")
      summarized_array[, new_column_name] <- value
    }
    
    min_list <- c('2m_temperature')
    if (column %in% min_list) {
      value <- min(arr)
      new_column_name <- paste(column, "_min", collapse = "")
      summarized_array[, new_column_name] <- value
    }
    
    mean_list <- c('surface_pressure')
    if (column %in% mean_list) {
      value <- mean(arr)
      new_column_name <- paste(column, "_mean", collapse = "")
      summarized_array[, new_column_name] <- value
    }
    
    sum_list <- c('evaporation', 'surface_net_solar_radiation', 'total_precipitation')
    if (column %in% sum_list) {
      value <- sum(arr)
      new_column_name <- paste(column, "_sum", collapse = "")
      summarized_array[, new_column_name] <- value
    }
  }
  
  a <- ans[['10m_u_component_of_wind']]^2
  b <- ans[['10m_v_component_of_wind']]^2
  vsum <- a^2 + b^2
  vsum <- sqrt(vsum)
  value <- quantile(vsum, 0.8)
  summarized_array[, '10m_component_of_wind'] <- value
  names(df_list) <- names(summarized_array)
  df_list <- rbind(df_list, summarized_array)
}


write.csv(df_list, file = "/Users/aggarwal.mohit/Documents/projects/Diksha16/data/weather_data/gj_weather.csv")

# Combine all data frames into a single data frame
combined_df <- bind_rows(df_list)

# Print the first few rows of the combined data frame
print(head(combined_df))