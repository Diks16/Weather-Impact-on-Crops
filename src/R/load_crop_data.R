if (!require("dplyr")) install.packages("dplyr")

library(dplyr)

### Load Data.
load_crop_yield_data <- function() {
  data_dir = '/Users/aggarwal.mohit/Documents/projects/Diksha16/data/'
  crop_data = read.csv(paste(data_dir, 'crop_yield.csv', sep=""))
  crop_data$Crop = trimws(crop_data$Crop)
  crop_data$Season = trimws(crop_data$Season)
  return(crop_data)
}

### Normalize crop yields
### Linear regression of crop yield over time and subtract this from crop yield data 
### for offsetting increase in yield due to technological development over time.

# Define a function to fit the model and adjust the yield
adjust_yield <- function(df) {
  model <- lm(Yield ~ Crop_Year, data = df)
  m <- coef(model)[2]
  min_year <- min(df$Crop_Year)
  if (!is.na(m) && !is.na(min_year)) {
    df <- df %>%
      mutate(Adjusted_Yield = Yield - m * Crop_Year + m * min_year)
  } else {
    df <- df %>%
      mutate(Adjusted_Yield=Yield)
  }
  return(df)
}

adjust_yield_all <- function(crop_data) {
  # Apply the function to each group of Crop, Season, and State
  crop_data <- crop_data %>% filter(Yield != 0)
  crop_data <- crop_data %>%
    group_by(Crop, Season, State) %>%
    group_modify(~ adjust_yield(.x))
    
  return(crop_data)
}

crop_data <- load_crop_yield_data()
crop_data <- adjust_yield_all(crop_data)

### Filter data.
get_filtered_yields <- function(crop_name=NA, season_name=NA, region_name=NA) {
  filtered_crop_data <- crop_data
  if (!is.na(crop_name)) {
    filtered_crop_data <- filtered_crop_data %>% filter(Crop == crop_name)
  }
  if (!is.na(season_name)) {
    filtered_crop_data <- filtered_crop_data %>% filter(Season == season_name)
  }
  if (!is.na(region_name)) {
    filtered_crop_data <- filtered_crop_data %>% filter(State == region_name)
  }
  return(filtered_crop_data)
}

# Normalization for a Single season, single crop, single region
crop_rice <- "Rice"
crop_wheat <- "Wheat"
season_rabi <- "Rabi"
season_kharif <- "Kharif"
region_punjab <- "Punjab"

crop_rice_punjab <- get_filtered_yields(crop_name = crop_rice, season_name = season_kharif, region_name = region_punjab)
