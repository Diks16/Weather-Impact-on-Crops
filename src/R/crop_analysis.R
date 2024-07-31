if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(dplyr)

data_dir = '/Users/aggarwal.mohit/Documents/project/Diksha16/data/'
crop_data = read.csv(paste(data_dir, 'crop_yield.csv', sep=""))


### Normalize crop yields
### Linear regression of crop yield over time and subtract this from crop yield data 
### for offsetting increase in yield due to technological development over time.
# Single season, single crop, single region
crop_name <- "Rice"
season_name <- "Rabi       "
season_name <- "Kharif     "
region_name <- "Punjab"

timeseries_yield <- crop_data %>% filter(Crop == crop_name) %>% filter(Season == season_name) %>% filter(State == region_name)
model <- lm(Yield ~ Crop_Year, data = timeseries_yield)
m <- coef(model)[2]
min_year <- min(timeseries_yield$Crop_Year)
timeseries_yield$Adjusted_Yield <- timeseries_yield$Yield - m * timeseries_yield$Crop_Year + m*min_year


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

# Apply the function to each group of Crop, Season, and State
crop_data <- crop_data %>%
  group_by(Crop, Season, State) %>%
  group_modify(~ adjust_yield(.x))



### Plot the times series yield for a given crop across all regions.
# Filter data for a specific crop (e.g., 'Coconut')
crop_name <- "Wheat"
rice_data <- crop_data %>% filter(Crop == crop_name)

season_name <- "Kharif     "
rice_data <- rice_data %>% filter(Season == season_name)

states <- unique(rice_data$State)
for (state in states) {
  state_data <- rice_data %>% filter(State == state)
  

  # Plotting
  p <- ggplot(state_data, aes(x = Crop_Year, y = Yield, color = State, group = State)) +
  geom_line(size=1) +
  geom_point(size = 3) +
  labs(title = paste("Timeline of Crop Yield for", crop_name),
       x = "Year",
       y = "Yield") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(state_data$Crop_Year)) +
  scale_y_continuous(breaks = seq(floor(min(state_data$Yield)), ceiling(max(state_data$Yield)), by = 0.5))
  
  # Define file name
  file_name <- paste0("plot_", state, ".png")
  # Save the plot as an image
  ggsave(filename = file_name, plot = p, width = 10, height = 6, dpi = 300)
}




### Plot the times series yield for all crops in a given region.
# Filter data for a specific crop (e.g., 'Coconut')
region_name <- "Punjab"
punjab_data <- crop_data %>% filter(State == region_name)

season_name <- "Rabi       "
punjab_data <- punjab_data %>% filter(Season == season_name)

crops <- unique(punjab_data$Crop)
for (crop in crops) {
  all_crop_data <- punjab_data %>% filter(Crop == crop)
  
  # Plotting
  p <- ggplot(all_crop_data, aes(x = Crop_Year, y = Yield, color = Crop, group = Crop)) +
    geom_line(size=1) +
    geom_point(size = 3) +
    labs(title = paste("Timeline of Crop Yield for", crop),
         x = "Year",
         y = "Yield") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = unique(all_crop_data$Crop_Year)) +
    scale_y_continuous(breaks = seq(floor(min(all_crop_data$Yield)), ceiling(max(all_crop_data$Yield)), by = 0.5))
  
  # Define file name
  file_name <- paste0("plot_", crop, ".png")
  # Save the plot as an image
  ggsave(filename = file_name, plot = p, width = 10, height = 6, dpi = 300)
}




crop_name <- "Rice"
#crop_name <- "Wheat"
season_name <- "Kharif     "
#season_name <- "Rabi       "
region_name <- "Punjab"

timeseries_yield <- adjusted_data %>% filter(Crop == crop_name) %>% filter(Season == season_name) %>% filter(State == region_name)

# Plotting
p <- ggplot(timeseries_yield, aes(x = Crop_Year, y = Adjusted_Yield)) +
  geom_line(size=1) +
  geom_point(size = 3) +
  labs(title = paste("Timeline of Crop Yield for"),
       x = "Year",
       y = "Yield") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(timeseries_yield$Crop_Year)) +
  scale_y_continuous(breaks = seq(floor(min(timeseries_yield$Adjusted_Yield)), ceiling(max(timeseries_yield$Adjusted_Yield)), by = 0.5))

p

### Plot the distribution of temperature for all years for a given region
### Compare the plot with a plot of yields across different years.

