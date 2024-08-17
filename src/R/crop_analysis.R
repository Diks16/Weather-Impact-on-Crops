if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(dplyr)

total_production <- crop_data %>%
  group_by(crop_data$Crop) %>%
  summarise(Total_Production = sum(`Production`, na.rm=TRUE)) %>%
  arrange(desc(Total_Production))

considered_crops = c("Cotton(lint)", "Gram", "Wheat", "Rice")
considered_states = c("Uttar Pradesh", "Madhya Pradesh", "Punjab", "Bihar", "Gujarat")

### Plot the times series yield for a given crop across all regions.
plot_crop_yields <- function(data, pivot_col, yield_col, filter_name) {
  p <- ggplot(data, aes(x = Crop_Year, y = !!sym(yield_col), color = !!sym(pivot_col), group = !!sym(pivot_col))) +
    geom_line(size=1) +
    geom_point(size = 3) +
    labs(title = paste("Timeline of Crop Yield for", filter_name),
         x = "Year",
         y = "Yield") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = unique(data$Crop_Year)) +
    scale_y_continuous(breaks = seq(floor(min(data[, yield_col])), ceiling(max(data[, yield_col])), by = 0.5))
}

plot_crop_yields_for_considered_crops <- function(adjusted) {
  yield_column_name = ifelse(adjusted, "Adjusted_Yield" ,"Yield")
  filtered_states_data <- crop_data %>% filter (State %in% considered_states)
  for (crop_name in considered_crops) {
    data <- filtered_states_data %>% filter(Crop == crop_name)
    p <- plot_crop_yields(data, "State", yield_column_name, crop_name)
    # Save the plot as an image
    file_name <- paste0("plot_", yield_column_name, "_crop_", crop_name, ".png")
    ggsave(filename = file_name, plot = p, width = 10, height = 6, dpi = 300)
  }
}

plot_crop_yields_for_considered_crops(adjusted=FALSE)
plot_crop_yields_for_considered_crops(adjusted=TRUE)

### Plot the times series yield for a given region across all crops for the same season.
plot_crop_yields_for_considered_states <- function(adjusted) {
  yield_column_name = ifelse(adjusted, "Adjusted_Yield" ,"Yield")
  filtered_crop_data <- crop_data %>% filter (Crop != "Rice" | Season == "Kharif")
  filtered_crop_data <- filtered_crop_data %>% filter (Crop %in% considered_crops)
  for (state_name in considered_states) {
    data <- filtered_crop_data %>% filter(State == state_name)
    p <- plot_crop_yields(data, "Crop", yield_column_name, state_name)
    file_name <- paste0("plot_", yield_column_name, "_crop_", state_name, ".png")
    # Save the plot as an image
    ggsave(filename = file_name, plot = p, width = 10, height = 6, dpi = 300)
  }
}

plot_crop_yields_for_considered_states(adjusted=FALSE)
plot_crop_yields_for_considered_states(adjusted=TRUE)

### Plot the distribution of temperature for all years for a given region
### Compare the plot with a plot of yields across different years.

