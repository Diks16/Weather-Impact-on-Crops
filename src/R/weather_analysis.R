if (!require("corrplot")) install.packages("corrplot")
if (!require("dplyr")) install.packages("dplyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("MASS")) install.packages("MASS")

library(dplyr)
library(dplyr)
library(corrplot)
library(lubridate)
library(tidyr)
library(ggplot2)
library(MASS)
library(stringr)

# Create the time series plot
ggplot(monthly_data, aes(x=as.Date(paste(2024, month, 1, sep="-")), y=Average_Temperature, color=year, group=year)) +
  geom_line(size=1) +
  labs(title="Monthly Temperature Readings (1990-2000)",
       x="Month",
       y="Average Temperature (°C)",
       color="Year") +
  theme_minimal()

# Create the time series plot
ggplot(monthly_data, aes(x=year, y=Average_Temperature, color=month, group=month)) +
  geom_line(size=1) +
  labs(title="Monthly Temperature Readings (1990-2000)",
       x="Year",
       y="Average Temperature (°C)",
       color="Month") +
  theme_minimal()

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


# Fit a quadratic model
annual_temps <- weather_kharif %>%
  group_by(year) %>%
  summarise(max_temp = list(X2m_temperature._max))

crop_name <- "Rice"
season_name <- "Rabi       "
season_name <- "Kharif     "
region_name <- "Punjab"

timeseries_yield <- crop_data %>% filter(Crop == crop_name) %>% filter(Season == season_name) %>% filter(State == region_name)

combined_data <- timeseries_yield %>%
  left_join(annual_temps, by = c("Crop_Year" = "year"))

quadratic_regression <- function(temperature, yield) {
  # Create a data frame with temperature readings
  temp_df <- data.frame(
    Temperature = unlist(temperature),
    Day = 1:length(unlist(temperature)),
    Yield = yield
  )
  
  # Fit a quadratic model
  model <- lm(Yield ~ poly(Day, 2, raw = TRUE), data = temp_df)
  
  # Get model summary and coefficients
  summary <- summary(model)
  coefficients <- coef(summary)
  
  return(list(
    model = model,
    coefficients = coefficients,
    summary = summary
  ))
}

model <- lm(y ~ poly(Index, 2, raw = TRUE), data = temp_df)

weather_kharif$date_month <- paste(weather_kharif$month, '_', weather_kharif$date, sep="")

temperature_values <- weather_kharif[c("date_month", "X2m_temperature._max", "year")]
temperature_values <- temperature_values %>%
  pivot_wider(
    names_from = date_month,  # Column to become new column names
    values_from = X2m_temperature._max,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )


# Add new columns where each new column is the square of the existing column
for (col in names(temperature_values)) {
  # Create the squared column name
  squared_col_name <- paste0(col, "_squared")
  
  # Add the squared column to the data frame
  temperature_values[[squared_col_name]] <- temperature_values[[col]]^2
}

crop_values <- timeseries_yield[c("Crop_Year", "Yield")]
yield_temperature_values <- crop_values %>%
  left_join(temperature_values, by = c("Crop_Year" = "year"))

response <- yield_temperature_values$Yield

# Extract predictor variables
predictors <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year", "Yield", "year_squared"))]
# Add an intercept column (a column of ones) to the predictor data frame
predictors <- cbind(Intercept = 1, predictors)

model <- lm(response ~ ., data = predictors)

cor_matrix <- cor(predictors)

# Display the correlation matrix
print(cor_matrix)

# Identify highly correlated pairs
high_cor <- which(abs(cor_matrix) > 0.9, arr.ind = TRUE)

# Display highly correlated pairs
print(high_cor)



response <- data.matrix(yield_temperature_values["Yield"])
d <- 2
predictors <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year", "Yield"))]
predictors_poly <- as.data.frame(lapply(predictors, function(col) {
  poly(col, degree = d, raw = TRUE)
}))
# Add an intercept column (a column of ones) to the predictor data frame
predictors_poly <- cbind(Intercept = 1, predictors_poly)



# Fit the polynomial regression model
model <- lm(response ~ ., data = predictors_poly)



monthly_temperature <- weather_kharif %>%
  group_by(year, month) %>%
  summarise(Max_Temperature = max(`X2m_temperature._min`, na.rm=TRUE))

monthly_temperature_flat <- monthly_temperature %>%
  pivot_wider(
    names_from = month,  # Column to become new column names
    values_from = Max_Temperature,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0)  # Fill missing values with 0 (if needed)
  )

ggplot(monthly_temperature, aes(x = year, y = Max_Temperature, group=month, color=month)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Heat Map of Correlation by Variable and Crop",
       x = "Precipitation in different months",
       y = "Crop",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(monthly_temperature, aes(x = year, y = Max_Temperature, group=month, color = month)) +
  geom_line() +
  geom_point() +
  labs(title = "Values of Max Temperature for months Over Years",
       x = "Year",
       y = "Max Temperature") +
  theme_minimal()




crop_values <- timeseries_yield[c("Crop_Year", "Yield")]
yield_temperature_values <- crop_values %>%
  left_join(monthly_temperature_flat, by = c("Crop_Year" = "year"))
response <- data.matrix(yield_temperature_values["Yield"])
d <- 2
predictors <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year", "Yield"))]
predictors_poly <- as.data.frame(lapply(predictors, function(col) {
  poly(col, degree = d, raw = TRUE)
}))
# Add an intercept column (a column of ones) to the predictor data frame
predictors_poly <- cbind(Intercept = 1, predictors_poly)

model <- lm(response ~ ., data = predictors_poly)
coefficients <- coef(model)

# Remove NA values and filter out the intercept for this example
coefficients <- coefficients[!is.na(coefficients) & names(coefficients) != "(Intercept)"]

# Separate first-degree and second-degree coefficients
first_degree <- grep("^X[0-9]+\\.[1]+$", names(coefficients), value = TRUE)
second_degree <- grep("^X[0-9]+\\.[2]+$", names(coefficients), value = TRUE)


df <- as.data.frame(cbind(names(coefficients), coefficients))
df$degree <- str_sub(df$V1, -1)
df$month <- str_sub(df$V1, 2, nchar(df$V1) - 2)
degree_coefficients <- df[, c("coefficients", "degree", "month")] %>%
  pivot_wider(
    names_from = degree,  # Column to become new column names
    values_from = coefficients,    # Column with values to fill the new columns
    values_fill = list(Frequency = 0),  # Fill missing values with 0 (if needed)
    names_prefix = "degree"
  )

degree_coefficients <- transform(degree_coefficients, offset = as.numeric(degree1) / (2 * as.numeric(degree2)))
degree_coefficients$offset <- degree_coefficients$degree2 / (2 * degree_coefficients$degree1)


# Separate first and second degree coefficients
first_degree_vars <- grep("^[^:]+$", names(coefficients), value = TRUE)
second_degree_vars <- grep(":", names(coefficients), value = TRUE)
