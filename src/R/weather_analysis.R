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


















# Normalization for a Single season, single crop, single region
crop_rice <- "Rice"
crop_wheat <- "Wheat"
season_rabi <- "Rabi"
season_kharif <- "Kharif"
region_punjab <- "Punjab"
region_punjab <- "Gujarat"

crop_rice_gujarat <- get_filtered_yields(crop_name = crop_rice, season_name = season_kharif, region_name = region_punjab)


quadratic_regression <- function(crop_values, input_variables) {
  crop_values <- crop_values[c("Crop_Year", "Yield")]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
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
  degree_coefficients$offset <- as.numeric(degree_coefficients$degree2) / (2 * as.numeric(degree_coefficients$degree1))
  return (degree_coefficients)
}

precipitation_qr_coefficients <- quadratic_regression(crop_cotton_mp, precipitation_distribition)

precipitation_qr_coefficients <- quadratic_regression(crop_rice_gujarat, precipitation_distribition)
precipitation_max_qr_coefficients <- quadratic_regression(crop_rice_gujarat, precipitation_max_distribition)

plot_crop_vs_all_variables <- function(crop_values, input_variables) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  response <- data.matrix(yield_temperature_values[target_col])
  d <- 2
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  values <- values %>%
    pivot_longer(cols = -Adjusted_Yield, names_to = "variable", values_to = "value")
  
  # Create a single scatter plot for all variables against the target column
  ggplot(values, aes(x = Adjusted_Yield, y = value, color = variable)) +
    geom_point(size = 3) +  # Scatter points
    #geom_smooth(method = "lm", se = FALSE) +  # Add regression lines
    labs(title = "Scatter Plot of All Columns vs Target Column",
         x = "Target Column",
         y = "Values") +
    theme_minimal()
}

plot_crop_vs_variables <- function(crop_values, input_variables) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  # Get all column names except the target column
  cols_to_plot <- setdiff(names(values), target_col)
  
  # Loop through each column and plot against the target column
  for (col in cols_to_plot) {
    p <- ggplot(data = values, aes(x = .data[[target_col]], y = .data[[col]])) +
      geom_point(color = "blue", size = 3) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Scatter Plot of", col, "vs", target_col), 
           x = target_col, 
           y = col) +
      theme_minimal()
    
    print(p)
  }
}


plot_crop_vs_variables(crop_cotton_mp, precipitation_distribition)
plot_crop_vs_all_variables(crop_cotton_mp, precipitation_distribition)


plot_crop_vs_variables(crop_cotton_mp, temperature_max_distribition)
plot_crop_vs_all_variables(crop_cotton_mp, temperature_max_distribition)

plot_crop_vs_variables(crop_cotton_mp, radiation_distribition)
plot_crop_vs_all_variables(crop_cotton_mp, radiation_distribition)


plot_crop_vs_variable(crop_rice_gujarat, precipitation_distribition)


# Load necessary libraries
install.packages("corrplot")
install.packages("Hmisc")  # Optional
library(corrplot)
library(Hmisc)  # Optional

plot_correlation <- function(crop_values, input_variables, variable_description) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  
  
  # Compute correlation matrix
  cor_matrix <- cor(values, use = "pairwise.complete.obs")  # Basic correlation matrix
  
  # Plot the correlation matrix
  corrplot(cor_matrix, method = "color", 
           col = colorRampPalette(c("blue", "white", "red"))(200), 
           type = "upper", 
           tl.col = "black", 
           tl.srt = 45, 
           title = paste("Correlation matrix of yield with ", variable_description),
           addCoef.col = "black",
           mar = c(2, 2, 2, 2))  # Adding coefficients to the plot
  
}

plot_correlation(crop_rice_mp, precipitation_distribition, "monthly precipitation")
plot_correlation(crop_cotton_mp, precipitation_distribition, "monthly precipitation")
plot_correlation(crop_cotton_mp, temperature_max_distribition, "maximum monthly temperature")


plot_correlation(crop_cotton_mp, combined_weather_data, "combined weather data")
plot_correlation(crop_cotton_mp, relevant_weather_data, "relevant weather data")

plot_correlation(crop_cotton_gj, combined_weather_data, "combined weather data")
plot_correlation(crop_cotton_gj, relevant_weather_data, "relevant weather data")

plot_correlation(crop_rice_mp, combined_weather_data, "combined weather data")
plot_correlation(crop_rice_mp, relevant_weather_data, "relevant weather data")

plot_correlation(crop_rice_gj, combined_weather_data, "combined weather data")
plot_correlation(crop_rice_gj, relevant_weather_data, "relevant weather data")

plot_correlation(crop_rice_pb, combined_weather_data, "combined weather data")
plot_correlation(crop_rice_pb, relevant_weather_data, "relevant weather data")

plot_correlation(crop_wheat_gj, combined_weather_data, "combined weather data")
plot_correlation(crop_wheat_mp, combined_weather_data, "combined weather data")
plot_correlation(crop_wheat_pb, combined_weather_data, "combined weather data")

plot_correlation(crop_cotton_gj, precipitation_distribition, "monthly precipitation")

plot_correlation(crop_rice_gj, precipitation_distribition, "monthly precipitation")


linear_regression <- function(crop_values, input_variables, variable_description) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  # Linear regression of 'y' over all other variables
  model <- lm(Adjusted_Yield ~ ., data = values)  # '.' means all other variables in the data frame
  
  # Summary of the linear regression model
  summary(model)
}

linear_regression(crop_cotton_mp, precipitation_distribition, "monthly precipitation")

linear_regression(crop_cotton_mp, relevant_weather_data, "relevant weather metrics")


if (!require(rpart)) install.packages("rpart", dependencies = TRUE)
if (!require(rpart.plot)) install.packages("rpart.plot", dependencies = TRUE)

library(rpart)
library(rpart.plot)



decision_tree <- function(crop_values, input_variables) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  # Step 3: Fit the Decision Tree Model
  model <- rpart(Adjusted_Yield ~ ., data = values, method = "class")
  
  # Step 4: Visualize the Decision Tree
  rpart.plot(model, type = 3, extra = 101, under = TRUE, varlen = 0, faclen = 0)
  
  # Step 5: Model Summary
  printcp(model)  # Displays the complexity parameter table
  summary(model)  # Detailed summary of splits
}


# Merging Data Frames
merged_df <- merge(precipitation_distribition, precipitation_max_distribition, by = "year", suffixes = c("_p", "_pm"))
merged_df <- merge(merged_df, temperature_max_distribition, by = "year", suffixes = c("", "_tmx"))
merged_df <- merge(merged_df, radiation_distribition, by = "year", suffixes = c("", "_r"))

decision_tree(crop_cotton_mp, relevant_weather_data)

decision_tree(crop_cotton_mp, merged_df)

install.packages("keras")
install.packages("tensorflow")
library(keras)
library(tensorflow)

# Install Keras and TensorFlow
install_keras()

lstm <- function(crop_values, input_variables) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  # Split data into input (X) and output (y)
  X <- values[setdiff(names(values), c(target_col))]
  y <- values$Adjusted_Yield
  X_array <- array(as.matrix(X), dim = c(nrow(X), 1, ncol(X)))
  
  # Split the data into training and testing sets
  set.seed(123)
  train_indices <- sample(1:nrow(X), size = 0.8 * nrow(X))
  X_train <- X_array[train_indices,,]
  X_test <- X_array[-train_indices,,]
  y_train <- y[train_indices]
  y_test <- y[-train_indices]
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = 50, input_shape = c(dim(X_train)[2], dim(X_train)[3]), return_sequences = FALSE) %>%
    layer_dense(units = 1)  # Output layer with one unit for regression
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = 'adam',
    metrics = list('mean_absolute_error')
  )
  
  history <- model %>% fit(
    X_train, y_train,
    epochs = 100,   # Set epochs to a higher value (e.g., 100 or more) for actual use
    batch_size = 8, # Adjust batch size as needed
    validation_split = 0.2,  # Use 20% of the training data for validation
    verbose = 1
  )
  
  model %>% evaluate(X_test, y_test)
  
  plot(history)
}

lstm(crop_cotton_mp, merged_df)


# Generalized Additive Model (GAM)
# Install the mgcv package if not already installed
if (!require(mgcv)) {
  install.packages("mgcv")
  library(mgcv)
}

train_gam <- function(crop_values, input_variables) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  
  predictors <- setdiff(names(values), "Adjusted_Yield") # Exclude the target variable
  formula_str <- paste("Adjusted_Yield ~", paste("s(", predictors, ")", collapse = " + "))
  formula <- as.formula(formula_str)
  
  gam_model <- gam(formula, data = values)
  # linear_model <- lm(Adjusted_Yield ~ ., data = values)
  
  # print(anova(gam_model, linear_model))
  
  # Summary of the GAM model
  # print(summary(gam_model))
  
  # Visualizing the smooth terms
  plot(gam_model, pages = 1)
  plot(residuals(gam_model), main = "Residuals of GAM Model")
  
  
  # Generate predictions
  predictions <- predict(gam_model, newdata = values)
  
  # Actual values
  actual <- values$Adjusted_Yield
  
  # Calculate Mean Squared Error
  mse <- mean((predictions - actual)^2)
  print(paste("Mean Squared Error:", mse))
  
  # Root Mean Squared Error (RMSE)
  rmse <- sqrt(mse)
  print(paste("Root Mean Squared Error:", rmse))
  
  # Calculate R-squared
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predictions)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  print(paste("R-squared:", r_squared))
}

train_gam(crop_cotton_mp, relevant_weather_data)
train_gam(crop_wheat_mp, combined_weather_data)


install.packages("xgboost")
install.packages("Matrix")
# Load required libraries
library(xgboost)
library(Matrix)
library(caret)


train_xgb <- function(crop_values, input_variables) {
  target_col <- "Adjusted_Yield"
  crop_values <- crop_values[c("Crop_Year", target_col)]
  yield_temperature_values <- crop_values %>%
    left_join(input_variables, by = c("Crop_Year" = "year"))
  values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year"))]
  train_values <- yield_temperature_values[setdiff(names(yield_temperature_values), c("Crop_Year", "Adjusted_Yield"))]
  
  
  # Split data into training (75%) and test (25%) sets
  set.seed(123) # Set seed for reproducibility
  trainIndex <- createDataPartition(values$Adjusted_Yield, p = 0.75, list = FALSE)
  train_data <- train_values[trainIndex, ]
  test_data <- train_values[-trainIndex, ]
  
  train_labels <- values$Adjusted_Yield[trainIndex]
  test_labels <- values$Adjusted_Yield[-trainIndex]
  
  # Convert training and test data into DMatrix format
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
  dtest <- xgb.DMatrix(data = as.matrix(test_data), label = test_labels)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_values), label = values$Adjusted_Yield)
  xgb_model <- xgboost(data = dtrain, objective = "reg:squarederror", nrounds = 100)
  
  predictions <- predict(xgb_model, as.matrix(train_values))
  
  train_predictions <- predict(xgb_model, as.matrix(train_data))
  test_predictions <- predict(xgb_model, as.matrix(test_data))
  
  # Mean Squared Error (MSE)
  test_mse <- mean((test_predictions - test_labels)^2)
  print(paste("Test Mean Squared Error:", test_mse))
  
  # Root Mean Squared Error (RMSE)
  test_rmse <- sqrt(test_mse)
  print(paste("Test Root Mean Squared Error:", test_rmse))
  
  # R-squared for the test set
  test_ss_total <- sum((test_labels - mean(test_labels))^2)
  test_ss_residual <- sum((test_labels - test_predictions)^2)
  test_r_squared <- 1 - (test_ss_residual / test_ss_total)
  print(paste("Test R-squared:", test_r_squared))
  
  # Feature Importance
  importance_matrix <- xgb.importance(model = xgb_model)
  xgb.plot.importance(importance_matrix)
}

train_xgb(crop_cotton_mp, relevant_weather_data)

