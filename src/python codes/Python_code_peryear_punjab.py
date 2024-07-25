#!/usr/bin/env python
# coding: utf-8

# In[43]:


import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the datasets
weather_df = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/Punjab/pnb_weather.csv')
crop_yield_df = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/crop_yield.csv')

# Display the first few rows of the datasets and their columns
print("Weather Data:")
print(weather_df.head())
print(weather_df.columns)
print("\nCrop Yield Data:")
print(crop_yield_df.head())
print(crop_yield_df.columns)

# Clean and convert 'date' column to datetime format
def clean_and_convert_date(df, date_column):
    df[date_column] = pd.to_datetime(df[date_column], errors='coerce')
    df = df.dropna(subset=[date_column])  # Drop rows with invalid dates
    return df

weather_df = clean_and_convert_date(weather_df, 'date')

# Extract 'year' from the date in the weather dataset
weather_df['year'] = weather_df['date'].dt.year

# Ensure 'Crop_Year' is used as the year column in the crop yield dataset
crop_yield_df['year'] = crop_yield_df['Crop_Year']

# Print unique years in both datasets to debug
unique_years_weather = weather_df['year'].unique()
unique_years_crop_yield = crop_yield_df['year'].unique()

print("\nUnique Years in Weather Data:")
print(unique_years_weather)
print("\nUnique Years in Crop Yield Data:")
print(unique_years_crop_yield)  

# Find the common years between the two datasets
common_years = list(set(unique_years_weather).intersection(set(unique_years_crop_yield)))

print("\nCommon Years in Both Datasets:")
print(common_years)

# Update the column names based on the actual column names in your dataset
weather_agg = weather_df.groupby('year').agg({
    '2m_temperature _max': 'mean',  # Replace with actual column name for temperature
    'total_precipitation _sum': 'sum',      # Replace with actual column name for rainfall
    '10m_component_of_wind': 'mean'      # Replace with actual column name for humidity
}).reset_index()

# Display the aggregated weather data and crop yield data
print("\nAggregated Weather Data:")
print(weather_agg.head())
print(weather_agg.columns)
print("\nCrop Yield Data:")
print(crop_yield_df.head())
print(crop_yield_df.columns)

# Use the weather data as features and crop yield as the target

X = weather_agg[['year', '2m_temperature _max', 'total_precipitation _sum', '10m_component_of_wind']]
y = crop_yield_df[['Crop_Year', 'Yield']]

# Drop the 'year' column from X as it's not needed for prediction
X = X.drop('year', axis=1)

# Align 'y' DataFrame to have the same index as 'X'
y = y.set_index('Crop_Year').loc[common_years].reset_index()
y = y['Yield']

# Ensure both datasets have the same number of samples
print(f"Number of samples in X after alignment: {len(X)}")
print(f"Number of samples in y after alignment: {len(y)}")


import matplotlib.pyplot as plt
import seaborn as sns

# Verify the shapes of X and y
print(X.shape)
print(y.shape)

# Ensure X and y have the same length
if len(X) == len(y):
    plt.figure(figsize=(14, 8))
    for i, column in enumerate(X.columns):
        plt.subplot(2, 2, i + 1)
        plt.scatter(X[column], y)
        plt.xlabel(column)
        plt.ylabel('Yield')  # Optionally add a label for the y-axis
    plt.tight_layout()
    plt.show()
else:
    print("X and y must be the same length.")

# Assuming combined_df is already defined and has no missing values
combined_df['crop_yield'] = y

# Plotting the heatmap
plt.figure(figsize=(10, 6))
sns.heatmap(combined_df.corr(), annot=True, cmap='coolwarm')
plt.title('Correlation Matrix')
plt.show()


 


# In[ ]:




