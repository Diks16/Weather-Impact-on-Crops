pip install pandas

import pandas as pd

# Load datasets
#weather_data = pd.read_csv('/Users/dikshamishra/Desktop/Dissertation/Crop and weather/varanasi_weather.csv')
#crop_data = pd.read_csv('/Users/dikshamishra/Desktop/Dissertation/Crop and weather/crop_yield.csv')
#agri_data = pd.read_csv('Users/dikshamishra/Desktop/Dissertation/Crop and weather/India Agriculture Crop Production.csv')

# Inspect datasets
print(weather_data.head())
print(crop_data.head())
print(agri_data.head())

# Handling missing values
weather_data = weather_data.dropna()
crop_data = crop_data.dropna()
agri_data = agri_data.dropna()

# Merge datasets based on common columns
merged_data = pd.merge(crop_data, weather_data, on=['Year', 'Region'], how='inner')
merged_data = pd.merge(merged_data, agri_data, on=['Year', 'Region'], how='inner')

print(merged_data.head())
