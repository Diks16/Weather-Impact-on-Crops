#!/usr/bin/env python
# coding: utf-8

# In[13]:


import pandas as pd

# Load the datasets
weather_df = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/varanasi_weather.csv')
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


# In[ ]:




