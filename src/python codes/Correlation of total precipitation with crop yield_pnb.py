#!/usr/bin/env python
# coding: utf-8

# In[6]:


import pandas as pd
import matplotlib.pyplot as plt

# Load the datasets
weather_df = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/data/weather_data/pnb_weather.csv')
crop_yield_df = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/crop_yield.csv')

# Display the first few rows of both dataframes to understand their structure
print(weather_df.head())
print(crop_yield_df.head())

# Aggregate total precipitation by year
total_precipitation_by_year = weather_df.groupby('year')['total_precipitation _sum'].sum().reset_index()
total_precipitation_by_year.rename(columns={'total_precipitation _sum': 'total_precipitation'}, inplace=True)

# Merge the datasets on the year
merged_df = pd.merge(crop_yield_df, total_precipitation_by_year, left_on='Crop_Year', right_on='year')

# Calculate the correlation between total precipitation and crop yield
correlation_matrix = merged_df[['total_precipitation', 'Yield']].corr()

# Display the correlation matrix
print(correlation_matrix)


# Plot the relationship between total precipitation and crop yield
plt.figure(figsize=(10, 6))
plt.scatter(merged_df['total_precipitation'], merged_df['Yield'], alpha=0.6)
plt.title('Scatter Plot of Total Precipitation vs. Crop Yield')
plt.xlabel('Total Precipitation')
plt.ylabel('Crop Yield')
plt.grid(True)
plt.show()


# In[ ]:




