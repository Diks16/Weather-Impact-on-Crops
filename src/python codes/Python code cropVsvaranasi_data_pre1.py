#!/usr/bin/env python
# coding: utf-8

# In[137]:


import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset
weather_data = r'C:/Users/dikshamishra/Documents/Crop and weather/Punjab/pnb_weather.csv'
weather_data = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/Punjab/pnb_weather.csv')

# Display the first few rows of the datasets
print("Weather Data:")
print(weather_data.head())

# Correcting column names to match the date parsing format
weather_data = weather_data.rename(columns={
    'date': 'day',
    'evaporation _sum': 'evaporation_sum',
    '2m_temperature _max': 'temperature_max',
    '2m_temperature _min': 'temperature_min',
    'surface_net_solar_radiation _sum': 'solar_radiation_sum',
    'total_precipitation _sum': 'precipitation_sum'
})

# Ensure the day column is correctly parsed as integer
weather_data['day'] = weather_data['day'].astype(int)

# Convert to datetime format
weather_data['date'] = pd.to_datetime(weather_data[['year', 'month', 'day']])

# Group by year and month
weather_data['year_month'] = weather_data['date'].dt.to_period('M')
monthly_data = weather_data.groupby('year_month').agg({
    'temperature_max': 'mean',
    'temperature_min': 'mean',
    'precipitation_sum': 'sum',
    'evaporation_sum': 'sum',
    'solar_radiation_sum': 'sum'
}).reset_index()

# Group by year
yearly_data = weather_data.groupby('year').agg({
    'temperature_max': 'mean',
    'temperature_min': 'mean',
    'precipitation_sum': 'sum',
    'evaporation_sum': 'sum',
    'solar_radiation_sum': 'sum'
}).reset_index()

# Plotting the trends
fig, axs = plt.subplots(2, 2, figsize=(15, 10))


# In[ ]:




