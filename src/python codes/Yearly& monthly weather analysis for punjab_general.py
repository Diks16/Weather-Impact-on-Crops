#!/usr/bin/env python
# coding: utf-8

# In[15]:


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
# Plotting the trends
fig, axs = plt.subplots(2, 2, figsize=(15, 10))

# Yearly trends
axs[0, 0].plot(yearly_data['year'], yearly_data['temperature_max'], label='Max Temperature')
axs[0, 0].plot(yearly_data['year'], yearly_data['temperature_min'], label='Min Temperature')
axs[0, 0].set_title('Yearly Temperature Trends')
axs[0, 0].set_xlabel('Year')
axs[0, 0].set_ylabel('Temperature (K)')
axs[0, 0].legend()

axs[0, 1].plot(yearly_data['year'], yearly_data['precipitation_sum'], label='Total Precipitation', color='b')
axs[0, 1].set_title('Yearly Precipitation Trends')
axs[0, 1].set_xlabel('Year')
axs[0, 1].set_ylabel('Precipitation (mm)')
axs[0, 1].legend()

axs[1, 0].plot(yearly_data['year'], yearly_data['evaporation_sum'], label='Total Evaporation', color='g')
axs[1, 0].set_title('Yearly Evaporation Trends')
axs[1, 0].set_xlabel('Year')
axs[1, 0].set_ylabel('Evaporation (mm)')
axs[1, 0].legend()

axs[1, 1].plot(yearly_data['year'], yearly_data['solar_radiation_sum'], label='Total Solar Radiation', color='r')
axs[1, 1].set_title('Yearly Solar Radiation Trends')
axs[1, 1].set_xlabel('Year')
axs[1, 1].set_ylabel('Solar Radiation (W/m^2)')
axs[1, 1].legend()

plt.tight_layout()
plt.show()

# Plotting monthly trends
fig, axs = plt.subplots(2, 2, figsize=(15, 10))

axs[0, 0].plot(monthly_data['year_month'].astype(str), monthly_data['temperature_max'], label='Max Temperature')
axs[0, 0].plot(monthly_data['year_month'].astype(str), monthly_data['temperature_min'], label='Min Temperature')
axs[0, 0].set_title('Monthly Temperature Trends')
axs[0, 0].set_xlabel('Year-Month')
axs[0, 0].set_ylabel('Temperature (K)')
axs[0, 0].legend()

axs[0, 1].plot(monthly_data['year_month'].astype(str), monthly_data['precipitation_sum'], label='Total Precipitation', color='b')
axs[0, 1].set_title('Monthly Precipitation Trends')
axs[0, 1].set_xlabel('Year-Month')
axs[0, 1].set_ylabel('Precipitation (mm)')
axs[0, 1].legend()

axs[1, 0].plot(monthly_data['year_month'].astype(str), monthly_data['evaporation_sum'], label='Total Evaporation', color='g')
axs[1, 0].set_title('Monthly Evaporation Trends')
axs[1, 0].set_xlabel('Year-Month')
axs[1, 0].set_ylabel('Evaporation (mm)')
axs[1, 0].legend()

axs[1, 1].plot(monthly_data['year_month'].astype(str), monthly_data['solar_radiation_sum'], label='Total Solar Radiation', color='r')
axs[1, 1].set_title('Monthly Solar Radiation Trends')
axs[1, 1].set_xlabel('Year-Month')
axs[1, 1].set_ylabel('Solar Radiation (W/m^2)')
axs[1, 1].legend()

plt.tight_layout()
plt.show()


# In[ ]:




