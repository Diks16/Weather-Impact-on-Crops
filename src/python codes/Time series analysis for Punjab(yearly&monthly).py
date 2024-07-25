#!/usr/bin/env python
# coding: utf-8

# In[29]:


import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.holtwinters import ExponentialSmoothing


# Load and prepare the data
weather_data = r'C:/Users/dikshamishra/Documents/Crop and weather/Punjab/pnb_weather.csv'
weather_data = pd.read_csv('/Users/dikshamishra/Documents/Crop and weather/Punjab/pnb_weather.csv')
weather_data.rename(columns={'date': 'day'}, inplace=True)
weather_data['date'] = pd.to_datetime(weather_data[['year', 'month', 'day']])
weather_data.set_index('date', inplace=True)
weather_data.drop(columns=['Unnamed: 0', 'year', 'month', 'day'], inplace=True)

# Fit an ARIMA model to 2m_temperature_max
model = ARIMA(weather_data['2m_temperature _max'], order=(5, 1, 0))  # Adjust the order based on ACF and PACF plots
model_fit = model.fit()

# Forecast the next 12 periods (e.g., months)
forecast = model_fit.forecast(steps=12)

# Plot the forecast
plt.figure(figsize=(10, 5))
plt.plot(weather_data['2m_temperature _max'], label='Observed')
plt.plot(forecast, label='Forecast', color='red')
plt.legend()
plt.show()

# Display the forecasted values
print(forecast)


# Fit the ETS model
model = ExponentialSmoothing(weather_data['2m_temperature _max'], trend='add', seasonal='add', seasonal_periods=12)
model_fit = model.fit()

# Forecast the next 12 periods (e.g., months)
forecast = model_fit.forecast(steps=12)

# Plot the forecast
plt.figure(figsize=(10, 5))
plt.plot(weather_data['2m_temperature _max'], label='Observed')
plt.plot(forecast, label='Forecast', color='red')
plt.legend()
plt.show()

# Display the forecasted values
print(forecast)



# In[ ]:




