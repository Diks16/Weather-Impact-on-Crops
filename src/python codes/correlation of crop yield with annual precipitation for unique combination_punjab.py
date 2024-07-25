#!/usr/bin/env python
# coding: utf-8

# In[5]:


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

# Get unique combinations of State, Crop, and Season
unique_combinations = crop_yield_df[['State', 'Crop', 'Season']].drop_duplicates()

# Initialize a list to store the results
results = []

# Loop through each unique combination
for _, row in unique_combinations.iterrows():
    state = row['State']
    crop = row['Crop']
    season = row['Season']
    
    # Filter the crop yield dataset for the specific combination
    filtered_crop_yield_df = crop_yield_df[(crop_yield_df['State'] == state) & 
                                           (crop_yield_df['Crop'] == crop) & 
                                           (crop_yield_df['Season'] == season)]
    
    # Merge the filtered crop yield dataset with the aggregated precipitation data
    merged_df_filtered = pd.merge(filtered_crop_yield_df, total_precipitation_by_year, left_on='Crop_Year', right_on='year')
    
    # Drop rows with NaN values
    merged_df_filtered = merged_df_filtered.dropna(subset=['total_precipitation', 'Yield'])
    
    if not merged_df_filtered.empty:
        # Calculate the correlation between total precipitation and crop yield
        correlation = merged_df_filtered[['total_precipitation', 'Yield']].corr().iloc[0, 1]
        
        # Append the result to the list
        results.append({
            'State': state,
            'Crop': crop,
            'Season': season,
            'Correlation': correlation
        })

# Convert the results list to a DataFrame
results_df = pd.DataFrame(results)


# Sort by absolute value of correlation and select top 20 for plotting
top_results_df = results_df.reindex(results_df['Correlation'].abs().sort_values(ascending=False).index).head(20)
print(top_results_df)
# Plotting the top 20 correlations
plt.figure(figsize=(14, 8))
plt.barh(top_results_df.index, top_results_df['Correlation'], color='skyblue')
plt.yticks(top_results_df.index, top_results_df.apply(lambda row: f"{row['State']} - {row['Crop']} ({row['Season']})", axis=1))
plt.xlabel('Correlation')
plt.title('Top 20 Correlations of Crop Yield with Annual Precipitation\nby State, Crop, and Season')
plt.gca().invert_yaxis()
plt.grid(axis='x')
plt.show()


# In[ ]:




