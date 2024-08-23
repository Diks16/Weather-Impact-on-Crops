import cdsapi

# place = 'up'
# lat = 27.3
# long = 80.80

#place = 'pnb'
#lat = 36.5
#long = 75

#place = 'mp'
#lat = 23.6
#long = 78

place = 'gj'
lat = 22.7
long = 71.5

start_year = 1994
end_year = 2024

c = cdsapi.Client()

months = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12']
days = [
	'01', '02', '03',
	'04', '05', '06',
	'07', '08', '09',
	'10', '11', '12',
	'13', '14', '15',
	'16', '17', '18',
	'19', '20', '21',
	'22', '23', '24',
	'25', '26', '27',
	'28', '29', '30',
	'31',
]

def download_weather_data(year_str, month, day):
	file_name = place + '/' + place + '_' + year_str + '_' + month + '_' + day + '.grib'
	print(file_name)
	try:
		c.retrieve(
			'reanalysis-era5-single-levels',
			{
				'product_type': 'reanalysis',
				'variable': [
					'10m_u_component_of_wind', '10m_v_component_of_wind', 'evaporation',
					'2m_temperature', 'soil_type', 'surface_net_solar_radiation',
					'surface_pressure', 'total_cloud_cover', 'total_precipitation',
				],
				'year': year_str,
				'month': [month],
				'day': [
					day
				],
				'time': [
					'00:00', '01:00', '02:00',
					'03:00', '04:00', '05:00',
					'06:00', '07:00', '08:00',
					'09:00', '10:00', '11:00',
					'12:00', '13:00', '14:00',
					'15:00', '16:00', '17:00',
					'18:00', '19:00', '20:00',
					'21:00', '22:00', '23:00',
				],
				'area': [
					lat, long, lat - 0.2,
					long + 0.2,
				],
				'format': 'grib',
			},
			file_name)
	except Exception as e:
		print(f"An unexpected error occurred: {e}")


for year in range(start_year, end_year):
	year_str = str(year)
	for month in months:
		for day in days:
			download_weather_data(year_str, month, day)


