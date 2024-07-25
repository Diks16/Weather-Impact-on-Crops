import cdsapi

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

for year in range(1990, 2024):
	year_str = str(year)
	for month in months:
		for day in days:
			file_name = 'rj/download_rajasthan_' + year_str + '_' + month + '_' + day + '.grib'
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
							27.7, 73, 27.9,
							73.2,
						],
						'format': 'grib',
					},
					file_name)
			except Exception as e:
				print(f"An unexpected error occurred: {e}")
