##### Comparison with the original file #####
# The original data URL:
#  https://s3.amazonaws.com/babs-open-data/babs_open_data_year_1.zip
#
# babs_open_data_year_1.zip
# │
# ├───201402_babs_open_data -------> Not used
# │       201402_station_data.csv
# │       201402_status_data.csv
# │       201402_trip_data.csv
# │       201402_weather_data.csv
# │       README.txt
# │
# └───201408_babs_open_data -------> Target of analysis
#         201408_station_data.csv
#         201408_status_data.csv
#         201408_trip_data_.csv
#         201408_weather_data.csv
#         README.txt

# Read the original csv file
org.trip <- fread('./original/201408_babs_open_data/201408_trip_data.csv', data.table=F,
                  colClasses=c('integer', 'integer', 'character', 'character',
                               'integer', 'character', 'character', 'integer',
                               'integer', 'character', 'character'))
names(org.trip) <- c('id', 'duration', 's.date', 's.station',
                     's.terminal', 'e.date', 'e.station', 'e.terminal',
                     'bike', 'type', 'zip')

# Check difference between the file given and the original
sum(trip$id != org.trip$id)
sum(trip$duration != org.trip$duration)
sum(trip$s.date != org.trip$s.date)
sum(trip$s.station != org.trip$s.station)
sum(trip$s.terminal != org.trip$s.terminal)
sum(trip$e.date != org.trip$e.date)
sum(trip$e.station != org.trip$e.station)
sum(trip$e.terminal != org.trip$e.terminal)
sum(trip$bike != org.trip$bike)
sum(trip$type != org.trip$type)
sum(trip$zip != org.trip$zip)
rm(org.trip)



##### Data integrity check #####
# Is ID unique?
length(unique(trip$id)) == nrow(trip)

# Check the trend of Duration
range(trip$duration)
range(trip$duration/60)
range(trip$duration/(60*60))
range(trip$duration/(60*60*24))
hist(trip$duration/60, xlim=c(0, 60), breaks=10000)
hist(trip$duration/60, xlim=c(0, 180), ylim=c(0, 500), breaks=10000)
hist(trip$duration/(60*60), xlim=c(0, 1), breaks=10000)
hist(trip$duration/(60*60*24), ylim=c(0, 10), breaks=9)
sum(as.numeric(trip$e.date - trip$s.date)*60 - trip$duration > 60)

# Check range and trend of Date
range(trip$s.date)
range(trip$e.date)
table(trip$s.month)
table(trip$e.month)
table(trip$s.month, trip$type)
table(trip$e.month, trip$type)
table(trip$s.wday)
table(trip$e.wday)

# Check number of station (should be 70)
length(unique(trip$s.station))
length(unique(trip$s.terminal))
length(unique(trip$e.station))
length(unique(trip$e.terminal))
nrow(unique(trip[, c('s.station', 's.terminal')]))
nrow(unique(trip[, c('e.station', 'e.terminal')]))

# Check number of combination of stations (maximum is 70*70=4900)
nrow(unique(trip[, c('s.station', 'e.station')]))
nrow(unique(trip[, c('s.terminal', 'e.terminal')]))

# Check number of bike (should be around 700)
length(unique(trip$bike))

# Check rider type (blank or NA should not exist)
table(trip$type, useNA='always')



##### Create transition matrix csv file #####
write.csv(table(trip[, c('e.terminal', 's.terminal')]), './link.csv')
