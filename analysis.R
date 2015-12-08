##### Load libraries #####
library(lubridate)
library(zipcode)
library(dplyr)
library(plotly)
library(geosphere)


##### Load data #####
# Load trip data
trip <- read.csv('./input/201408_trip_data.csv', 
                 colClasses=c('integer', 'integer', 'character', 'character',
                              'integer', 'character', 'character', 'integer',
                              'integer', 'character', 'character'))
# Rename columns
names(trip) <- c('id', 'duration', 's.date', 's.station',
                 's.terminal', 'e.date', 'e.station', 'e.terminal',
                 'bike', 'type', 'zip')

# Load station master
station <- read.csv('./input/201408_station_data.csv', 
                    colClasses=c('integer', 'factor', 'numeric', 'numeric',
                                 'integer', 'factor', 'character'))
# Rename column 
names(station) <- c('id', 'name', 'lat', 'long',
                    'dockcount', 'landmark', 'installed.on')



##### Data Type Conversion #####
# Convert date (character) into POSIXct class
trip$s.date <- mdy_hm(trip$s.date, tz='US/Pacific')
trip$e.date <- mdy_hm(trip$e.date, tz='US/Pacific')
station$installed.on <- mdy(station$installed.on, tz='US/Pacific')

# Reorder landmark from south to north
station$landmark <- factor(station$landmark, 
                           levels=c("San Jose", "Mountain View", "Palo Alto", 
                                    "Redwood City", "San Francisco"))



##### Data Cleansing #####
# Remove zipcode for Customer users
trip$zip[trip$type == 'Customer'] <- NA
table(nchar(trip$zip, keepNA=T), useNA='always')
# Clean up zipcode
trip$zip <- clean.zipcodes(trip$zip)
table(nchar(trip$zip, keepNA=T), useNA='always')
# Remove zipcodes which do not exist
data(zipcode)
trip$zip[!(trip$zip %in% zipcode$zip)] <- NA
table(trip$zip, useNA='always')
length(unique(trip$zip))
# Convert to factor
trip$zip <- as.factor(trip$zip)


##### Feature Engineering #####
# Add minutes version of duration
trip$duration.m <- trip$duration / 60

# Extract month, day of the week and hour
trip$s.month <- as.factor(month(trip$s.date, label=T, abbr=T))
trip$s.wday <- as.factor(wday(trip$s.date, label=T, abbr=T))
trip$s.day <- day(trip$s.date)
trip$s.hour <- as.factor(hour(trip$s.date))
trip$e.month <- as.factor(month(trip$e.date, label=T, abbr=T))
trip$e.wday <- as.factor(wday(trip$e.date, label=T, abbr=T))
trip$e.day <- day(trip$e.date)
trip$e.hour <- as.factor(hour(trip$e.date))



##### Link Analysis - PageRank of station #####
# Create transition matrix for stations (total of each column is 1)
link <- table(trip[, c('e.terminal', 's.terminal')])
link <- t(t(link) / colSums(link))
# Compute rank of stations
r <- rep(1, 70)
beta <- 0.9
for (i in 1:500) {
  r_ <- beta * link %*% r + (1 - beta)
  #print(r_[1])
  if (sum(abs(r - r_)) < 1e-06) break
  r <- r_
}
# Add rank info to station data.frame
rr <- data.frame(terminal=unlist(dimnames(r)), 
                 rank=as.vector(r), 
                 row.names=NULL)
station <- merge(station, rr, by.x='id', by.y='terminal')
rm(link, beta, i, r, r_, rr)



##### Balance of departure and arrival #####
link <- table(trip[, c('e.terminal', 's.terminal')])
balance <- data.frame(departure=colSums(link), arrival=rowSums(link))
balance$dif <- balance$arrival - balance$departure
balance$terminal <- row.names(balance)

# Add departure, arrival and diff to station
station <- merge(station, balance, by.x='id', by.y='terminal')
rm(link, balance)



##### Exploratory analysis - Station Rank #####
# Station Rank 1
barplot(station$rank[order(station$rank)], 
        names.arg=station$name[order(station$rank)],
        horiz=T, cex.names=0.5)
# Station Rank 2
station.ordered <- station[order(station$landmark,  station$rank, decreasing=T), ]
plot_ly(station.ordered, x=name, y=rank, type='bar', color=landmark)

# Station Rank 3
station.ordered <- station[order(station$landmark,  station$rank, decreasing=F), ]
station.ordered$name <- factor(station.ordered$name, 
                               levels=station.ordered$name)
ggplot(station.ordered, aes(x=name, y=rank, fill=landmark)) + 
  geom_bar(stat='identity') +
  coord_flip()

##### Exploratory analysis - Number of arrival #####
# Total number of arrival by heatmap 1
heatmap(table(trip[, c('e.terminal', 's.terminal')]),
        Rowv=NA, Colv=NA, scale='column',
        col=cm.colors(256))

# Total number of arrival by heatmap 2
plot_ly(z=table(trip[, c('e.terminal', 's.terminal')]),
        type='heatmap')

##### Exploratory analysis - Penalty ride (more than 30 mins) #####
# How many trips exceeded 30 mins?
table(trip$duration.m > 30)
# How much propotion of trips exceeded 30 mins?
prop.table(table(trip$duration.m > 30))

# How many trips exceeded 30 mins by member type?
table(trip$duration.m > 30, trip$type)
# How much propotion of trips exceeded 30 mins by member type?
prop.table(table(trip$duration.m > 30, trip$type), margin=2)

plot_ly(Z=table(trip$duration.m > 30, trip$type), type='table')

##### Exploratory analysis - Subscriber user's penalty ride #####
# Extract penalty ride of Subscriber
penalty <- trip[trip$type == 'Subscriber' & trip$duration.m > 30, ]

# Summary of travel time
summary(penalty$duration.m)

# Visualize distribution
qplot(penalty$duration.m, geom='histogram', binwidth=2, xlim=c(30, 100))
gg <- ggplot(penalty, aes(x=duration.m)) + geom_histogram(binwidth=2)
ggplotly(gg)

# Show distribution by 15 mins interval
table(cut(penalty$duration.m, breaks=seq(30, 210, 15)), useNA='always')
round(prop.table(table(cut(penalty$duration.m, breaks=seq(30, 210, 15)), useNA='always')), 3)

##### Exploratory analysis - Subscriber user's penalty ride between 30 and 45 mins #####
penalty2 <- trip[trip$type == 'Subscriber' & 
                   trip$duration.m > 30 &
                   trip$duration.m <= 45 &, ]

# Compare propotions between the original and 30-45 mins trips
write.csv(prop.table(table(trip$e.station)), 'pt1.csv')
write.csv(prop.table(table(penalty2$e.station)), 'pt2.csv')

# Any tendency? -> No
table(penalty2$e.hour)
table(penalty2$e.wday)
table(penalty2[, c('e.month', 'e.day')])



##### Summarize trip routes #####
summarizeByRoute <- function(trip) {
  # Aggregate and summarize by route
  #
  # Args:
  #   trip: data.frame, 201408_trip_data.csv + preparation above
  #
  # Return:
  #   Route summary
  #
  trip$less_30 <- ifelse(trip$duration.m < 30, 1, 0)
  trip$r_30_45 <- ifelse(trip$duration.m >= 30 & trip$duration.m < 45, 1, 0)
  trip$r_45_60 <- ifelse(trip$duration.m >= 45 & trip$duration.m < 60, 1, 0)
  trip$more_60 <- ifelse(trip$duration.m >= 60, 1, 0)
  
  route <-
    trip %>%
    group_by(s.terminal, e.terminal) %>%
    summarise(count=n(), m.ave=mean(duration.m), m.md=median(duration.m),
              less_30=sum(less_30), r_30_45=sum(r_30_45),
              r_45_60=sum(r_45_60), more_60=sum(more_60)) %>%
    inner_join(station[, 1:6], by=c('s.terminal' = 'id')) %>%
    inner_join(station[, 1:6], by=c('e.terminal' = 'id'))
  names(route) <- c('s.terminal', 'e.terminal', 'count', 'ave', 'median', 
                    'less_30', 'r_30_45', 'r_45_60', 'more_60',
                    's.station', 's.lat', 's.long', 's.dockcount', 's.landmark',
                    'e.station', 'e.lat', 'e.long', 'e.dockcount', 'e.landmark')
  
  # Compute the distance (km, WGS84 ellipsoid) of each route
  route$distance <- apply(route[, c('s.long','s.lat','e.long','e.lat')], 1, 
                          FUN=function(x) {
                            distGeo(p1=c(x[1], x[2]), p2=c(x[3], x[4])) / 1000
                          })
  # Add area:
  #  - if s.landmark == e.landmark then landmark
  #  - is s.landmark == e.landmark then "Crossing"
  route$area <- ifelse(route$s.landmark == route$e.landmark, 
                       as.character(route$e.landmark), 'Crossing')
  route$area <- factor(route$area, 
                       levels=c("Crossing", "San Jose", "Mountain View", 
                                "Palo Alto", "Redwood City", "San Francisco"))
  return(route)
}
route <- summarizeByRoute(trip[trip$type == 'Subscriber', ])



##### Visualize penalty rides 1 #####
visualizePenalty <- function(route) {
  # Visualize penalty rides in distance * median dimentions
  #
  # Args:
  #   route: data.frame created by summarizeByRoute
  #
  # Return:
  #   None, just draw a scatter plot
  #
  route <- route[route$r_30_45 != 0, ]
  Penalty_ratio <- route$r_30_45 / route$count
  p <-
    route %>% 
    plot_ly(type='scatter', 
            x=distance, 
            y=median, 
            mode='markers', 
            color=area, 
            size=Penalty_ratio, 
            hoverinfo='x+text',
            textposition='bottom right', 
            text=paste0('Start: ', s.terminal, ': ', s.station, '<br>',
                        'End: ', e.terminal, ': ', e.station, '<br><br>',
                        'Total trips : ', count, '<br>',
                        'Penalty rides : ', r_30_45)) %>%
    add_trace(y=rep(30, nrow(route)), 
              type='', 
              name="30 mins", 
              mode='lines', 
              showlegend=F, 
              text=NULL, 
              line=list(color=toRGB('red', alpha=0.5), 
                        width=1)) %>%
    layout(title='Which route has more 30-45 minutes\' penalty rides? Part 1',
           xaxis=list(title='Distance between stations (km)'),
           yaxis=list(title='Median of Trip Time (mins)'),
           hovermode='closest')
  print(p)
  # For publication to Plotly website
  # plotly_POST(p, 'bike-sharing-uber/penalty1', 'overwrite', world_readable=T)
  #  -> https://plot.ly/~watanabe8760/47
}
visualizePenalty(route[route$count > 2, ])


##### Visualize penalty rides 2 #####
visualizePenalty2 <- function(route) {
  # Visualize penalty rides in distance * count dimentions
  #
  # Args:
  #   route: data.frame created by summarizeByRoute
  #
  # Return:
  #   None, just draw a scatter plot
  #
  route <- route[route$r_30_45 != 0, ]
  Penalty_ratio <- route$r_30_45 / route$count
  p <-
    route %>% 
    plot_ly(type='scatter', 
            x=distance, 
            y=count, 
            mode='markers', # Load trip data

            color=area, 
            size=Penalty_ratio, 
            hoverinfo='x+text',
            textposition='bottom right', 
            text=paste0('Start: ', s.terminal, ': ', s.station, '<br>',
                        'End: ', e.terminal, ': ', e.station, '<br><br>',
                        'Total trips : ', count, '<br>',
                        'Penalty rides : ', r_30_45)) %>%
    layout(title='Which route has more 30-45 minutes\' penalty rides? Part 2',
           xaxis=list(title='Distance between stations (km)'),
           yaxis=list(title='Number of Trips'),
           hovermode='closest')
  print(p)
  # For publication to Plotly website
  # plotly_POST(p, 'bike-sharing-uber/penalty2', 'overwrite', world_readable=T)
  #  -> https://plot.ly/~watanabe8760/51
}
visualizePenalty2(route[route$count > 2, ])



##### Visualize trip time distribution for a specific route#####
showTripTimeDist <- function(trip, s.terminal, e.terminal, lower, upper) {
  # Visualize distribution of trip time for a specific route and trip time
  #
  # Args:
  #   trip      : data.frame, 201408_trip_data.csv + preparation above
  #   s.terminal: integer, terminal id of start station
  #   e.terminal: integer, terminal id of end station
  #   lower     : integer, lower limit of trip time in minutes, less than this will be excluded
  #   upper     : integer, upper limit of trip time in minutes, more than this will be excluded
  #
  # Returns:
  #   None, just make a barchart of trip time
  #
  tgt <- trip[trip$s.terminal == s.terminal & 
                trip$e.terminal == e.terminal &
                trip$duration.m >= lower &
                trip$duration.m <= upper, ]
  p <-
    tgt %>% 
    plot_ly(type='histogram',
            x=duration.m) %>%
    layout(title=paste0('Trip Time Distribution between ', lower, ' and ', upper, ' mins<br>', 
                        'From ', s.terminal[1], ' : ', tgt$s.station[1], '<br>',
                        'To ', e.terminal[1], ' : ', tgt$e.station[1]),
           xaxis=list(title='Trip Time (minutes)'),
           yaxis=list(title='Frequency'),
           annotation=list())
  print(p)
  # For publication to Plotly website 
  # plotly_POST(p, 'bike-sharing-uber/distribution-67-63', 'new', world_readable=T)
  #  -> https://plot.ly/~watanabe8760/55
  
#   # Aggregate barchart version
#   totalcnt <- nrow(trip[trip$s.terminal == start & trip$e.terminal == end, ])
#   interval <- seq(from=0, to=max(trip$duration.m)+5, by=5)
#   tgtagg <- aggregate(rep(1, nrow(tgt)), 
#                       list(cut(tgt$duration.m, breaks=interval)), 
#                       FUN=sum)
#   names(tgtagg) <- c('range', 'count')
#   tgtagg$range <- factor(tgtagg$range, levels=tgtagg$range)
#   tgtagg$percent <- round(tgtagg$count / totalcnt * 100, 2)
#   p <-
#     tgtagg %>%
#     plot_ly(type='bar',
#             x=range,
#             y=count,
#             text=paste0(percent, '%'))
#   print(p)
}
showTripTimeDist(trip, 67, 63, 0, 60)



##### (Prototype) Map Visualization #####
plot_ly(locationmode='USA-states', 
        type='scattergeo') %>%
  layout(geo=list(
    scope = 'usa',
    lataxis=list(range=c(37.274053, 37.833649)),
    lonaxis=list(range=c(-122.567596, -121.787567)),
    projection = list(type = 'lbers usa'),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    countrycolor = toRGB("gray80")
  ))



##### (Prototype) SVM - Subscriber user's trip between 0 and 60 mins #####
majority <- trip[trip$type == 'Subscriber' & trip$duration.m <= 60, ]
majority$penalty <- as.factor(ifelse(majority$duration.m > 30, 1, 0))
library(e1071)
majority$s.station <- as.factor(majority$s.station)
majority$e.station <- as.factor(majority$e.station)
svm.model <- 
  svm(penalty ~ s.station + e.station + zip + s.month + s.wday + s.hour, majority)


