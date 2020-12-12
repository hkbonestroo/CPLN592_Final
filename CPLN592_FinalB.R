## Setup
setwd("C:/Users/Hannah/Documents/Penn/Fall 2020/CPLN-592/Assignments/Final Project/CPLN592_Final")
library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)
library(mapview)
library(FNN)
library(osmdata)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 24,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 24,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey60", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey60", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}
palette6 <-c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c","#D2FBD4")
palette5 <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c")
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")
palette2 <- c("#6baed6","#08519c")

# Functions
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

# Load census API key

census_api_key("91a259a2aaac3093a636d189040e0ff263fc823b", overwrite = TRUE)

## Read data

sfCounty <-
  st_read("https://data.sfgov.org/api/geospatial/p5b7-5n3h?method=export&format=GeoJSON") %>% 
  st_union()
sfCounty <- st_transform(sfCounty, 'EPSG:6339')

neighborhoods <- 
  st_read("https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=GeoJSON") %>%
  dplyr::select(-link)
neighborhoods <-  st_transform(neighborhoods,'EPSG:6339') 

#load sf tracts
sfCensus <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001"), 
          year = 2018, 
          state = "CA", 
          geometry = TRUE, 
          county=c("San Francisco"),
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
  dplyr::select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
                Means_of_Transport, Total_Public_Trans,
                Med_Age,
                GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport)
sfTracts <- 
  sfCensus %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  dplyr::select(GEOID, geometry) %>% 
  st_sf

`%nin%` = Negate(`%in%`)
sfTracts <- subset(sfTracts, GEOID %nin% c("06075980401","06075017902","06075017902","06075017902"))

#load parking data
ParkingMeters.dat <- st_read("SFMTA_Parking_Meter_Detailed_Revenue_Transactions.csv")
ParkingMeters <- subset(ParkingMeters.dat, Lot %in% "street")
ParkingMeters<- ParkingMeters%>%
  dplyr::select(POST_ID,STREET_BLOCK, SESSION_START_DT, SESSION_END_DT, GROSS_PAID_AMT)
ParkingMeters <- ParkingMeters %>%
  mutate(interval60 = floor_date(mdy_hm(SESSION_END_DT), unit = "hour"),
         interval15 = floor_date(mdy_hm(SESSION_END_DT), unit = "15 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE))
# convert the string columns to dates
ParkingMeters$SESSION_START_DT <- as.POSIXct(ParkingMeters$SESSION_START_DT,
                                             format='%m/%d/%Y %H:%M')
ParkingMeters$SESSION_END_DT       <- as.POSIXct(ParkingMeters$SESSION_END_DT,
                                                 format='%m/%d/%Y %H:%M')
ParkingMeters$ParkTime <-(difftime(ParkingMeters$SESSION_START_DT,ParkingMeters$SESSION_END_DT,units="hours"))*(-1)
ParkingMeters$ParkTimeMin <-(ParkingMeters$ParkTime)*60
ParkingMeters$ParkTimeMin2 <-as.numeric(ParkingMeters$ParkTimeMin)
ParkingMeters$GROSS_PAID_AMT2 <-as.numeric(ParkingMeters$GROSS_PAID_AMT)
ParkingMeters$ParkingRate <- ParkingMeters$GROSS_PAID_AMT2/ParkingMeters$ParkTimeMin2

#load meters shp
ParkingMeters.sf <- 
  st_read("Parking_Meters.csv")

#creating block stations
ParkingMetersUnique <-
  ParkingMeters %>%
  dplyr::select(STREET_BLOCK, POST_ID)

ParkingMetersUnique <- ParkingMetersUnique[!duplicated(ParkingMetersUnique$STREET_BLOCK), ]
ParkingMetersUnique<-
  ParkingMetersUnique%>%
  rename(StationID =POST_ID)

ParkingMeters <- merge(ParkingMeters, ParkingMetersUnique, by='STREET_BLOCK')
ParkingMeters.sf<-
  ParkingMeters.sf%>%
  rename(StationID=POST_ID)
ParkingMeters.sf <- merge(ParkingMeters, ParkingMeters.sf, by='StationID')
ParkingMeters.sf$LONGITUDE2 <-ParkingMeters.sf$LONGITUDE
ParkingMeters.sf$LATITUDE2 <- ParkingMeters.sf$LATITUDE

#join 

ParkingMeters.sf <- ParkingMeters.sf[!(ParkingMeters.sf$LATITUDE == ""), ]
ParkingMeters.sf <- ParkingMeters.sf[!(ParkingMeters.sf$LONGITUDE == ""), ]
ParkingMeters.sf <- st_as_sf(x = ParkingMeters.sf, coords = c("LONGITUDE","LATITUDE"),crs = "+proj=longlat +crs = 'EPSG:6339'")
ParkingMeters.sf<- st_transform(ParkingMeters.sf,"EPSG:6339")

#neighborhoods

ParkingMeters.sf <- st_join(ParkingMeters.sf,neighborhoods, join = st_within)

ParkingMeters.sf <-
  ParkingMeters.sf%>%
  dplyr::select(StationID,STREET_BLOCK,POST_ID,SESSION_START_DT,SESSION_END_DT,GROSS_PAID_AMT,interval60,interval15,week,dotw,ParkTime,
                ParkTimeMin2,GROSS_PAID_AMT2,ParkingRate,Neighborhoods,COLLECTION_SUBROUTE,LONGITUDE2,LATITUDE2,name)

## Adding census data to parking data
ParkingMeter_census <- st_join(ParkingMeters.sf %>% 
                           filter(is.na(LONGITUDE2) == FALSE &
                                    is.na(LATITUDE2) == FALSE)  %>%
                           st_as_sf(., coords = c("LONGITUDE2", "LATITUDE2"), crs = 'EPSG:6339'),
                         sfTracts %>%
                           st_transform(crs='EPSG:6339'),
                         join=st_intersects,
                         left = TRUE) %>%
  rename(Origin.Tract = GEOID) %>%
  mutate(start_station_longitude = unlist(map(geometry, 1)),
         start_station_latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  dplyr::select(-geometry) 

#weather
weather.Panel <- 
  riem_measures(station = "SFO", date_start = "2018-04-30", date_end = "2018-05-04") %>%
  dplyr::select(valid, tmpf, p01i, sknt)%>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

grid.arrange(
  ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
    labs(title="Percipitation", x="Hour", y="Perecipitation") + plotTheme(),
  ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
    labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
  ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
    labs(title="Temperature", x="Hour", y="Temperature") + plotTheme(),
  top="Weather Data - San Francisco SFO - April-May, 2018")

#data exploration
ggplot(ParkingMeter_census %>%
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="Parking Sessions per hr,\n San Francisco, May, 2018",
       x="Date", 
       y="Number of Sessions")+
  plotTheme()

## Mean number of hourly trips
ParkingMeter_census %>%
  mutate(time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
  group_by(interval60, StationID, time_of_day) %>%
  tally()%>%
  group_by(StationID, time_of_day)%>%
  summarize(mean_trips = mean(n))%>%
  ggplot()+
  geom_histogram(aes(mean_trips), binwidth = 1)+
  labs(title="Mean Number of Hourly Parking Sessions Per Block,\nSan Francisco, April - May, 2018",
       x="Number of Sessions", 
       y="Frequency")+
  facet_wrap(~time_of_day)+
  plotTheme()


## By block
ggplot(ParkingMeter_census %>%
         group_by(interval60, StationID) %>%
         tally())+
  geom_histogram(aes(n), binwidth = 2)+
  labs(title="Parking Sessions per hr by block,\nSan Francisco, April-May, 2018",
       x="Number of Blocks", 
       y="Session Counts")+
  plotTheme()

## Number of trips by hr in a week
ggplot(ParkingMeter_census %>% mutate(hour = hour(SESSION_END_DT)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Parking Sessions by day of the week,\nSan Francisco, April-May, 2018",
       x="Hour", 
       y="Session Counts")+
  plotTheme()

ParkingMeter_census_plot <- ParkingMeter_census
ParkingMeter_census_plot <- ParkingMeter_census_plot[!(ParkingMeter_census_plot$LATITUDE == ""), ]
ParkingMeter_census_plot <- ParkingMeter_census_plot[!(ParkingMeter_census_plot$LONGITUDE == ""), ]
ParkingMeter_census_plot <- st_as_sf(x = ParkingMeter_census_plot, coords = c("LONGITUDE2","LATITUDE2"),crs = "+proj=longlat +crs = 'EPSG:6339'")
ParkingMeter_census_plot<- st_transform(ParkingMeter_census_plot,"EPSG:6339")
#Parking By Block
ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs='EPSG:6339'), colour = '#efefef')+
  geom_sf(data = ParkingMeter_census_plot %>% 
               mutate(hour = hour(SESSION_END_DT),
                      time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                              hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                              hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                              hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
               group_by(StationID, time_of_day) %>%
               tally(),
             aes( color = n), 
             fill = "transparent",  size = 1)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  facet_grid(~ time_of_day)+
  labs(title="Parking Sessions per hr by block,\nSan Francisco, April-May, 2018")+
  mapTheme()

#panel
length(unique(ParkingMeter_census$interval60)) * length(unique(ParkingMeter_census$StationID))

study.panel <- 
  expand.grid(interval60 = unique(ParkingMeters$interval60), 
              StationID = unique(ParkingMeters$StationID))
study.panel <-
left_join(study.panel, ParkingMeter_census %>%
            dplyr::select(StationID,  Origin.Tract, name, LONGITUDE2, LATITUDE2 )%>%
            distinct() %>%
            group_by(StationID) %>%
            slice(1))
nrow(study.panel)  

#ride panel
ride.panel <- 
  ParkingMeter_census %>%
  mutate(Trip_Counter = 1) 

ride.panel <- 
  ride.panel%>%
  right_join(study.panel)

ride.panel <-
  ride.panel%>%
  group_by(interval60, StationID, Origin.Tract,name, LONGITUDE2, LATITUDE2) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>%
  left_join(weather.Panel) %>%
  ungroup() %>%
  filter(is.na(StationID) == FALSE) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label = TRUE)) %>%
  filter(is.na(Origin.Tract) == FALSE)

ride.panel <- 
  left_join(ride.panel, sfCensus %>%
              as.data.frame() %>%
              dplyr::select(-geometry), by = c("Origin.Tract" = "GEOID"))

#add lags
ride.panel <- 
  ride.panel %>% 
  arrange(StationID, interval60) %>% 
  mutate(lagHour = dplyr::lag(Trip_Count,1),
         lag2Hours = dplyr::lag(Trip_Count,2),
         lag3Hours = dplyr::lag(Trip_Count,3),
         lag4Hours = dplyr::lag(Trip_Count,4),
         lag12Hours = dplyr::lag(Trip_Count,12),
         lag1day = dplyr::lag(Trip_Count,24)) %>%
  mutate(day = yday(interval60))

ride.panel <-
  ride.panel %>%
  mutate(X = LONGITUDE2, Y = LATITUDE2 )%>%
  st_as_sf(coords = c("X", "Y"), crs = 'EPSG:6339', agr = "constant") %>%
  st_transform('EPSG:6339')

as.data.frame(ride.panel) %>%
  group_by(interval60) %>% 
  summarise_at(vars(starts_with("lag"), "Trip_Count"), mean, na.rm = TRUE) %>%
  gather(Variable, Value, -interval60, -Trip_Count) %>%
  mutate(Variable = factor(Variable, levels=c("lagHour","lag2Hours","lag3Hours","lag4Hours",
                                              "lag12Hours","lag1day")))%>%
  group_by(Variable) %>%  
  summarize(correlation = round(cor(Value, Trip_Count),2))

### Other features
# bars
sfbox <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001"), 
          year = 2018, 
          state = "CA", 
          geometry = TRUE, 
          county=c("San Francisco"),
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
  dplyr::select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
                Means_of_Transport, Total_Public_Trans,
                Med_Age,
                GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport)
sfbox <- 
  sfbox %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  dplyr::select(GEOID, geometry) %>% 
  st_sf

`%nin%` = Negate(`%in%`)
sfbox <- subset(sfbox, GEOID %nin% c("06075980401","06075017902","06075017902","06075017902"))

sfbox <- st_union(sfbox)
sfbox <-sfbox%>%
  st_transform(crs=4326)
xmin = st_bbox(sfbox)[[1]]
ymin = st_bbox(sfbox)[[2]]
xmax = st_bbox(sfbox)[[3]]  
ymax = st_bbox(sfbox)[[4]]

ggplot() +
  geom_sf(data=sfbox, fill="black") +
  geom_sf(data=st_as_sfc(st_bbox(sfbox)), colour="red", fill=NA)
bars <- opq(bbox = c(xmin, ymin, xmax, ymax)) %>% 
  add_osm_feature(key = 'amenity', value = c("bar", "pub", "restaurant")) %>%
  osmdata_sf()

bars <- 
  bars$osm_points %>%
  .[sfbox,]

bars <- st_transform(bars,'EPSG:6339')
bars <- st_set_crs(bars,'EPSG:6339')
st_c <- st_coordinates

ride.panel <-
  ride.panel %>% 
  mutate(
    bar_dist = nn_function(st_coordinates(ride.panel), st_coordinates(bars), 1))

#distance to transit

transit_stops <-st_read("https://opendata.arcgis.com/datasets/561dc5b42fa9451b95faf615a3054260_0.geojson") %>%
  st_transform(st_crs('EPSG:6339'))
transit_stops <- st_transform(transit_stops,'EPSG:6339')

ride.panel <-
  ride.panel %>% 
  mutate(
    station_dist = nn_function(st_coordinates(ride.panel), st_coordinates(transit_stops), 1))
#time of day
ride.panel<-
  ride.panel%>%
  mutate(
       time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                               hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                               hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                               hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))


# training test
## Regression
ride.Train <- subset(ride.panel, dotw %in% c("Mon","Tue","Wed"))
ride.Test <- subset(ride.panel, dotw %in% c("Thu","Fri"))

### 5 regressions
reg1 <- lm(Trip_Count ~  hour(interval60),  data=ride.Train)

reg2 <- lm(Trip_Count ~  StationID,  data=ride.Train)

reg3 <- lm(Trip_Count ~  StationID + hour(interval60), 
           data=ride.Train)

reg4 <- lm(Trip_Count ~  StationID +  hour(interval60) +
             lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day, 
           data=ride.Train)
reg5 <- lm(Trip_Count ~  StationID +  hour(interval60) +
             lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day +bar_dist +station_dist, 
           data=ride.Train)
reg6 <-lm(Trip_Count ~  StationID +  hour(interval60) +
            lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day +bar_dist +station_dist+name+time_of_day, 
          data=ride.Train)
## Predicting 
ride.Test.weekNest <- 
  ride.Test %>%
  nest(-week) 

model_pred <- function(dat, fit){
  pred <- predict(fit, newdata = dat)}

week_predictions <- 
  ride.Test.weekNest %>% 
  mutate(ATime_FE = map(.x = data, fit = reg1, .f = model_pred),
         BSpace_FE = map(.x = data, fit = reg2, .f = model_pred),
         CTime_Space_FE = map(.x = data, fit = reg3, .f = model_pred),
         DTime_Space_FE_timeLags = map(.x = data, fit = reg4, .f = model_pred),
         ETime_Space_FE_timeLags_Features = map(.x = data, fit = reg5, .f = model_pred),
         All_features=map(.x = data, fit = reg6, .f = model_pred)) %>% 
  gather(Regression, Prediction, -data, -week) %>%
  mutate(Observed = map(data, pull, Trip_Count),
         Absolute_Error = map2(Observed, Prediction,  ~ abs(.x - .y)),
         MAE = map_dbl(Absolute_Error, mean, na.rm = TRUE),
         sd_AE = map_dbl(Absolute_Error, sd, na.rm = TRUE))

week_predictions

#week predictions plot neighborhood
week_predictions_plot_neighbor2 <-week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         name = map(data,pull,name),
         StationID = map(data, pull, StationID), 
         LATITUDE2 = map(data, pull, LATITUDE2), 
         LONGITUDE2 = map(data, pull, LONGITUDE2),
         dotw = map(data, pull, dotw) ) %>%
  dplyr::select(interval60, StationID, LATITUDE2, 
                LONGITUDE2, Observed, Prediction, Regression,
                dotw,name) %>%
  unnest() %>%
  filter(Regression == "All_features")%>%
  mutate(
    time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                            hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                            hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                            hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush")) %>%
  group_by(StationID, time_of_day, LONGITUDE2, LATITUDE2,name) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))
week_predictions_plot_neighbor2 <- week_predictions_plot_neighbor2[!(week_predictions_plot_neighbor2$LATITUDE2 == ""), ]
week_predictions_plot_neighbor2 <- week_predictions_plot_neighbor2[!(week_predictions_plot_neighbor2$LONGITUDE2 == ""), ]
week_predictions_plot_neighbor2 <- st_as_sf(x = week_predictions_plot_neighbor2, coords = c("LONGITUDE2","LATITUDE2"),crs = "+proj=longlat +crs = 'EPSG:6339'")
week_predictions_plot_neighbor2<- st_transform(week_predictions_plot_neighbor2,"EPSG:6339")

## Cross validation
library(caret)

netsample <- sample_n(ride.panel, 100000)%>%
  na.omit()

fitControl <- trainControl(method = "cv", 
                           number = 100,
                           savePredictions = TRUE)

set.seed(1000)
# for k-folds CV

reg.cv <-  
  train(Trip_Count ~ StationID +  hour(interval60) +
          lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day +bar_dist +station_dist+name+time_of_day, 
        data = netsample,  
        method = "lm",  
        trControl = fitControl,  
        na.action = na.pass)

reg.cv

# predictions
week_predictions <- 
  ride.Test.weekNest %>% 
  mutate(ATime_FE = map(.x = data, fit = reg1, .f = model_pred),
         BSpace_FE = map(.x = data, fit = reg2, .f = model_pred),
         CTime_Space_FE = map(.x = data, fit = reg3, .f = model_pred),
         DTime_Space_FE_timeLags = map(.x = data, fit = reg4, .f = model_pred),
         ETime_Space_FE_timeLags_Features = map(.x = data, fit = reg5, .f = model_pred),
         All_features=map(.x = data, fit = reg6, .f = model_pred)) %>% 
  gather(Regression, Prediction, -data, -week) %>%
  mutate(Observed = map(data, pull, Trip_Count),
         Absolute_Error = map2(Observed, Prediction,  ~ abs(.x - .y)),
         MAE = map_dbl(Absolute_Error, mean, na.rm = TRUE),
         sd_AE = map_dbl(Absolute_Error, sd, na.rm = TRUE))

## Bar plot
week_predictions %>%
  dplyr::select(week, Regression, MAE) %>%
  gather(Variable, MAE, -Regression, -week) %>%
  ggplot(aes(week, MAE)) + 
  geom_bar(aes(fill = Regression), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette6) +
  labs(title = "Mean Absolute Errors by model specification") +
  plotTheme()

## Graph
week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         StationID = map(data, pull, StationID)) %>%
  dplyr::select(interval60, StationID, Observed, Prediction, Regression) %>%
  unnest() %>%
  gather(Variable, Value, -Regression, -interval60, -StationID) %>%
  group_by(Regression, Variable, interval60) %>%
  summarize(Value = sum(Value)) %>%
  ggplot(aes(interval60, Value, colour=Variable)) + 
  geom_line(size = 1.1) + 
  facet_wrap(~Regression, ncol=1) +
  labs(title = "Predicted/Observed bike share time series", subtitle = "SF + Alameda",  x = "Hour", y= "Station Trips") +
  plotTheme()

## MAE by station Need to decide on model!!!
week_predictions_plot <-week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         StationID = map(data, pull, StationID), 
         LATITUDE2 = map(data, pull, LATITUDE2), 
         LONGITUDE2 = map(data, pull, LONGITUDE2),
         dotw = map(data, pull, dotw) ) %>%
  select(interval60, StationID, LATITUDE2, 
         LONGITUDE2, Observed, Prediction, Regression,
         dotw) %>%
  unnest() %>%
  filter(Regression == "All_features")%>%
  mutate(
    time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                            hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                            hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                            hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush")) %>%
  group_by(StationID, time_of_day, LONGITUDE2, LATITUDE2) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))
week_predictions_plot <- week_predictions_plot[!(week_predictions_plot$LATITUDE2 == ""), ]
week_predictions_plot <- week_predictions_plot[!(week_predictions_plot$LONGITUDE2 == ""), ]
week_predictions_plot <- st_as_sf(x = week_predictions_plot, coords = c("LONGITUDE2","LATITUDE2"),crs = "+proj=longlat +crs = 'EPSG:6339'")
week_predictions_plot<- st_transform(week_predictions_plot,"EPSG:6339")


ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs='EPSG:6339'), colour = '#efefef')+
  geom_sf(data=week_predictions_plot,aes( color = MAE), 
             fill = "transparent", alpha = 0.4)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  labs(title="Mean Abs Error, Test Set")+
  mapTheme()

## Scatterplot
week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, StationID), 
         start_station_latitude = map(data, pull, LATITUDE2), 
         start_station_longitude = map(data, pull, LONGITUDE2),
         dotw = map(data, pull, dotw)) %>%
  select(interval60, start_station_id, start_station_longitude, 
         start_station_latitude, Observed, Prediction, Regression,
         dotw) %>%
  unnest() %>%
  filter(Regression == "All_features")%>%
  mutate(weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
         time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
  ggplot()+
  geom_point(aes(x= Observed, y = Prediction))+
  geom_smooth(aes(x= Observed, y= Prediction), method = "lm", se = FALSE, color = "red")+
  geom_abline(slope = 1, intercept = 0)+
  facet_grid(~time_of_day)+
  labs(title="Observed vs Predicted",
       x="Observed trips", 
       y="Predicted trips")+
  plotTheme()

##Error Map
week_predictions_plot <-week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         StationID = map(data, pull, StationID), 
         LATITUDE2 = map(data, pull, LATITUDE2), 
         LONGITUDE2 = map(data, pull, LONGITUDE2),
         dotw = map(data, pull, dotw) ) %>%
  select(interval60, StationID, LATITUDE2, 
         LONGITUDE2, Observed, Prediction, Regression,
         dotw) %>%
  unnest() %>%
  filter(Regression == "All_features")%>%
  mutate(
    time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                            hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                            hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                            hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush")) %>%
  group_by(StationID, time_of_day, LONGITUDE2, LATITUDE2) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))
week_predictions_plot <- week_predictions_plot[!(week_predictions_plot$LATITUDE2 == ""), ]
week_predictions_plot <- week_predictions_plot[!(week_predictions_plot$LONGITUDE2 == ""), ]
week_predictions_plot <- st_as_sf(x = week_predictions_plot, coords = c("LONGITUDE2","LATITUDE2"),crs = "+proj=longlat +crs = 'EPSG:6339'")
week_predictions_plot<- st_transform(week_predictions_plot,"EPSG:6339")


ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs='EPSG:6339'), colour = '#efefef')+
  geom_sf(data=week_predictions_plot,aes( color = MAE), 
          fill = "transparent", size = 0.5, alpha = 1.5)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  labs(title="Mean Absolute Errors, Test Set")+
  facet_grid(~time_of_day)+
  mapTheme()

## Error and census
week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, StationID), 
         start_station_latitude = map(data, pull, LATITUDE2), 
         start_station_longitude = map(data, pull, LONGITUDE2),
         dotw = map(data, pull, dotw),
         Percent_Taking_Public_Trans = map(data, pull, Percent_Taking_Public_Trans),
         Med_Inc = map(data, pull, Med_Inc),
         Percent_White = map(data, pull, Percent_White)) %>%
  select(interval60, start_station_id, start_station_longitude, 
         start_station_latitude, Observed, Prediction, Regression,
         dotw, Percent_Taking_Public_Trans, Med_Inc, Percent_White) %>%
  unnest() %>%
  filter(Regression == "DTime_Space_FE_timeLags")%>%
  mutate(weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
         time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush")) %>%
  filter(time_of_day == "AM Rush") %>%
  group_by(start_station_id, Percent_Taking_Public_Trans, Med_Inc, Percent_White) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))%>%
  gather(-start_station_id, -MAE, key = "variable", value = "value")%>%
  ggplot(.)+
  #geom_sf(data = sfCensus, color = "grey", fill = "transparent")+
  geom_point(aes(x = value, y = MAE), alpha = 0.4)+
  geom_smooth(aes(x = value, y = MAE), method = "lm", se= FALSE)+
  facet_wrap(~variable, scales = "free")+
  labs(title="Errors as a function of socio-economic variables",
       y="Mean Absolute Error (Trips)")+
  plotTheme()

## Animation

library(gganimate)
library(gifski)

monday <-
  filter(ParkingMeter_census ,dotw == "Mon")

monday.panel <-
  expand.grid(
    interval15 = unique(monday$interval15),
    Pickup.Census.Tract = unique(ParkingMeter_census$StationID))

ride.animation.data <-
  mutate(monday, Trip_Counter = 1) %>%
  select(interval15, StationID, LONGITUDE2, LATITUDE2, Trip_Counter) %>%
  group_by(interval15, StationID, LONGITUDE2, LATITUDE2) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Trips = case_when(Trip_Count == 0 ~ "0 trips",
                           Trip_Count > 0 & Trip_Count <= 2 ~ "0-2 trips",
                           Trip_Count > 2 & Trip_Count <= 5 ~ "2-5 trips",
                           Trip_Count > 5 & Trip_Count <= 10 ~ "5-10 trips",
                           Trip_Count > 10 & Trip_Count <= 15 ~ "10-15 trips",
                           Trip_Count > 15 ~ "15+ trips")) %>%
  mutate(Trips  = fct_relevel(Trips, "0 trips","0-2 trips","2-5 trips",
                              "5-10 trips","10-15 trips","15+ trips"))
ride.animation.data <- ride.animation.data[!(ride.animation.data$LATITUDE2 == ""), ]
ride.animation.data <- ride.animation.data[!(ride.animation.data$LONGITUDE2 == ""), ]
ride.animation.data <- st_as_sf(x = ride.animation.data, coords = c("LONGITUDE2","LATITUDE2"),crs = "+proj=longlat +crs = 'EPSG:6339'")
ride.animation.data<- st_transform(ride.animation.data,"EPSG:6339")

rideshare_animation <-
  ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs='EPSG:6339'), colour = '#efefef')+
  geom_sf(data = ride.animation.data, 
             aes( fill = Trips), size = 0.5, alpha = 1.5) +
  scale_colour_manual(values = palette5) +
  labs(title = "Parking Sessions by Block for one day in April 2018",
       subtitle = "15 minute intervals: {current_frame}") +
  transition_manual(interval15) +
  mapTheme()

animate(rideshare_animation, duration=20, renderer = gifski_renderer())

#test set
ride.test2 <-
  ride.Test %>%
  mutate(Sessions.Predict = predict(reg.cv, ride.Test))

ride.test3 <-
  ride.test2%>%
  dplyr::select(LONGITUDE2,LATITUDE2,Sessions.Predict,dotw,interval60)

ride.test3$hour <- hour(ride.test3$interval60)
ride.test3.1 <- ride.test3%>%filter(hour==13)
ride.test3.1 <-subset(ride.test3.1, dotw %in% "Thu")

#heat map for 1pm thursday
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.1<-setDT(ride.test3.1)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.1$LATITUDE2))), ceiling(max(as.numeric(ride.test3.1$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.1$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.1$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.1$latbin <- xbreaks[cut(as.numeric(ride.test3.1$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.1$longbin <- ybreaks[cut(as.numeric(ride.test3.1$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.1[, list(Sessions.Predict = mean(Sessions.Predict)), 
                by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)




#heat map for 2pm thursday
ride.test3.2 <- ride.test3%>%filter(hour==14)
ride.test3.2 <-subset(ride.test3.2, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.2<-setDT(ride.test3.2)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.2$LATITUDE2))), ceiling(max(as.numeric(ride.test3.2$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.2$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.2$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.2$latbin <- xbreaks[cut(as.numeric(ride.test3.2$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.2$longbin <- ybreaks[cut(as.numeric(ride.test3.2$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.2[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for 3pm thursday
ride.test3.3 <- ride.test3%>%filter(hour==15)
ride.test3.3 <-subset(ride.test3.3, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.3<-setDT(ride.test3.3)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.3$LATITUDE2))), ceiling(max(as.numeric(ride.test3.3$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.3$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.3$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.3$latbin <- xbreaks[cut(as.numeric(ride.test3.3$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.3$longbin <- ybreaks[cut(as.numeric(ride.test3.3$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.3[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for 4pm thursday
ride.test3.4 <- ride.test3%>%filter(hour(interval60)==16)
ride.test3.4 <-subset(ride.test3.4, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.4<-setDT(ride.test3.4)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.4$LATITUDE2))), ceiling(max(as.numeric(ride.test3.4$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.4$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.4$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.4$latbin <- xbreaks[cut(as.numeric(ride.test3.4$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.4$longbin <- ybreaks[cut(as.numeric(ride.test3.4$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.4[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for 5pm thursday
ride.test3.5 <- ride.test3%>%filter(hour(interval60)==17)
ride.test3.5 <-subset(ride.test3.5, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.5<-setDT(ride.test3.5)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.5$LATITUDE2))), ceiling(max(as.numeric(ride.test3.5$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.5$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.5$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.5$latbin <- xbreaks[cut(as.numeric(ride.test3.5$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.5$longbin <- ybreaks[cut(as.numeric(ride.test3.5$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.5[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for 6pm thursday
ride.test3.6 <- ride.test3%>%filter(hour==18)
ride.test3.6 <-subset(ride.test3.6, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.6<-setDT(ride.test3.6)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.6$LATITUDE2))), ceiling(max(as.numeric(ride.test3.6$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.6$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.6$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.6$latbin <- xbreaks[cut(as.numeric(ride.test3.6$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.6$longbin <- ybreaks[cut(as.numeric(ride.test3.6$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.6[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for7pm thursday
ride.test3.7 <- ride.test3%>%filter(hour==19)
ride.test3.7 <-subset(ride.test3.7, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.7<-setDT(ride.test3.7)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.7$LATITUDE2))), ceiling(max(as.numeric(ride.test3.7$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.7$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.7$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.7$latbin <- xbreaks[cut(as.numeric(ride.test3.7$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.7$longbin <- ybreaks[cut(as.numeric(ride.test3.7$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.7[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for 8pm thursday
ride.test3.8 <- ride.test3%>%filter(hour==20)
ride.test3.8 <-subset(ride.test3.8, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.8<-setDT(ride.test3.8)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.8$LATITUDE2))), ceiling(max(as.numeric(ride.test3.8$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.8$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.8$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.8$latbin <- xbreaks[cut(as.numeric(ride.test3.8$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.8$longbin <- ybreaks[cut(as.numeric(ride.test3.8$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.8[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)

#heat map for 9pm thursday
ride.test3.9 <- ride.test3%>%filter(hour==21)
ride.test3.9 <-subset(ride.test3.9, dotw %in% "Thu")
library(ggplot2)
library(ggmap)
library(data.table)
library(devtools)
register_google("", write = FALSE)
# import data and libaries 

map <- get_map(location = "san francisco", zoom = 15)
ride.test3.9<-setDT(ride.test3.9)


# generate bins for the x, y coordinates
xbreaks <- seq(floor(min(as.numeric(ride.test3.9$LATITUDE2))), ceiling(max(as.numeric(ride.test3.9$LATITUDE2))), by = 0.001)
ybreaks <- seq(floor(min(as.numeric(ride.test3.9$LONGITUDE2))), ceiling(max(as.numeric(ride.test3.9$LONGITUDE2))), by = 0.001)

# allocate the data points into the bins
ride.test3.9$latbin <- xbreaks[cut(as.numeric(ride.test3.9$LATITUDE2), breaks = xbreaks, labels=F)]
ride.test3.9$longbin <- ybreaks[cut(as.numeric(ride.test3.9$LONGITUDE2), breaks = ybreaks, labels=F)]

# Summarise the data for each bin
datamat <- ride.test3.9[, list(Sessions.Predict = mean(Sessions.Predict)), 
                        by = c("latbin", "longbin")]

# Merge the summarised data with all possible x, y coordinate combinations to get 
# a value for every bin
datamat <- merge(setDT(expand.grid(latbin = xbreaks, longbin = ybreaks)), datamat, 
                 by = c("latbin", "longbin"), all.x = TRUE, all.y = FALSE)

# Fill up the empty bins 0 to smooth the contour plot
datamat[is.na(Sessions.Predict), ]$Sessions.Predict <- 0


# Plot the contours
ggmap(map, extent = "device") +
  stat_contour(data = datamat, aes(x = longbin, y = latbin, z = Sessions.Predict, 
                                   fill = ..level.., alpha = ..level..), geom = 'polygon', binwidth =1) +
  scale_fill_gradient(name = "Sessions", low = "green", high = "red") +
  guides(alpha = FALSE)
# Graph of predictions

week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         StationID = map(data, pull, StationID)) %>%
  dplyr::select(interval60, StationID, Observed, Prediction, Regression) %>%
  unnest() %>%
  gather(Variable, Value, -Regression, -interval60, -StationID) %>%
  group_by(Regression, Variable, interval60) %>%
  summarize(Value = sum(Value)) %>%
  ggplot(aes(interval60, Value, colour=Variable)) + 
  geom_line(size = 1.1, color = "#de564d") + 
  facet_wrap(~Regression, ncol=1) +
  labs(title = "Predicted/Observed bike share time series", subtitle = "SF + Alameda",  x = "Hour", y= "Station Trips",caption="Figure 8") +
  plotTheme()

ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs='EPSG:6339'), colour = '#efefef')+
  geom_sf(data=week_predictions_plot,aes( color = MAE), 
          fill = "transparent", size = 0.5, alpha = 1.5)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  labs(title="Mean Absolute Errors, Test Set")+
  facet_grid(~time_of_day)+
  mapTheme()
