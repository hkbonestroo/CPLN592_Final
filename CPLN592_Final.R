setwd("C:/Users/Hannah/Documents/Penn/Fall 2020/CPLN-592/Assignments/Final Project/CPLN592_Final")
library(tidyverse)
library(tidycensus)
library(kableExtra)
library(tidycensus)
library(sf)
library(gridExtra)
library(grid)
library(knitr)
library(rmarkdown)
library(ggcorrplot)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(tidyr)
library(dplyr)
library(osmdata)
library(mapview)
library(RANN)
library(ggplot2)
library(stargazer)
library(table1)
library(summarytools)
library(arsenal)
library(expss)
library(spatstat)
library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(RSocrata)

options(scipen=999)
options(tigris_class = "sf")

#Load Styles

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
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
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
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

# Load hexadecimal color palette

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")
palette6 <- c("#C48C04", "#FA7800","#8FA108",   "#5AB60C", "#25CB10")
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")
palette2 <- c("#6baed6","#08519c")
# Load nn function

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
    pull()}

# Load census API key

census_api_key("91a259a2aaac3093a636d189040e0ff263fc823b", overwrite = TRUE)

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

#load neighborhoods
neighborhoods <- 
  st_read("https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=GeoJSON") %>%
  st_transform("EPSG:6339") %>%
  dplyr::select(-link)


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

ParkingMeters.sf <-
  ParkingMeters.sf%>%
  dplyr::select(StationID,STREET_BLOCK,POST_ID,SESSION_START_DT,SESSION_END_DT,GROSS_PAID_AMT,interval60,interval15,week,dotw,ParkTime,
                ParkTimeMin2,GROSS_PAID_AMT2,ParkingRate,Neighborhoods,COLLECTION_SUBROUTE,LONGITUDE2,LATITUDE2)
# Exploratory
ParkingMeters.sf.nogeometry <-st_drop_geometry(ParkingMeters.sf)
ggplot(ParkingMeters.sf.nogeometry %>%
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="Parking Sessions per Hour. San Francisco, May, 2018",
       x="Date", 
       y="Number of Parking Sessions")+
  plotTheme()

ParkingMeters.sf.nogeometry %>%
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
  labs(title="Mean Number of Hourly Parking Sessions Per Block. San Frrancisco, May, 2018",
       x="Number of parking sessions", 
       y="Frequency")+
  facet_wrap(~time_of_day)+
  plotTheme()

ggplot(ParkingMeters.sf.nogeometry %>%
         group_by(interval60, StationID) %>%
         tally())+
  geom_histogram(aes(n), binwidth = 5)+
  labs(title="Parking Sessions per hr by block. San Francisco, May, 2018",
       x="Number of Parking Sessions", 
       y="Number of Blocks")+
  plotTheme()
ParkingMeters.sf.nogeometry%>%
ggplot(ParkingMeters.sf.nogeometry %>% mutate(hour = hour(SESSION_END_DT)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Parking Sessions in San Francisco, by day of the week, May, 2018",
       x="Hour", 
       y="Parking Session Count")+
  plotTheme()

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
  top="Weather Data - San Francisco SFO - March-April, 2018")
                      
#join Census
ParkingMeters.sf <-
  ParkingMeters.sf%>%
  st_transform('EPSG:6346')
sfCensus <-
  sfCensus %>%
  st_transform('EPSG:6346')
ParkingMeter_census <- st_join(ParkingMeters.sf,sfCensus, join = st_within)

#not working at all not sure
ParkingMeter_censusPLOT <- subset(ParkingMeter_census, dotw %in% c("Mon"))
ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform("EPSG:6339"), colour = '#efefef')+
  geom_point(data = ParkingMeter_censusPLOT %>% 
               mutate(hour = hour(SESSION_END_DT),
                      time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                              hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                              hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                              hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
               group_by(StationID, LATITUDE2, LONGITUDE2,  time_of_day) %>%
               tally(),
             aes(x=LONGITUDE2, y = LONGITUDE2, color = n), 
             fill = "transparent", alpha = 0.4, size = 1.5)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  ylim(min(ParkingMeter_censusPLOT$LATITUDE2), max(ParkingMeter_censusPLOT$LATITUDE2))+
  xlim(min(ParkingMeter_censusPLOT$LONGITUDE2), max(ParkingMeter_censusPLOT$LONGITUDE2))+
  facet_grid(~ time_of_day)+
  labs(title="Bike share trips per hr by station,\nSF+Alameda, March-April, 2018")+
  mapTheme()



#study panel stuff



nrow(study.panel)  

study.panel <- 
  expand.grid(interval60 = unique(ParkingMeters$interval60), 
              StationID = unique(ParkingMeters$StationID))

ride.panel <- 
  ParkingMeters.sf %>%
  mutate(Trip_Counter = 1) %>%
  right_join(study.panel) %>% 
  group_by(interval60, StationID) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label = TRUE))
ride.panel <- st_drop_geometry(ride.panel)

latlong <- ParkingMeters.sf%>%
  dplyr::select(StationID, LATITUDE2,LONGITUDE2)
latlong<- st_drop_geometry(latlong)

latlong <- latlong[!duplicated(latlong$StationID), ]
ride.panel <- merge(ride.panel, latlong, by='StationID')


#join 

ride.panel <- ride.panel[!(ride.panel$LATITUDE2 == ""), ]
ride.panel<- ride.panel[!(ride.panel$LONGITUDE2 == ""), ]
ride.panel <- st_as_sf(x = ride.panel, coords = c("LONGITUDE2","LATITUDE2"),crs = "+proj=longlat +crs = 'EPSG:6339'")
ride.panel<- st_transform(ride.panel,"EPSG:6339")

#lags
ride.panel <- 
  ride.panel %>% 
  arrange(StationID, interval60) %>% 
  mutate(lagHour = dplyr::lag(Trip_Count,1),
         lag2Hours = dplyr::lag(Trip_Count,2),
         lag3Hours = dplyr::lag(Trip_Count,3),
         lag4Hours = dplyr::lag(Trip_Count,4),
         lag12Hours = dplyr::lag(Trip_Count,12),
         lag1day = dplyr::lag(Trip_Count,24))

ride.panel <- 
  left_join(ride.panel, sfCensus %>%
              as.data.frame() %>%
              select(-geometry), by = c("Origin.Tract" = "GEOID"))

##Exposure Features 

### Parks
parks_al <- 
  read.socrata("https://data.oaklandca.gov/resource/kq8i-6bzk.json") %>%
  dplyr::select(Y = location_1.latitude, X = location_1.longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102241') %>%
  mutate(Legend = "Park")

parks_sf <-
  st_read("C:/Users/agarw/Documents/MUSA508/MUSA508-Assignment3/Data/Recreation_and_Parks_Properties.csv") %>%
  dplyr::select(Y = Latitude, X = Longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102241') %>%
  mutate(Legend = "Park")
#help :(
as.data.frame(ride.panel) %>%
  group_by(interval60) %>% 
  summarise_at(vars(starts_with("lag"), "Trip_Count"), mean, na.rm = TRUE) %>%
  gather(Variable, Value, -interval60, -Trip_Count) %>%
  mutate(Variable = factor(Variable, levels=c("lagHour","lag2Hours","lag3Hours","lag4Hours",
                                              "lag12Hours","lag1day")))%>%
  group_by(Variable) %>%  
  summarize(correlation = round(cor(Value, Trip_Count),2))

as.data.frame(ride.panel) %>%
  group_by(interval60) %>% 
  summarise_at(vars("lagHour",  "lag2Hours", "lag3Hours", "lag4Hours","lag12Hours","lag1day","Trip_Count"), mean, na.rm = TRUE) %>%
  gather(Variable, Value, -interval60, -Trip_Count) %>%
  mutate(Variable = factor(Variable, levels=c("lagHour","lag2Hours","lag3Hours","lag4Hours",
                                              "lag12Hours","lag1day")))%>%
  group_by(Variable) %>%  
  summarize(correlation = round(cor(Value, Trip_Count),2))


ride.panel$dotw2 <-as.character(ride.panel$dotw)

#split training test
ride.Train <- subset(ride.panel, dotw %in% c("Mon","Tue","Wed"))
ride.Test <- subset(ride.panel, dotw %in% c("Thu","Fri"))

#Parking session by week
st_drop_geometry(rbind(
  mutate(ride.Train, Legend = "Training"), 
  mutate(ride.Test, Legend = "Testing"))) %>%
  group_by(Legend, interval60) %>% 
  summarize(Trip_Count = sum(Trip_Count)) %>%
  ungroup() %>% 
  ggplot(aes(interval60, Trip_Count, colour = Legend)) + geom_line() +
  scale_colour_manual(values = palette2) +
  labs(title="Parking Sessions by week: May 2018", 
       x="Day", y="Session Count") +
  plotTheme() + theme(panel.grid.major = element_blank())    

plotData.lag <-
  filter(as.data.frame(ride.panel)) %>%
  dplyr::select(starts_with("lag"), Trip_Count) %>%
  gather(Variable, Value, -Trip_Count) %>%
  mutate(Variable = fct_relevel(Variable, "lagHour","lag2Hours","lag3Hours",
                                "lag4Hours","lag12Hours","lag1day"))
correlation.lag <-
  group_by(plotData.lag, Variable) %>%
  summarize(correlation = round(cor(Value, Trip_Count, use = "complete.obs"), 2)) 

ggplot(plotData.lag, aes(Value, Trip_Count)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.lag, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Parking Session Count as Function of time lags") +
  plotTheme()

#Models
reg1 <- lm(Trip_Count ~  hour(interval60) + dotw,  data=ride.Train)

reg2 <- lm(Trip_Count ~  StationID + dotw,  data=ride.Train)

reg3 <- lm(Trip_Count ~  StationID + hour(interval60) + dotw , 
           data=ride.Train)

reg4 <- lm(Trip_Count ~  StationID +  hour(interval60) + dotw +
             lagHour + lag2Hours + lag3Hours + lag12Hours + lag1day, 
           data=ride.Train)



ride.Test.dotwNest <- 
  ride.Test %>%
  nest(-dotw) 

ride.Test.weekNest