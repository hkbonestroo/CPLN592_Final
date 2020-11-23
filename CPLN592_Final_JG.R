
#SETUP

setwd("C:/Users/Hannah/Documents/Penn/Fall 2020/CPLN-592/Assignments/Final Project/CPLN592_Final")
setwd("C:/Users/green/OneDrive/Documents/GitHub/CPLN592_Final")
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

#load parking data

ParkingMeters.dat <- st_read("Mayparking.csv")
ParkingMeters <- subset(ParkingMeters.dat, Lot %in% "Street")
ParkingMeters<- ParkingMeters%>%
  dplyr::select(POST_ID,STREET_BLOCK, Address2, SESSION_START_DT, SESSION_END_DT, GROSS_PAID_AMT)
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
#join 

ParkingMeters.sf <- ParkingMeters.sf[!(ParkingMeters.sf$LATITUDE == ""), ]
ParkingMeters.sf <- ParkingMeters.sf[!(ParkingMeters.sf$LONGITUDE == ""), ]
ParkingMeters.sf <- st_as_sf(x = ParkingMeters.sf, coords = c("LONGITUDE","LATITUDE"),crs = "+proj=longlat +crs = 'EPSG:6339'")
ParkingMeters.sf<- st_transform(ParkingMeters.sf,"EPSG:6339")

#Import Census Information
#JG NOte: Problem with select function here?

SFCensus <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001"), 
          year = 2018, 
          state = "CA", 
          geometry = TRUE, 
          county=c("San Francisco County"),
          output = "wide")%>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E)%>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans,
         Med_Age,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport)


SFTracts <- 
  SFCensus %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  select(GEOID, geometry) %>% 
  st_sf


dat_census <- st_join(dat2 %>% 
                        filter(is.na(from_longitude) == FALSE &
                                 is.na(from_latitude) == FALSE &
                                 is.na(to_latitude) == FALSE &
                                 is.na(to_longitude) == FALSE) %>%
                        st_as_sf(., coords = c("from_longitude", "from_latitude"), crs = 4326),
                      SFTracts %>%
                        st_transform(crs=4326),
                      join=st_intersects,
                      left = TRUE) %>%
  rename(Origin.Tract = GEOID) %>%
  mutate(from_longitude = unlist(map(geometry, 1)),
         from_latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)%>%
  st_as_sf(., coords = c("to_longitude", "to_latitude"), crs = 4326) %>%
  st_join(., SFTracts %>%
            st_transform(crs=4326),
          join=st_intersects,
          left = TRUE) %>%
  rename(Destination.Tract = GEOID)  %>%
  mutate(to_longitude = unlist(map(geometry, 1)),
         to_latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)


#Import Weather Data

weather.Panel <- 
  riem_measures(station = "SFO", date_start = "2018-04-30", date_end = "2018-05-03") %>%
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

glimpse(weather.Panel)


grid.arrange(
  ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
    labs(title="Percipitation", x="Hour", y="Perecipitation"),
  ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
    labs(title="Wind Speed", x="Hour", y="Wind Speed"),
  ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
    labs(title="Temperature", x="Hour", y="Temperature"),
  top="Weather Data - Boston KBOS - May, 2018")






