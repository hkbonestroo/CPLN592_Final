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
ParkingMeters.sf$LONGITUDE2 <-ParkingMeters.sf$LONGITUDE
ParkingMeters.sf$LATITUDE2 <- ParkingMeters.sf$LATITUDE

#join 

ParkingMeters.sf <- ParkingMeters.sf[!(ParkingMeters.sf$LATITUDE == ""), ]
ParkingMeters.sf <- ParkingMeters.sf[!(ParkingMeters.sf$LONGITUDE == ""), ]
ParkingMeters.sf <- st_as_sf(x = ParkingMeters.sf, coords = c("LONGITUDE","LATITUDE"),crs = "+proj=longlat +crs = 'EPSG:6339'")
ParkingMeters.sf<- st_transform(ParkingMeters.sf,"EPSG:6339")

ParkingMeters.sf <-
  ParkingMeters.sf%>%
  dplyr::select(StationID,STREET_BLOCK,POST_ID,Address2,SESSION_START_DT,SESSION_END_DT,GROSS_PAID_AMT,interval60,interval15,week,dotw,ParkTime,
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

ggplot(ParkingMeters.sf.nogeometry %>% mutate(hour = hour(SESSION_END_DT)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Parking Sessions in San Francisco, by day of the week, May, 2018",
       x="Hour", 
       y="Parking Session Count")+
  plotTheme()

ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs='EPSG:6339'))+
  geom_point(data = ParkingMeters.sf %>% 
               mutate(hour = hour(SESSION_END_DT),
                      time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                              hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                              hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                              hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
               group_by(StationID,time_of_day) %>%
               tally(),
             aes(x=LONGITUDE2, y = LATITUDE2, color = n), 
             fill = "transparent", alpha = 0.4, size = 0.3)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  ylim(min(ParkingMeters.sf$LATITUDE2), max(ParkingMeters.sf$LATITUDE2))+
  xlim(min(ParkingMeters.sf$LONGITUDE2), max(ParkingMeters.sf$LONGITUDE2))+
  labs(title="Bike share trips per hr by station. Chicago, May, 2018")+
  mapTheme()
