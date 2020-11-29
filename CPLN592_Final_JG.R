
#Create fishnet for sf

#sf_Boundary_geometry

sf_bound <- 
  read.socrata("https://data.sfgov.org/api/views/wamw-vt4s/rows.json?accessType=DOWNLOAD") %>%
  filter(county == "San Francisco") %>%
  st_as_sf(coords = c("the_geom", crs = 4326, agr = "constant")) %>%
  st_transform('ESRI:102241') 


spdf <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")

sf

foodretail <-
  read.socrata("https://data.sfgov.org/resource/g8m3-pdis.json") %>%
  filter(naic_code_description == "Food Services")
  dplyr::select(location.coordinates) %>%
  na.omit() %>%
  st_as_sf(coords = c("location.coordinates"), crs = 4326, agr = "constant") %>%
  st_transform('ESRI:102241') %>%
  mutate(Legend = "Food Services")  
  
  
  
  

  
  