
#Create fishnet for sf

#sf_Boundary_geometry


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
  
  # Google maps API key AIzaSyAWq_0IELtG0z_e8n10iZKCVRLB-bZKc_A


  library(ggmap)
  library(data.table)
  
  
  register_google(key = "AIzaSyAWq_0IELtG0z_e8n10iZKCVRLB-bZKc_A")
  
  
  
  
  

  
  