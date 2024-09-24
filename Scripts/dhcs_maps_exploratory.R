#Reggie Ferrell
#Project - Mapping DCHS data 
#Load packages
library("leaflet")
library("dplyr")
library("tidyverse")
library("rgdal") 
library("sf") 
library("htmltools")
library("janitor")
library("readxl")
library("reshape2")
library("RColorBrewer")
library("viridis")     
library("tidycensus")
library("htmlwidgets")

packages <- c("leaflet","dplyr","tidyverse","rgdal" ,"sf" ,"htmltools","janitor",
  "readxl","reshape2","RColorBrewer","viridis","tidycensus","htmlwidgets")
invisible(lapply(packages,install.packages,character.only=TRUE)) #Invisible() = Don't print

#Reggie Ferrell
#Project - CYBHI Dataset (2021-2022)

#Load packages
packages <- c("haven", "ggplot2", "gapminder", "tidyverse", "dplyr", "stringr", 
              "tidyr", "Cairo", "devtools", "RODBC", "RColorBrewer", "foreign", "knitr", "markdown", 
              "rmarkdown", "tinytex", "kableExtra", "stargazer", "xtable", "readxl", "tidyr", "reshape2",
              "lubridate", "viridis", "haven", "janitor", "wesanderson", "cowplot", "forcats", "ggrepel", 
              "hrbrthemes", "ggalt", "scales", "psych", "corrplot", "gtools", "gapminder", "sf",
              "tigris", "censusapi","tmap", "tidycensus", "mapview","ggmap","lattice","leafpop",
              "maps","spData","magick","readxl","writexl","vroom","WriteXLS",
              "leaflet","dplyr","tidyverse","rgdal" ,"sf" ,"htmltools","janitor",
              "readxl","reshape2","RColorBrewer","viridis","tidycensus","htmlwidgets")
invisible(lapply(packages, library, character.only = TRUE))


my_api <- census_api_key("6b6805b560df241745c776e79e67fb6abe362db3", install = T, overwrite = T) #Install API key
census_api <- census_api_key("6b6805b560df241745c776e79e67fb6abe362db3", install = T, overwrite = T) #Install API key

#Import Source Data file from George's file
source <- read_xlsx("/Users/rferrel/Documents/DHCS/Inputs/LEA BOP Enrollment and Demographics Pivot_Tables_v.1.1.xlsx", sheet = "Source Data") %>% 
  clean_names() %>% 
  rename(nces_id = nces_district_id) %>%
  mutate(nces_id = paste0("0",nces_id))

counties_shp <- readOGR("/Users/rferrel/Documents/DHCS/Inputs/tl_2021_us_county/tl_2021_us_county.shp") #CA county boundaries  
california_counties_sf <- st_as_sf(counties_shp) %>% filter(STATEFP=="06") #Turn shapefile to object
california_counties_shp <- as(california_counties_sf, Class= "Spatial") #Return to shapefile 


schDistricts_shp <- readOGR("/Users/rferrel/Documents/DHCS/Inputs/tl_2021_06_unsd/tl_2021_06_unsd.shp")
california_schooldistricts_sf <- st_as_sf(schDistricts_shp) %>% rename(LEA = UNSDLEA) #Unified
california_schooldistricts_shp <- as(california_schooldistricts_sf, Class = "Spatial")

secondary_join <- tigris::school_districts(state = "CA", type = "secondary") %>% rename(LEA = SCSDLEA)
elementary_join <- tigris::school_districts(state = "CA", type = "elementary") %>% rename(LEA = ELSDLEA)
new_join <- rbind(secondary_join,elementary_join, california_schooldistricts_sf)

#Import ELSI data
elsi_CA <- read_excel("/Users/rferrel/Documents/DHCS/Inputs/ELSI_CDE_Districts.xlsx") %>% clean_names() %>%
  dplyr::select(agency_name, state_name_district_2020_21,
                county_name_district_2020_21,agency_id_nces_assigned_district_latest_available_year,
                location_address_1_district_2020_21, location_city_district_2020_21,
                location_zip_district_2020_21, locale_district_2020_21, latitude_district_2020_21,
                longitude_district_2020_21, agency_type_district_2020_21) %>%
  rename(state = state_name_district_2020_21,address = location_address_1_district_2020_21,
         county = county_name_district_2020_21,city = location_city_district_2020_21,
         zip = location_zip_district_2020_21,lon = longitude_district_2020_21,
         lat = latitude_district_2020_21,district = agency_name,
         nces_id = agency_id_nces_assigned_district_latest_available_year)

# #Join Files
# joined <- right_join(x=source,y=elsi_CA,
#                     by=c("nces_id"))
joined <- left_join(
  new_join,
  source,
  by = c("GEOID" = "nces_id" )
)


#Limit DF
limited <- joined %>% 
  dplyr::select(nces_id, npi,district.x,lat,lon,
                enr_status_code,teachers,schools,city,zip,
                county.x, locale, lec, type_nces, type_cde,
                total_interim_2020, flag, tier, frpm, frpmshare,
                el, elshare, avgbilling_cde,avgbilling_nces,billingstatus_cde,
                norecentbilling) %>% rename(district=district.x,
                                            county = county.x)

billing <- limited %>% 
  dplyr::select(nces_id,district,county,city,zip,lat,lon,enr_status_code,locale,type_nces,type_cde, avgbilling_cde, avgbilling_nces,billingstatus_cde,norecentbilling)

demographics <- limited %>% 
  dplyr::select(nces_id,district,county,city,zip,lat,lon,enr_status_code,locale,type_nces,type_cde,frpm,frpmshare,el,elshare)

districts <- limited %>% 
  dplyr::select(nces_id,district,county,city,zip,lat,lon,enr_status_code,locale,type_nces,type_cde,lec,teachers, schools) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  filter(enr_status_code != "NA") # Change this to be not enrolled 
         
bins <- c(.1,.2,.3,.4,.5)
# pal <- colorBin("Set1",domain = districts$enr_status_code, bins = bins)

pal_districts <- colorFactor("Set1",domain = districts$enr_status_code)
locale_districts <- colorFactor("Set1",domain = districts$locale)


#Enrollment Status
enr_status_map <- leaflet(joined) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_schooldistricts_sf,
              weight = 1,
              fillColor = "green",
              color = "black",
              smoothFactor = .25,
              highlightOptions = highlightOptions(color="black", weight=5, bringToFront = TRUE),
              label = ~ NAME) %>%
  addCircleMarkers(lng = ~lon, 
                   lat = ~lat,
                   color = "#641E16",weight = 1,
                   fillColor = ~pal_districts(enr_status_code),
                   radius = 8,stroke = FALSE, fillOpacity = 0.5,
                   group = joined$enr_status_code,
                   label = lapply(ca_no$label, HTML)) %>%
  addLayersControl(overlayGroups = c(joined$enr_status_code),
                   options = layersControlOptions(collapsed = FALSE))
enr_status_map

  
#Locale Map
locale_map <- leaflet(districts) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_counties_shp,
              weight = 1,
              fillColor = "green",
              color = "black",
              smoothFactor = .25,
              highlightOptions = highlightOptions(color="black", weight=5, bringToFront = TRUE),
              label = ~ NAME) %>%
  addCircleMarkers(lng = ~lon, 
                   lat = ~lat,
                   color = "#641E16",weight = 1,
                   fillColor = ~locale_districts(locale),
                   radius = 8,stroke = FALSE, fillOpacity = 0.5,
                   group = districts$locale) %>%
  addLayersControl(overlayGroups = c(districts$enr_status_code,districts$locale),
                   options = layersControlOptions(collapsed = FALSE))
locale_map

################ Teachers and Schools  
districts <- districts %>% dplyr::select(district,nces_id,lat,lon,teachers,schools,locale) %>%
  mutate(teachers = as.numeric(teachers),
         schools = as.numeric(schools)) %>% na.omit()

teachers_bins <- c(0,25,50,100,500,1000,1500,25000)
teachers_pal <- colorBin("Spectral",domain = districts$teachers, bins = teachers_bins,reverse = F)

teachers_labels <- paste("<p>","County:"," ", california_counties_sf$NAME, "</p>",
                      "<p>", "Number of Teachers:"," ", source$teachers, "</p>",
                      sep = "")


teachers_choropleth <- leaflet(districts) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_counties_shp,
              weight = 1,
              smoothFactor = 1,
              color = "white",
              fillOpacity = .6,
              fillColor = teachers_pal(districts$teachers),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(teachers_labels, HTML)) %>% 
addLegend(pal = teachers_pal,
          values  = districts$teachers,
          position = "bottomright",
          title = "Teachers Count",
          labFormat = labelFormat(digits=1))%>%
  addMarkers(~lon, ~lat, popup = districts$locale, label = districts$district,
             group = districts$locale) %>%
  addLayersControl(overlayGroups = districts$locale,
                   options = layersControlOptions(collapsed = TRUE))
teachers_choropleth

#Schools

schools_bins <- c(0,10,20,30,50,100,500,1000)
schools_pal <- colorBin("Spectral",domain = districts$schools, bins = schools_bins,reverse = T)

schools_labels <- paste("<p>","County:"," ",california_counties_sf$NAME, "</p>",
                         "<p>", "Number of schools:"," ",source$schools, "</p>",
                         sep = "")

schools_choropleth <- leaflet(districts) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  # addPolygons(data = california_schooldistricts_shp,
  addPolygons(data = schDistricts_shp,
              weight = 0,
              smoothFactor = 1,
              color = "white",
              fillOpacity = .6,
              fillColor = schools_pal(districts$schools),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(schools_labels, HTML)) %>% 
  addLegend(pal = schools_pal,
            values  = districts$schools,
            position = "bottomright",
            title = "Schools Count",
            labFormat = labelFormat(digits=1))%>%
  addMarkers(~lon, ~lat, popup = districts$locale, label = districts$district,
             group = districts$locale) %>%
  addLayersControl(overlayGroups = districts$locale,
                   options = layersControlOptions(collapsed = TRUE))
schools_choropleth
  
saveWidget(schools_choropleth, file="testing.html")

  