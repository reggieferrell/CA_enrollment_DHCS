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


#Import Source Data file from George's file
source <- read_xlsx("/Users/rferrel/Documents/DHCS/Inputs/LEA BOP Enrollment and Demographics Pivot_Tables_v.1.1.xlsx", sheet = "Source Data") %>% 
  clean_names() %>% 
  rename(nces_id = nces_district_id,
         district_name = district) %>%
  mutate(nces_id = paste0("0",nces_id)) %>%
  mutate(CA_regions = "",
         CA_regions = ifelse(lec== "Sonoma" | lec == "Glenn" | lec == "Sutter","Far North",CA_regions),
         CA_regions = ifelse(lec== "Contra Costa" | lec == "Santa Cruz","Bay Area",CA_regions),
         CA_regions = ifelse(lec=="Stanislaus" | lec =="Madera" | lec== "Kern","Central/Mountain",CA_regions),
         CA_regions = ifelse(lec== "Orange" | lec == "San Bernardino" | lec =="LA USD" | lec == "Los Angeles COE","Southern California",CA_regions)) %>%
  mutate(elshare = as.numeric(elshare),                                                                                                                                                           frpmshare = as.numeric(frpmshare)) %>%
  mutate(elshare = round(elshare,2),
         frpmshare = round(frpmshare,2))

counties_shp <- readOGR("/Users/rferrel/Documents/DHCS/Inputs/tl_2021_us_county/tl_2021_us_county.shp") #CA county boundaries  
california_counties_sf <- st_as_sf(counties_shp) %>% filter(STATEFP=="06") #Turn shapefile to object
california_counties_shp <- as(california_counties_sf, Class= "Spatial") #Return to shapefile 


schDistricts_shp <- readOGR("/Users/rferrel/Documents/DHCS/Inputs/tl_2021_06_unsd/tl_2021_06_unsd.shp")
california_schooldistricts_sf <- st_as_sf(schDistricts_shp) 
california_schooldistricts_shp <- as(california_schooldistricts_sf, Class = "Spatial")

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
joined <- right_join(x=source,y=elsi_CA,
        by=c("nces_id")) %>%
    mutate(lat = as.numeric(lat),
           lon = as.numeric(lon)) %>%
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))

bins <- c(.1,.2,.3,.4,.5)

districts_col <- colorFactor("Set1",domain = joined$CA_regions)
# pal_districts <- colorFactor("Set1",domain = joined$enr_status_code)
# locale_districts <- colorFactor("Set1",domain = joined$locale)

#Limit the data
ca_bay <- joined %>% filter(CA_regions == "Bay Area") %>% 
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>",
         "<p>","% FRPM: ",frpmshare,"</p>",
         "<p>","% EL: ",elshare,"</p>",
         "<p>","School Type: ",type_cde,"</p>"))
ca_central <- joined %>% filter(CA_regions == "Central/Mountain") %>%  
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>",
        "<p>","% FRPM: ",frpmshare,"</p>",
        "<p>","% EL: ",elshare,"</p>",
        "<p>","School Type: ",type_cde,"</p>"))
ca_farnorth <- joined %>% filter(CA_regions == "Far North") %>%  
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>",
        "<p>","% FRPM: ",frpmshare,"</p>",
        "<p>","% EL: ",elshare,"</p>",
        "<p>","School Type: ",type_cde,"</p>"))
ca_south <- joined %>% filter(CA_regions == "Southern California") %>% 
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>",
        "<p>","% FRPM: ",frpmshare,"</p>",
        "<p>","% EL: ",elshare,"</p>",
        "<p>","School Type: ",type_cde,"</p>"))

#Enrollment data - Regions
enrollment_regions <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_counties_shp,
              weight = 1,
              fillColor = "grey",
              color = "black",
              smoothFactor = .25,
              highlightOptions = highlightOptions(color="black", weight=5, bringToFront = FALSE),
              label = ~ NAME) %>%
  addCircleMarkers(lng = ca_bay$lon, 
                   lat = ca_bay$lat,
                   color = "#641E16",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "Bay Area",
                   label = lapply(ca_bay$label, HTML)) %>%
  addCircleMarkers(lng = ca_central$lon, 
                   lat = ca_central$lat,
                   color = "#512E5F",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "Central/Mountain",
                   label = lapply(ca_central$label, HTML)) %>% 
  addCircleMarkers(lng = ca_farnorth$lon, 
                   lat = ca_farnorth$lat,
                   color = "#1B4F72",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "Far North",
                   label = lapply(ca_farnorth$label, HTML)) %>%
  addCircleMarkers(lng = ca_south$lon, 
                   lat = ca_south$lat,
                   color = "#0B5345",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "Southern California",
                   label = lapply(ca_south$label, HTML)) %>% 
  addLayersControl(overlayGroups = c("Bay Area","Central/Mountain","Far North","Southern California"),
                   options = layersControlOptions(collapsed = FALSE))
enrollment_regions

saveWidget(enrollment_regions, file="Enr_Regions.html")



#Enrollment data - Locale
ca_cityMid <- joined %>% filter(locale == "City: Midsize") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_citySmall <- joined %>% filter(locale == "City: Small") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_suburbLarge <- joined %>% filter(locale == "Suburb: Large") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_suburbSmall <- joined %>% filter(locale == "Suburb: Small") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_townFringe <- joined %>% filter(locale == "Town: Fringe") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_townDistant <- joined %>% filter(locale == "Town: Distant") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_townRemote <- joined %>% filter(locale == "Town: Remote") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_ruralFringe <- joined %>% filter(locale == "Rural: Fringe") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_ruralDistant <- joined %>% filter(locale == "Rural: Distant") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))
ca_ruralRemote <- joined %>% filter(locale == "Rural: Remote") %>% mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))

enrollment_locale <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_counties_shp,
              weight = 1,
              fillColor = "grey",
              color = "black",
              smoothFactor = .25,
              highlightOptions = highlightOptions(color="black", weight=5, bringToFront = FALSE),
              label = ~ NAME)  %>% 
  addCircleMarkers(lng = ca_cityMid$lon, 
                   lat = ca_cityMid$lat,
                   color = "#641E16",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "12-City: Mid-size",
                   label = lapply(ca_cityMid$label, HTML)) %>%
  addCircleMarkers(lng = ca_citySmall$lon, 
                   lat = ca_citySmall$lat,
                   color = "#512E5F",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "13-City: Small",
                   label = lapply(ca_citySmall$label, HTML)) %>% 
  addCircleMarkers(lng = ca_suburbLarge$lon, 
                   lat = ca_suburbLarge$lat,
                   color = "#1B4F72",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "21-Suburb: Large",
                   label = lapply(ca_suburbLarge$label, HTML)) %>%
  addCircleMarkers(lng = ca_suburbSmall$lon, 
                   lat = ca_suburbSmall$lat,
                   color = "#0B5345",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "23-Suburb: Small",
                   label = lapply(ca_suburbSmall$label, HTML)) %>% 
  addCircleMarkers(lng = ca_townFringe$lon, 
                   lat = ca_townFringe$lat,
                   color = "#F1C40F",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "31-Town: Fringe",
                   label = lapply(ca_townFringe$label, HTML)) %>%
  addCircleMarkers(lng = ca_townDistant$lon, 
                   lat = ca_townDistant$lat,
                   color = "#D35400",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "32-Town: Distant",
                   label = lapply(ca_townDistant$label, HTML)) %>% 
  addCircleMarkers(lng = ca_townRemote$lon, 
                   lat = ca_townRemote$lat,
                   color = "#DE3163",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "33-Town: Remote",
                   label = lapply(ca_townRemote$label, HTML)) %>%
  addCircleMarkers(lng = ca_ruralFringe$lon, 
                   lat = ca_ruralFringe$lat,
                   color = "#9FE2BF",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "41-Rural: Fringe",
                   label = lapply(ca_ruralFringe$label, HTML)) %>% 
  addCircleMarkers(lng = ca_ruralDistant$lon, 
                   lat = ca_ruralDistant$lat,
                   color = "#6495ED",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "42-Rural: Distant",
                   label = lapply(ca_ruralDistant$label, HTML)) %>%
  addCircleMarkers(lng = ca_ruralRemote$lon, 
                   lat = ca_ruralRemote$lat,
                   color = "#00a185",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "43-Rural: Remote",
                   label = lapply(ca_ruralRemote$label, HTML)) %>% 
  addLayersControl(overlayGroups = c("12-City: Mid-size","13-City: Small","21-Suburb: Large","23-Suburb: Small",
                                     "31-Town: Fringe","32-Town: Distant","33-Town: Remote","41-Rural: Fringe",
                                     "42-Rural: Distant","43-Rural: Remote"),
                   options = layersControlOptions(collapsed = FALSE))
enrollment_locale

saveWidget(enrollment_locale, file="Enr_Locale.html")


#Enrollment data - Enrolled
ca_yes <- joined %>% filter(enrolled == "Yes") %>% 
  # mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>")) %>%
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>",
                       "<p>","% FRPM: ",frpmshare,"</p>",
                       "<p>","% EL: ",elshare,"</p>",
                       "<p>","School Type: ",type_cde,"</p>",
                       "<p>", "Number of Schools: ", schools, "</p>"))

ca_no <- joined %>% filter(enrolled == "No") %>% 
  # mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>")) %>%
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>",
                       "<p>","% FRPM: ",frpmshare,"</p>",
                       "<p>","% EL: ",elshare,"</p>",
                       "<p>","School Type: ",type_cde,"</p>",
                "<p>", "Number of Schools: ", schools, "</p>"))


enrollment_status <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% #addProvider allows one to choose the proider of the basefile (Esri, Carto, etc.)
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_counties_shp,
              weight = 1,
              fillColor = "grey",
              color = "black",
              smoothFactor = .25,
              highlightOptions = highlightOptions(color="black", weight=5, bringToFront = FALSE),
              label = ~ NAME)  %>% 
  addCircleMarkers(lng = ca_yes$lon, 
                   lat = ca_yes$lat,
                   color = "#d55e00",weight = 1,
                   radius = 5,stroke = FALSE, fillOpacity = 0.5,
                   group = "Yes",
                   label = lapply(ca_yes$label, HTML)) %>%
  addCircleMarkers(lng = ca_no$lon, 
                   lat = ca_no$lat,
                   color = "#0072b2",weight = 1,
                   radius = 5, stroke = FALSE, fillOpacity = 0.5,
                   group = "No",
                   label = lapply(ca_no$label, HTML)) %>%
  addLayersControl(overlayGroups = c("Yes","No"),
                   options = layersControlOptions(collapsed = FALSE))
enrollment_status

saveWidget(enrollment_status, file="Enr_Status.html")

################  Choropleths
source_short <- joined %>% dplyr::select(district_name,nces_id,lat,lon,elshare,frpmshare,enr_nces) %>%
  mutate(elshare = as.numeric(elshare),
         frpmshare = as.numeric(frpmshare)) %>%
mutate(elshare = round(elshare,2),
         frpmshare = round(frpmshare,2)) %>%
mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))

source_el <- source_short %>% filter(!is.na(elshare)) %>% 
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total EL Share: ",elshare,"</p>"))
source_frpm <- source_short %>% filter(!is.na(frpmshare)) %>%
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total FRPM Share: ",frpmshare,"</p>"))
source_enr <- source_short %>% mutate(enr_nces = as.numeric(enr_nces)) %>% filter(!is.na(enr_nces)) %>%
  mutate(label = paste("<b>",district_name,"</b>","<p>","Total Enrollment: ",enr_nces,"</p>"))


choro_bins <- c(0,.25,.50,.75,100)
enr_bins <- c(0,500,1000,1500,5000,10000, 20000,30000)

el_pal <- colorBin("RdYlGn",domain = source_el$elshare, bins = choro_bins,reverse = T)
frpm_pal <- colorBin("YlGnBu",domain = source_short$frpmshare, bins = choro_bins,reverse = F)
enr_pal <- colorBin("Spectral",domain = source_enr$enr_nces, bins = enr_bins,reverse = F)

#FRPM
el_choropleth <- leaflet(source_el) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_schooldistricts_shp,
              weight = 1,
              smoothFactor = 1,
              color = "white",
              fillOpacity = .6,
              fillColor = el_pal(source_el$elshare),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(source_el$label, HTML)) %>% 
addLegend(pal = el_pal,
          values  = source_el$elshare,
          position = "bottomright",
          title = "Percentage of English Learner Students",
          labFormat = labelFormat(digits=1))
  # addMarkers(~lon, ~lat, popup = source_short$locale, label = source_short$district,
  #            group = source_short$locale) %>%
  # addLayersControl(overlayGroups = source_short$locale,
  #                  options = layersControlOptions(collapsed = TRUE))
el_choropleth

saveWidget(el_choropleth, file="EL_choropleth.html")

#FRPM
frpm_choropleth <- leaflet(source_frpm) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_schooldistricts_shp,
              weight = 1,
              smoothFactor = 1,
              color = "white",
              fillOpacity = .6,
              fillColor = frpm_pal(source_frpm$frpmshare),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(source_frpm$label, HTML)) %>% 
  addLegend(pal = frpm_pal,
            values  = source_frpm$frpmshare,
            position = "bottomright",
            title = "Percentage of Students Elgible for Free and Reduced Priced Meals",
            labFormat = labelFormat(digits=1))
frpm_choropleth

saveWidget(frpm_choropleth, file="FRPM_choropleth.html")


#Enrollment
enr_choropleth <- leaflet(source_enr) %>% 
  addProviderTiles(providers$OpenStreetMap.DE) %>% 
  setView(lng = -119.417931, lat = 36.778259, zoom=6) %>%
  addPolygons(data = california_schooldistricts_shp,
              weight = 1,
              smoothFactor = 1,
              color = "white",
              fillOpacity = .6,
              fillColor = enr_pal(source_enr$enr_nces),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(source_enr$label, HTML)) %>% 
  addLegend(pal = enr_pal,
            values  = source_enr$enr_nces,
            position = "topright",
            title = "Student Enrollment by District",
            labFormat = labelFormat(digits=1))
enr_choropleth

saveWidget(enr_choropleth, file="Enr_choropleth.html")

