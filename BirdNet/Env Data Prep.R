#Environmental Data Prep Libraries
library(rgdal)
library(raster)
library(units)
library(sf)
library(mapview)
sf_use_s2(FALSE)

#Metadata Prep
hrec_crs <- "+init=epsg:4326"
hrec_utm <- "+proj=utm +zone=10 ellps=WGS84"

HRECboundary <- readOGR("env_data/HREC_boundary/HREC_boundary.shp")
HRECboundary <- spTransform(HRECboundary, CRS(hrec_utm))

audiomoths <- read_csv("data/Operation Logs/audiomoth_oplog_phase123.csv") %>%
  dplyr::rename(Site_ID = Site)


audiomoths <- audiomoths %>%
  st_as_sf(coords = 9:8, crs = hrec_crs)

audiomoths <- st_transform(audiomoths, crs = hrec_utm)

#Environmental covariates
severity <- raster("env_data/dNBR Burn Severity HREC.tif")
severity <- projectRaster(severity, crs = hrec_utm)

canopy <- raster("env_data/Canopy_2020.tif")
canopy <- projectRaster(canopy, crs = hrec_utm)

elev <- raster("env_data/elev_500mbuff.tif")
elev <- projectRaster(elev, crs = hrec_utm)

veg <- raster("env_data/vegetation.coarser.clean2.tif")
veg <- projectRaster(veg, crs = hrec_utm)

streams <- st_read("env_data/streams/streams.shp")
streams <- st_transform(streams, crs = hrec_utm)
streams <- st_crop(streams, HRECboundary)

audiomoth_metadata <- audiomoths %>% 
  mutate(Severity = raster::extract(severity, as(., "Spatial"),
                                    buffer = 100, fun = mean),
         Elevation = raster::extract(elev, as(., "Spatial"),
                                     buffer = 100, fun = mean),
         Canopy = 1-raster::extract(canopy, as(., "Spatial"),
                                    buffer = 100, fun = mean),
         Pyrodiversity = raster::extract(severity, as(., "Spatial"),
                                         buffer = 100, fun = sd)
  )

audiomoth_metadata$DistWater <- st_distance(audiomoth_metadata, streams, by_element = TRUE)

audiomoth_metadata$DistWater <- drop_units(audiomoth_metadata$DistWater)

pct <- function(x){
  1 - sum(x)/length(x)
}

reclass_df <- c(1,1.5,1,
                1.5,2.5,2,
                2.5,Inf,3)
reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

veg <- reclassify(veg,
                  reclass_m)  

#1 = chaparral
#2 = Woodland
#3 = grassland

reclass_df <- c(0,1.5,1,
                1.5,Inf,0)

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = TRUE)

chap <- reclassify(veg,
                   reclass_m)

temp <- audiomoth_metadata %>%
  mutate(chap = 1-raster::extract(chap, as(., "Spatial"),
                                  buffer = 100, fun = pct)) %>%
  dplyr::select(.,Site_ID, chap)%>%
  as.data.frame()


reclass_df <- c(0,1.5,0,
                1.5,2.5,1,
                2.5,Inf,0)

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = TRUE)

wood <- reclassify(veg,
                   reclass_m)

temp2 <- audiomoth_metadata %>%
  mutate(wood = 1-raster::extract(wood, as(., "Spatial"),
                                  buffer = 100, fun = pct)) %>%
  dplyr::select(.,Site_ID, wood)%>%
  as.data.frame()



reclass_df <- c(0,2.5,0,
                2.5,Inf,1)

reclass_m <- matrix(reclass_df,
                    ncol = 3, 
                    byrow = TRUE)

grass <- reclassify(veg,
                    reclass_m)

temp3 <- audiomoth_metadata %>%
  mutate(grass = 1-raster::extract(grass, as(., "Spatial"),
                                   buffer = 100, fun = pct)) %>%
  dplyr::select(.,Site_ID, grass) %>%
  as.data.frame()

veg <- left_join(temp, temp2)
veg <- left_join(veg,temp3)

veg$geometry <- NULL

audiomoth_metadata <- left_join(audiomoth_metadata, veg)

#metadata_archive <- read_csv("data/metadata_archive.csv")

site_ids <- audiomoth_metadata$Site_ID
# metadata_archive <- metadata_archive %>%
#   filter(Site_ID %in% site_ids)

# audiomoth_metadata <- left_join(audiomoth_metadata, metadata_archive)

audiomoth_metadata$Start <- mdy(audiomoth_metadata$Start)
audiomoth_metadata$Year <- year(audiomoth_metadata$Start)

audiomoth_metadata <- audiomoth_metadata %>%
  mutate(Site = strtrim(Site_ID, 3))

audiomoth_metadata <- audiomoth_metadata %>%
  dplyr::select(Site_ID, Site, Year, no_days, Severity, Elevation, Canopy, DistWater, 
                chap, wood, grass, Pyrodiversity)

audiomoth_metadata$BurnLag2 <- NA
audiomoth_metadata$BurnLag3 <- NA
audiomoth_metadata$BurnLag4 <- NA

for(i in 1:nrow(audiomoth_metadata)) {
  
  if(audiomoth_metadata$Year[i] == 2020) {
    audiomoth_metadata$BurnLag2[i] = "1"
  }
  
  else(audiomoth_metadata$BurnLag2[i] = "0") 
}

for(i in 1:nrow(audiomoth_metadata)) {
  
  if(audiomoth_metadata$Year[i] == 2021) {
    audiomoth_metadata$BurnLag3[i] = "1"
  }
  
  else(audiomoth_metadata$BurnLag3[i] = "0") 
}

for(i in 1:nrow(audiomoth_metadata)) {
  
  if(audiomoth_metadata$Year[i] == 2022) {
    audiomoth_metadata$BurnLag4[i] = "1"
  }
  
  else(audiomoth_metadata$BurnLag4[i] = "0") 
}

audiomoth_metadata <- audiomoth_metadata %>% 
  dplyr::rename(Effort = no_days)


audiomoth_metadata$geometry <- NULL

write_csv(audiomoth_metadata, "data/audiomoth_metadata.csv")

am_oplog <- read_csv("data/Operation Logs/audiomoth_oplog_phase123AprJul.csv") %>%
  dplyr::rename(Site_ID = Site) %>%
  dplyr::select(Site_ID, Start)

am_oplog$Start <- mdy(am_oplog$Start)

audiomoth_metadata <- left_join(audiomoth_metadata, am_oplog)

audiomoth_metadata$FireDate <- ymd("2018-07-27") 

audiomoth_metadata <- audiomoth_metadata %>%
  mutate(LagTime = interval(FireDate, Start) / days(1))

#audiomoth_metadata <- read_csv("data/audiomoth_metadata.csv")

write_csv(audiomoth_metadata, "data/audiomoth_metadata.csv")

wind_2020 <- read.table("env_data/am_wind_2020.csv", sep=";", header=T)
wind_2021 <- read.table("env_data/am_wind_2021.csv", sep=";", header=T)
wind_2022 <- read.table("env_data/am_wind_2022.csv", sep=";", header=T)

wind <- rbind(wind_2020, wind_2021, wind_2022)
wind <- wind %>%
  dplyr::rename(Wind_Max = "Wind.Speed..MAX...mph.",
                Wind_Avg = "Wind.Speed..AVG...mph.")

wind$Date <- mdy(wind$Date)

write_csv(wind, "env_data/am_wind_log.csv")

temp_2020 <- read.table("env_data/am_temp_2020.csv", sep=",", header=T)
temp_2021 <- read.table("env_data/am_temp_2021.csv", sep=",", header=T)
temp_2022 <- read.table("env_data/am_temp_2022.csv", sep=",", header=T)
temp <- rbind(temp_2020, temp_2021, temp_2022)

temp <- temp %>%
  dplyr::rename(Temp_Avg = "Temp.Avg..F.",
                Temp_Max = "Temp.Max..F.",
                Temp_Min = "Temp.Min..F.")
temp$Date <- mdy(temp$Date)

write_csv(temp, "env_data/am_temp_log.csv")