#rm(list=ls());graphics.off()

# Install packages
# install.packages("cowplot"); install.packages("googleway");install.packages("ggplot2"); install.packages("ggrepel"); install.packages("ggspatial"); install.packages("libwgeom"); install.packages("sf");install.packages("rnaturalearth");install.packages("rnaturalearthdata")

#Load packages                 
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata") 
library("rgdal")
library("tidyverse")
library("ggspatial")
library(spData)
library(ggplot2)
library(ggspatial)
library(ggmap)
library(ggsn)
library(cowplot)

# Load shp file of the YRB
setwd("C:/Users/gara009/OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/VGC/Map/")
shp <- readOGR(dsn = paste0(getwd(),"/YRB_shp_files_from_KS/HUC4_Yakima_GCS_NA1983.shp"), stringsAsFactors = F)
github = "C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/SSS_metabolism/SM_analysis/"

# read rivers 
# ogrListLayers(paste0(getwd(),"/ECY_WAT_NHDWAMajor.gdb"))
rivers <- readOGR(dsn = paste0(getwd(),"/ECY_WAT_NHDWAMajor.gdb"),layer = "ECY_WAT_NHDWAMajor_NHDStreams", stringsAsFactors = F)
# # Changing projection to map the shp file with the YRB
rivers = spTransform(rivers, CRS("+proj=longlat +datum=NAD83 +no_defs"))

#yrb <- readOGR(dsn = paste0(getwd(),"/Yakima_River_Streams_NHD_HighRes/Yakima_River_Streams_NHD_HighRes.shp"), stringsAsFactors = F)
#yrb = spTransform(yrb, CRS("+proj=longlat +datum=NAD83 +no_defs"))


# Load in the data and mapping file 
data = read.csv(paste0(github,"combined_results_updated_040623.csv"))
names(data)[1] = "Site_ID"

# Extract only site name
#data = data %>% mutate(Site_Vial_ID = stringr::str_extract(Sample_ID,"SSS[0-9]{3}"))

# Read in metadata
mapping = read.csv("SSS Sample Team Metadata.csv")

common_crs <- 4326

df_map <- left_join(data, mapping, by = "Site_ID") %>% dplyr::select(Site_ID,Site_Vial_ID,ERdailymeanmean_gO2.m2day,ERwaterdaily_gO2.m2day,ERseddaily_gO2.m2day,Sediment_Latitude,Sediment_Longitude) 
# %>% st_as_sf(., coords = c("Sediment_Longitude", "Sediment_Latitude"), crs = common_crs)
point_size = 2

df_map = na.omit(df_map)
#create bounding box for us map
wa_bbox <- c(left = -121.7, bottom = 45.7, right = -119.15, top = 47.79)

map = ggmap(get_stamenmap(wa_bbox,maptype = "terrain-background", zoom = 6))+
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_polygon(data = rivers, aes(x = long, y = lat, group = group), colour = "lightblue", fill = NA)+
  geom_point(data = df_map, aes(x = as.numeric(Sediment_Longitude), y = as.numeric(Sediment_Latitude), color = ERdailymeanmean_gO2.m2day),size = 4)+
  scale_color_viridis_c(limits = c(min(df_map$ERdailymeanmean_gO2.m2day), signif(max(df_map$ERdailymeanmean_gO2.m2day),3)), option = "magma", direction = 1) + 
  labs(x = "", y = "", color = "Daily Mean ERtot") +
    ggsn::scalebar(x.min = -119.5, x.max = -120,y.min = 47.5, y.max = 49.1,dist = 25, transform = TRUE, model = "WGS84",dist_unit = 'km') 
png('Daily_Mean_ERtot.png')
north2(map, .55, .75, symbol = 10, scale = 0.1)
dev.off()


map1 = ggmap(get_stamenmap(wa_bbox,maptype = "terrain-background", zoom = 6))+
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_polygon(data = rivers, aes(x = long, y = lat, group = group), colour = "lightblue", fill = NA)+
  geom_point(data = df_map, aes(x = as.numeric(Sediment_Longitude), y = as.numeric(Sediment_Latitude), color = ERwaterdaily_gO2.m2day),size = 4)+
  scale_color_viridis_c(limits = c(min(df_map$ERwaterdaily_gO2.m2day), signif(max(df_map$ERwaterdaily_gO2.m2day),3)), option = "magma", direction = 1) + 
  labs(x = "", y = "", color = "Daily Mean ERwater") +
  ggsn::scalebar(x.min = -119.5, x.max = -120,y.min = 47.5, y.max = 49.1,dist = 25, transform = TRUE, model = "WGS84",dist_unit = 'km') 
png('Daily_Mean_ERwater.png')
north2(map1, .55, .75, symbol = 10, scale = 0.1)
dev.off()
 

map2 = ggmap(get_stamenmap(wa_bbox,maptype = "terrain-background", zoom = 6))+
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_polygon(data = rivers, aes(x = long, y = lat, group = group), colour = "lightblue", fill = NA)+
  geom_point(data = df_map, aes(x = as.numeric(Sediment_Longitude), y = as.numeric(Sediment_Latitude), color = ERseddaily_gO2.m2day),size = 4)+
  scale_color_viridis_c(limits = c(min(df_map$ERseddaily_gO2.m2day), signif(max(df_map$ERseddaily_gO2.m2day),3)), option = "magma", direction = 1) + 
  labs(x = "", y = "", color = "Daily Mean ERsed") +
  ggsn::scalebar(x.min = -119.5, x.max = -120,y.min = 47.5, y.max = 49.1,dist = 25, transform = TRUE, model = "WGS84",dist_unit = 'km') 
png('Daily_Mean_ERsed.png')
north2(map2, .55, .75, symbol = 10, scale = 0.1)
dev.off()



