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

# Load shp file of the YRB
setwd("C:/Users/gara009/OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/VGC/Map/")
shp <- readOGR(dsn = paste0(getwd(),"/YRB_shp_files_from_KS/HUC4_Yakima_GCS_NA1983.shp"), stringsAsFactors = F)

# read rivers 
ogrListLayers(paste0(getwd(),"/ECY_WAT_NHDWAMajor.gdb"))

rivers <- readOGR(dsn = paste0(getwd(),"/ECY_WAT_NHDWAMajor.gdb"),layer = "ECY_WAT_NHDWAMajor_NHDStreams", stringsAsFactors = F)

# Changing projection to map the shp file with the YRB
rivers = spTransform(rivers, CRS("+proj=longlat +datum=NAD83 +no_defs"))

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Load in the data and mapping file 
data = read.csv("SM_for_RC2_mar_2023_for_maps.csv")
names(data)[1] = "Site_ID"

# Extract only site name
#data = data %>% mutate(Site_Vial_ID = stringr::str_extract(Sample_ID,"SSS[0-9]{3}"))

# Read in metadata
mapping = read.csv("SSS Sample Team Metadata.csv")

common_crs <- 4326

df_map <- left_join(data, mapping, by = "Site_ID") %>% dplyr::select(Site_ID,Site_Vial_ID,K600dailymeanmean_m.day,ERdailymeanmean_gO2.m3day,GPPdailymeanmean_gO2.m3day,DO_Slope_mean_Narm_g.m3day,Ersed.percent,Sediment_Latitude,Sediment_Longitude) %>% st_as_sf(., coords = c("Sediment_Longitude", "Sediment_Latitude"), crs = common_crs)
point_size = 2

df_map = na.omit(df_map)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_polygon(data = rivers, aes(x = long, y = lat, group = group), colour = "lightblue", fill = NA)+
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_sf(data = df_map, 
          aes_string(color = df_map$ERdailymeanmean_gO2.m3day), size = point_size * 2.5) +
  coord_sf(xlim = c(-121.7, -119.15), ylim = c(45.7, 47.79), expand = FALSE)+
  theme(legend.background = element_rect(fill = alpha("white", 0.0)), legend.key = element_rect(fill = "transparent")) + 
 # scale_color_gradientn(colours = heat.colors, limits = c(0.5, 4)) +
  scale_color_viridis_c(limits = c(min(df_map$ERdailymeanmean_gO2.m3day), signif(max(df_map$ERdailymeanmean_gO2.m3day),3)), option = "magma", direction = 1) + 
  labs(x = "", y = "", color = "Mean ERtot") +
  annotation_scale(location = "tr", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),style = north_arrow_fancy_orienteering) + theme(axis.text = element_text(size = 8)) 
ggsave("SSS_ERtot.png",width = 6, height = 6, dpi = 300)
