# ==============================================================================
#
# Make maps for ERsed manuscript 
#
# Status: 
#
# ==============================================================================
#
# Author: Brieanne Forbes 
# 6 June 2023
#
# ==============================================================================
library(tidyverse) #keep it tidy
library(raster) # work with rasters, NOTE: masks dplyr::select
library(janitor) # clean_names()
library(ggthemes) # theme_map()
library(ggsflabel) # add labels to sf objects
library(ggnewscale) # set multiple color scales
library(ggspatial) # add north arrow and scale bar
library(nhdplusTools) # get watershed boundary/flowlines
library(elevatr) # pull elevation maps
library(sf) # tidy spatial
library(spData)
library(cowplot)

rm(list=ls(all=T))

# ================================= User inputs ================================

metadata_file <- './v2_RCSFA_Geospatial_Data_Package/v2_RCSFA_Geospatial_Site_Information.csv'

data_file <- './SSS_Ecosystem_Respiration_Data_Package/SSS_Water_Sediment_Total_Respiration.csv'

shp_dir <- './Maps/YakimaRiverBasin_Boundary'

common_crs = 4326

# ============================== read in and merge =============================

metadata <- read_csv(metadata_file) %>%
  dplyr::select(Site_ID, Latitude, Longitude)

data <- read_csv(data_file, skip = 8)

merge <- data %>%
  left_join(metadata)
  
# ============================ read in YRB shp file ============================

YRB_shp <- list.files(shp_dir, 'shp', full.names = T)

YRB_boundary <- read_sf(YRB_shp) %>%
  st_transform(common_crs)

# ============================ convert to sf object ============================

sites <- st_as_sf(merge, coords = c('Longitude','Latitude'), crs = common_crs)

# ======================== pull NHD data and elevation =========================

# YRB_huc10 <- get_huc(AOI = YRB_boundary$geometry,
#                      type = "huc10")
# 
# YRB_huc12 <- get_huc(AOI = YRB_boundary$geometry,
#                      type = "huc12")

YRB_flowlines <- get_nhdplus(AOI = YRB_boundary$geometry, streamorder = 3)

elevation_raw <- get_elev_raster(YRB_boundary$geometry, z = 10)

elevation_crop <- mask(elevation_raw, YRB_boundary)

elevation <- as.data.frame(elevation_crop, xy = T) %>% 
  as_tibble() %>% 
  rename("long" = x, 
         "lat" = y, 
         "elevation" = 3) %>% #column index > name (changing resolution changes colname)
  filter(!is.na(elevation))


# ================================== create map ================================

data("us_states", package = "spData")
us_states_4326 = st_transform(us_states, crs = 4326)

wa <- us_states_4326 %>% filter(NAME == "Washington")

insert <- ggplot() +
  geom_sf(data = us_states_4326, fill = "white") + 
  geom_sf(data = wa, fill = "black",colour = "black")+
  geom_sf(data = YRB_boundary, colour = "red", fill = 'red') +
  labs(x = "", y = "")+
  theme_map()

site_map <- ggplot()+
  geom_sf(data = YRB_boundary)+
  geom_raster(data = elevation, aes(long, lat, fill = elevation), show.legend = T, alpha = 0.4)+
  scale_fill_gradient(low = 'white', high = 'black')+
  geom_sf(data = YRB_flowlines, color = "royalblue", alpha = 0.8)+
  geom_sf(data = sites, size = 1.5, color = 'black', show.legend = F) +
  theme_map() + 
  labs(x = "", y = "", fill = "Elevation (m)") + 
  ggspatial::annotation_scale(
    location = "br",
    pad_x = unit(0.5, "in"), 
    bar_cols = c("black", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(1.1, "in"), pad_y = unit(0.5, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black", "white"),
      line_col = "grey20"))

full <- ggdraw() +
  draw_plot(site_map) +
  draw_plot(insert, x = 0.4, y = 0.4, width = 0.4, height = 0.4)

ggsave('C:/Brieanne/Maps/test.pdf',
       full,
       width = 6,
       height = 5
)








