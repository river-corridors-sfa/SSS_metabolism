rm(list=ls());graphics.off()
library("tidyverse")
library("ggspatial")

# Load shp file of the YRB

resp.path = "C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/SSS_metabolism/SM_analysis/rough_results_for_RC2_03012023/"
geo.path = "//pnl/projects/SBR_SFA/RC2/07_Geospatial_Data/"

data.geo = read.csv(paste0(geo.path,"RC2_all_stream_Attributes_updated.csv"))
colnames(data.geo) = gsub("site_ID","Site_ID",colnames(data.geo))
colnames(data.geo) = gsub("StreamOrde","StreamOrder",colnames(data.geo))

data.geo2 = read.csv(paste0(geo.path,"RC2_all_topographic_Attributes_updated.csv"))
data.geo2$Site_ID = data.geo2$site_ID
df = read.csv(paste0(resp.path,"SM_for_RC2_mar_2023_for_maps.csv"))
names(df)[1] = "Site_ID"

# merging data and selecting columns of interest
df_map <- full_join(data.geo,df, by = "Site_ID")
df_map <- full_join(data.geo2,df_map, by = "Site_ID")%>% dplyr::select(Site_ID,D50_m,StreamOrder,TOT_BASIN_AREA,ERdailymeanmean_gO2.m3day)
df_map = na.omit(df_map)

ggplot(df_map, aes(x=log10(D50_m), y=ERdailymeanmean_gO2.m3day)) + 
  geom_point()+ theme_bw()

ggplot(df_map, aes(x=log10(TOT_BASIN_AREA), y=ERdailymeanmean_gO2.m3day)) + 
  geom_point()+ theme_bw()

ggplot(df_map, aes(x=as.factor(StreamOrder), y=ERdailymeanmean_gO2.m3day)) + 
  geom_boxplot()+ theme_bw()
