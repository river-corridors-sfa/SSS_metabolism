# ==============================================================================
#
# Make figures for ERsed manuscript
#
# Status: In progress
#
# "HOBO_Temp",'Mean_Depth',"Slope","Velocity" ,"Discharge","TSS", 'TN','NPOC',
# 'D50_m',"hz_spring","Chlorophyll_A",'GPP_Square'
#  geospatial: "totdasqkm","PctFst","PctAg",'PctShrb2019Ws',"AridityWs" ,'streamorde'
# ==============================================================================
#
# Author: Brieanne Forbes 
# 8 January 2025
#
# ==============================================================================
library(tidyverse) 

rm(list=ls(all=T))

# Setting wd to parent folder
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")

# =================================== find files ===============================

er_gpp <- read_csv('./v2_SSS_Water_Sediment_Total_Respiration_GPP.csv',
                   comment = '#', na = '-9999') %>%
  select(Parent_ID, Site_ID, Sediment_Respiration, Gross_Primary_Production)

geospatial <- read_csv('https://github.com/river-corridors-sfa/Geospatial_variables/raw/refs/heads/main/v2_RCSFA_Extracted_Geospatial_Data_2023-06-21.csv') %>%
  select(site, totdasqkm, PctMxFst2019Ws,PctConif2019Ws,PctGrs2019Ws, PctShrb2019Ws, AridityWs) %>%
  filter(site %in% er_gpp$Site_ID)%>%
  mutate(PctFst = PctMxFst2019Ws + PctGrs2019Ws + PctMxFst2019Ws) %>%
  rename(Site_ID = site)%>%
  select(Site_ID, totdasqkm, PctFst, PctShrb2019Ws, AridityWs)

d50 <- read_csv('./v2_SSS_ER_d50_TotalOxygenConsumed.csv', 
                comment = '#', na = '-9999') %>%
  select(Parent_ID, D50_m)

slope_vel_dis <- read_csv('./Stream_Metabolizer/Inputs/v2_SSS_Slope_Discharge_Velocity.csv',
                          comment = '#', na = '-9999') %>%
  select(Site_ID, Slope, Discharge, Velocity)

tss <- read_csv('./Published_Data/v3_SSS_Data_Package/Sample_Data/SSS_Water_TSS.csv',
                skip = 2, na = c('', 'N/A', '-9999')) %>%
  filter(!is.na(Sample_Name)) %>%
  mutate(Parent_ID = str_extract(Sample_Name, "^.{1,6}"),
         '00530_TSS_mg_per_L' = case_when(str_detect(`00530_TSS_mg_per_L`, 'LOD') ~ 0.12, # replace below LOD values with half LOD (LOD = 0.24)
                                          TRUE ~ as.numeric(`00530_TSS_mg_per_L`)))%>%
  select(Parent_ID, contains('TSS')) 

npoc_tn <- read_csv('./Published_Data/v4_CM_SSS_Data_Package/Sample_Data/v3_CM_SSS_Water_NPOC_TN.csv',
                    skip = 2, na = c('', 'N/A', '-9999'))%>%
  filter(!is.na(Sample_Name),
         str_detect(Sample_Name, 'SSS')) %>%
  mutate(Parent_ID = str_extract(Sample_Name, "^.{1,6}"),
         '00602_TN_mg_per_L_as_N' = case_when(str_detect(`00602_TN_mg_per_L_as_N`, 'LOD') ~ 0.013, # replace below LOD values with half LOD (LOD = 0.026)
                                              str_detect(`00602_TN_mg_per_L_as_N`, 'Standard') ~ 0.05, # replace below standard values with half standard (standard = 0.1)
                                              TRUE ~ as.numeric(`00602_TN_mg_per_L_as_N`))) %>%
  select(Parent_ID, contains('NPOC'), contains('TN')) %>%
  group_by(Parent_ID) %>%
  summarise(Mean_00602_TN_mg_per_L_as_N = round(mean(`00602_TN_mg_per_L_as_N`), 2)) %>%
  ungroup()

depth <- read_csv('./Published_Data/v3_SSS_Data_Package/v3_SSS_Water_Depth_Summary.csv',
                  comment = '#', na = c('', 'N/A', '-9999')) %>%
  select(Parent_ID, Average_Depth)

hobo_temp <- read_csv('./Published_Data/v3_SSS_Data_Package/Sensor_Data/DepthHOBO/Plots_and_Summary_Statistics/v3_SSS_Water_Press_Temp_Summary.csv',
                      comment = '#', na = c('', 'N/A', '-9999')) %>%
  select(Parent_ID, Temperature_Mean)


# =============================== combine data ===============================

all_data <- er_gpp %>%
  full_join(geospatial, by = 'Site_ID')%>%
  full_join(slope_vel_dis, by = 'Site_ID')%>%
  full_join(d50, by = 'Parent_ID')%>%
  full_join(tss, by = 'Parent_ID')%>%
  full_join(npoc_tn, by = 'Parent_ID')%>%
  full_join(depth, by = 'Parent_ID')%>%
  full_join(hobo_temp, by = 'Parent_ID') %>%
  filter(!is.na(Sediment_Respiration))
  
  
  
# print correlation matrix of values; histograms of all data 
# need to figure out normalization 
# get Maggi's code for lasso
