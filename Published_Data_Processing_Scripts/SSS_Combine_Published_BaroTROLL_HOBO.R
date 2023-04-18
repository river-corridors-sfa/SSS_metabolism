# ==============================================================================
#
# Combine barotroll and hobo data from SSS published data package 
# (https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1969566)
#
# Status: In progress
# ==============================================================================
#
# Author: Brieanne Forbes
# 18 April 2023
#
# ==============================================================================

library(tidyverse)

# ================================= User inputs ================================

data_dir <- 'C:/Users/Forb086/Downloads/SSS_Data_Package/'

outdir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/1 - SSS_BaroTROLL_HOBO_Combined/'

# ========================== get baro and hobo files =======================

baro_files <- list.files(str_c(data_dir, 'BarotrollAtm/Data/'), full.names = T)

hobo_files <- list.files(str_c(data_dir, 'DepthHOBO/Data/'), full.names = T)

# ========================== combine baro and hobo files =======================

for (baro in baro_files) {
  
  baro_data <- read_csv(baro, skip = 5) %>%
    rename(Baro_Pressure = Pressure, 
           Baro_Air_Temperature = Air_Temperature)
  
  parent_id <- unique(baro_data$Parent_ID)
  
  hobo <- hobo_files[grepl(parent_id,hobo_files)]
  
  hobo_data <- read_csv(hobo, skip = 5)%>%
    rename(HOBO_Absolute_Pressure = Absolute_Pressure, 
           HOBO_Temperature = Temperature)
  
  combine <- baro_data %>%
    full_join(hobo_data)
  
  out_file <- str_c(outdir, parent_id, '_HOBO_BaroTROLL_Combined.csv')
  
  write_csv(combine, out_file)
  
}

