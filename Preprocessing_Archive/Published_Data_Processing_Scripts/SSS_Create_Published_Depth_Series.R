# ==============================================================================
#
# Use difference in hobo data to create time series from average depth using 
# SSS published data package 
# (https://data.ess-dive.lbl.gov/datasets/doi:10.15485/1969566)
#
#
# Status: In progress
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 19 April 2023
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# ================================= User inputs ================================

summary_file <- 'C:/Users/forb086/Downloads/SSS_Data_Package/SSS_Water_Depth_Summary.csv'
  
data_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/1 - SSS_BaroTROLL_HOBO_Combined'

outdir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/3 - SSS_Baro_Hobo_Depth/'

# ================================= read in data ===============================

depth <- read_csv(summary_file, skip = 8)%>%
  mutate(Date = ymd(Date),
         DateTime = as_datetime(paste(Date, Start_Time)))

data_files <- list.files(data_dir, '.csv', full.names = T)

# =========================== loop through sites ===============================

for (data_file in data_files) {
  
  data <- read_csv(data_file)
  
  site <- unique(data$Site_ID)
  
  parent_id <- unique(data$Parent_ID)
  
  site_depth <- depth %>%
    filter(Site_ID == site)
  
  if (site == 'S10'){
    
    site_depth$DateTime <- as_datetime('2022-08-11 08:30:00 UTC')
    
  } else if (site == 'W10'){
    
    site_depth$DateTime <- as_datetime('2022-08-12 08:45:00 UTC')
    
  } else if (site == 'U20'){
    
    site_depth$DateTime <- as_datetime('2022-08-12 08:45:00 UTC')
    
  } else if (site == 'W20'){
    
    site_depth$DateTime <- as_datetime('2022-08-12 09:30:00 UTC')
    
  } else if (site == 'T02'){
    
    site_depth$DateTime <- as_datetime('2022-07-26 00:00:00 UTC')
    
  } else if (site == 'S58'){
    
    site_depth$DateTime <- as_datetime('2022-07-27 00:00:00 UTC')
    
  } else if (site == 'T41'){
    
    site_depth$DateTime <- as_datetime('2022-07-27 00:00:00 UTC')
    
  } else if (site == 'T03'){
    
    site_depth$DateTime <- as_datetime('2022-07-26 00:00:00 UTC')
    
  } else if (site == 'S57'){
    
    site_depth$DateTime <- as_datetime('2022-07-27 00:00:00 UTC')
    
  } else if (site == 'T07'){
    
    site_depth$DateTime <- as_datetime('2022-08-08 10:00:00 UTC')
    
  }

  
  data <- data %>%
    mutate(compensated_hobo_water_pressure_mbar = HOBO_Absolute_Pressure - Baro_Pressure,
      density_kg_per_m3 = (999.84847 + (0.06337563 * HOBO_Temperature) - (0.008523829 * HOBO_Temperature^2) + (0.0000694324 * HOBO_Temperature^3) - (0.0000003821216 * HOBO_Temperature^4)),
           depth_from_pressure_m = (compensated_hobo_water_pressure_mbar*100)/(9.80  * density_kg_per_m3),
           DateTime = as_datetime(DateTime))
  
  data_reference_depth_m <- data %>%
    filter(DateTime <= site_depth$DateTime + 450 & DateTime >= site_depth$DateTime - 450) %>%
    select(depth_from_pressure_m) %>%
    pull(1)
  
  data <- data %>%
    mutate(offset_cm = (depth_from_pressure_m - data_reference_depth_m) * 100,
           time_series_average_depth_cm = site_depth$Average_Depth + offset_cm,
           DateTime = str_c(" ",as.character(DateTime)))
  
  
  # ================================= write file===============================
  
  out_file <- paste0(outdir, parent_id, '_Baro_HOBO_Depth.csv')
  
  write_csv(data, out_file)
  
  rm(data)
  
  
}
