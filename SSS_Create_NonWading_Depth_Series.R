# ==============================================================================
#
# Use difference in hobo data to create time series from transect depth measurements
#
# Status: In progress
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 18 October 2022
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# ================================= User inputs ================================

nonwading_summary_file <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_non_wading_Depth_Summary.csv'

hobo_files_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/2 - SSS_BaroTROLL_HOBO_Trimmed'

outdir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/3 - SSS_Baro_Hobo_Depth/'

# ================================= read in data ===============================

nonwading_summary <- read_csv(nonwading_summary_file)%>%
  mutate(Date = mdy(Date),
         date_time = as_datetime(paste(Date, Start_Time)))

hobo_files <- list.files(hobo_files_dir, 'HOBO', full.names = T)

nonwading_sites <- nonwading_summary$Site_ID

# =========================== loop through sites ===============================

for (site in nonwading_sites) {
  
  hobo <- hobo_files[grepl(site ,hobo_files)]
  
  hobo_data <- read_csv(hobo)
  
  site_nonwading <- nonwading_summary %>%
    filter(Site_ID == site)
  
  if (site == 'T02'){
    
    site_nonwading$date_time <- as_datetime('2022-07-26 00:00:00 UTC')
    
  } else if (site == 'S58'){
    
    site_nonwading$date_time <- as_datetime('2022-07-27 00:00:00 UTC')
    
  } else if (site == 'T41'){
    
    site_nonwading$date_time <- as_datetime('2022-07-27 00:00:00 UTC')
    
  } else if (site == 'T03'){
    
    site_nonwading$date_time <- as_datetime('2022-07-26 00:00:00 UTC')
    
  } else if (site == 'S57'){
    
    site_nonwading$date_time <- as_datetime('2022-07-27 00:00:00 UTC')
    
  } else if (site == 'T07'){
    
    site_nonwading$date_time <- as_datetime('2022-08-08 10:00:00 UTC')
    
  }
  
  
  hobo_data <- hobo_data %>%
    mutate(compensated_hobo_water_pressure_mbar = HOBO_Absolute_Pressure_mbar - BaroTROLL_Barometric_Pressure_mBar,
           density_kg_per_m3 = (999.84847 + (0.06337563 * HOBO_Temperature_degC) - (0.008523829 * HOBO_Temperature_degC^2) + (0.0000694324 * HOBO_Temperature_degC^3) - (0.0000003821216 * HOBO_Temperature_degC^4)),
           depth_from_pressure_m = (compensated_hobo_water_pressure_mbar*100)/(9.80  * density_kg_per_m3),
           Date_Time = as_datetime(Date_Time))
  
  hobo_data_reference_depth_m <- hobo_data %>%
    filter(Date_Time <= site_nonwading$date_time + 450 & Date_Time >= site_nonwading$date_time - 450) %>%
    select(depth_from_pressure_m) %>%
    pull(1)
  
  hobo_data <- hobo_data %>%
    mutate(offset_cm = (depth_from_pressure_m - hobo_data_reference_depth_m) * 100,
           time_series_average_depth_cm = site_nonwading$Average_Depth_cm + offset_cm) %>%
    add_column(Site_ID = site, .before = 'Date_Time')
  
  
  # ================================= write file===============================
  
  out_file <- paste0(outdir, 'SSS_', site, '_Baro_HOBO_Depth.csv')
  
  write_csv(hobo_data, out_file)
  
  
}
