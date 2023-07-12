# ==============================================================================
#
# Calculate average reach depth (2 hours before data removed during sampling)
#
# Status: in progress
#
# ==============================================================================
#
# Author: Brieanne Forbes 
# 5 April 2023
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# =========================== User inputs ======================================

hobo_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/3 - SSS_Baro_HOBO_Depth/'

out_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/'

# ============================ get files =======================================

files <- list.files(hobo_dir, '.csv',full.names = T)

# ============= create empty tibble to combine all data ========================

combine <- tibble(Site_ID = as.character(),
                  mean_reach_depth_cm = as.numeric())

# ============ loop through each file to get average depth =====================

for (file in files) {
  
  data <- read_csv(file)
  
  if(unique(data$Site_ID) %in% c('T07', 'W20')){ 
    # these two sites do not have data before sampling week so the first 2 hours of data are pulled
    
    reach_data <- data %>%
      slice(1:9)
    
    
  } else {
    # finding where data was cut from sampling week and grabbing 2 hours of data before that  
  
  cutoff <- data %>%
    filter(Date_Time-lag(data$Date_Time)!=15) %>%
    select(Date_Time) %>%
    pull()
  
  reach_data <- data %>%
    filter(Date_Time < as_datetime(cutoff)) %>%
    arrange(desc(Date_Time)) %>%
    slice(1:9)
  
  }
  
  avg <- reach_data %>%
    summarise(Site_ID = unique(Site_ID),
              mean_reach_depth_cm = mean(time_series_average_depth_cm))
  
  combine <- combine %>% 
    add_row(avg)
  
}

out_file <- paste0(out_dir, 'SSS_Average_Reach_Depth_', Sys.Date(), ".csv")

write_csv(combine, out_file)
