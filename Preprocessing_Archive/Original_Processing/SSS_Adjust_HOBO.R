# ==============================================================================
#
# adjust SSS hobo data for any change in depth after replaced during sample week
#
# Status: In progress.
# ==============================================================================
#
# Author: Brieanne Forbes
# 21 Nov. 2022
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# ================================= User inputs ================================

hobo_files_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/09_HOBO/02_FormattedData/'

metadata <- read_csv(
  'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/06_Metadata/SSS_Metadata_Deploy_Sample_Retrieve_2022-11-21.csv', 
  na = c('N/A', -9999, 'NA')
) 

outdir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/09_HOBO/03_ProcessedData/'

# ================================= User inputs ================================

combined_files <- list.files(hobo_files_dir, 'Combined', full.names = T)

for (file in combined_files) {
  
  data <- read_csv(file)%>%
    mutate(Date_Time = as_datetime(Date_Time))
  
  site <- unlist(str_split(file, '_'))[4]
  
  site_metadata <- metadata %>%
    filter(Site_ID == site)
  
  
  if(site == 'T02'){ 
    
    sample_datetime_removed <- as_datetime('2022-08-12 11:45:00')
    sample_datetime_redeployed<- as_datetime('2022-08-12 11:55:00')
    # temp <- data %>% filter(str_detect(Date_Time,'2022-08-12'))
    # plot(as_datetime(temp$Date_Time), temp$Absolute_Pressure_mbar)
    
    
  # }else if(site == 'S01'){ 
  #   
  #   sample_datetime_removed <- as_datetime('2022-08-10 13:02:00')
  #   sample_datetime_redeployed<- as_datetime('2022-08-10 13:20:00')
  #   # temp <- data %>% filter(str_detect(Date_Time,'2022-08-12'))
  #   # plot(as_datetime(temp$Date_Time), temp$Absolute_Pressure_mbar)
    
    
  } else{
   
    sample_datetime_removed <- as_datetime(paste(ymd(site_metadata$Sample_Date), hms(site_metadata$HOBO_Time_Removed_PST)))
    
    sample_datetime_redeployed <- as_datetime(paste(ymd(site_metadata$Sample_Date), hms(site_metadata$HOBO_Time_Redeployed_PST)))
    
  }
  
  if(site == 'S08'){
    
    data <- data %>%
      filter(Date_Time < sample_datetime_removed | Date_Time > sample_datetime_redeployed + 2100)
    
    
  }else{
  
  
  data <- data %>%
    filter(Date_Time < sample_datetime_removed | Date_Time > sample_datetime_redeployed + 1800)
  
  }
  
  pressure_at_removed <- data %>%
    filter(Date_Time >= sample_datetime_removed - 900 & Date_Time <= sample_datetime_removed)%>%
    select(Absolute_Pressure_mbar)%>%
    distinct()%>%
    pull()
    
  pressure_at_redeployed <- data %>%
    filter(Date_Time <= sample_datetime_redeployed + 2700 & Date_Time >= sample_datetime_redeployed)%>%
    select(Absolute_Pressure_mbar)%>%
    pull(1)
  
  
  offset <- pressure_at_redeployed  - pressure_at_removed
  
  if(site=='W20'|site=='T07'){
    
    adjusted_hobo <- data %>%
      mutate(Absolute_Pressure_Adjust_mbar = Absolute_Pressure_mbar)
    
    
  } else{
  
  adjusted_hobo <- data %>%
    mutate(Absolute_Pressure_Adjust_mbar = ifelse(Date_Time>=sample_datetime_redeployed, Absolute_Pressure_mbar-offset, Absolute_Pressure_mbar))
  
  }
  
  out_file <- paste0(outdir, 'SSS_', site, '_HOBO_Adjusted.csv')
  
  write_csv(adjusted_hobo, out_file)
  
}
