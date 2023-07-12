# ==============================================================================
#
# combine hobo data 
#
# Status: complete
# ==============================================================================
#
# Author: Brieanne Forbes
# 26 Sept 2022
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# ================================= User inputs ================================

hobo_files_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/09_HOBO/02_FormattedData/'


hobo_files <- list.files(hobo_files_dir, 
                         '.csv',
                         full.names = T)


# ============================== combine hobo files ============================

for (site in metadata$Site_ID) {
  
  
  hobo_site_files <- hobo_files[grepl(site, hobo_files)]
  hobo_site_files <- hobo_site_files[!grepl('Combined', hobo_site_files)]
  
  hobo_combine <- tibble('Date_Time' = as.character(),
                         'Temperature_degC' = as.numeric(),
                         'Absolute_Pressure_mbar' = as.numeric())
  
  for (hobo_site_file in hobo_site_files) {
    
    temp <- read_csv(hobo_site_file) %>%
      rename(Date_Time = 1,
             Temperature_degC = 2,
             Absolute_Pressure_mbar = 3)
    
    hobo_combine <- hobo_combine %>%
      add_row(temp)
    
  }
  
  hobo_combined_out_dir <- paste(hobo_files_dir, 'SSS_', site, '_HOBO_Combined.csv', sep = '')
  
  write_csv(hobo_combine, hobo_combined_out_dir)
  
}
