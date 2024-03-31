# ==============================================================================
#
# Move and rename barotroll hobo files
#
# Status: In progress. 
# ==============================================================================
#
# Author: Brieanne Forbes
# 26 Sept 2022
#
# ==============================================================================

library(tidyverse)

# ================================= User inputs ================================

raw_data <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/08_AtmosphericPressure/01_RawData'

formatted_data <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/08_AtmosphericPressure/02_FormattedData/'

# ================================ move and rename ==============================

files <- list.files(raw_data, recursive = T, pattern = '.csv', full.names = T)

for (file in files) {
  
  date <- unlist(str_split(file, '/'))[8]
  
  site <- unlist(str_split(file, '/'))[9]
  
  new_dir <- paste(formatted_data, 'SSS_', site, '_', date, '_BaroTROLL.csv', sep = '')
  
  file.copy(file, new_dir)
  
}

