# ==============================================================================
#
# Rename field photos with date and site for SSS
#
# Status: Complete
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 16 November 2022
#
# ==============================================================================

library(tidyverse)
library(tools)

# ================================= User inputs ================================

photo_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/05_FieldPhotos/01_RawData/Grain_Size_Grid_Photos'

out_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/05_FieldPhotos/02_FormattedData/SSS_SedimentQuadratPhotos/'

study_code <- 'SSS'

# comment out line 55 for grid photos or line 54 for environmental context photos

# ============================== rename the images =============================

file_list <-  list.files(photo_dir, recursive = T, full.names = T)

# number_of_files <- tibble(
#   dir = dirname(file_list)) %>% 
#   count(dir) 



for (i in file_list){
  
  date <- unlist(str_split(i, '/')) [9]
  date <- gsub('-', '', as.character(date))
  site <- unlist(str_split(i, '/')) [10]
  extension <- file_ext(i)
  
  subfolder_files <- list.files(dirname(i), full.names = T)
  
  subfolder_count <- tibble(
    file = subfolder_files)
  
  subfolder_count <- subfolder_count  %>%
    add_column(number = 1:nrow(subfolder_count))
  
  subfolder <- dirname(i)
  
  temp <- subfolder_count %>%
    filter(file %in% i)
  
  out_file <- paste(out_dir, study_code, '_', site,'_', date,'_', temp$number,  '.jpg', sep = '')
  # out_file <- paste(out_dir, study_code, '_', site,'_', date,'_EnvContext_', temp$number,'.', extension , sep = '')
  
  file.copy(i, out_file)
}



