# ==============================================================================
#
# combine barotroll and hobo data and trim off sample day, retrieve day, and two 
# hours of retrieve 
#
# Status: In progress. Need to trim data next, figure out how to deal with files with baro and files without
# ==============================================================================
#
# Author: Brieanne Forbes
# 26 Sept 2022
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# ================================= User inputs ================================

hobo_files_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/09_HOBO/03_ProcessedData/'


hobo_files <- list.files(hobo_files_dir, 
                   '.csv',
                   full.names = T)

baro_files <- list.files('C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/08_AtmosphericPressure/02_FormattedData/', 
                   full.names = T)

metadata <- read_csv(
  'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/06_Metadata/SSS_Metadata_Deploy_Sample_Retrieve_2022-11-21.csv', 
  na = c('N/A', -9999, 'NA')
) 

combined_outdir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/1 - SSS_BaroTROLL_HOBO_Combined/'

trimmed_outdir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/2 - SSS_BaroTROLL_HOBO_Trimmed/'

# ========================== combine baro and hobo files =======================

for (baro in baro_files) {
  
  baro_data <- read_csv(baro) %>%
    mutate(Date = mdy(Date),
           Date_Time = as_datetime(paste(Date, Time))) 
  
  if ('Barometric Pressure (mmHg)' %in% colnames(baro_data)) {
    
    baro_data <- baro_data %>% 
      rename(Barometric_Pressure_mmHg = 'Barometric Pressure (mmHg)') %>%
      mutate(Barometric_Pressure_mBar = Barometric_Pressure_mmHg * 1.333) %>%
      select(-Barometric_Pressure_mmHg,
             "Temperature_degC" = "Temperature (C)")
    
    
  } else {
  
    baro_data <- baro_data %>%
    rename("Barometric_Pressure_mBar" = "Barometric Pressure (mBar)",
           "Temperature_degC" = "Temperature (C)")
    
  }
  
  site <- unlist(str_split(baro, '_'))[4]

  hobo_match_file <- hobo_files[grepl(site, hobo_files)]
  
 
  
  if (length(hobo_match_file) == 1){
    
   baro_hobo <- read_csv(hobo_match_file) %>%
     mutate(Date_Time = as_datetime(Date_Time)) %>%
     left_join(baro_data, by = 'Date_Time') %>%
     rename(HOBO_Temperature_degC = Temperature_degC.x,
            BaroTROLL_Temperature_degC = Temperature_degC.y,
            HOBO_Absolute_Pressure_mbar = Absolute_Pressure_mbar,
            HOBO_Absolute_Pressure_Adjust_mbar = Absolute_Pressure_Adjust_mbar,
            BaroTROLL_Barometric_Pressure_mBar = Barometric_Pressure_mBar) %>%
     select(-Date, -Time, -Minutes) %>%
     mutate(Date_Time = as.character(Date_Time))
    
    
  }
  
  write_csv(baro_hobo, paste(combined_outdir, 'SSS_', site, '_HOBO_BaroTROLL_Combined.csv', sep = ''))
  
}


# =================================== Trim data ================================

combined_files <- list.files(combined_outdir, full.names = T, pattern = '.csv')

for (file in combined_files) {
  
  data <- read_csv(file)
  
  site <- unlist(str_split(file, '_'))[7]
  
  site_metadata <- metadata %>%
    filter(Site_ID == site)
  
  # ========================== get dates =======================
  
  if(site == 'T02'){ 
    
    sample_date <- ymd('2022-08-12') 
    
    
  } else {
    
    sample_date <- ymd(site_metadata$Sample_Date)
    
  }
  
  # deploy_datetime <- as_datetime(paste(mdy(site_metadata$Deploy_Date), site_metadata$Minidot_Time_Deployed_PST))
  
  deploy_date <- ymd(site_metadata$Deploy_Date)
  
  retrieve_date <- ymd(site_metadata$Retrieve_Date)
  
  retrieve_datetime <- as_datetime(paste(ymd(site_metadata$Retrieve_Date), site_metadata$Retrieve_Minidot_Time_Removed))
  
  retrieve_datetime_eq <- retrieve_datetime - 7200
  
  # ===========================================================
  
  trim_data <- data %>%
    mutate(Date = date(Date_Time),
           Flag = ifelse(Date == sample_date, 'Sample_Day', NA)) %>%
    filter(Date > deploy_date,
           Date_Time < retrieve_datetime_eq) %>%
    select(-Date) %>%
    mutate(Date_Time = as.character(Date_Time))
  
  file_name <- basename(file)
  
  if(str_detect(file_name, 'BaroTROLL') == TRUE){
    
    write_csv(trim_data, paste(trimmed_outdir, 'SSS_', site, '_HOBO_BaroTROLL_Trimmed.csv', sep = ''))
    
  } else{
  
  write_csv(trim_data, paste(trimmed_outdir, 'SSS_', site, '_HOBO_Trimmed.csv', sep = ''))
  
  }
  
  if('Absolute_Pressure_Adjust_mbar' %in% colnames(trim_data)){
  
  plot <- ggplot(data = trim_data, aes(x = as_datetime(Date_Time), y = Absolute_Pressure_Adjust_mbar))+
    geom_point(aes(color = Flag), shape = 1) + 
    labs(x = 'Date', y = 'Adjusted Pressure (mbar)')+
    theme_classic()+
    ggtitle(site)+
    theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
  } else{
    
    plot <- ggplot(data = trim_data, aes(x = as_datetime(Date_Time), y = HOBO_Absolute_Pressure_Adjust_mbar))+
      geom_point(aes(color = Flag), shape = 1) + 
      labs(x = 'Date', y = 'Adjusted Pressure (mbar)')+
      theme_classic()+
      ggtitle(site)+
      theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
    
  }
  
  plot_outfile <- paste0(trimmed_outdir, 'SSS_HOBO_Trimmed_Plots/SSS_', site,'_HOBO_Trimmed_Plot.pdf' )
  
  ggsave(plot_outfile,
         plot,
         device = 'pdf',
         width = 15,
         height = 5,
         units = 'in',
         dpi = 300
         )
    
}



