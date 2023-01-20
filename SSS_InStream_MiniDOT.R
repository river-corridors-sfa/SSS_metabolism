# ==============================================================================
#
# Process MiniDot Data from the RC2 Second Spatial Study to go into a model to 
# get ecosystem repiration 
#
# Status: Complete
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 7 Sept 2022
#
# ==============================================================================

library(tidyverse)
library(lubridate)
library(crayon)
library(plotly)

# ================================= User inputs ================================

metadata <-
  read_csv(
    'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/06_Metadata/SSS_Metadata_Deploy_Sample_Retrieve_2022-09-09.csv', 
    na = c('N/A', -9999, 'NA')
  ) 


#miniDOT data in UNIX UTC/GMT, metadata in PST. This offset will be subtracted from the 
# miniDOT data to get PST but date/time within R will say UTC
timezone_offset <- 28800

site_IDs <- metadata$Site_ID

DO_offset_file <- 'Z:/RC2/01_Sensor_Calibration_and_Correction_Files/01_Minidot.bucket/05_MiniDotCorrectionFactors/minidot_correction_factors_2022-07-18.csv'
  
minidot_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/01_RawData'

# plot_out_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/04_Plots'

formatted_out_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/02_FormattedData'

processed_out_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/03_ProcessedData'


# ======================== select columns and format times =====================

metadata <- metadata %>%
  select(
    Site_ID,
    Site_Vial_ID,
    Deploy_Date,
    Sample_Date,
    Retrieve_Date,
    Deploy_Minidot_SN,
    Minidot_Time_Deployed_PST,
    Sample_Minidot_Time_Removed,
    Sample_MiniDot_Time_Redeployed_PST,
    Retrieve_Minidot_Time_Removed) %>%
  mutate(Deploy_Date = mdy(Deploy_Date),
         Sample_Date = mdy(Sample_Date),
         Retrieve_Date = mdy(Retrieve_Date),
         Minidot_Time_Deployed_PST = hms(Minidot_Time_Deployed_PST),
         Sample_Minidot_Time_Removed = hms(Sample_Minidot_Time_Removed),
         Sample_MiniDot_Time_Redeployed_PST = hms(Sample_MiniDot_Time_Redeployed_PST),
         Retrieve_Minidot_Time_Removed = hms(Retrieve_Minidot_Time_Removed)
         )

# ================================= combine data ===============================

all_minidot_files <-
  list.files(minidot_dir,
             full.names = T,
             recursive = T)

Q_values <- tibble(
  Site_ID = as.character(),
  Q_Value_Good = as.character()
)

for (site in site_IDs) {
  
  
  kit_metadata <- metadata %>%
    filter(Site_ID == site) 
  
  
  serial <- kit_metadata$Deploy_Minidot_SN
  

  
  # date_character <- as.character(date)
  
  # year <- unlist(str_split(date_character, '-')) [1]

  # ========================== calculate date/time =======================

  if(site == 'T02'){ 
    
    sample_date <- ymd('2022-08-12') 

    
  } else {

    sample_date <- ymd(kit_metadata$Sample_Date)
    
  }
  
  deploy_datetime <- as_datetime(paste(kit_metadata$Deploy_Date, kit_metadata$Minidot_Time_Deployed_PST))

  deploy_date <- ymd(kit_metadata$Deploy_Date)
  
  retrieve_date <- ymd(kit_metadata$Retrieve_Date)
  
  retrieve_datetime <- as_datetime(paste(kit_metadata$Retrieve_Date, kit_metadata$Retrieve_Minidot_Time_Removed))
  
  retrieve_datetime_eq <- retrieve_datetime - 7200

# =========================== continue combining data =========================
  
  minidot_data_files <- all_minidot_files[grepl(site, all_minidot_files) ]
  
  combined <-
    tibble(
      'Time (sec)' = as.numeric(),
      'BV (Volts)' = as.numeric(),
      'T (deg C)' = as.numeric(),
      'DO (mg/l)' = as.numeric(),
      'Q' = as.numeric()
    )
  
  sn <- paste('7450-', serial , sep = '')
  
  
  for (file in minidot_data_files) {
    temp <- read_csv(file, skip = 2) %>%
      rename(Q = `Q ()`)
    
    temp$Q <-  gsub('\\+', '', temp$Q)
      
    temp <- temp %>%
      mutate(Q = as.numeric(Q))
    
    combined <- combined %>%
      add_row(temp)
    
  }
  
combined <- combined %>%
  arrange(`Time (sec)`)
  
  # ========================== Apply DO correction factor ======================
  
  DO_offset <- read_csv(DO_offset_file) %>%
    filter(serial_number == sn) %>%
  arrange(test_date)%>%
  tail(n=1)
  
  combined_offset <- combined %>%
    mutate(DO_offset = combined$`DO (mg/l)` * DO_offset$mean_data_correction_factor)
  
   # ========================== check MiniDOT Q values =======================
  
  if (combined_offset$Q < 0.90) {
    
    Q_values <- Q_values %>%
      add_row(Site_ID = site,
              Q_Value_Good = 'No')
    
  } else {
    
    Q_values <- Q_values %>%
      add_row(Site_ID = site,
              Q_Value_Good = 'Yes')
  }
  

  # ============================== format full data set =========================
  
  formatted_full <- combined_offset %>%
    rename(
      Time_unix = "Time (sec)",
      BV_volt = "BV (Volts)",
      Temp_degC = "T (deg C)",
      DO_mg_l =  "DO_offset"
    ) %>%
    select(-'DO (mg/l)') %>%
    mutate(
      Time_PST = Time_unix - timezone_offset,
      Date_Time_GMT = as_datetime(Time_unix),
      Date_Time_PST = as_datetime(Time_PST),
      Temp_K = Temp_degC + 273.15,
      DO_sat = (DO_mg_l / (exp(
        -139.34411 + (1.575701 * 1e5 / Temp_K) - (6.642308 * 1e7 / (Temp_K ^ 2)) +
          (1.2438 * 1e10 / (Temp_K ^ 3)) - (8.621949 * 1e11 / (Temp_K ^ 4))
      ))) * 100,
      
    )
  
  formatted_full_out <- combined_offset %>%
    rename(
      Time_unix = "Time (sec)",
      BV_volt = "BV (Volts)",
      Temp_degC = "T (deg C)",
      DO_mg_l =  "DO_offset"
    ) %>%
    select(-'DO (mg/l)') %>%
    mutate(
      Time_PST = Time_unix - timezone_offset,
      Date_Time_GMT = as.character(as_datetime(Time_unix)),
      Date_Time_PST = as.character(as_datetime(Time_PST)),
      Temp_K = Temp_degC + 273.15,
      DO_sat = (DO_mg_l / (exp(
        -139.34411 + (1.575701 * 1e5 / Temp_K) - (6.642308 * 1e7 / (Temp_K ^ 2)) +
          (1.2438 * 1e10 / (Temp_K ^ 3)) - (8.621949 * 1e11 / (Temp_K ^ 4))
      ))) * 100,
      
    )
  
  write_csv(formatted_full_out, paste(formatted_out_dir,'/SSS_MiniDOT_', site, '_combined.csv', sep = ''))
  
  # ========================= filter to after deployed time ====================
  
  deployed_data <- formatted_full %>%
    filter(Date_Time_PST >= deploy_datetime)

  # ========= trim out deploy day, sample day, and a few hours of retrieve =====
  
  trim_data <- formatted_full %>%
    mutate(Date = date(Date_Time_PST)) %>%
    filter(Date > deploy_date,
           Date != sample_date,
           Date_Time_PST < retrieve_datetime_eq) %>%
    select(-Date)
  
  trim_data_out <- formatted_full_out %>%
    mutate(Date = date(Date_Time_PST)) %>%
    filter(Date > deploy_date,
           Date != sample_date,
           Date_Time_PST < retrieve_datetime_eq) %>%
    select(-Date)
  
  write_csv(trim_data_out, paste(processed_out_dir,'/SSS_MiniDOT_', site, '_trimmed.csv', sep = ''))


  
}
 