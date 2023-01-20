# ==============================================================================
#
# Combine all metadata from RC2 Second Spatial Study (SSS)
#
# Status: In progress.
#
# Next step: fix column names
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 7 Sept 2022
#
# ==============================================================================

library(tidyverse)
library(gsheet)

# ================================= User inputs ================================

deploy <- construct_download_url('https://docs.google.com/spreadsheets/d/1HxMGw2h2qgGyxPghPELRRBUICYhNMsOidWLHFdgHDFY/edit?usp=sharing') %>%
  read_csv()
  
sample <- construct_download_url('https://docs.google.com/spreadsheets/d/1nr261SoW_ntcb3pptgLhAXT3p4AckCsZAy5TCm6Jr-U/edit?usp=sharing') %>%
  read_csv() %>%
  select(-Time_Zone) 
  
retrieve <- construct_download_url('https://docs.google.com/spreadsheets/d/1-f4NH5-M3ru607wH49UMlrCQQ_HZFPeAvZdbKVWKRm8/edit?usp=sharing') %>%
  read_csv() %>%
  select(-Time_Zone) 

out_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/06_Metadata/'
  
# ================================== combine  ==================================

deploy_sample <- deploy%>%
  left_join(sample, by = 'Site_ID') %>%
  select(-Timestamp.x, -Timestamp.y) %>%
  rename(Deploy_Date = Date.x,
         Deploy_Time_Arriving = Start_Time,
         Deploy_Time_Leaving = End_Time,
         Deploy_Latitude = Latitude.x,
         Deploy_Longitude = Longitude.x,
         Deploy_GPS_Accuracy_ft = GPS_Accuracy_ft.x,
         Deploy_Field_Crew = Field_Crew.x,
         Minidot_Time_Deployed_PST = Minidot_Start_Time_PST,
         Deploy_Minidot_Notes = Minidot_Notes,
         Deploy_Wiper_Notes = Wiper_Notes,
         Deploy_Cotton_Strip_Notes = Cotton_Strip_Notes,
         Deploy_Atmospheric_Sensor_Notes = Atmospheric_Sensor_Notes,
         Deploy_Depth_Hobo_Notes = Depth_Hobo_Notes,
         Deploy_Notes = Notes.x,
         Sample_Date = Date.y,
         Sample_Time_Arriving_PST = Time_Arriving_PST,
         Sample_Time_Leaving_PST = Time_Leaving_PST,
         Sample_Field_Crew = Field_Crew.y,
         Sample_Latitude = Latitude.y,
         Sample_Longitude = Longitude.y,
         Sample_GPS_Accuracy_ft  = GPS_Accuracy_ft.y,
         Sample_Notes = Notes.y)

combine_retrieve <- deploy_sample %>%
  left_join(retrieve, by = 'Site_ID') %>%
  select(-Timestamp) %>%
  rename(Retrieve_Date = Date,
         Retrieve_Time_Arriving = Start_Time,
         Retrieve_Time_Leaving = End_Time,
         Retrieve_Latitude = Latitude, 
         Retrieve_Longitude = Longitude, 
         Retrieve_GPS_Accuracy_ft =GPS_Accuracy_ft, 
         Retrieve_Field_Crew=Field_Crew,
         Sample_Minidot_Time_Removed = MiniDot_Time_Removed_PST,
         Sample_MiniDot_Time_Redeployed_PST = MiniDot_Time_Redeployed_PST,
         Retrieve_Minidot_Time_Removed = Minidot_End_Time_PST,
         Retrieve_Minidot_Notes = Minidot_Notes,
         Retrieve_Minidot_Notes = Minidot_Notes,
         Retrieve_Wiper_Notes = Wiper_Notes,
         Retrieve_Cotton_Strip_Notes = Cotton_Strip_Notes,
         Retrieve_Atmospheric_Sensor_Notes = Atmospheric_Sensor_Notes,
         Retrieve_Notes = Notes,
         Retrieve_Depth_Hobo_Notes = Depth_Hobo_Notes,
         Deploy_Minidot_SN = Minidot_SN.x,
         Deploy_Wiper_SN = Wiper_SN.x,
         Deploy_Depth_Hobo_SN = Depth_Hobo_SN.x,
         Deploy_Atmospheric_Sensor_SN = Atmospheric_Sensor_SN.x,
         Retrieve_Minidot_SN = Minidot_SN.y,
         Retrieve_Wiper_SN = Wiper_SN.y,
         Retrieve_Depth_Hobo_SN = Depth_Hobo_SN.y,
         Retrieve_Atmospheric_Sensor_SN = Atmospheric_Sensor_SN.y,
         
  )


out_name <- paste(out_dir, 'SSS_Metadata_Deploy_Sample_Retrieve_', Sys.Date(), '.csv', sep = '')
 
write_csv(combine_retrieve, out_name)
 

 



