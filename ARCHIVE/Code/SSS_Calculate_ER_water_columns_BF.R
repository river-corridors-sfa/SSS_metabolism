# RC2 spatial study
# calculate ERwc
# X Lin Oct 30 2020
rm(list=ls(all=TRUE))
library(xlsx)
library(tidyverse)
#setwd('//pnl/projects/SBR_SFA/RC2/04_Spatial_Study/02_MantaRiver/03_ProcessedData/')
#setwd('C:/Users/linx882/OneDrive - PNNL/XLin/automation of respiration calculations/RC_2/sss_data_processing/codes')
#source('./MRhelper.R')
sdir<- 'C:/Brieanne/GitHub/SSS_metabolism/'
# findir<- 'C:/Users/forb086/Downloads/v3_SSS_Data_Package/'
findir<- "C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/03_MinidotManualChamber2hr/03_ProcessedData/MinidotManualChamber_Summary_Statistics.csv"
##########################################################################
## DO slope
# DO_summary<-read_csv(file.path(findir,'miniDOTManualChamber/Plots_and_Summary_Statistics','SSS_Water_DO_Temp_Summary.csv'), comment='#', na = c('N/A', -9999))
DO_summary<-read_csv(findir, comment='#', na = c('N/A', -9999))
Do_slope<-DO_summary%>%select(Date,Parent_ID,Site_ID,Dissolved_Oxygen_1_Slope,
                              Dissolved_Oxygen_2_Slope,Dissolved_Oxygen_3_Slope,
                              Dissolved_Oxygen_1_NRMSE,Dissolved_Oxygen_2_NRMSE,Dissolved_Oxygen_3_NRMSE)

for (i in c(1:3)){
  k = which(Do_slope[,paste0('Dissolved_Oxygen_',i,'_NRMSE')]>0.03)
  Do_slope[k,paste0('Dissolved_Oxygen_',i,'_Slope')]<-NA
}

## mean_slope: milligrams_per_liter_per_minute(mg/L/min <-->g/m3/min)
Do_slope['DO_Slope_mean']<-rowMeans(Do_slope[,grep('Slope',names(Do_slope))],na.rm = TRUE) 
# mean depth
mean_depth<- read_csv("C:/Users/forb086/Downloads/v2_SSS_Data_Package/v2_SSS_Water_Depth_Summary.csv", comment = '#')
mean_depth$mean_reach_depth_m <- mean_depth$Average_Depth/100 
#
Do_slope<- merge(Do_slope,mean_depth[c("Site_ID","mean_reach_depth_m")],by=c('Site_ID'))
# calculate ER water 
Do_slope['ERwaterdaily_gO2/m2day'] <-Do_slope$DO_Slope_mean*(60*24*60)*Do_slope$mean_reach_depth_m
ERwater<-Do_slope[c("Site_ID",'ERwaterdaily_gO2/m2day')]

ERwater <- ERwater %>%
  mutate(`ERwaterdaily_gO2/m2day` = case_when(`ERwaterdaily_gO2/m2day` > 0 ~ 0,
                                              TRUE ~ `ERwaterdaily_gO2/m2day`))
#########################
ERtotal <- read_csv(file.path(sdir,'/Stream_Metabolizer/Outputs/v2_SSS_combined_SM_results.csv'), na = '-9999')
# names(ERwater)[1]<-'SiteID'
ERtotal<-merge(ERtotal,ERwater,by=c('Site_ID'))
##
#calculate ER sed
ERtotal['ERseddaily_gO2/m2day']<-ERtotal$`ERdailymeanmean_gO2/m2day`-ERtotal$`ERwaterdaily_gO2/m2day`

#subset to needed columns, rename, and round
final_data <- ERtotal %>%
  select("Site_ID","Parent_ID", "ERdailymeanmean_gO2/m2day", "ERwaterdaily_gO2/m2day","ERseddaily_gO2/m2day","GPPdailymeanmean_gO2/m2day") %>%
  rename(Total_Ecosystem_Respiration_Square = "ERdailymeanmean_gO2/m2day",
         Water_Column_Respiration_Square = "ERwaterdaily_gO2/m2day",
         Sediment_Respiration_Square = "ERseddaily_gO2/m2day",
         Gross_Primary_Production_Square = "GPPdailymeanmean_gO2/m2day") %>%
  mutate(Total_Ecosystem_Respiration_Square = round(Total_Ecosystem_Respiration_Square, 2),
         Water_Column_Respiration_Square = signif(Water_Column_Respiration_Square, 3),
         Sediment_Respiration_Square = round(Sediment_Respiration_Square, 2),
         Gross_Primary_Production_Square = round(Gross_Primary_Production_Square, 2))

header <- tibble(x1 = c('# HeaderRows_7',
                   '# HeaderRows_Format: Column_Header; Unit; InstallationMethod_ID; Instrument_Summary',
                   '# Total_Ecosystem_Respiration_Square; grams_O2_per_square_meter_per_day; N/A; Estimated via streamMetabolizer model.',
                   '# Water_Column_Respiration_Square; grams_O2_per_square_meter_per_day; ManualChamberDark_02; PME MiniDOT Logger with optical dissolved oxygen sensor (fluorescence quenching).',
                   '# Sediment_Respiration_Square; grams_O2_per_square_meter_per_day; N/A; Calculated using Ecosystem Respiration from the streamMetabolizer model and Water Column Respiration from PME MiniDOT logger manual chamber.',
                   '# Gross_Primary_Production_Square; grams_O2_per_square_meter_per_day; N/A; Estimated via streamMetabolizer model.')
)

write_csv(header,file.path(sdir,'Stream_Metabolizer/v2_SSS_Water_Sediment_Total_Respiration_GPP.csv'), append = F, col_names = F)

write_csv(final_data,file.path(sdir,'Stream_Metabolizer/v2_SSS_Water_Sediment_Total_Respiration_GPP.csv'), na = '-9999', append = T, col_names = T)
