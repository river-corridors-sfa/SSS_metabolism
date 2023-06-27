# RC2 spatial study
# Extract test data from Manta River raw data
# X Lin Oct 30 2020
rm(list=ls(all=TRUE))
library(xlsx)
#setwd('//pnl/projects/SBR_SFA/RC2/04_Spatial_Study/02_MantaRiver/03_ProcessedData/')
#setwd('C:/Users/linx882/OneDrive - PNNL/XLin/automation of respiration calculations/RC_2/sss_data_processing/codes')
#source('./MRhelper.R')
sdir<- 'C:/Users/linx882/OneDrive - PNNL/XLin/automation of respiration calculations/RC_2/sss_data_processing'
findir<- 'C:/Users/linx882/PNNL/RC-2, River Corridor SFA - Spatial Study 2022'
##########################################################################
## DO slope
DO_summary<-read_csv(file.path(findir,'03_MinidotManualChamber2hr/03_ProcessedData','RC2_Minidot_lm_summary.csv'))
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
mean_depth<- read_csv(file.path(findir,'SSS_Data_Processing','SSS_Average_Reach_Depth_2023-04-05.csv'))
mean_depth$mean_reach_depth_m <- mean_depth$mean_reach_depth_cm/100 
#
Do_slope<- merge(Do_slope,mean_depth[c("Site_ID","mean_reach_depth_m")],by=c('Site_ID'))
# calculate ER water 
Do_slope['ERwaterdaily_gO2/m2day'] <-Do_slope$DO_Slope_mean*(60*24)*Do_slope$mean_reach_depth_m
Do_slope['ERwaterdaily_gO2/m3day'] <-Do_slope$DO_Slope_mean*(60*24)
ERwater<-Do_slope[c("Site_ID",'ERwaterdaily_gO2/m2day','ERwaterdaily_gO2/m3day')]
#########################
library("readxl")
ERtotal <- read_excel(file.path(sdir,'SSS_metabolism/SM_analysis','combined_results_040323.xlsx'))
names(ERwater)[1]<-'SiteID'
ERtotal<-merge(ERtotal,ERwater,by=c('SiteID'))
##
#calculate ER sed
ERtotal['ERseddaily_gO2/m2day']<-ERtotal$`ERdailymeanmean_gO2/m2day`-ERtotal$`ERwaterdaily_gO2/m2day`
ERtotal['ERseddaily_gO2/m3day']<-ERtotal$`ERdailymeanmean_gO2/m3day`-ERtotal$`ERwaterdaily_gO2/m3day`

write.csv(ERtotal,file.path(sdir,'SSS_metabolism/SM_analysis','combined_results_updated_040623.csv'),row.names = FALSE)
