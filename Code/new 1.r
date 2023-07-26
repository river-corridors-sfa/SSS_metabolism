# RC2 spatial study 
# preparing data for ER_total, ER_water,ER_sed related analysis
# X Lin April 18 2023
################################################
# Read in data 
################################################
rm(list=ls(all=TRUE))

library(MASS)
library(relaimpo)
library(visreg)
library(ggstatsplot)
library(caret)
library(leaps)
library(car)
library(patchwork)
library(tidyr)
library(stringr)

# read in ER data 
ERdata<-read.csv('./SSS_Ecosystem_Respiration_Data_Package/SSS_Water_Sediment_Total_Respiration.csv',skip=8)
#model data from kyongho's model
mdata<-read.csv('./SSS_Ecosystem_Respiration_Data_Package/SSS_ER_d50_TotalOxygenConsumed.csv',skip=4)


ERdata<-merge(ERdata,mdata,by= c('Parent_ID','Site_ID'))
data[data==-9999] =0
png(file.path('./MLR_Analysis_Figures',paste0('ER_sed_vs_total_oxygen_consumed','.png')), 
    width = 8, height = 6, units = 'in', res = 600)
par(mgp=c(2.2,1,0),mar=c(3.1,4.1,2,1.5))
plot(data$Sediment_Respiration_Square,data$Total_Oxygen_Consumed_g_per_m2_per_day,
     xlab=expression(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), pch=20,
     ylab=expression(paste("Total Oxygen Consumed "*" (g O"[2]*" m"^2*" day"^-1*")")))
#abline(lm(data$Total_Oxygen_Consumed_g_per_m2_per_day ~ data$Total_Ecosystem_Respiration_Square))
dev.off()

names(data)<-c("Site_ID","ndays","ERtotal_gm2day",  "GPP_gm2day","K600_mday","mean_depth_m","ERtotal_gm3day","GPP_gm3day",
                        "K600_ERrsq","K600_ERp","ERwater_gm2day","ERwater_gm3day","ERsed_gm2day","ERsed_gm3day")
data$Site_ID[data$Site_ID=='S63P'] ='S63'
#data$DO_slope[data$DO_slope>0]<-0

# water temperature
wdata <-read.csv('Water_temperature.csv')
wdata$Site_ID[wdata$Site_ID=='S63P'] ='S63'
## discharge and velocity data
vdata<- read.csv('all_sites_Depth_Summary_with_width_and_velocity_for_K.csv')
vdata<-vdata[c("Site_ID","COMID","Average_Depth_cm","estimated_discharge_cms","estimated_velocity_m_s")]
names(vdata)<-c("Site_ID","COMID","Average_Depth_cm","discharge_cms","velocity_ms")
vdata$Site_ID[vdata$Site_ID=='S63P'] ='S63'
# TN data
tdata <- read.csv('CM_SSS_Water_NPOC_TN_Summary.csv',skip=2)
tdata<- tdata[grep('SSS',tdata$Sample_Name),c(2,4,5)]
names(tdata)<-c('Parent_ID','NPOC','TN')
tdata$TN[tdata$TN==-9999]=NA
## merge the data
cdata <- merge(data,wdata,by=c("Site_ID"))
cdata <- merge(cdata,vdata,by=c("Site_ID"))
cdata <- merge(cdata,tdata,by=c("Parent_ID"))


### TSS data
tss<- read.csv('SSS_Water_TSS.csv',skip=13)
tss<- tss[,c(2,4)]
names(tss) <- c('Parent_ID','TSS')
tss<- tss %>% separate(Parent_ID, c("Parent_ID", "tx"), "_")
tss$TSS[grep('TSS',tss$TSS)]<- str_split_fixed(tss$TSS[grep('TSS',tss$TSS)],'_',3)[,2]
tss$TSS <-as.numeric(tss$TSS)
#write.csv(cdata,'combined_results_updated_04192023.csv',row.names = FALSE)
cdata <- merge(cdata,tss[,c(1,3)],by=c('Parent_ID'))
## 


##
# NHD and streamcat data 
gdf <- read.csv('Geospatial_SSS_short_2023-04-25.csv',row.names = NULL) 
names(gdf) <- names(gdf)[2:length(names(gdf))]
names(gdf)[1]<-c('Site_ID')

cdata <- merge(cdata,gdf[,c(1,3:8)],by=c('Site_ID'))
cdata <- cdata[order(cdata$Parent_ID),]

## 
d50<- read.csv('SSS_COMIDs_NEXSS_D50.csv')
names(d50)[c(1,2)]<-c("COMID","Site_ID")


cdata <- merge(cdata,d50,by=c("Site_ID"),all = TRUE)
cdata<-cdata[order(cdata$Parent_ID),]
write.csv(cdata,'combined_results_updated_04262023.csv',row.names = FALSE)

cdata <- read.csv('combined_results_updated_04262023.csv')


###
# COMID checking
all(sort(vdata$Site_ID)==sort(d50$Site_ID))

mdf<- merge(vdata[c("COMID","Site_ID")],d50[c("COMID","Site_ID")],by=c('Site_ID'))
mdf['Same_check'] = mdf$COMID.x== mdf$COMID.y

## COMID from data package 'v2_RCSFA_Geospatial_Data_Package'
dpf<- read.csv(file.path('v2_RCSFA_Geospatial_Data_Package','v2_RCSFA_Geospatial_Site_Information.csv'))
dpf <-dpf[dpf$Site_ID%in%mdf$Site_ID,]
mdf<-merge(mdf,dpf[c("COMID","Site_ID")],by=c('Site_ID'))
mdf['Same_check_dp1'] = mdf$COMID.x== mdf$COMID
mdf['Same_check_dp2'] = mdf$COMID.y== mdf$COMID

write.csv(mdf,'COMID_checking.csv',row.names = FALSE)
