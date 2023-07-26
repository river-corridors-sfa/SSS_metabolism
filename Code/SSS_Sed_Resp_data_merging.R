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
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
library(ggbreak)
library(tidyr)
library(stringr)
library("PerformanceAnalytics")
library(ggpubr)
library(datawizard)

data_merge<-function(){
  # read in ER data 
  sdata<-read.csv('./SSS_Ecosystem_Respiration_Data_Package/SSS_Water_Sediment_Total_Respiration.csv',skip=8)
  
  #model data from kyongho's model
  mdata<-read.csv('./SSS_Ecosystem_Respiration_Data_Package/SSS_ER_d50_TotalOxygenConsumed.csv',skip=4)
  sdata<-merge(sdata,mdata,by= c('Parent_ID','Site_ID'))
  sdata[sdata==-9999] =0
  names(sdata) <-c("Parent_ID","Site_ID","ERtotal_Square","ERtotal_Cubic","ERwc_Square",  "ERwc_Cubic",        
                   "ERsed_Square", "ERsed_Cubic","D50_m","Total_Oxygen_Consumed")
  
  # Slope, Discharge, Velocity from national hydrography dataset (NHD)
  nhd_data<-read.csv('./SSS_Ecosystem_Respiration_Data_Package/Inputs/SSS_Slope_Discharge_Velocity.csv',skip=6)
  
  # streamcat data 
  scat <-read.csv('./v2_RCSFA_Extracted_Geospatial_Data_2023-06-21.csv')
  cols<-c('site',"totdasqkm","PctMxFst2019Ws","PctCrop2019Ws","AridityWs",'streamorde')
  scat <- scat[scat$site%in%sdata$Site_ID,grep(paste(cols, collapse = "|"),names(scat))]
  names(scat)[1]<-'Site_ID'
  
  # # minidot temperature
  # mdot <-read.csv('./SSS_Data_Package/miniDOT/Plots_and_Summary_Statistics/SSS_Water_DO_Temp_Summary.csv',skip=9)
  # mdot <-mdot[,c(2:4)]; names(mdot)[3]<-"Minidot_Temp"
  
  # HOBO temperature
  hobo <-read.csv('./SSS_Data_Package/DepthHOBO/Plots_and_Summary_Statistics/SSS_Water_Press_Temp_Summary.csv',skip=7)
  hobo <-hobo[,c(3:4)]; names(hobo)[2]<-"HOBO_Temp"
  hobo$Site_ID[hobo$Site_ID=='S55'] ='S55N' ; hobo$Site_ID[hobo$Site_ID=='S56'] ='S56N'
  
  
  # # average depth 
  # depth <- read.csv('./SSS_Data_Package/SSS_Water_Depth_Summary.csv',skip=8)
  # depth <-depth[,c(1,5)]
  
  ## tss data
  tss <-read.csv('./SSS_Data_Package/SSS_Water_TSS.csv',skip=2)
  tss <- tss[grep('TSS',tss$Sample_Name),]
  tss <-tss[,c(2,4)]; 
  names(tss)<-c('Parent_ID','TSS')
  tss$Parent_ID <-sapply(strsplit(as.character(tss$Parent_ID), "_"), `[`, 1)
  tss$TSS <- as.numeric(tss$TSS)
  
  # chemical data NPOC and TN
  chemdata <- read.csv(file.path('./v2_CM_SSS_Data_Package','v2_CM_SSS_Water_NPOC_TN_Summary.csv'),skip=2)
  chemdata <-chemdata[grep('SSS',chemdata$Sample_Name),]
  chemdata <-chemdata[,c(2,4,5)]; 
  names(chemdata)<-c('Parent_ID','NPOC','TN')
  chemdata[chemdata=='-9999'] =NA
  chemdata[c('NPOC','TN')] <- sapply(chemdata[c('NPOC','TN')],as.numeric)
  chemdata$Parent_ID <-sapply(strsplit(as.character(chemdata$Parent_ID), "_"), `[`, 1)
  
  #Chlorophyll_A
  chla <-read.csv('./SSS_Data_Package/MantaRiver/Plots_and_Summary_Statistics/SSS_Water_Temp_SpC_Turb_pH_ChlA_Summary.csv',skip=15)
  chla <-chla[,c(2,12)]; names(chla)[2]<-"Chlorophyll_A"
  ## GPP
  gpp<-read.csv('./SSS_Ecosystem_Respiration_Data_Package/Outputs/SSS_combined_SM_results.csv')
  gpp<- gpp[,c(1,5,7)]
  names(gpp)[2:3] <-c('GPP_Square','Mean_Depth')
  
  #Hyporheic exchange flux
  aflux<-readRDS(file='nhd_CR_stream_annual_resp_inputs_outputs.rda')[c('COMID','logq_hz_total_m_s')]
  names(aflux)<-c('COMID','hz_annual')
  sflux<-readRDS(file='nhd_CR_stream_spring_resp_inputs_outputs.rda')[c('COMID','logq_hz_total_m_s')]
  names(sflux)<-c('COMID','hz_spring')
  wflux<-readRDS(file='nhd_CR_stream_summer_resp_inputs_outputs.rda')[c('COMID','logq_hz_total_m_s')]
  names(wflux)<-c('COMID','hz_winter')
  #fluxs <- Reduce(function(x, y) merge(x, y,by ='COMID',all =TRUE), list(aflux,sflux,wflux))
  gspatial<- read.csv('./v2_RCSFA_Geospatial_Data_Package/v2_RCSFA_Geospatial_Site_Information.csv')[c('COMID','Site_ID')]
  gdata<- gspatial[gspatial$Site_ID%in%sdata$Site_ID,]
  gdata <- Reduce(function(x, y) merge(x, y,by ='COMID',all.x =TRUE), list(gdata,aflux,sflux,wflux))
  gdata <- gdata[c("Site_ID","hz_annual", "hz_spring", "hz_winter")]
  
   ## merge the data
  cdata <- Reduce(function(x, y) merge(x, y,by ='Parent_ID',all =TRUE), list(sdata,tss,chemdata,gpp,chla))
  cdata <- Reduce(function(x, y) merge(x, y,by ='Site_ID',all.x =TRUE), list(cdata,scat,hobo,nhd_data,gdata))
  
  return(cdata)
}



## figure set up
librarian::shelf(tidyverse,# for plotting
                 plot3D,# for 3D plots
                 plot3Drgl,# for interactive 3D plots
                 rgl,# required by plot3Drgl
                 entropy,#Information theory calculations
                 GGally,#pair plots
                 scales,# manipulating log scales
                 stringr,# editing text
                 Hmisc,# Harrell's miscellaneaous for stats
                 gtable)# To manipulate ggplot objects

theme_httn<-  theme(axis.text=element_text(colour="black",size=10),
                    axis.title = element_text(size = 12, face = "bold"),
                    panel.grid.minor= element_line(colour = "gray", linetype = "dotted"), 
                    panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
                    panel.border = element_rect(fill=NA, colour = "black", linewidth = 1.5),
                    panel.background=element_rect(fill="white"),
                    axis.ticks.length = unit(0.254, "cm"),
                    axis.ticks = element_line(colour = "black", linewidth = 1), 
                    axis.line = element_line(colour = "black"),
                    legend.position = c(0.85,0.25),
                    legend.direction = "vertical",
                    legend.background = element_blank(),
                    legend.key.height = unit(0.6, "inch"),
                    legend.key.size = unit(1.0, 'lines'),#Changing spacing between legend keys
                    legend.title = element_text())

make_fullsize <- function() structure("", class = "fullsizebar")
# Creating breaks for logarithmic scale 
# (see: https://r-graphics.org/recipe-axes-axis-log)



