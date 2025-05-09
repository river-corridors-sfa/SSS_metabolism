# Initilize parameter 
rm(list=ls())


## calculating the O2 consumption rate (gO2/m2/day)
## compare the RC2 O2 consumption rate with our river corridor model outputs
## our model's raw model outputs are O2 consumption rate (mole) and NO2 and NO3 consumption rate
## aerobic respiration's reaction network f1=0.65*1/3
## anaerobic respiration reaction network f2=0.65 (NO3->NO2), f3=0.99 (NO2-> N2)
## O2 consumption rate (O2 mole)/f1*32g/365day/m2(surface area)


#https://ryanpeek.github.io/2017-11-21-mapping-with-sf-part-3/
#https://www.nceas.ucsb.edu/scicomp/usecases/joinmergeattributetables
# https://ryanpeek.github.io/2017-11-05-mapping-with-sf-Part-2/
#https://usgs-r.github.io/nhdplusTools/
#http://docs.opengeospatial.org/is/14-111r6/14-111r6.html#_the_named_feature_model
#https://github.com/mbtyers/riverdist
#https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html
#https://github.com/jsta/nhdR

# reading MRMT model outputs (total NO3 removal)
# reading inputs (NEXSS,SPARROW,stream DOC rates)
# sensititivity results (1/2 DOC, 2*DOC, 1/10 NO3, and 10*NO3)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(rasterVis)
library(rgeos)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(lattice)
library(grid)
library(spatstat)
library(plotKML)
library(fasterize)
library(egg)
library(nhdplusTools)
library(nhdR)
library(rgeos)
library(colorspace)
library(stars)
library(pals)
library(foreign)
library(tidyverse)

## color scheme
#https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html
#http://colorspace.r-forge.r-project.org/articles/hcl_palettes.html
#https://rdrr.io/r/grDevices/palettes.html
#https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
#https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html
#http://blogs.nature.com/onyourwavelength/2018/07/18/heuristics-for-better-figures/


############ loading NHD Data----
home<-"C:/Project/Columbia_SPARROW/gis"
nhd_CR<-"C:/Project/Columbia_SPARROW/gis/NHD_CR.shp"
nhd_CR_file<-"NHD_CR"


nhd_CR_stream<-st_read(home,nhd_CR_file)
tmp<-st_zm(nhd_CR_stream)
nhd_CR_poly<-tmp[,"COMID"]

########## stream surface area 
dir_nexss<-"D:/Project/SFA/Columbia_SPARROW/data/nxss/"
file<-"dataHUC17_May29_2020.rds"
nexss<-readRDS((paste0(dir_nexss,file)))
nexss$area_m2<-nexss$length_m*10^(nexss$logw_m)

############ annual/seasonal substrate data year data-----------
home_data<-"C:/Users/gara009/Downloads/annual"
home_season_data<-paste(home_data,sep="/")
#vertical direction # # acctual N and DOC for vertical #
annual_10vert<-"vert_ann.dat" #
annual_10vert<-read.delim(paste(home_season_data,annual_10vert,sep="/"),header=T,sep='',skip=0)

annual_10lat<-"lat_annual.dat" #
annual_10lat<-read.delim(paste(home_season_data,annual_10lat,sep="/"),header=T,sep='',skip=0)

# extract 2nd and 3rd year cumulative respiration amounts (moles)
tmp=subset(annual_10vert,annual_10vert$day==730)
tmp2=subset(annual_10vert,annual_10vert$day==1095)

# compute net annual  cumulative respiration---------

tmp3<-as.data.frame(cbind(tmp$COMID,tmp2$ver_o2_cons_mol-tmp$ver_o2_cons_mol,tmp2$ver_no3_prod_mol-tmp$ver_no3_prod_mol,tmp2$ver_no2_prod_mol-tmp$ver_no2_prod_mol))
colnames(tmp3)=c("COMID","ver_o2_cons_mole","ver_no3_cons_mole","ver_no2_cons_mole")
tmp3<-tmp3[,c(1,2)]
ver_annual_hr_CR<-tmp3

# extract 2nd and 3rd year cumulative respiration amounts (moles)
tmp=subset(annual_10lat,annual_10lat$day==730)
tmp2=subset(annual_10lat,annual_10lat$day==1095)

tmp3<-as.data.frame(cbind(tmp$COMID,tmp2$lat_o2_cons_mol-tmp$lat_o2_cons_mol,tmp2$lat_no3_prod_mol-tmp$lat_no3_prod_mol,tmp2$lat_no2_prod_mol-tmp$lat_no2_prod_mol))
colnames(tmp3)=c("COMID","lat_o2_cons_mole","lat_no3_cons_mole","lat_no2_cons_mole")
tmp3<-tmp3[,c(1,2)]
lat_annual_hr_CR<-tmp3

#### merge lateral and vertical O2 consumption (moles)
resp_hr_CR_annual_o2_consump_mole<-merge(ver_annual_hr_CR,lat_annual_hr_CR,by.x="COMID",by.y="COMID")

nhd_CR_stream_annual_o2_consum<-merge(nhd_CR_poly,resp_hr_CR_annual_o2_consump_mole, by.x = "COMID",by.y="COMID")
nhd_CR_stream_annual_o2_consum$tot_o2_cons_mole<-nhd_CR_stream_annual_o2_consum$ver_o2_cons_mole+nhd_CR_stream_annual_o2_consum$lat_o2_cons_mole
nhd_CR_stream_annual_o2_consum$tot_o2_cons_mole_day<-nhd_CR_stream_annual_o2_consum$tot_o2_cons_mole/365 #convert form annual to per day

nhd_CR_stream_annual_o2_consum<-merge(nhd_CR_stream_annual_o2_consum,nexss[,c("comid_nhd","area_m2")],by.x="COMID",by.y="comid_nhd") # merge with Nexss data to get surface area

nhd_CR_stream_annual_o2_consum$tot_o2_cons_g_m2_day<-nhd_CR_stream_annual_o2_consum$tot_o2_cons_mole_day*32/nhd_CR_stream_annual_o2_consum$area_m2

## Yakima river basin
nhd_CR_stream_annual_o2_consum_df<-data.frame(nhd_CR_stream_annual_o2_consum)
nhd_CR_stream_annual_o2_consum_df$geometry<-NULL


write.csv(nhd_CR_stream_annual_o2_consum_df,file="nhd_CR_stream_annual_o2_consum_df.csv",row.names =FALSE)



nhd_YRB_stream_annual_o2_consum=merge(nhd_sub9_stream,nhd_CR_stream_annual_o2_consum_df,by="COMID")


par(cex.main=1.5,cex.axis=1.5)  
plot(nhd_CR_stream_annual_o2_consum[,"tot_o2_cons_g_m2_day"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=coolwarm(10),reset=FALSE)
plot(st_geometry(CR_basin_shapes),add=T)

title("(a) Aerobic respiration (gO2/m2/day)",line=-24, adj = 0.2)


jpeg("o2_consumption_rate_gO2_m2_day.jpg", width = 6, height = 6, units = 'in', res = 300)

par(cex.main=1.5,cex.axis=1.5)  
plot(nhd_YRB_stream_annual_o2_consum[,"tot_o2_cons_g_m2_day"],main="", key.pos = 1, key.width = lcm(2), key.length = 1.0,breaks = "fisher",pal=coolwarm(10),reset=FALSE)
plot(st_geometry(CR_basin_shapes),add=T)

title(" Aerobic respiration (gO2/m2/day)",line=-24, adj = 0.2)

dev.off()