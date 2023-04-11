# ==============================================================================
#Get average of wading transects
#
# Status: In progress. 
# ==============================================================================
#
# Author: Brieanne Forbes and Lupita Renteria
# 11 Oct 2022
#
# ==============================================================================

library(tidyverse)
library(lubridate)

# ================================= User inputs ================================

wading_file <- "C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_Wading_Transect_Depth_Full.csv"

out_file <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_Wading_Depth_Summary_2023-03-01.csv'

# ================================ read in data ==============================

wading <- read_csv(wading_file,na=c("-9999", "N/A", "") ) %>%
  select(-Transect_One_Near_Bank_Depth_cm, -Transect_One_Far_Bank_Depth_cm, 
         -Transect_Two_Near_Bank_Depth_cm, -Transect_Two_Far_Bank_Depth_cm,
         -Transect_Three_Near_Bank_Depth_cm, -Transect_Three_Far_Bank_Depth_cm,
         -Transect_Four_Near_Bank_Depth_cm, -Transect_Four_Far_Bank_Depth_cm,
         -Transect_Five_Near_Bank_Depth_cm, -Transect_Five_Far_Bank_Depth_cm,
         -Transect_Six_Near_Bank_Depth_cm, -Transect_Six_Far_Bank_Depth_cm,
         -Transect_Seven_Near_Bank_Depth_cm, -Transect_Seven_Far_Bank_Depth_cm,
         -Transect_Eight_Near_Bank_Depth_cm, -Transect_Eight_Far_Bank_Depth_cm,
         -Transect_Nine_Near_Bank_Depth_cm, -Transect_Nine_Far_Bank_Depth_cm,
         -Transect_Ten_Near_Bank_Depth_cm, -Transect_Ten_Far_Bank_Depth_cm)
         

# ================================ calculate means =============================

means <- wading %>%
  summarize(Site_ID=Site_ID, 
            Date=Date,
            Start_Time=Start_Time,
            Average_Depth_cm=rowMeans(select(.,contains("Depth") ),na.rm=T),
            Number_of_Points=rowSums(!is.na(select(.,contains("Depth")))),
            Total_Distance_m=rowSums(select(.,contains("Distance")),na.rm=T),
            Average_River_Width_m=rowMeans(select(.,contains('Width')), na.rm = T)
            ) %>%
  mutate(Average_Depth_cm = round(Average_Depth_cm, 2),
         Total_Distance_m = round(Total_Distance_m, 2),
         Average_River_Width_m = round(Average_River_Width_m, 2))

# site_s37_mean <- means%>%
#   filter(Site_ID == 'S37')
# 
# site_s37 <- wading%>%
#   filter(Site_ID == 'S37')
# 
# s37_main_depth_sum <- site_s37 %>%
#   slice(1) %>%
#   mutate(s37_main_depth=sum((select(.,contains("Depth"))), na.rm = T))%>%
#   pull()
# 
# s37_main_depth_mean <- site_s37_mean$Average_Depth_cm[1]
# 
# s37_side_depth_mean <- site_s37_mean$Average_Depth_cm[2]
# 
# s37_transect3to8_depth_sum <- 654
# 
# s37_transect3to8_percent <- s37_transect3to8_depth_sum/s37_main_depth_sum
# 
# s37_depth <- (s37_main_depth_mean*s37_transect3to8_percent)+(s37_side_depth_mean*(1-s37_transect3to8_percent))
# 
# s37_width <- site_s37_mean$Average_River_Width_m[1]
# 
# s37_date <- site_s37_mean$Date[1]

# means <- means %>%
#   filter(Site_ID != "S37")%>%
#   mutate(Start_Time=as.character(Start_Time)) %>%
#   add_row(Site_ID ='S37',Date=s37_date, Start_Time='09:33:00', Average_Depth_cm=s37_depth, Total_Distance_m=121, Number_of_Points = 65, Average_River_Width_m = s37_width)
# ================================== write file ===============================

write_csv(means, out_file)


