# ==============================================================================
#
# Investigate TN data
#
# Status: In progress
#
# ==============================================================================
#
# Author: Brieanne Forbes 
# 23 January 2025
#
# ==============================================================================
library(tidyverse) 

rm(list=ls(all=T))

# Setting wd to parent folder
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")

# =================================== find files ===============================

tn <- read_csv('./Published_Data/v4_CM_SSS_Data_Package/Sample_Data/v3_CM_SSS_Water_NPOC_TN.csv',
                    skip = 2, na = c('', 'N/A', '-9999'))%>%
  filter(!is.na(Sample_Name),
         str_detect(Sample_Name, 'SSS')) %>%
  select(contains('TN')) %>%
  rename(TDN = `00602_TN_mg_per_L_as_N`)%>%
  mutate(all_TDN = 
           case_when(str_detect(TDN, "[|]") ~ as.numeric(str_extract(TDN, "[0-9\\.]+(?=[^0-9]*$)")),
                     TRUE ~ as.numeric(TDN)),
         text_string = str_detect(TDN, "[|]"), # TRUE if text string, FALSE if just a number) 
         cube_TDN = sign(all_TDN) * (abs(all_TDN))^(1/3)) 

histo <- ggplot(tn, aes(x = all_TDN, fill = text_string)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(aes(xintercept = 0.0015, color = "Bob's LOD"), linetype = "dashed") +
  geom_vline(aes(xintercept = 0.026, color = "Our LOD"), linetype = "dashed") +
  geom_vline(aes(xintercept = 0.1, color = "Lowest Standard")) +
  scale_color_manual(name = "", values = c("Bob's LOD" = "red", 'Our LOD' = 'blue', "Lowest Standard" = 'black'))+
  scale_fill_manual(name = "Below LOD/Standard", values = c("TRUE" = "lightgrey", "FALSE" = "darkgray"))+
  xlab('TDN (mg/L)')

histo_cubed <- ggplot(tn, aes(x = cube_TDN, fill = text_string)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(aes(xintercept = 0.1145, color = "Bob's LOD"), linetype = "dashed") +
  geom_vline(aes(xintercept = 0.2962, color = "Our LOD"), linetype = "dashed") +
  geom_vline(aes(xintercept = 0.464, color = "Lowest Standard")) +
  scale_color_manual(name = "", values = c("Bob's LOD" = "red", 'Our LOD' = 'blue', "Lowest Standard" = 'black'))+
  scale_fill_manual(name = "Below LOD/Standard", values = c("TRUE" = "lightgrey", "FALSE" = "darkgray"))+
  xlab('TDN (mg/L) ^(1/3)')
  
