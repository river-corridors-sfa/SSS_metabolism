# ==============================================================================
#
# Make figures for ERsed manuscript
#
# Status: In progress
#
# do cube and scale transformation, run maggi's code that decides what to drop
# ==============================================================================
#
# Author: Brieanne Forbes 
# 8 January 2025
#
# ==============================================================================
library(tidyverse) 
library(corrplot)

rm(list=ls(all=T))

# Setting wd to parent folder
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")

# =================================== find files ===============================

er_gpp <- read_csv('./v2_SSS_Water_Sediment_Total_Respiration_GPP.csv',
                   comment = '#', na = '-9999') %>%
  select(Parent_ID, Site_ID, Sediment_Respiration, Gross_Primary_Production)

geospatial <- read_csv('https://github.com/river-corridors-sfa/Geospatial_variables/raw/refs/heads/main/v2_RCSFA_Extracted_Geospatial_Data_2023-06-21.csv') %>%
  select(site, totdasqkm, PctMxFst2019Ws,PctConif2019Ws,PctGrs2019Ws, PctShrb2019Ws, AridityWs) %>%
  filter(site %in% er_gpp$Site_ID)%>%
  mutate(PctFst = PctMxFst2019Ws + PctGrs2019Ws + PctMxFst2019Ws) %>%
  rename(Site_ID = site)%>%
  select(Site_ID, totdasqkm, PctFst, PctShrb2019Ws, AridityWs)

d50 <- read_csv('./v2_SSS_ER_d50_TotalOxygenConsumed.csv', 
                comment = '#', na = '-9999') %>%
  select(Parent_ID, D50_m)

slope_vel_dis <- read_csv('./Stream_Metabolizer/Inputs/v2_SSS_Slope_Discharge_Velocity.csv',
                          comment = '#', na = '-9999') %>%
  select(Site_ID, Slope, Discharge, Velocity)

tss <- read_csv('./Published_Data/v3_SSS_Data_Package/Sample_Data/SSS_Water_TSS.csv',
                skip = 2, na = c('', 'N/A', '-9999')) %>%
  filter(!is.na(Sample_Name)) %>%
  mutate(Parent_ID = str_extract(Sample_Name, "^.{1,6}"),
         '00530_TSS_mg_per_L' = case_when(str_detect(`00530_TSS_mg_per_L`, 'LOD') ~ 0.12, # replace below LOD values with half LOD (LOD = 0.24)
                                          TRUE ~ as.numeric(`00530_TSS_mg_per_L`)))%>%
  select(Parent_ID, contains('TSS')) 

npoc_tn <- read_csv('./Published_Data/v4_CM_SSS_Data_Package/Sample_Data/v3_CM_SSS_Water_NPOC_TN.csv',
                    skip = 2, na = c('', 'N/A', '-9999'))%>%
  filter(!is.na(Sample_Name),
         str_detect(Sample_Name, 'SSS')) %>%
  mutate(Parent_ID = str_extract(Sample_Name, "^.{1,6}"),
         '00602_TN_mg_per_L_as_N' = case_when(str_detect(`00602_TN_mg_per_L_as_N`, 'LOD') ~ 0.013, # replace below LOD values with half LOD (LOD = 0.026)
                                              str_detect(`00602_TN_mg_per_L_as_N`, 'Standard') ~ 0.05, # replace below standard values with half standard (standard = 0.1)
                                              TRUE ~ as.numeric(`00602_TN_mg_per_L_as_N`))) %>%
  select(Parent_ID, contains('NPOC'), contains('TN')) %>%
  group_by(Parent_ID) %>%
  summarise(Mean_00602_TN_mg_per_L_as_N = round(mean(`00602_TN_mg_per_L_as_N`), 2)) %>%
  ungroup()

depth <- read_csv('./Published_Data/v3_SSS_Data_Package/v3_SSS_Water_Depth_Summary.csv',
                  comment = '#', na = c('', 'N/A', '-9999')) %>%
  select(Parent_ID, Average_Depth)

hobo_temp <- read_csv('./Published_Data/v3_SSS_Data_Package/Sensor_Data/DepthHOBO/Plots_and_Summary_Statistics/v3_SSS_Water_Press_Temp_Summary.csv',
                      comment = '#', na = c('', 'N/A', '-9999')) %>%
  select(Parent_ID, Temperature_Mean)


# =============================== combine data ===============================

all_data <- er_gpp %>%
  full_join(geospatial, by = 'Site_ID')%>%
  full_join(slope_vel_dis, by = 'Site_ID')%>%
  full_join(d50, by = 'Parent_ID')%>%
  full_join(tss, by = 'Parent_ID')%>%
  full_join(npoc_tn, by = 'Parent_ID')%>%
  full_join(depth, by = 'Parent_ID')%>%
  full_join(hobo_temp, by = 'Parent_ID') %>%
  filter(!is.na(Sediment_Respiration))
  
  
# ======================= assess co-correlation ===============================

long_data <-  all_data %>% 
  pivot_longer(cols = -c(Site_ID, Parent_ID), names_to = "variable", values_to = "value")

ggplot() + 
  geom_histogram(long_data, mapping = aes(x = value)) + 
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

## ======== Spearman correlation before transformations ============

spearman <- cor(all_data %>% select(-Site_ID, -Parent_ID), method = "spearman", use = "complete.obs")

png(file = paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()),"_Scale_Spearman_Correlation_Matrix.png"), width = 12, height = 12, units = "in", res = 300)

corrplot(spearman,type = "upper", method = "number", tl.col = "black", tl.cex = 1.6, cl.cex = 1.25,  title = "Spearman Correlation")

dev.off()

spear.panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  
  r = (cor(x, y, method = c("spearman")))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) {cex.cor <- 0.8/strwidth(txt)}
  text(0.5, 0.5, txt, cex = cex.cor * (1 + abs(r))/2)
  
  # if(missing(cex.cor)) {cex <- 1.2/strwidth(txt)} else {cex = cex.cor}
  # text(0.5, 0.5, txt, cex = cex * sin(sqrt(abs(r))))
  
  test <- cor.test(x,y, method = "spearman")
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", " "))
  #text(0.5, 0.5, txt, cex = cex * r)
  text(.5, .8, Signif, cex=cex.cor, col=2)
  
}

panel.smooth <- function(x, y) {
  points(x, y, pch = 19, col = rgb(0.1, 0.2, 0.5, alpha = 0.3))
  abline(lm(y ~ x), col = 'blue', lty = 2)
}

panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1))
  
  h <- hist(x, plot = FALSE, breaks = "FD")
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  
  rect(breaks[-nB], 0, breaks[-1], y, col="grey", border="white", ...)
}

png(file = paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()),"_Pairs_Spearman_Correlation_Matrix.png"), width = 12, height = 12, units = "in", res = 300)

pairs(all_data %>% select(-Site_ID, -Parent_ID),
      lower.panel = panel.smooth, 
      upper.panel = spear.panel.cor, 
      diag.panel = panel.hist,
      labels = colnames(all_data %>% select(-Site_ID, -Parent_ID)),
      cex.labels = 0.8) 

dev.off()

## ======== Pearson correlation before transformations ============
# function for pearson corr matrix

pear.panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y, method = c("pearson")))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) {cex.cor <- 0.8/strwidth(txt)} 
  text(0.5, 0.5, txt, cex = cex.cor * (1 + abs(r))/2)
  
  # if(missing(cex.cor)) {cex <- 1.2/strwidth(txt)} else {cex = cex.cor}
  # text(0.5, 0.5, txt, cex = cex * sin(sqrt(abs(r))))
  
  test <- cor.test(x,y)
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", " "))
  #text(0.5, 0.5, txt, cex = cex * r)
  text(.5, .8, Signif, cex=cex.cor, col=2)
  
}

png(file = paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()),"_Pairs_Pearson_Correlation_Matrix.png"), width = 12, height = 12, units = "in", res = 300)

pairs(all_data %>% select(-Site_ID, -Parent_ID),
      lower.panel = panel.smooth, 
      upper.panel = pear.panel.cor, 
      diag.panel = panel.hist,
      labels = colnames(all_data %>% select(-Site_ID, -Parent_ID)),
      cex.labels = 0.8) 

dev.off()

## ======== Cube root ======

cube_root <- function(x) sign(x) * (abs(x))^(1/3)

cube_data <-  all_data %>% 
  mutate(across(where(is.numeric), cube_root)) %>% 
  rename_with(where(is.numeric), .fn = ~ paste0("cube_", .x)) 

long_cube_data <-  cube_data %>%
  pivot_longer(cols = -c(Site_ID, Parent_ID), names_to = "variable", values_to = "value")

ggplot() +
  geom_histogram(long_cube_data, mapping = aes(x = value)) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

### ======== Spearman correlation with cube transformation ============

spearman <- cor(cube_data %>% select(-Site_ID, -Parent_ID), method = "spearman", use = "complete.obs")

png(file = paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()),"_Scale_Spearman_Correlation_Matrix_Cubed.png"), width = 12, height = 12, units = "in", res = 300)

corrplot(spearman,type = "upper", method = "number", tl.col = "black", tl.cex = 1.6, cl.cex = 1.25,  title = "Spearman Correlation")

dev.off()

png(file = paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()),"_Pairs_Spearman_Correlation_Matrix_Cubed.png"), width = 12, height = 12, units = "in", res = 300)

pairs(cube_data %>% select(-Site_ID, -Parent_ID),
      lower.panel = panel.smooth, 
      upper.panel = spear.panel.cor, 
      diag.panel = panel.hist,
      labels = colnames(cube_data %>% select(-Site_ID, -Parent_ID)),
      cex.labels = 0.8) 

dev.off()

## ======== Pearson correlation cube transformation ============
# function for pearson corr matrix

png(file = paste0("./Figures/LASSO_Analysis/", as.character(Sys.Date()),"_Pairs_Pearson_Correlation_Matrix_Cubed.png"), width = 12, height = 12, units = "in", res = 300)

pairs(cube_data %>% select(-Site_ID, -Parent_ID),
      lower.panel = panel.smooth, 
      upper.panel = pear.panel.cor, 
      diag.panel = panel.hist,
      labels = colnames(cube_data %>% select(-Site_ID, -Parent_ID)),
      cex.labels = 0.8) 

dev.off()

## ===== run function to automatically determine best variable ====



# ======== LASSO  ============
# everything in LASSO should be scaled 
# stop at 472, 557-562 
# lasso changes based on seed, can be unstable line 570 start looping through at different seeds, normalizes each to highest coefficient 







