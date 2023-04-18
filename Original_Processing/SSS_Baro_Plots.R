# ==============================================================================
#
# Plot SSS BaroTROLL data
#
# Status: Complete
#
# ==============================================================================
#
# Author: Brieanne Forbes
# 30 Sept 2022
#
# ==============================================================================

library(tidyverse)
# ================================= User inputs ================================

baro_folder <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_BaroTROLL_HOBO_Trimmed'

# =================================== combine data =============================

baro_files <- list.files(baro_folder, 'BaroTROLL', full.names = T)

combine_baro <- read_csv(baro_files[1]) %>%
  filter(is.na(Date_Time)) %>%
  add_column(Site = 'NA')

for (baro_file in baro_files) {
  
  site <- unlist(str_split(baro_file, '_'))[7]
  
  temp <- read_csv(baro_file) %>%
    add_column(Site = site)
  
  combine_baro <- combine_baro %>%
    add_row(temp)
  
}


# =================================== theme ================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    line = element_line(size = 0.05),
    axis.line = element_line(size = 0.5),
    panel.background = element_rect(color = 'white'),
    panel.border = element_rect(
      colour = 'black',
      fill = NA,
      size = 0.5
    ),
    plot.title = element_text(size = 25, face = 'bold'),
    axis.ticks.length = unit(.25, 'cm')
  )
)

# =================================== plot ================================

baro <- ggplot(combine_baro, aes(x = Date_Time, y = BaroTROLL_Barometric_Pressure_mBar)) +
  geom_point(aes(color = Site), size = 0.5)+
  scale_color_brewer(palette = 'Paired')+ 
  guides(colour = guide_legend(override.aes = list(size=5))) +
  labs(x = 'Date', y = 'BaroTROLL Barometric Pressure (mbar)')


# ggsave(
#   'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_BaroTROLL_Trimmed.pdf',
#   baro,
#   device = 'pdf',
#   width = 10,
#   height = 7,
#   units = 'in',
#   dpi = 300
# )

# ==================================== Baro mean  ==============================

mean <- combine_baro %>%
  group_by(Site) %>%
  summarize(Mean_BaroTROLL_Barometric_Pressure_mBar = mean(BaroTROLL_Barometric_Pressure_mBar))

write_csv(mean, 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_BaroTROLL_Mean_Pressure.csv')

# ====================================== plot  =================================

pressure_elevation_data <- read_csv('C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_BaroTROLL_Mean_Pressure_Elevation.csv')

pressure_elevation <- ggplot(pressure_elevation_data, aes(x = Elevation_m, y = Mean_BaroTROLL_Barometric_Pressure_mBar)) +
  geom_smooth(method = lm) +
  geom_point() +
  labs(x = 'Elevation (m)', y = 'Mean BaroTROLL Pressure (mBar)') +
  geom_text(aes(label = Site, hjust = -0.1, vjust = -0.1), size = 2.5)

ggsave(
  'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/SSS_Data_Processing/SSS_BaroTROLL_Elevation.pdf',
  pressure_elevation,
  device = 'pdf',
  width = 5,
  height = 4.5,
  units = 'in',
  dpi = 300
)








