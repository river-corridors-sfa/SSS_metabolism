# ==============================================================================

# Exploratory miniDOT data cleaning

# Status: In progress

# ===================================== Notes ==================================

# Columns in input file: datetime, parent id, site id, lat/long, temp (minidot), DO (minidot), pressure (baro), depth
# need to include hobo and avg depth in intermediate step to calculate depth

# ==============================================================================

# Author: Brieanne Forbes (brieanne.forbes@pnnl.gov)
# 18 October 2024

# remove all files
rm(list = ls(all = TRUE))

# ==============================================================================

library(tidyverse)
library(plotly)
library(scales)
library(tsrobprep)

# ================================= User inputs ================================

# CHANGE TO DATA PACKAGE WHEN PUBLISHED

data_dir <-
  'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/05_PublishReadyData'

# ============================ find and read files =============================

files <- list.files(data_dir, 'Water_DO_Temp.csv', full.names = T)


# ============================ set plot theme =============================
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
    plot.title = element_text(size = 20, face = 'bold'),
    axis.ticks.length = unit(.25, 'cm'),
    strip.background = element_blank(),
    strip.text = element_text(size = 9, family = 'serif'),
    strip.placement = "outside",
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    plot.margin = margin(0, 1, 0, 0, "cm")
  )
)

# ============================ loop through files =============================

for (file in files) {
  data <- read_csv(file, comment = '#', na = c('', '-9999',-9999, NA, 'N/A'))
  
  parent_ID <- str_extract(file, "[A-Z]{3}\\d{3}")
  
  subset <- data %>%
    filter(minute(DateTime) %% 15 == 0)
  
  ## ============================ tsrobprep cleaning =============================
  
  auto_clean <- auto_data_cleaning(
    data = subset$Dissolved_Oxygen, # Dissolved Oxygen data
    S = 96, # 96 intervals for daily seasonality (96 rows of 15 min data = 1 day of data)
    tau = NULL, # Performs lasso to determine tau
    no.of.last.indices.to.fix = nrow(tsrobprep_clean), # Fix all data points
    indices.to.fix = NULL, # Automatically fix indices
    detect.outliers.pars = list(method = c("IQR"),  # Use IQR for outlier detection
                                threshold = c(1.5)  # Common threshold for IQR
                                ))
    
    clean_DO <- subset %>%
      add_column(auto_clean$clean.data)
    
    ## ============================== check data ================================
    
    # use original data when cleaned values are 20 mg/L or more (ie the cleaning did not work correctly)
    
    
    ## ============================ fix sample day ==============================
    
    # use metadata to revert last two and first two points before and after sample day back to original data
    
    ## ============================ remove biofouling =============================
    
    
    ## ============================ plot data =============================
    
    # cleaned_DO_plot <- ggplot(tsrobprep_clean, aes(x = DateTime)) +
    #   geom_point(aes(y = Dissolved_Oxygen, color = "Original"), size = 3.5) +  # Original DO points
    #   geom_point(aes(y = cleaned_DO, color = "Cleaned_tsrobprep_tau_null"), size = 1.5) +        # Cleaned DO points
    #   labs(title = str_c("Parent ID: ", parent_ID, "                 Cleaned Dissolved Oxygen Data with 'tsrobprep' package"),
    #        x = "DateTime", y = "Dissolved Oxygen (mg/L)", color = NULL) +  # Remove legend title
    #   scale_color_manual(values = c("Original" = "grey", "Cleaned_tsrobprep_tau_null" = "darkred")) +  # Custom colors
    #   scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
    #   theme(legend.position = c(0.1, 0.85))
    #
    #
    # plotly <- ggplotly(cleaned_DO_plot)
    #
    # plotly_outname <- str_c()
    #
    # htmlwidgets::saveWidget(as_widget(plotly), plotly_outname)
    
    ## ============================ create input file =============================
    
    ### ============================ merge DO with baro and hobo ===================
    
    ### ============================ calculate depth ===================
    
    ### ============================ output cleaned input file with headers ===================
    
    }
