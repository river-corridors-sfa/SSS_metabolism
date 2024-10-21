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
library(crayon)

# ================================= User inputs ================================

# CHANGE TO DATA PACKAGE WHEN PUBLISHED

data_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/05_PublishReadyData'

# ============================ find files =============================

DO_files <- list.files(data_dir, 'Water_DO_Temp.csv', full.names = T)

metadata <- 'Z:/00_ESSDIVE/01_Study_DPs/SSS_Data_Package_v3/v3_SSS_Data_Package/v2_SSS_Field_Metadata.csv' %>% #CHANGE TO PUBLISHED DATA 
 read_csv()
  
baro_files <- ''

hobo_files <- ''

depth_file <- ''

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

for (file in DO_files) {
  
  data <- read_csv(file, comment = '#', na = c('', '-9999',-9999, NA, 'N/A')) %>%
    select(-Dissolved_Oxygen_Saturation, -Battery)
  
  parent_ID <- str_extract(file, "[A-Z]{3}\\d{3}")
  
  subset <- data %>%
    filter(minute(DateTime) %% 15 == 0)
  
  ## ============================ tsrobprep cleaning =============================
  
  set.seed(7)
  
  auto_clean <- auto_data_cleaning(
    data = subset$Dissolved_Oxygen, # Dissolved Oxygen data
    S = 96, # 96 intervals for daily seasonality (96 rows of 15 min data = 1 day of data)
    tau = NULL, # Performs lasso to determine tau
    no.of.last.indices.to.fix = nrow(subset), # Fix all data points
    indices.to.fix = NULL, # Automatically fix indices
    detect.outliers.pars = list(method = c("IQR"),  # Use IQR for outlier detection
                                threshold = c(1.5)  # Common threshold for IQR
                                ))
    
    clean_DO <- subset %>%
      add_column(Cleaned_DO = auto_clean$clean.data[,1, drop = T])
    
    # look at cleaned data
    
    # p <- ggplot(clean_DO, aes(x = DateTime)) +
    #   geom_point(aes(y = Dissolved_Oxygen, color = "Original"), size = 3.5) +  # Original DO points
    #   geom_point(aes(y = Cleaned_DO, color = "Cleaned_tsrobprep_tau0.5"), size = 1.5) +        # Cleaned DO points
    #   labs(title = str_c("Parent ID: ", parent_ID, "                 Cleaned Dissolved Oxygen Data with 'tsrobprep' package"),
    #        x = "DateTime", y = "Dissolved Oxygen (mg/L)", color = NULL) +  # Remove legend title
    #   scale_color_manual(values = c("Original" = "grey", "Cleaned_tsrobprep" = "black")) +  # Custom colors
    #   scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
    #   theme(legend.position = c(0.1, 0.85))
    # 
    # ggplotly(p)
    

    ## ============================ fix sample day ==============================
    # use metadata to revert last two and first two points before and after sample day back to original data
    
    site_metadata <- metadata %>%
      filter(Parent_ID == parent_ID)
    
    sample_day <- site_metadata$Sample_Date
    
    datetime_to_fix <- c(as_datetime(paste0(sample_day-(1), '23:30:00')), # second to last point before sample day
                         as_datetime(paste0(sample_day-(1), '23:45:00')), # last point before sample day
                         as_datetime(paste0(sample_day+(1), '00:00:00')), # first point after sample day
                         as_datetime(paste0(sample_day+(1), '00:15:00'))) # second point after sample day
    
    clean_DO <- clean_DO %>%
      mutate(Dissolved_Oxygen_final = case_when(DateTime %in% datetime_to_fix ~ Dissolved_Oxygen,
                                                 TRUE ~ Cleaned_DO))
    
    
    ## ============================== check data ================================
    
    # assuming the cleaning algorithm did not work when cleaned values are more than 20 mg/L
    # in this case, use original data  
    # 17 mg/L is the reported maximum in the DO summary file, rounding up to 20 for the threshold
    
    if(max(clean_DO$Cleaned_DO) > 20){
      
      clean_DO <- clean_DO %>%
        select(DateTime, Parent_ID, Site_ID, Temperature, Dissolved_Oxygen)
      
      
      
      cat(red$bold(str_c(parent_ID, ' has data >20 mg/L. Removing cleaned DO and using original DO.')), "\n")
      
    } else {
      
      clean_DO <- clean_DO %>%
        select(DateTime, Parent_ID, Site_ID, Temperature, Dissolved_Oxygen_final) %>%
        rename(Dissolved_Oxygen = Dissolved_Oxygen_final)
      
    }
    
    
    
    ## ============================ remove biofouling =============================
    
    
    ## ============================ plot data =============================
    
    cleaned_DO_plot <- ggplot(clean_DO, aes(x = DateTime, y = Dissolved_Oxygen)) +
      geom_point() + 
      labs(title = str_c("Parent ID: ", parent_ID, "                 Cleaned Dissolved Oxygen Data"),
           x = "DateTime", y = "Dissolved Oxygen (mg/L)", color = NULL) + 
      scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
      theme(legend.position = c(0.1, 0.85))


    plotly <- ggplotly(cleaned_DO_plot)

    # plotly_outname <- str_c()
    #
    # htmlwidgets::saveWidget(as_widget(plotly), plotly_outname)
    
    ## ============================ create input file =============================
    
    ### ============================ merge DO with baro and hobo ===================
    
    ### ============================ calculate depth ===================
    
    ### ============================ output cleaned input file with headers ===================
    
    }
