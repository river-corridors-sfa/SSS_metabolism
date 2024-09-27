# ==============================================================================
#
# Exploratory miniDOT data cleaning
# 
# Status: 
#
# ==============================================================================
#
# Author: Brieanne Forbes (brieanne.forbes@pnnl.gov)
# 19 September 2024

# remove all files
rm(list=ls(all=TRUE))

# ==============================================================================

library(tidyverse)
library(zoo)
library(seismicRoll)
library(plotly)

# ================================= User inputs ================================

data_dir <- 'C:/Users/forb086/OneDrive - PNNL/Spatial Study 2022/04_Minidot/05_PublishReadyData'

# ============================ find and read files =============================

files <- list.files(data_dir, 'Water_DO_Temp.csv', full.names = T)

# # parent IDs of sites that are over saturation, not going to use so filtering out
# over_saturation <- 'SSS045|SSS044|SSS011|SSS010|SSS022|SSS048|SSS033|SSS041|SSS012|SSS042|SSS007|SSS005|SSS017|SSS001|SSS013|SSS016'
# 
# files <- files[!grepl(over_saturation,files)]

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

for(file in files){
  
  data <- read_csv(file, comment = '#', na = c('', '-9999', -9999, NA, 'N/A'))

  subset <- data %>%
    filter(minute(DateTime) %% 15 == 0)
#   
#   ggplot(subset, aes(x = DateTime, y = Dissolved_Oxygen)) + 
#     geom_point() +
#     geom_smooth(method = 'gam', formula = y ~ s(x, k = 150))
#   
#   # ============================ test spline interp =============================
#   
#   interp <-   subset %>%
#     mutate(Dissolved_Oxygen_na = case_when((DateTime >= as_datetime('2022-07-30 11:28:00') & DateTime <= as_datetime('2022-07-30 23:55:00')) ~ NA,
#                                         TRUE ~ Dissolved_Oxygen),
#            flag = case_when((DateTime >= as_datetime('2022-07-30 11:28:00') & DateTime <= as_datetime('2022-07-30 23:55:00')) ~ 'Interpolated',
#                                         TRUE ~ 'OG data'),
#            Dissolved_Oxygen_linear = round(na.approx(Dissolved_Oxygen_na, na.rm = FALSE), 3),
#            Dissolved_Oxygen_spline = round(na.spline(Dissolved_Oxygen_na, na.rm = FALSE), 3))
#                                                 
#   # DO data
#   ggplot(interp %>% filter(date(DateTime) =='2022-07-30'|date(DateTime) =='2022-07-31'), 
#                  aes(x = DateTime, y = Dissolved_Oxygen)) + 
#     geom_point()
#   
#   # spline interp DO data 
#   ggplot(interp %>% filter(date(DateTime) =='2022-07-30'|date(DateTime) =='2022-07-31'), 
#          aes(x = DateTime, y = Dissolved_Oxygen_spline)) + 
#     geom_point(aes(color = flag)) +
#     scale_color_manual(values = c("red", "black"))
#   
#   # scatter DO vs spline interp DO 
#   ggplot(interp %>% filter(flag == 'Interpolated'), 
#          aes(x = Dissolved_Oxygen, y = Dissolved_Oxygen_spline)) + 
#     geom_point() +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed")
#   
#   # ============================ test seismicRoll =============================
#     
#   window_size <- 20
#   threshold <- 1
#   
#   hampel <- subset %>% 
#     mutate(
#       rolling_mean = roll_mean(Dissolved_Oxygen, window_size, align = "right"),
#       rolling_sd = roll_sd(Dissolved_Oxygen, window_size, align = "right"),
#       is_outlier = abs(Dissolved_Oxygen - rolling_mean) > threshold * rolling_sd
#     ) %>%
#     mutate(
#       rolling_mean = replace_na(rolling_mean, NA),
#       rolling_sd = replace_na(rolling_sd, NA),
#       is_outlier = replace_na(is_outlier, FALSE)
#     )
#     
#   ggplot(hampel, aes(x = DateTime, y = Dissolved_Oxygen)) +
#     geom_point(aes(color = is_outlier)) +
#     scale_color_manual(values = c('black', 'red')) +
#     labs(
#       title = 'Dissolved Oxygen Timeseries with Outliers',
#       y = 'Dissolved Oxygen',
#       color = 'Outlier'
#     )
#  
#   window_size_2 <- 20
#   threshold_2 <- 1
#   
#   hampel_2 <- subset %>%
#     select(DateTime, Dissolved_Oxygen) %>%
#     arrange(DateTime) %>%
#     mutate(roll_hampel = roll_hampel(Dissolved_Oxygen, window_size_2),
#            outlier = case_when(roll_hampel >= threshold_2 ~ TRUE,
#                                TRUE ~ FALSE))
#   
#     
#   plot <- ggplot(hampel_2, aes(x = DateTime, y = Dissolved_Oxygen)) +
#     geom_point(aes(color = outlier)) +
#     scale_color_manual(values = c('black', 'red')) +
#     labs(
#       title = 'Dissolved Oxygen Timeseries with Outliers',
#       y = 'Dissolved Oxygen',
#       color = 'Outlier'
#     )
#   
#   ggplotly(plot)
# 
#     
#   find_outliers <- findOutliers(
#     subset$Dissolved_Oxygen,
#     n = 20, # window size
#     thresholdMin = 4, # median abs stdev, lower = more outliers
#     selectivity = 0.25, # 0-1, smaller = more outliers
#     increment = 1, # doesnt change
#     fixedThreshold = TRUE
#   )
#   
# find_outliers_df <- subset %>%
#   mutate(outlier = row_number() %in% find_outliers)
# 
# plot2 <- ggplot(find_outliers_df, aes(x = DateTime, y = Dissolved_Oxygen)) +
#   geom_point(aes(color = outlier)) +
#   scale_color_manual(values = c('black', 'red')) +
#   labs(
#     title = 'Dissolved Oxygen Timeseries with Outliers',
#     y = 'Dissolved Oxygen',
#     color = 'Outlier'
#   )
# 
# ggplotly(plot2)

# ============================ chat gpt test other pacakges =============================
# # Load necessary libraries
# library(forecast)     # For ARIMA and tsoutliers
# library(anomalize)    # For time series decomposition and anomaly detection
# library(tidyverse)    # For data manipulation
# library(tibbletime)   # For time-based manipulation
# library(robustbase)   # For robust regression
# library(tsoutliers)   # For time series outlier detection
# library(bfast)        # For time series break and outlier detection
# 
# 
# # Convert to time series object
# ts_data <- ts(subset$Dissolved_Oxygen, frequency = 96)
# 
# # Forecast package - ARIMA and tsoutliers method
# forecast_outliers <- function(ts_data) {
#   fit <- auto.arima(ts_data)
#   outliers <- tsoutliers(fit)
#   is_outlier <- rep(FALSE, length(ts_data))
#   if (!is.null(outliers$index)) {
#     is_outlier[outliers$index] <- TRUE
#   }
#   return(is_outlier)
# }
# 
# # Anomalize package method
# anomalize_outliers <- function(subset) {
#   anomalies <- subset %>%
#     time_decompose(Dissolved_Oxygen, method = "stl", frequency = "daily") %>%
#     anomalize(remainder) %>%
#     time_recompose()
#   return(anomalies$anomaly == "Yes")
# }
# 
# # Robustbase package - Robust regression method
# robust_outliers <- function(subset) {
#   robust_fit <- lmrob(Dissolved_Oxygen ~ as.numeric(datetime), data = subset)
#   residuals <- abs(residuals(robust_fit))
#   threshold <- 2 * mad(residuals)
#   return(residuals > threshold)
# }
# 
# # Tsoutliers package method
# tsoutliers_outliers <- function(ts_data) {
#   outliers <- tso(ts_data)
#   is_outlier <- rep(FALSE, length(ts_data))
#   if (!is.null(outliers$outliers)) {
#     is_outlier[outliers$outliers$ind] <- TRUE
#   }
#   return(is_outlier)
# }
# 
# # Bfast package method
# bfast_outliers <- function(ts_data) {
#   fit <- bfast(ts_data, season = "harmonic")
#   is_outlier <- !is.na(fit$output[[1]]$outliers)
#   return(is_outlier)
# }
# 
# # Create a data frame for outliers detection results
# outlier_tests <- subset %>%
#   mutate(
#     forecast_outliers = forecast_outliers(ts_data),
#     # anomalize_outliers = anomalize_outliers(subset),
#     robust_outliers = robust_outliers(subset),
#     tsoutliers_outliers = tsoutliers_outliers(ts_data),
#     bfast_outliers = bfast_outliers(ts_data)
#   )

# ============================ test pracma package =============================
# 
# library(pracma)
# 
# pracma <- hampel(subset$Dissolved_Oxygen, 20, t0 = 3)
# 
# df <- subset
# 
# df$filtered_DO <- hampel(df$Dissolved_Oxygen, k = 20, t0 = 1)$y     
# 
# # Identify outliers  
# df$outlier <- df$Dissolved_Oxygen != df$filtered_DO
# 
# plot3 <- ggplot(df, aes(x = DateTime, y = Dissolved_Oxygen)) +
#   geom_point(aes(color = outlier)) +
#   scale_color_manual(values = c('black', 'red')) +
#   labs(
#     title = 'Dissolved Oxygen Timeseries with Outliers',
#     y = 'Dissolved Oxygen',
#     color = 'Outlier'
#   )
# 
# ggplotly(plot3)

# ============================ test forcast =============================

library(forecast)
library(scales)

# Convert your data to a time series object
do_ts <- ts(subset$Dissolved_Oxygen, frequency = 96)

# Clean the time series by detecting and correcting outliers
cleaned_do <- tsclean(do_ts)

# Add this information back to your original data frame
subset$cleaned_DO <- as.numeric(cleaned_do)


parent_ID <- str_extract(file, "[A-Z]{3}\\d{3}")

# Plot the original and cleaned series
plot4 <- ggplot(subset, aes(x = DateTime)) +
  geom_point(aes(y = Dissolved_Oxygen, color = "Original"), size = 3.5) +  # Original DO points
  geom_point(aes(y = cleaned_DO, color = "Cleaned"), size = 1.5) +        # Cleaned DO points
  labs(title = str_c("Parent ID: ", parent_ID, "                 Cleaned Dissolved Oxygen Data with 'Forecast' package"),
       x = "DateTime", y = "Dissolved Oxygen (mg/L)", color = NULL) +  # Remove legend title
  scale_color_manual(values = c("Original" = "grey", "Cleaned" = "black")) +  # Custom colors
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
  theme(legend.position = c(0.1, 0.85))

plot5 <- ggplot(subset, aes(x = DateTime)) +
  geom_point(aes(y = cleaned_DO, color = "Cleaned"), size = 1.5) +  # Cleaned DO points only
  labs(title = str_c("Parent ID: ", parent_ID, "                 Cleaned Dissolved Oxygen Data with 'Forecast' package"),
       x = "DateTime", y = "Dissolved Oxygen (mg/L)", color = NULL) +  # Remove legend title
  scale_color_manual(values = c("Cleaned" = "black")) +  # Custom color for cleaned points
  scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M")) +
  theme(legend.position = c(0.1, 0.85))  # Position legend

p <- subplot(plot4, plot5, nrows = 2, shareY = TRUE)


plot_name <- str_c('C:/Brieanne/GitHub/SSS_metabolism/Cleaned_DO/', tools::file_path_sans_ext(basename(file)),'_CLEANED.html')

htmlwidgets::saveWidget(as_widget(p), plot_name)

out_file_name <- str_c('C:/Brieanne/GitHub/SSS_metabolism/Cleaned_DO/', tools::file_path_sans_ext(basename(file)),'_CLEANED.csv')

write_csv(subset, out_file_name)

}
