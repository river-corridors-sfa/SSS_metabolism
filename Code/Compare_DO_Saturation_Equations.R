# ==============================================================================
#
# Test difference in DO saturation equation we use and the one stream metabolizer
# uses. Test range in temp 0-35 degrees C. Avg DO = 8.6. Avg BP = 945.2.
#
# Status: In progress. 
# ==============================================================================
#
# Author: Brieanne Forbes
# 26 January 2024
#
# ==============================================================================

library(tidyverse)

# ============================ create test data frame ==========================

a0<-2.00907
a1<-3.22014
a2<-4.0501
a3<-4.94457
a4<- -0.256847
a5<- 3.88767

data <- tibble(DO = 8.6,
               Temp = 0:35,
               BP = 945.2) %>%
  mutate(Temp_K = Temp + 273.15, 
         DO_sat_perc_benson = (DO / (exp(-139.34411 + (1.575701 * 1e5 / Temp_K) - (6.642308 * 1e7 / (Temp_K ^ 2)) +(1.2438 * 1e10 / (Temp_K ^ 3)) - (8.621949 * 1e11 / (Temp_K ^ 4))))) * 100,
         tstd = log(298.15 - Temp) / (273.15 + Temp),
         u = 10^(8.10765-(1750.286/(235+Temp))),
         DO_sat_garcia = (exp(a0 + a1*tstd + a2*tstd^2 + a3*tstd^3 + a4*tstd^4+a5*tstd^5))*((BP-u)/(760-u))*1.42905,
         DO_sat_perc_garcia = 100 * (DO / DO_sat_garcia))
