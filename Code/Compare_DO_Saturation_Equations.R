# ==============================================================================
#
# Test difference in DO saturation equation we used in calibration (Benson and Krauss) 
# and the one stream metabolizer uses (Garcia and Gordon). 

# Test range in temp 0-35 degrees C. SSS Avg DO = 8.6. SSS Avg BP = 945.2 (709.1 mmHg).
#
# Status: In progress. 
# ==============================================================================
#
# Author: Brieanne Forbes
# 30 January 2024
#
# ==============================================================================

library(tidyverse)
library(streamMetabolizer)

# ============================ create test data frame ==========================

a0<-2.00907
a1<-3.22014
a2<-4.0501
a3<-4.94457
a4<- -0.256847
a5<- 3.88767

data <- tibble(DO = 8.6,
               Temp = 0:35,
               BP_mbar = 945.2,
               BP_mmhg = 709.1,
               BP_hpa = 945.2) %>%
  mutate(Temp_K = Temp + 273.15, 
         DO_sat_benson = (exp(-139.34411 + (1.575701 * 1e5 / Temp_K) - (6.642308 * 1e7 / (Temp_K ^ 2)) +(1.2438 * 1e10 / (Temp_K ^ 3)) - (8.621949 * 1e11 / (Temp_K ^ 4)))),
         DO_sat_perc_benson = (DO / (exp(-139.34411 + (1.575701 * 1e5 / Temp_K) - (6.642308 * 1e7 / (Temp_K ^ 2)) +(1.2438 * 1e10 / (Temp_K ^ 3)) - (8.621949 * 1e11 / (Temp_K ^ 4))))) * 100,
         tstd = log((298.15-Temp) / (273.15 + Temp)),
         u = 10^(8.10765-(1750.286/(235+Temp))),
         DO_sat_garcia = (exp(a0 + a1*tstd + a2*tstd^2 + a3*tstd^3 + a4*tstd^4+a5*tstd^5))*((BP_mmhg-u)/(760-u))*1.42905,
         DO_sat_perc_garcia = 100 * (DO / DO_sat_garcia),
         u_atm = exp(11.8571 - (3840.70/(Temp+273.15))-(216961/(Temp+273.15)^2)),
         theta = 0.000975 - (1.426E-05 * Temp) + (6.436E-08*(Temp)^2),
         Barometric_Pressure_in_atm = BP_hpa * 0.000986923,
         Fp = ((Barometric_Pressure_in_atm - u_atm)*(1-(theta*Barometric_Pressure_in_atm)))/ ((1- u_atm)*(1 - theta)),
         DOknot = exp(-139.34411 + (1.575701E5/(Temp+273.15)) - (6.642308E7/(Temp+273.15)^2) + (1.243800E10/(Temp+273.15)^3) - (8.621949E11/(Temp+273.15)^4)),
         DOsat_correction = DOknot * Fp,
         USGS_Function = calc_DO_sat(Temp, BP_mbar),
         USGS_Function_garcia_benson = calc_DO_sat(Temp, BP_mbar, model = "garcia-benson"),
         diff = DO_sat_garcia - DOsat_correction)

plot <- ggplot(data = data, aes(x = DO_sat_benson, y = DO_sat_garcia)) +
  geom_point()+
  geom_abline(slope=1, intercept = 0, color = 'grey')

plot2 <- ggplot(data = data, aes(x = DOknot, y = DO_sat_garcia)) +
  geom_point()+
  geom_abline(slope=1, intercept = 0, color = 'grey')

plot3 <- ggplot(data = data, aes(x = DOknot, y = DO_sat_benson)) +
  geom_point()+
  geom_abline(slope=1, intercept = 0, color = 'grey')

plot4 <- ggplot(data = data, aes(x = DO_sat_garcia, y = USGS_Function)) +
  geom_point()+
  geom_abline(slope=1, intercept = 0, color = 'grey')








