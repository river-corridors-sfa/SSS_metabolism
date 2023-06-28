rm(list=ls());graphics.off()

setwd("C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/SSS_metabolism/SM_analysis/")
library(tidyverse)
data = read.csv("combined_results_updated_040623.csv")

data = data %>% dplyr::select(ERdailymeanmean_gO2.m2day,ERwaterdaily_gO2.m2day,ERseddaily_gO2.m2day)
                              

ggplot(data, aes(x=ERdailymeanmean_gO2.m2day)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,fill = 'white', color = 'black', bins = 40)+
  geom_density(alpha=0.6)+
  theme_bw()
ggsave("Hist_ERtot.png")

ggplot(data, aes(x=ERwaterdaily_gO2.m2day)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,fill = 'white', color = 'black', bins = 50)+
  geom_density(alpha=0.6)+
  theme_bw()
ggsave("Hist_ERwater.png")

ggplot(data, aes(x=ERseddaily_gO2.m2day)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5,fill = 'white', color = 'black', bins = 40)+
  geom_density(alpha=0.6)+
  theme_bw()
ggsave("Hist_ERsed.png")