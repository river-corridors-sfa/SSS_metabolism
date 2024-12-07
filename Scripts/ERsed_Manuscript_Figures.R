# ==============================================================================
#
# Make figures for ERsed manuscript
#
# Status: In progress
#
# Note: use blue for water and brown for sediment, change everything from aerobic to ecosystem 
# put contribution map in SI
# 
# ==============================================================================
#
# Author: Brieanne Forbes 
# 3 December 2024
#
# ==============================================================================
library(tidyverse) 
library(segmented)

rm(list=ls(all=T))

# Setting wd to parent folder
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("./..")

# =================================== find files ===============================

ER <- './v2_SSS_Water_Sediment_Total_Respiration_GPP.csv' %>%
  read_csv(comment = '#', na = '-9999') %>%
  mutate(Total_Ecosystem_Respiration= case_when(Total_Ecosystem_Respiration > 0 ~ NA, # set postive ERtot values to NA
                                                TRUE ~ Total_Ecosystem_Respiration))


# pulls gap-filled (QAQCd) data from Bernhardt et al. (2022). Repo: https://github.com/streampulse/metabolism_synthesis/tree/master
ER_lit <- readRDS(url("https://raw.githubusercontent.com/streampulse/metabolism_synthesis/master/output_data/lotic_gap_filled.rds")) %>%
  bind_rows() %>%
  group_by(Site_ID) %>%
  summarise(mean_ER = mean(ER_filled, na.rm = T))

# =============================== set theme ====================================
theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
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
    plot.subtitle = element_text(size = 14),
    legend.title = element_blank()
  )
)

# ====================== Density:ERtot and ERlit =============================

p1 <- ggplot() + 
  geom_density(data = ER, aes(x = Total_Ecosystem_Respiration, color="tot", fill='tot'),adjust = 6)+ #ER total from SM
  geom_vline(aes(xintercept = median(ER$Total_Ecosystem_Respiration, na.rm=TRUE)), color="black",  size=1)+
  geom_density(data = ER_lit, aes(x= mean_ER , colour="tot_lit",fill='tot_lit'),adjust = 6,alpha = 0.5)+ #ERtot from Appling
  geom_vline(aes(xintercept = median(ER_lit$mean_ER, na.rm=TRUE)), color="seagreen", size = 1)+
  labs(x = expression(paste("Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*"))")), y = 'Density')+
  scale_fill_manual("",breaks = c("tot",'tot_lit'),labels = c(expression("ER"[tot]*" (YRB)"),expression("ER"[tot]*" (Lit)")),
                    values = c("black",'seagreen'))+
  scale_colour_manual("",breaks = c("tot",'tot_lit'),labels = c(expression("ER"[tot]*" (YRB)"),expression("ER"[tot]*" (Lit)")),
                      values = c("black",'seagreen')    
  )+
  theme(
    legend.position = c(.2, .95),
    legend.justification = c( "top"),
    legend.text = element_text(size=12,hjust = 0), #, margin = margin(l = 0, r = 5, unit = "pt")
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.box.just = "right"
  )

# ====================== Density:ERtot, ERsed, ERwc =============================

p2 <- ggplot(data=ER) + 
  geom_rect(aes(xmin=min(Water_Column_Respiration,na.rm=TRUE),xmax=max(Water_Column_Respiration,na.rm=TRUE),
                ymin=0,ymax=0.08,colour="wc",fill='wc'), alpha = 0.9)+ #ER WC
  geom_density(data = ER_lit, aes(x=mean_ER,colour="lit",fill='lit'),adjust = 6)+
  geom_density(aes(x=Sediment_Respiration,colour="sed",fill='sed'),adjust = 6,alpha=0.8)+
  geom_density(aes(x=Total_Ecosystem_Respiration,colour="tot",fill='tot'),adjust = 6,alpha=0.6)+
  geom_vline(aes(xintercept=median(Total_Ecosystem_Respiration, na.rm = T)),color="black",  size=1)+
  geom_vline(aes(xintercept=median(Sediment_Respiration, na.rm = T)),color="grey",  size=1)+
  geom_vline(aes(xintercept=median(Sediment_Respiration, na.rm = T)),color="coral4",  size=1, linetype = "dashed")+
  labs(x = expression(paste("Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")), y = 'Density')+
  scale_fill_manual("",breaks = c("tot",'sed','wc', 'lit'),labels = c(expression("ER"[tot]*" (YRB)"),expression("ER"[sed]*" (YRB)"),expression("ER"[wc]*" (YRB) range"),expression("ER"[tot]*" (Lit)")),
                    values = c('grey','coral4','azure2', "black"))+
  scale_colour_manual("",breaks = c("tot","sed",'wc', 'lit'),labels = c(expression("ER"[tot]*" (YRB)"),expression("ER"[sed]*" (YRB)"),expression("ER"[wc]*" (YRB) range"),expression("ER"[tot]*" (Lit)")),
                      values = c('darkgrey','coral4','white', "black")    
  )+
  theme(
    legend.position = c(.2, .95),
    legend.justification = c( "top"),
    legend.text = element_text(size=12,hjust = 0), #, margin = margin(l = 0, r = 5, unit = "pt")
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.box.just = "right",
    legend.title = element_blank()
  )

# ====================== Density: ERwc =============================

p3 <- ggplot(data=ER) +
  geom_density(aes(x=Water_Column_Respiration,colour="wc",fill='wc'),adjust = 6)+
  geom_vline(aes(xintercept=median(Water_Column_Respiration, na.rm = T)),color="blue",  size=1)+
  labs(x = expression(paste("Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")), y = 'Density')+
  scale_fill_manual("",breaks = c('wc'),labels = c(expression("ER"[wc]*" (YRB)")),
                    values = c('skyblue'))+
  scale_colour_manual("",breaks = c('wc'),labels = c(expression("ER"[wc]*" (YRB)")),
                      values = c('blue')
  )+ theme(
    legend.position = c(.2, .95),
    legend.justification = c( "top"),
    legend.text = element_text(size=12,hjust = 0), #, margin = margin(l = 0, r = 5, unit = "pt")
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.box.just = "right"
  )

# ====================== Scatter: GPP vs ERtot+ERsed =============================
# compare linear and segmented regression
# there is barely a difference between the AIC and BIC between the two regressions. 
# Both have a (very) slightly lower value for linear so going with this for simplicity

# fit <- lm(Sediment_Respiration~Gross_Primary_Production, data=ER)
# summary(fit)
# AIC(fit)
# BIC(fit)
# 
# segmented.fit <- segmented(fit, seg.Z = ~Gross_Primary_Production, psi=9,
#                            control =seg.control(display = TRUE, maxit.glm=3))
# summary(segmented.fit)
# AIC(segmented.fit)
# BIC(segmented.fit)


p4 <- ggplot(ER, aes(x = Gross_Primary_Production)) +
  geom_point(aes(y = Total_Ecosystem_Respiration), color = "grey32", size = 3) +  # Add ERtot_Square points
  geom_smooth(aes(y = Total_Ecosystem_Respiration), method = "lm", se = FALSE, color = "grey32") +  # Add linear regression line for ERtot_Square
  geom_point(aes(y = Sediment_Respiration), color = "coral4", size = 3) +  # Add ERsed_Square points
  geom_smooth(aes(y = Sediment_Respiration), method = "lm", se = FALSE, color = "coral4") +  # Add linear regression line for ERsed_Square
  annotate("text", x = 5, y = -18, label = paste("ERtot R2 =", sprintf("%.2f", summary(lm(Total_Ecosystem_Respiration ~ Gross_Primary_Production, data = ER))$r.squared), "\n", "p =", sprintf("%.2e", summary(lm(Total_Ecosystem_Respiration ~ Gross_Primary_Production, data = ER))$coefficients[8])), color = "black", vjust = -0.5) +  # Add R-squared and p-value for ERtot_Square
  annotate("text", x = 5, y = -19, label = paste("ERsed R2 =", sprintf("%.2f", summary(lm(Sediment_Respiration ~ Gross_Primary_Production, data = ER))$r.squared), "\n", "p =", sprintf("%.2e", summary(lm(Sediment_Respiration ~ Gross_Primary_Production, data = ER))$coefficients[8])), color = "coral4", vjust = 0.5) +  # Add R-squared and p-value for ERsed_Square
  labs(x = expression(paste("Gross Primary Productivity"*" (g O"[2]*" m"^-2*" day"^-1*")")), y = expression(paste("Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")))  # Add axis labels
 

# =========== Scatter: Normalized + Rank order: ERtot vs ERhz =================



