# RC2 spatial study - Multiple linear regression 
# ER_sed
# X Lin April 18 2023
################################################################################################
# Read in data 
################################################################################################
rm(list=ls(all=TRUE))
outdir<-'./Figures/MLR_Analysis_Figures/VGC'
library(tidyverse)
###############################################################
## density plots 
ERtot <- read.csv(file.path('./Published_Data','mean_ERtot_bestSiteIDs.csv'))

#colors <- c(expression("median ER"[wc]*"") = "blue", expression("ER"[wc]*" lit") = "black")

#ERwc <- cdata$ERwc_Square[(cdata$ERsed_Square<=0)&(!is.na(cdata$ERsed_Square))] #cdata$ERwc_Square[cdata$ERtotal_Square<=0]
sdata <- read_csv('./v2_SSS_Water_Sediment_Total_Respiration_GPP.csv',comment = '#',na = c('-9999','N/A'))
colnames(sdata) = c('Site_ID','Parent_ID','ERtotal_Square','ERwc_Square','ERsed_Square','GPP')

p1 <- ggplot() + 
  geom_density(data=ERtot[ERtot$ERtot<=0,], aes(x=ERtot,colour="lit",fill='lit'),adjust = 6,alpha=1)+
  geom_density(data=sdata[sdata$ERsed_Square<=0,], aes(x=ERsed_Square,colour="sed",fill='sed'),adjust = 6,alpha=0.8)+
  geom_density(data=sdata[sdata$ERtotal_Square<=0,], aes(x=ERtotal_Square,colour="tot",fill='tot'),adjust = 6,alpha=0.8)+
  # geom_density(data=sdata[sdata$ERwc_Square<=0,], aes(x=ERwc_Square,colour="wc",fill='wc'),adjust = 6,alpha=0.8)+
  # geom_vline(aes(xintercept=median(na.omit(sdata$ERtotal_Square[sdata$ERtotal_Square<=0]))),color="grey32",  size=1)+
  # geom_vline(aes(xintercept=median(na.omit(ERtot$ERtot[ERtot$ERtot<=0]))),color="black",  size=1, linetype = 'dashed')+
  # geom_vline(aes(xintercept=median(na.omit(sdata$ERsed_Square[sdata$ERsed_Square<=0]))),color="darkgreen",  size=1,linetype = "dashed")+
  # geom_vline(aes(xintercept=median(sdata$ERwc_Square[sdata$ERwc_Square<=0])),color="blue",  size=1,linetype = "dashed")+
  geom_vline(aes(xintercept=median(sdata$ERtot[ERtot$ERtot<=0])),color="darkgreen",  size=1,linetype = "dashed")+
  labs(x = expression(bold(paste("Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")"))), y = 'Density')+
  scale_fill_manual("",breaks = c("tot",'sed','lit'),labels = c(expression("ER"[tot]*"(YRB)"),expression("ER"[sed]*"(YRB)"),expression("ER"[tot]*"(Lit) ")),
                    values = c('darkgrey','lightgreen',"black"))+
  scale_colour_manual("",breaks = c("tot","sed",'lit'),labels = c(expression("ER"[tot]*""),expression("ER"[sed]*""),expression("ER"[tot]*"(Lit)")),
                      values = c('grey','darkgreen',"black")
  )+
  scale_linetype_manual("",breaks = c("tot","sed"),labels = c(expression("ER"[tot]*""),expression("ER"[sed]*"")),
                        values = c('solid',"dashed"))+theme_classic()+
  #theme(legend.position ="none")
  theme(
    legend.position = c(.2, .95),
    legend.justification = c( "top"),
    #legend.margin = margin(6, 4, 6, 6),
    legend.text = element_text(size=12,hjust = 0), #, margin = margin(l = 0, r = 5, unit = "pt")
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.box.just = "right"
  )

ggsave(plot = p1, filename =file.path(outdir,
                                      paste0('ERtot_sed_density_plot48wc','.pdf')),
       width = 5,height = 3 )

library(ggplot2)
library(ggpmisc)

# Scatter plot with linear regression line
p2 <- ggplot(sdata, aes(x = GPP, y = ERtotal_Square)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = expression(paste("GPP"*" (g O"[2]*" m"^-2*" day"^-1*")")), y = expression(paste("ER"[tot]*" (g O"[2]*" m"^-2*" day"^-1*")"))) +  # Add axis labels
  theme_bw()+
  stat_fit_glance(method = "lm", geom = "text", aes(label = paste("R² =", round(..r.squared.., digits = 3),
                                                                  "p =", signif(..p.value.., digits = 3))))

ggsave(plot = p2, filename =file.path(outdir,
                                      paste0('ERtot_GPP','.pdf')),
       width = 5,height = 3 )

p3 <- ggplot(sdata, aes(x = GPP, y = ERsed_Square)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(x = expression(paste("GPP"*" (g O"[2]*" m"^-2*" day"^-1*")")), y = expression(paste("ER"[sed]*" (g O"[2]*" m"^-2*" day"^-1*")"))) +  # Add axis labels
  theme_bw()+
  stat_fit_glance(method = "lm", geom = "text", aes(label = paste("R² =", round(..r.squared.., digits = 3),
                                                                  "p =", signif(..p.value.., digits = 3))))

ggsave(plot = p3, filename =file.path(outdir,
                                      paste0('ERsed_GPP','.pdf')),
       width = 5,height = 3 )


############################################################
library(tidyverse)
filtered_sdata <- sdata %>%
  filter(!is.na(ERtotal_Square)) %>%
  filter(ERtotal_Square <= 0)

scatter_plot <- ggplot(filtered_sdata, aes(x = GPP)) +
  geom_point(aes(y = ERtotal_Square), color = "grey32", size = 3) +  # Add ERtot_Square points
  geom_smooth(aes(y = ERtotal_Square), method = "lm", se = FALSE, color = "grey32") +  # Add linear regression line for ERtot_Square
  geom_point(aes(y = ERsed_Square), color = "coral4", size = 3) +  # Add ERsed_Square points
  geom_smooth(aes(y = ERsed_Square), method = "lm", se = FALSE, color = "coral4") +  # Add linear regression line for ERsed_Square
  annotate("text", x = 5, y = -18, label = paste("ERtot R² =", sprintf("%.2f", summary(lm(ERtotal_Square ~ GPP, data = sdata))$r.squared), "\n", "p =", sprintf("%.2e", summary(lm(ERtotal_Square ~ GPP, data = sdata))$coefficients[8])), color = "black", vjust = -0.5) +  # Add R-squared and p-value for ERtot_Square
  annotate("text", x = 5, y = -19, label = paste("ERsed R² =", sprintf("%.2f", summary(lm(ERsed_Square ~ GPP, data = sdata))$r.squared), "\n", "p =", sprintf("%.2e", summary(lm(ERsed_Square ~ GPP, data = sdata))$coefficients[8])), color = "coral4", vjust = 0.5) +  # Add R-squared and p-value for ERsed_Square
  labs(x = expression(paste("Gross Primary Productivity"*" (g O"[2]*" m"^-2*" day"^-1*")")), y = expression(paste("Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")"))) +  # Add axis labels
  theme_bw()  # Set theme


ggsave(plot = scatter_plot, filename =file.path(outdir,
                                      paste0('ERsed_ERtot_GPP','.pdf')),
       width = 5,height = 3 )
