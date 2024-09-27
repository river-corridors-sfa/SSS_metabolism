# RC2 spatial study - Multiple linear regression 
# ER_sed
# X Lin April 18 2023
################################################################################################
# Read in data 
################################################################################################
rm(list=ls(all=TRUE))
outdir<-'./Figures/MLR_Analysis_Figures/VGC'
library(tidyverse); library(ggpubr)
#################################
sdata <- read_csv('./v2_SSS_Water_Sediment_Total_Respiration_GPP.csv',comment = '#',na = c('-9999','N/A'))
colnames(sdata) = c('Site_ID','Parent_ID','ERtotal_Square','ERwc_Square','ERsed_Square','GPP')

# Read in model data
model =  read_csv('./v2_SSS_ER_d50_TotalOxygenConsumed.csv',comment = '#',na = c('-9999','N/A'))
names(model)[4] = 'Total_Oxygen_Consumed'
sdata = merge(sdata,model, by = c('Site_ID','Parent_ID'), all = TRUE)

sdata =sdata[(sdata$ERsed_Square<=0)&(!is.na(sdata$ERsed_Square)),]

# scatterplot for ERtot and model data
# rank order
rank_tot<- rank(sdata$ERtotal_Square)
rank_model<- rank(sdata$Total_Oxygen_Consumed)

plotdf <-data.frame(x=rank_tot,y=rank_model)
iplot <- plotdf%>%
  ggplot(aes(x=x,y=y))+
  geom_point(alpha = 0.5,size=3)+
  #geom_smooth(method="lm", se=FALSE,color='black')+
  #stat_cor(label.y = -12,color='red',size=4)+ 
  stat_cor(method = "spearman",cor.coef.name = c( "rho"),
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 17,label.y = 36,color='black',size=4)+ 
  xlab(expression(paste("Rank Order - Observed Total Ecosystem Respiration")))+
  ylab(expression(paste("Rank Order - Predicted Hyporheic Zone Respiration")))+
  #theme_httn+ 
  theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red") +
  theme_classic()#,limits = c(0,0.075)
ggsave(plot = iplot, filename =file.path(outdir,'ERsed',
                                         paste0('ERtot_vs_total_oxygen_consumed_rank_order_spearman_noline','.png')),
       width = 4,height = 4 )


##scatterplot for scale ERtot and model data
x <- scale(sdata$ERtotal_Square, center = TRUE, scale = TRUE)
y <- scale(sdata$Total_Oxygen_Consumed, center = TRUE, scale = TRUE)

plotdf <-data.frame(x=x,y=y)
iplot <- plotdf%>%
  ggplot(aes(x=x,y=y))+
  geom_point(alpha = 0.5,size=3)+
  #geom_smooth(method="lm", se=FALSE)+
  #geom_text(x=-2.8, y = -0.7, label = lm_eqn(plotdf),color='red',size=4, parse = TRUE)+
  #stat_cor(label.y = -12,color='red',size=4)+ 
  # stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), 
  #          label.y = -1,color='red',size=4)+ 
  xlab(expression(paste("Normalized Observed Total Ecosystem Respiration")))+
  ylab(expression(paste("Normalized Predicted Hyporheic Zone Respiration")))+
  #theme_httn+ 
  theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red")+
  theme_classic()#,limits = c(0,0.075)
ggsave(plot = iplot, filename =file.path(outdir,'ERsed',
                                         paste0('ERtot_vs_total_oxygen_consumed_scaled_dropline','.png')),
       width = 4,height = 4 )
###################################################################ERsed
####################################################

# scatterplot for ERtot and model data
# rank order
rank_tot<- rank(sdata$ERsed_Square)
rank_model<- rank(sdata$Total_Oxygen_Consumed)

plotdf <-data.frame(x=rank_tot,y=rank_model)
iplot <- plotdf%>%
  ggplot(aes(x=x,y=y))+
  geom_point(alpha = 0.5,size=3)+
  #geom_smooth(method="lm", se=FALSE,color='black')+
  #stat_cor(label.y = -12,color='red',size=4)+ 
  stat_cor(method = "spearman",cor.coef.name = c( "rho"),
           aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 17,label.y = 36,color='black',size=4)+ 
  xlab(expression(paste("Rank Order - Observed Sed Ecosystem Respiration")))+
  ylab(expression(paste("Rank Order - Predicted Hyporheic Zone Respiration")))+
  #theme_httn+ 
  theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red") +
  theme_classic()#,limits = c(0,0.075)
ggsave(plot = iplot, filename =file.path(outdir,'ERsed',
                                         paste0('ERsed_vs_total_oxygen_consumed_rank_order_spearman_noline','.png')),
       width = 4,height = 4 )


##scatterplot for scale ERsed and model data
x <- scale(sdata$ERsed_Square, center = TRUE, scale = TRUE)
y <- scale(sdata$Total_Oxygen_Consumed, center = TRUE, scale = TRUE)
plotdf <-data.frame(x=x,y=y)
iplot <- plotdf%>%
  ggplot(aes(x=x,y=y))+
  geom_point(alpha = 0.5,size=3)+
  #geom_smooth(method="lm", se=FALSE)+
  #geom_text(x=-2.8, y = -0.7, label = lm_eqn(plotdf),color='red',size=4, parse = TRUE)+
  #stat_cor(label.y = -12,color='red',size=4)+ 
  # stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")), 
  #          label.y = -1,color='red',size=4)+ 
  xlab(expression(paste("Normalized Observed Sed Ecosystem Respiration")))+
  ylab(expression(paste("Normalized Predicted Hyporheic Zone Respiration")))+
  #theme_httn+ 
  theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red")+
  theme_classic()#,limits = c(0,0.075)
ggsave(plot = iplot, filename =file.path(outdir,'ERsed',
                                         paste0('ERsed_vs_total_oxygen_consumed_scaled_dropline','.png')),
       width = 4,height = 4 )

#########################################################


png(file.path(outdir,'ERsed',paste0('ER_tot_vs_total_oxygen_consumed_and_rank_order',".png")),
    width = 8, height = 4, units = 'in', res = 600)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3.1,3.1,1,1))
plot(sdata$ERtotal_Square,sdata$Total_Oxygen_Consumed,pch=20,cex.lab=0.85,cex.axis=0.85,
     xlab=expression(paste("Observed Total Ecosystem Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")), 
     ylab=expression(paste("Predicted Hyporheic Zone Respiration "*" (g O"[2]*" m"^-2*" day"^-1*")")))
plot(rank_tot,rank_model,pch=20,cex.lab=0.85,cex.axis=0.85,
     xlab=expression(paste("Rank Order - Observed Total Ecosystem Respiration")), 
     ylab=expression(paste("Rank Order - Predicted Hyporheic Zone Respiration")))
dev.off()



# scatterplot for model ERsed and Hflux and D50
sdata0 <- na.omit(sdata[,c('Total_Oxygen_Consumed','hz_spring','D50_m')])

p1 <-ggplot(sdata0, aes(hz_spring, Total_Oxygen_Consumed)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)+
  ylab(expression(paste("Predicted Sediment Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")))+
  xlab('Hyporheic exchange flux')
ggsave(plot = p1, filename =file.path(outdir,'ERsed','ERsed_scatterplot',
                                         paste0('Scatter_','Total_Oxygen_Consumed','_vs_','Hflux','.png')),
       width = 6,height = 4 )


p2 <-ggplot(sdata0, aes(D50_m, Total_Oxygen_Consumed)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)+
  ylab(expression(paste("Predicted Sediment Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")))+
  xlab('D50')
ggsave(plot = p2, filename =file.path(outdir,'ERsed','ERsed_scatterplot',
                                      paste0('Scatter_','Total_Oxygen_Consumed','_vs_','D50','.png')),
       width = 6,height = 4 )



################################################################################################
## scatter plots using original values
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" ,"Discharge","TSS", 'TN','NPOC',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'PctShrb2019Ws',"AridityWs",
          'D50_m',"hz_spring","Chlorophyll_A",'streamorde','GPP_Square')
for (v in 1:length(xvars)){
  iplot <- sdata %>% 
    ggplot(aes_string(x=xvars[v],y='ERsed_Square'))+
    geom_point(alpha = 0.5,size=3)+
    geom_smooth(method="lm", se=FALSE)+
    #stat_cor(label.y = -12,color='red',size=4)+ 
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
             label.y = -0.25,color='red',size=4)+ 
    #xlab(expression(bold(paste("Total Nitrogen"))))+
    xlab(xvars[v])+
    ylab(expression(bold(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")"))))+
    # annotation_logticks(size = 0.75, sides = "tblr",
    #                     short = unit(0,"mm"),
    #                     mid = unit(0,"mm"),
    #                     long = unit(1.5,"mm"))+ #sides = "tblr"
    theme_httn+
    theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red") #,limits = c(0,0.075)
  ggsave(plot = iplot, filename =file.path(outdir,'ERsed','ERsed_scatterplot',
                                           paste0('Original_Scatter_','ERsed','_vs_',xvars[v],'.png')),
         width = 6,height = 4 )
}



# locally measured variables : TN(Total nitrogen),log(discharge), Water Temp, Log(D50), Log(Slope) and any other explanatory variables

breaks <- 10^(-10:10)
breaks_c <- 10^seq(-10,10,by=4)
#breaks_c <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

#sdata =cdata[cdata$ERsed_Square<=0,]
pvars <-c("HOBO_Temp", 'TN', "Discharge","AridityWs",'D50_m','Slope','totdasqkm','TSS',
          'Mean_Depth','Velocity',"hz_spring","Chlorophyll_A") #,'Ratio'
xlabels <-c(expression(bold("Temperature (?C)")),expression(bold(paste("Total Nitrogen"))),
            expression(bold(paste("Discharge"*" (m s"^-1*")"))),
            expression(bold(paste("Aridity"))),expression(bold(paste("D50"*" (m)"))),
            expression(bold(paste("Slope"))),
            expression(bold("Total Drained Area (km"^2*")")),
            expression(bold(paste("TSS"))),
            expression(bold("Average Depth (m)")),
            expression(bold(paste("Velocity"*" (m s"^-1*")"))),
            expression(bold(paste("Hyporheic exchange flux"*""))),
            expression(bold(paste("Chlorophyll_A")))
             #expression(bold(paste("Ratio(Depth/D50)")))
)

for (v in 1:length(pvars)){
  if (pvars[v] %in% c("HOBO_Temp",'TN',"AridityWs",'Velocity',"hz_spring")){
    iplot <- sdata %>% 
      ggplot(aes_string(x=pvars[v],y='ERsed_Square',color = 'Slope'))+
      geom_point(alpha = 0.5,size=3)+
      geom_smooth(method="lm", se=FALSE)+
      #stat_cor(label.y = -12,color='red',size=4)+ 
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
               label.y = -12,color='red',size=4)+ 
      #xlab(expression(bold(paste("Total Nitrogen"))))+
      xlab(xlabels[v])+
      ylab(expression(bold(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")"))))+
      # annotation_logticks(size = 0.75, sides = "tblr",
      #                     short = unit(0,"mm"),
      #                     mid = unit(0,"mm"),
      #                     long = unit(1.5,"mm"))+ #sides = "tblr"
      theme_httn+
      theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red") #,limits = c(0,0.075)
  }else{
    iplot <- sdata %>% 
      ggplot(aes_string(x=pvars[v],y='ERsed_Square',color = 'Slope'))+
      geom_point(alpha = 0.5,size=3)+
      geom_smooth(method="lm", se=FALSE)+
      #stat_cor(label.y = -12,color='red',size=4)+ 
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
               label.y = -12,color='red',size=4)+ 
      #xlab(expression(bold(paste("Discharge"*" (m s"^-1*")"))))+
      xlab(xlabels[v])+
      ylab(expression(bold(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")"))))+
      scale_x_log10(breaks = breaks, 
                    labels = trans_format("log10", math_format(10^.x)))+
      # scale_y_log10(breaks = breaks_c,
      #               labels = trans_format("log10", math_format(10^.x)))+
      annotation_logticks(size = 0.75, sides = "tb",
                          short = unit(1,"mm"),
                          mid = unit(2,"mm"),
                          long = unit(3,"mm"))+ #sides = "tblr"
      theme_httn+
      theme(legend.position = "right")+scale_color_gradient(low = "blue", high = "red") #,limits = c(0,0.075)
  }
  ggsave(plot = iplot, filename =file.path(outdir,'ERsed','ERsed_scatterplot',
                                           paste0('Scatter_',pvars[v],'_vs_','ERsed','.png')),
         width = 6,height = 4 )
  
}





###############################################################
## scatter plots 
# locally measured variables and other explanatory variables vs total drained area
pvars <-c("HOBO_Temp", 'TN', "Discharge","AridityWs",'D50_m','Slope')
ylabels <-c(expression(bold("Temperature (?C)")),expression(bold(paste("Total Nitrogen"))),
            expression(bold(paste("Discharge"*" (m s"^-1*")"))),
            expression(bold(paste("Aridity"))),expression(bold(paste("D50"*" (m)"))),
            expression(bold(paste("Slope"))))
breaks <- 10^(-10:10)
breaks_c <- 10^(-10:10) #10^seq(-10,10,by=2)

for (v in 1:length(pvars)){
  if (pvars[v] %in% c("HOBO_Temp",'TN',"AridityWs")){
    iplot <- sdata %>% 
      ggplot(aes_string(x='totdasqkm',y=pvars[v]))+
      geom_point(alpha = 0.5,size=3)+
      geom_smooth(method="lm", se=FALSE)+
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
               label.x = 2,color='red',size=4)+ 
      #stat_cor(label.y = 0.8*max(sdata[,pvars[v]],na.rm = TRUE),color='red',size=4)+ 
      #xlab(expression(bold(paste("Total Nitrogen"))))+
      ylab(xlabels[v])+
      #ylim(0.8*min(sdata[,pvars[v]],na.rm = TRUE), 1.1*max(sdata[,pvars[v]],na.rm = TRUE))+
      #ylab(expression(bold(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")"))))+
      xlab(expression(bold(paste("Total Drained Area"*" (km"^2*")"))))+
      scale_x_log10(breaks = breaks,labels = trans_format("log10", math_format(10^.x)))+theme_httn+
      annotation_logticks(size = 0.75, sides = "tb",
                          short = unit(1,"mm"),
                          mid = unit(2,"mm"),
                          long = unit(3,"mm")) #sides = "tblr")
    
    
  }else{
    iplot <- sdata %>% 
      ggplot(aes_string(x='totdasqkm',y=pvars[v]))+
      geom_point(alpha = 0.5,size=3)+
      geom_smooth(method="lm", se=FALSE)+
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
               label.x = 2,color='red',size=4)+ 
      #xlab(expression(bold(paste("Total Nitrogen"))))+
      ylab(xlabels[v])+
      #ylim(0.9*min(sdata[,pvars[v]],na.rm = TRUE), 1.1*max(sdata[,pvars[v]],na.rm = TRUE))+
      #ylab(expression(bold(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")"))))+
      xlab(expression(bold(paste("Total Drained Area"*" (km"^2*")"))))+
      scale_x_log10(breaks = breaks,
                    labels = trans_format("log10", math_format(10^.x)))+
      scale_y_log10(breaks = breaks_c,
                    labels = trans_format("log10", math_format(10^.x)))+
      annotation_logticks(size = 0.75, sides = "tblr",
                          short = unit(1,"mm"),
                          mid = unit(2,"mm"),
                          long = unit(3,"mm"))+ #sides = "tblr"
      theme_httn
  }
  
  ggsave(plot = iplot, filename =file.path(outdir,'ERwater','ERwater_scatterplot',
                                           paste0('Scatter_totdasqkm','_vs_',pvars[v],'.png')),
         width = 5,height = 4 )
  
}



###############################################################
## density plots 
ERtot <- read.csv(file.path('./Published_Data','mean_ERtot_bestSiteIDs.csv'))

#colors <- c(expression("median ER"[wc]*"") = "blue", expression("ER"[wc]*" lit") = "black")

#ERwc <- cdata$ERwc_Square[(cdata$ERsed_Square<=0)&(!is.na(cdata$ERsed_Square))] #cdata$ERwc_Square[cdata$ERtotal_Square<=0]
ERwc <- cdata$ERwc_Square

p1 <- ggplot() + 
  geom_density(data=sdata[sdata$ERtotal_Square<=0,], aes(x=ERtotal_Square,colour="tot",fill='tot'),adjust = 6)+
  geom_density(data=sdata[sdata$ERsed_Square<=0,], aes(x=ERsed_Square,colour="sed",fill='sed'),adjust = 6,alpha=0.8)+
  geom_vline(aes(xintercept=median(sdata$ERtotal_Square[sdata$ERtotal_Square<=0])),color="black",  size=1)+
  geom_vline(aes(xintercept=median(sdata$ERsed_Square[sdata$ERsed_Square<=0])),color="grey",  size=1,linetype = "dashed")+
  #geom_vline(data=ERwc2, aes(xintercept=ERwc,color='lit'),linetype="dashed")+
  #scale_x_cut(breaks=c(-0.13), which=c(1), scales=c(0.25, 1),space = 0.2)+ theme_bw()+ 
  # xlab(expression("ER"[wc]*"")) +
  # ylab('Density')  + theme_classic()+ #+ scale_fill_grey()
  #xlim(-25, 0)+
  labs(x = expression(bold(paste("Aerobic Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")"))), y = 'Density')+
  geom_rect(aes(xmin=min(ERwc,na.rm=TRUE),xmax=max(ERwc,na.rm=TRUE),
                ymin=0,ymax=0.04,colour="wc",fill='wc'))+ #ER WC
  scale_fill_manual("",breaks = c("tot",'sed','wc'),labels = c(expression("ER"[tot]*""),expression("ER"[sed]*""),expression("ER"[wc]*" range")),
                    values = c("black",'grey','skyblue'))+
  scale_colour_manual("",breaks = c("tot","sed",'wc'),labels = c(expression("ER"[tot]*""),expression("ER"[sed]*""),expression("ER"[wc]*" range")),
                      values = c("black",'grey','blue')    
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

ggsave(plot = p1, filename =file.path(outdir,'ERsed',
                                      paste0('ERtot_sed_density_plot48wc','.png')),
       width = 5,height = 3 )


p2 <- ggplot() + 
  geom_density(data=cdata, aes(x=ERwc_Square,color='wc',fill='wc'),adjust = 4)+
  geom_vline(aes(xintercept=median(cdata$ERwc_Square,na.rm=TRUE)),color="blue",  size=1)+
  labs(x = expression(bold(paste("Aerobic Respiration (g O"[2]*" m"^-2*" day"^-1*")"))), y = 'Density')+
  scale_fill_manual("",breaks = c('wc'),labels = c(expression("ER"[wc]*"")),
                    values = c("skyblue"))+
  scale_colour_manual("",breaks = c("wc"),labels = c(expression("ER"[wc]*"")),values = c("blue"),aesthetics = c("colour"))+
  scale_linetype_manual("",breaks = c("wc"),labels = c(expression("ER"[wc]*"")),
                        values = c('solid'))+theme_classic()+
  #theme(legend.position ="none")
  theme(
    legend.position = c(.2, .9),
    legend.justification = c( "top"),
    #legend.margin = margin(6, 4, 6, 6),
    legend.text = element_text(size=12,hjust = 0),#, margin = margin(l = 0, r = 5, unit = "pt")
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.box.just = "right"
  )

ggsave(plot = p2, filename =file.path(outdir,'ERsed',
                                      paste0('ERwc48_density_plot','.png')),
       width = 5,height = 3 )
# 
# ggsave(file.path('results2023',"hist_density_plot_bottom_gg2_NOLEGEND.png"),
#        plot=p3, width = 4, height = 3, dpi = 300,device = "png") #grid.arrange(p1,p2, nrow=1)
# 

bigplot1 <- arrangeGrob(p1, p2,nrow=2)
ggsave(plot = bigplot1, filename =file.path(outdir,'ERsed',
                                            paste0('ER_density_plot_2','.png')),
       width = 5,height = 6 )



ERtot0<-cdata[(cdata$ERtotal_Square<=0)&(!is.na(cdata$ERtotal_Square)),]
ERtot0$ERtotal_Square<-log10(-ERtot0$ERtotal_Square)
#ERtot0$Total_Oxygen_Consumed[ERtot0$Total_Oxygen_Consumed!=0]<-log10(-ERtot0$Total_Oxygen_Consumed[ERtot0$Total_Oxygen_Consumed!=0])

ERtot_lit<-ERtot[(ERtot$ERvolumetric<=0)&(!is.na(ERtot$ERvolumetric)),]
ERtot_lit$ERvolumetric<-log10(-ERtot_lit$ERvolumetric)

p3 <- ggplot() + 
  #ER total
  geom_density(data=ERtot0, aes(x=ERtotal_Square,colour="tot",fill='tot'),adjust = 6)+
  geom_vline(aes(xintercept=median(ERtot0$ERtotal_Square)),color="black",  size=1)+
  # rates from Kyongho's model
  #geom_density(data=ERtot0[ERtot0$Total_Oxygen_Consumed!=0,], aes(x=Total_Oxygen_Consumed,colour="ER_pred",fill='ER_pred'),adjust = 6,alpha=0.8)+
  #geom_vline(aes(xintercept=median(ERtot0$Total_Oxygen_Consumed[ERtot0$Total_Oxygen_Consumed!=0])),color="red",  size=1)+
  #ERtot from Appling
  geom_density(data=ERtot_lit, aes(x=ERvolumetric,colour="tot_lit",fill='tot_lit'),adjust = 6,alpha=0.5)+
  geom_vline(aes(xintercept=median(ERtot_lit$ERvolumetric,na.rm=TRUE)),color="seagreen",  size=1)+
  xlim(-3.0, 3)+
  # #geom_vline(data=ERwc2, aes(xintercept=ERwc,color='lit'),linetype="dashed")+
  #scale_x_cut(breaks=c(-0.13), which=c(1), scales=c(0.25, 1),space = 0.2)+ theme_bw()+ 
  # xlab(expression("ER"[wc]*"")) +
  # ylab('Density')  + theme_classic()+ #+ scale_fill_grey()
  labs(x = expression(bold(paste("log"[10]*"(Aerobic Respiration"*" (g O"[2]*" m"^-2*" day"^-1*"))"))), y = 'Density')+
  #geom_rect(aes(xmin=min(ERwc,na.rm=TRUE),xmax=max(ERwc,na.rm=TRUE),ymin=0,ymax=0.04,colour="wc",fill='wc'))+ #ER WC
  scale_fill_manual("",breaks = c("tot",'ER_pred','tot_lit'),labels = c(expression("ER"[tot]*""),expression("ER"[HZ]*""),expression("ER"[tot]*"(Lit)")),
                    values = c("black",'red','seagreen'))+
  scale_colour_manual("",breaks = c("tot",'ER_pred','tot_lit'),labels = c(expression("ER"[tot]*""),expression("ER"[HZ]*""),expression("ER"[tot]*"(Lit)")),
                      values = c("black",'red','seagreen')    
  )+theme_classic()+
  # scale_linetype_manual("",breaks = c("tot","sed"),labels = c(expression("ER"[tot]*""),expression("ER"[sed]*"")),
  #                       values = c('solid',"dashed"))+theme_classic()+
  theme(
    legend.position = c(.15, .98),
    legend.justification = c( "top"),
    #legend.margin = margin(6, 4, 6, 6),
    legend.text = element_text(size=12,hjust = 0), #, margin = margin(l = 0, r = 5, unit = "pt")
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.key = element_rect(fill = "white", color = "black", linewidth = 0.2),
    legend.box.just = "right"
  )
ggsave(plot = p3, filename =file.path(outdir,'ERsed',
                                            paste0('ERtot_density_plot','.png')),
       width = 5,height = 3 )

