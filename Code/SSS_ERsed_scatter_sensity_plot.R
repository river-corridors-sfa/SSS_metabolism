# RC2 spatial study - Multiple linear regression 
# ER_sed
# X Lin April 18 2023
################################################################################################
# Read in data 
################################################################################################
rm(list=ls(all=TRUE))
source('./COde/SSS_Sed_Resp_data_merging.R')
outdir<-'./MLR_Analysis_Figures'
##############################################################################################################
# read in data
cdata <- data_merge()

# remove positive ERsed
sdata =cdata[cdata$ERsed_Square<0,]

yvar ='ERsed_Square'
# vars0 = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" ,"Discharge","TSS", 'TN','NPOC',
#          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'PctShrb2019Ws',"AridityWs",
#          'D50_m',"hz_spring","Chlorophyll_A",'streamorde','GPP_Square')

xvars0 = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", 'NPOC',
          "totdasqkm","PctFst","PctAg",'PctShrb2019Ws','D50_m',
          "hz_spring","Chlorophyll_A",'streamorde','GPP_Square','TN')
# correlation matrix
png(file.path(outdir,'ERsed',paste0('exploratory_variables_correlation_matrix',".png")),
    width = 12, height = 8, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(sdata[c(yvar,vars0)], histogram=TRUE, pch=19)
dev.off()

########################
#log transform all variables
ldata<- sdata[c(yvar,xvars0)]
vars <-names(ldata)
#log transform variables
for ( v in 1:length(vars)){
  if(vars[v] %in% c("ERsed_Square")){
    ldata[vars[v]] <- log10(abs(ldata[vars[v]]))
  }else if (vars[v] %in% c('hz_spring')){
    ldata[vars[v]] <- ldata[vars[v]]
  }
  else if(vars[v] %in% c("PctMxFst2019Ws",'PctCrop2019Ws',"PctFst","PctAg","Chlorophyll_A","GPP_Square")){
    ldata[vars[v]] <- log10(ldata[,vars[v]]+1)
  }else{
    ldata[vars[v]] <- log10(ldata[vars[v]])
  }
  png(file.path(outdir,'ERsed','check_normality',paste0(vars[v],'_orig_vs_log',".png")),
      width = 3, height = 4, units = 'in', res = 600)
  par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(3.4,3.4,1,1.5))
  hist(sdata[,vars[v]],xlab=vars[v],main='')
  hist(ldata[,vars[v]],xlab=paste0(vars[v],'_log'),main='')
  dev.off()
  # else{
  #   sdata[xvars[v]] <- scale(sdata[xvars[v]], center = TRUE, scale = TRUE)
  # }
}
# correlation matrix
png(file.path(outdir,'ERsed',paste0('exploratory_variables_correlation_matrix_log_all',".png")),
    width = 12, height = 8, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(ldata[c(yvar,vars)], histogram=TRUE, pch=19)
dev.off()
################################################################################################
# scatterplot for ERsed and model data

# x <- normalize(cdata$ERsed_Square, method = "range", range = c(0, 1))
# y <- normalize(cdata$Total_Oxygen_Consumed, method = "range", range = c(0, 1))
sdata =cdata[cdata$ERsed_Square<=0,]
x <- scale(sdata$ERtotal_Square, center = TRUE, scale = TRUE)
y <- scale(sdata$Total_Oxygen_Consumed, center = TRUE, scale = TRUE)

# scatterplot for original ERsed and model data
png(file.path(outdir,'ERsed',paste0('ER_sed_vs_total_oxygen_consumed',".png")),
    width = 5, height = 4, units = 'in', res = 600)
par(mgp=c(2,1,0),mar=c(3.1,3.5,2,2.5))
plot(sdata$ERsed_Square,sdata$Total_Oxygen_Consumed,pch=20,cex.lab=0.85,cex.axis=0.85,
     xlab=expression(paste("Observed Sediment Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")), 
     ylab=expression(paste("Predicted Sediment Respiration"*" (g O"[2]*" m"^-2*" day"^-1*")")))
dev.off()
# scatterplot for scale ERsed and model data
png(file.path(outdir,'ERsed',paste0('ERtot_vs_total_oxygen_consumed_scaled',".png")),
    width = 6, height = 6, units = 'in', res = 600)
plot(x,y,pch=20,
     xlab=expression(paste("Normalized ER"[tot]*"")), 
     ylab=expression(paste("Normalized Total Oxygen Consumed")))
dev.off()

# scatterplot for original ERsed and model data
png(file.path(outdir,'ERsed',paste0('log_ER_sed_vs_total_oxygen_consumed',".png")),
    width = 6, height = 6, units = 'in', res = 600)
par(mgp=c(2,1,0),mar=c(3.1,3.5,2,2.5))
plot(log10(abs(sdata$ERsed_Square)),log10(abs(sdata$Total_Oxygen_Consumed)),pch=20,
     xlab=expression(paste("log(ER"[sed]*")")), 
     ylab=expression(paste("log(Total Oxygen Consumed)")))
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
          'D50_m',"hz_spring","Chlorophyll_A",'streamorde','GPP_Square','Ratio')
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

sdata =cdata[cdata$ERsed_Square<=0,]
pvars <-c("HOBO_Temp", 'TN', "Discharge","AridityWs",'D50_m','Slope','totdasqkm','TSS',
          'Mean_Depth','Velocity',"hz_spring","Chlorophyll_A",'Ratio')
xlabels <-c(expression(bold("Temperature (°C)")),expression(bold(paste("Total Nitrogen"))),
            expression(bold(paste("Discharge"*" (m s"^-1*")"))),
            expression(bold(paste("Aridity"))),expression(bold(paste("D50"*" (m)"))),
            expression(bold(paste("Slope"))),
            expression(bold("Total Drained Area (km"^2*")")),
            expression(bold(paste("TSS"))),
            expression(bold("Average Depth (m)")),
            expression(bold(paste("Velocity"*" (m s"^-1*")"))),
            expression(bold(paste("Hyporheic exchange flux"*""))),
            expression(bold(paste("Chlorophyll_A"))),
            expression(bold(paste("Ratio(Depth/D50)")))
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
ylabels <-c(expression(bold("Temperature (°C)")),expression(bold(paste("Total Nitrogen"))),
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
ERtot <- read.csv(file.path('.','mean_ERtot_bestSiteIDs.csv'))

#colors <- c(expression("median ER"[wc]*"") = "blue", expression("ER"[wc]*" lit") = "black")

ERwc <- cdata$ERwc_Square[cdata$ERtotal_Square<=0]

p1 <- ggplot() + 
  geom_density(data=cdata[cdata$ERtotal_Square<=0,], aes(x=ERtotal_Square,colour="tot",fill='tot'),adjust = 6)+
  geom_density(data=cdata[cdata$ERsed_Square<=0,], aes(x=ERsed_Square,colour="sed",fill='sed'),adjust = 6,alpha=0.8)+
  geom_vline(aes(xintercept=median(cdata$ERtotal_Square[cdata$ERtotal_Square<=0])),color="black",  size=1)+
  geom_vline(aes(xintercept=median(cdata$ERsed_Square[cdata$ERsed_Square<=0])),color="grey",  size=1)+
  #geom_vline(data=ERwc2, aes(xintercept=ERwc,color='lit'),linetype="dashed")+
  #scale_x_cut(breaks=c(-0.13), which=c(1), scales=c(0.25, 1),space = 0.2)+ theme_bw()+ 
  # xlab(expression("ER"[wc]*"")) +
  # ylab('Density')  + theme_classic()+ #+ scale_fill_grey()
  labs(x = expression(bold(paste("ER"*" (g O"[2]*" m"^-2*" day"^-1*")"))), y = 'Density')+
  geom_rect(aes(xmin=min(ERwc,na.rm=TRUE),xmax=max(ERwc,na.rm=TRUE),ymin=0,ymax=0.04,colour="wc",fill='wc'))+ #ER WC
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


p2 <- ggplot() + 
  geom_density(data=cdata[cdata$ERwc_Square<=0,], aes(x=ERwc_Square,color='wc',fill='wc'),adjust = 4)+
  geom_vline(aes(xintercept=median(cdata$ERwc_Square[cdata$ERwc_Square<=0],na.rm=TRUE)),color="blue",  size=1)+
  labs(x = expression(bold(paste("ER"[wc]*" (g O"[2]*" m"^-2*" day"^-1*")"))), y = 'Density')+
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

# 
# ggsave(file.path('results2023',"hist_density_plot_bottom_gg2_NOLEGEND.png"),
#        plot=p3, width = 4, height = 3, dpi = 300,device = "png") #grid.arrange(p1,p2, nrow=1)
# 

bigplot1 <- arrangeGrob(p1, p2,nrow=2)
ggsave(plot = bigplot1, filename =file.path(outdir,'ERsed',
                                            paste0('ER_density_plot_2','.png')),
       width = 5,height = 6 )



ERtot0<-cdata[cdata$ERtotal_Square<=0,]
ERtot0$ERtotal_Square<-log10(-ERtot0$ERtotal_Square)
ERtot0$Total_Oxygen_Consumed[ERtot0$Total_Oxygen_Consumed!=0]<-log10(-ERtot0$Total_Oxygen_Consumed[ERtot0$Total_Oxygen_Consumed!=0])
ERtot_lit<-ERtot[ERtot$ERvolumetric<=0,]
ERtot_lit$ERvolumetric<-log10(-ERtot_lit$ERvolumetric)

p3 <- ggplot() + 
  #ER total
  # geom_density(data=ERtot0, aes(x=ERtotal_Square,colour="tot",fill='tot'),adjust = 6)+
  # geom_vline(aes(xintercept=median(ERtot0$ERtotal_Square)),color="black",  size=1)+
  # rates from Kyongho's model
  geom_density(data=ERtot0[ERtot0$Total_Oxygen_Consumed!=0,], aes(x=Total_Oxygen_Consumed,colour="ER_pred",fill='ER_pred'),adjust = 6,alpha=0.8)+
  geom_vline(aes(xintercept=median(ERtot0$Total_Oxygen_Consumed[ERtot0$Total_Oxygen_Consumed!=0])),color="red",  size=1)+
  #ERtot from Appling
  # geom_density(data=ERtot_lit, aes(x=ERvolumetric,colour="tot_lit",fill='tot_lit'),adjust = 6,alpha=0.5)+
  # geom_vline(aes(xintercept=median(ERtot_lit$ERvolumetric,na.rm=TRUE)),color="seagreen",  size=1)+
  # #geom_vline(data=ERwc2, aes(xintercept=ERwc,color='lit'),linetype="dashed")+
  #scale_x_cut(breaks=c(-0.13), which=c(1), scales=c(0.25, 1),space = 0.2)+ theme_bw()+ 
  # xlab(expression("ER"[wc]*"")) +
  # ylab('Density')  + theme_classic()+ #+ scale_fill_grey()
  labs(x = expression(bold(paste("log"[10]*"(ER"*" (g O"[2]*" m"^-2*" day"^-1*"))"))), y = 'Density')+
  #geom_rect(aes(xmin=min(ERwc,na.rm=TRUE),xmax=max(ERwc,na.rm=TRUE),ymin=0,ymax=0.04,colour="wc",fill='wc'))+ #ER WC
  scale_fill_manual("",breaks = c("tot",'ER_pred','tot_lit'),labels = c(expression("ER"[tot]*""),expression("ER"[model]*""),expression("ER"[tot]*"(Lit)")),
                    values = c("black",'red','seagreen'))+
  scale_colour_manual("",breaks = c("tot",'ER_pred','tot_lit'),labels = c(expression("ER"[tot]*""),expression("ER"[model]*""),expression("ER"[tot]*"(Lit)")),
                      values = c("black",'red','seagreen')    
  )+theme_classic()+
  # scale_linetype_manual("",breaks = c("tot","sed"),labels = c(expression("ER"[tot]*""),expression("ER"[sed]*"")),
  #                       values = c('solid',"dashed"))+theme_classic()+
  theme(
    legend.position = c(.2, .95),
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

