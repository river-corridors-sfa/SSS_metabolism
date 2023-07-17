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

yvar ='ERsed_Square'
vars = c("HOBO_Temp","Slope","Velocity" ,"Discharge","TSS", 'TN','NPOC',
         "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws","AridityWs",'D50_m')

# correlation matrix
png(file.path(outdir,paste0('exploratory_variables_correlation_matrix',".png")),
    width = 10, height = 10, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(cdata[c(yvar,vars)], histogram=TRUE, pch=19)
dev.off()


sdata =cdata[cdata$ERsed_Square<=0,]
################################################################################################
# scatterplot for ERsed and model data

# x <- normalize(cdata$ERsed_Square, method = "range", range = c(0, 1))
# y <- normalize(cdata$Total_Oxygen_Consumed, method = "range", range = c(0, 1))
x <- scale(cdata$ERsed_Square, center = TRUE, scale = TRUE)
y <- scale(cdata$Total_Oxygen_Consumed, center = TRUE, scale = TRUE)

# scatterplot for original ERsed and model data
png(file.path(outdir,paste0('ER_sed_vs_total_oxygen_consumed',".png")),
    width = 6, height = 6, units = 'in', res = 600)
plot(cdata$ERsed_Square,cdata$Total_Oxygen_Consumed,pch=20,
     xlab=expression(paste("ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
     ylab=expression(paste("Total Oxygen Consumed"*" (g O"[2]*" m"^2*" day"^-1*")")))
dev.off()
# scatterplot for scale ERsed and model data
png(file.path(outdir,paste0('ERsed_vs_total_oxygen_consumed_scaled',".png")),
    width = 6, height = 6, units = 'in', res = 600)
plot(x,y,pch=20,
     xlab=expression(paste("ER"[sed]*"(scaled)")), 
     ylab=expression(paste("Total Oxygen Consumed (scaled)")))
dev.off()
################################################################################################
## scatter plots 
# locally measured variables : TN(Total nitrogen),log(discharge), Water Temp, Log(D50), Log(Slope) and any other explanatory variables

breaks <- 10^(-10:10)
breaks_c <- 10^seq(-10,10,by=4)
#breaks_c <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))


pvars <-c("HOBO_Temp", 'TN', "Discharge","AridityWs",'D50_m','Slope','totdasqkm')
xlabels <-c(expression(bold("Temperature (°C)")),expression(bold(paste("Total Nitrogen"))),
            expression(bold(paste("Discharge"*" (m s"^-1*")"))),
            expression(bold(paste("Aridity"))),expression(bold(paste("D50"*" (m)"))),
            expression(bold(paste("Slope"))),
            xlab=expression(bold("Total Drained Area (km"^2*")"))
)

for (v in 1:length(pvars)){
  if (pvars[v] %in% c("HOBO_Temp",'TN',"AridityWs")){
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
ylabels <-c(expression(bold("Temperature (Â°C)")),expression(bold(paste("Total Nitrogen"))),
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
  
  ggsave(plot = iplot, filename =file.path(outdir,'ERsed','ERsed_scatterplot',
                                           paste0('Scatter_totdasqkm','_vs_',pvars[v],'.png')),
         width = 5,height = 4 )
  
}

###############################################################
## density plots 


colors <- c(expression("median ER"[wc]*"") = "blue", expression("ER"[wc]*" lit") = "black")

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
  labs(x = expression(bold(paste("ER"*" (g O"[2]*" m"^2*" day"^-1*")"))), y = 'Density')+
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
    legend.position = c(.2, .9),
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
  labs(x = expression(bold(paste("ER"[wc]*" (g O"[2]*" m"^2*" day"^-1*")"))), y = 'Density')+
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
       width = 6,height = 6 )









