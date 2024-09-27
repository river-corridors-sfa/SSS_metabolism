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

yvar ='ERwc_Square'
vars = c("HOBO_Temp","Slope","Velocity" ,"Discharge","TSS", 'TN','NPOC',
         "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws","AridityWs",'D50_m')

# correlation matrix
png(file.path(outdir,paste0('exploratory_variables_correlation_matrix',".png")),
    width = 10, height = 10, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(cdata[c(yvar,vars)], histogram=TRUE, pch=19)
dev.off()


sdata =cdata[cdata$ERwc_Square<=0,]

################################################################################################
## scatter plots using original values
breaks <- 10^(-10:10)
breaks_c <- 10^seq(-10,10,by=4)
#breaks_c <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))


for (v in 1:length(vars)){
  iplot <- sdata %>% 
    ggplot(aes_string(x=vars[v],y='ERwc_Square'))+
    geom_point(alpha = 0.5,size=3)+
    geom_smooth(method="lm", se=FALSE)+
    #stat_cor(label.y = -12,color='red',size=4)+ 
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
             label.y = -0.25,color='red',size=4)+ 
    #xlab(expression(bold(paste("Total Nitrogen"))))+
    #xlab(vars[v])+
    xlab(paste0('log(',vars[v],')'))+
    ylab(expression(bold(paste("ER"[wc]*" (g O"[2]*" m"^2*" day"^-1*")"))))+
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
  # ggsave(plot = iplot, filename =file.path(outdir,'ERwater','ERwater_scatterplot',
  #                                          paste0('Original_Scatter_','ERwater','_vs_',vars[v],'.png')),
  #        width = 6,height = 4 )
  ggsave(plot = iplot, filename =file.path(outdir,'ERwater','ERwater_scatterplot',
                                           paste0('Log_Scatter_','ERwater','_vs_',vars[v],'.png')),
         width = 6,height = 4 )
}


## scatter plots using log of selected variables 
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
  if (pvars[v] %in% c("HOBO_Temp",'TN',"AridityWs",'Discharge','NPOC','TN','Velocity')){
    iplot <- sdata %>% 
      ggplot(aes_string(x=pvars[v],y='ERwc_Square',color = 'Slope'))+
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
      ggplot(aes_string(x=pvars[v],y='ERwc_Square',color = 'Slope'))+
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
  
  ggsave(plot = iplot, filename =file.path(outdir,'ERwater','ERwater_scatterplot',
                                           paste0('Scatter_',pvars[v],'_vs_','ERwater','.png')),
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
  
  ggsave(plot = iplot, filename =file.path(outdir,'ERwater','ERwater_scatterplot',
                                           paste0('Scatter_totdasqkm','_vs_',pvars[v],'.png')),
         width = 5,height = 4 )
  
}







