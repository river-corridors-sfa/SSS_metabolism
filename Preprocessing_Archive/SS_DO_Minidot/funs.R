# functions to support SS22 Minidot Data summary and Visualization
# Xinming Lin Sep 16th 2022
##########################################################################
## function to read metadata 
read_metadata<-function(indir,file){
  metadata <- read.csv(file.path(indir,file))
  #names(metadata)[grep("Date", names(metadata))][1]<-'Date'
  names(metadata)[grep("Time", names(metadata))]<-gsub('_.24_hr_hh_mm.','',names(metadata)[grep("Time", names(metadata))])
  #names(metadata)[grep("Time", names(metadata))]<-gsub('_.24_hr_hh.mm.','',names(metadata)[grep("Time", names(metadata))])
  didx<- grep("Date", names(metadata))
  for (d in didx){
    if (any(sub('.*\\/', '', metadata[,d])==2022)==TRUE) {
      metadata[,d] <-as.character(as.Date(metadata[,d],format="%m/%d/%Y"))
    }else{
      metadata[,d] <-as.character(as.Date(metadata[,d],format="%m/%d/%y"))
    }
  }
  #metadata['Date'] <-as.character(as.Date(metadata$Date,format="%m/%d/%Y"))
  scols= grep(paste(c("Start_Time",'End_Time'), collapse = "|"),names(metadata))
  for (s in 1:length(scols)){
    itime <- metadata[,scols[s]]
    #tid1 <- which(str_count(sdate, pattern = "/")<2); tid2 <- which(str_count(sdate, pattern = "/")==2)
    if (grepl(paste(c("Manta",'Manual_Chamber'), collapse = "|"),names(metadata)[scols[s]])){
      sdate<- paste(metadata$Sample_Date,itime)
    }else if(grepl('Depth_Hobo_Start_Time',names(metadata)[scols[s]])){
      sdate<- paste(metadata$Deploy_Date,itime)
    }
    else if(grepl('Depth_Hobo_End_Time',names(metadata)[scols[s]])){
      sdate<- paste(metadata$Retrieve_Date,itime)
    }
    else{
      sdate<- paste(metadata$Sample_Date,itime)
    }
    metadata[,scols[s]]<-as.character(as.POSIXct(sdate,"%Y-%m-%d %H:%M",tz='GMT'))
    # metadata[tid2,scols[s]]<-as.character(as.POSIXct(sdate[tid2],"%m/%d/%Y %H:%M",tz='GMT'))
  }
  #grepl(paste(c('00065_cd',"00060_cd"), collapse = "|"),names(hydro_data))
  #metadata['Date'] <-as.character(as.Date(metadata$Date,format="%m/%d/%y"))
  # metadata$Location[which(metadata$Location=="Cle Elum")]="Cle_Elum"
  # metadata$Location[which(metadata$Location=="Temporal Sites")]="Temporal_Sites"
  return(metadata)
}


################################################
process_data<-function(indir,filename,alpha=0.05){
  data = read.csv(file.path(indir,filename),header=T)
  data=data[c("Date_Time_PST", "Temp_degC" ,"DO_mg_l","DO_sat")]  
  cols=c("Temp_degC","DO_mg_l","DO_sat")
  site<-str_split_fixed(filename, '_', 4)[3]
  data['Site']<-site
  if (length(grep('T',data$Date_Time_PST))>0){
    data$Date_Time_PST= as.POSIXct(data$Date_Time_PST,"%Y-%m-%dT%H:%M:%SZ",tz='GMT')
  }else{
    data$Date_Time_PST= as.POSIXct(data$Date_Time_PST,"%Y-%m-%d %H:%M:%S",tz='GMT')
  }
  #data = distinct(data, Date_Time_PST, .keep_all = TRUE)
  data<-data%>% distinct()
  data['Start_DateTime']<-min(data$Date_Time_PST);data['End_DateTime']<-max(data$Date_Time_PST)
  data['npoints']<-nrow(na.omit(data))
  # sdata <-data[,c('Site','Start_DateTime','End_DateTime','npoints',cols)]%>%
  #   group_by(Site,Start_DateTime,End_DateTime,npoints) %>%
  #   summarise(across(everything(), .f = list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE))
  for (i in 1:length(cols)) {
    ## fill in na with the average of the previous and next day average at a specific hour
    idxs<-which(is.na(data[,cols[i]]))
    #sdata[paste0(cols[i],'_na%')]<-ifelse(length(idxs)> 0,length(idxs)/nrow(data)*100,0)
    if (length(idxs)>0){ ## plot the filled in NA values
      # fig1 <- fig1 %>% add_markers(x = data$Date_Time_PST[fna1], y = data[,paste0(c,'_fillna')][fna1],name = "Temp_fillna",
      #                              size = I(1),color = ~I('red'))
      data[paste0(cols[i],'_fillna')]<-na.interp(ts(data[paste0(cols[i],'_fillna')], frequency = 1440))
    }else{
      data[paste0(cols[i],'_fillna')]<-data[,cols[i]]
    }
    ### get anomaly 
    data = find_anomaly(data,cols[i],alpha=alpha)
    # tidx<-which((data[paste0(cols[i],'_outlier')]=='Yes')&(!is.na(data[,cols[i]])))
    # if (length(tidx)>0){
    #   sdata[paste0(cols[i],'_noutlier')]<-length(tidx)
    # }else{
    #   sdata[paste0(cols[i],'_noutlier')]<-0
    # }
  }
  # cindx = grep('outlier',names(data))
  # if (length(cindx)>0){
  #   #idata[[f]] =idata
  #   idata = data[apply(data[,cindx],1,function(x) any(x=='Yes')),]
  # }
  #return(list(data=data,sdata=sdata))
  return(data)
}

summary_data<-function(data){
  cols=c("Temp_degC","DO_mg_l","DO_sat")
  sdata <-data[,c('Site','Start_DateTime','End_DateTime','npoints',cols)]%>%
    group_by(Site,Start_DateTime,End_DateTime,npoints) %>%
    summarise(across(everything(), .f = list(mean = mean, min = min, max = max, sd = sd), na.rm = TRUE))
  for (i in 1:length(cols)) {
    ## fill in na with the average of the previous and next day average at a specific hour
    idxs<-which(is.na(data[,cols[i]]))
    sdata[paste0(cols[i],'_na%')]<-ifelse(length(idxs)> 0,length(idxs)/nrow(data)*100,0)
    tidx<-which((data[paste0(cols[i],'_outlier')]=='Yes')&(!is.na(data[,cols[i]])))
    if (length(tidx)>0){
      sdata[paste0(cols[i],'_noutlier')]<-length(tidx)
    }else{
      sdata[paste0(cols[i],'_noutlier')]<-0
    }
  }
  return(sdata)
}



############################################
select_data<-function(fzdata,id,nday,nhr){
  idate0<- fzdata$Date_Time_PST[id]-days(nday)
  idate1<- fzdata$Date_Time_PST[id]+days(nday)
  sdata0<-fzdata[(fzdata$Date_Time_PST>=idate0-hours(nhr))&(fzdata$Date_Time_PST<=idate0+hours(nhr)),]
  sdata1<-fzdata[(fzdata$Date_Time_PST>=idate1-hours(nhr))&(fzdata$Date_Time_PST<=idate1+hours(nhr)),]
  sdata<-rbind(sdata0,sdata1)
  return(sdata)
}

############################################
fillna<-function(fzdata,col){
  ### fill in na values using the 
  idxs<-which(is.na(fzdata[,col]))
  fzdata[paste0(col,'_fillna')] <-fzdata[,col]
  if (length(idxs)>0){
    for (id in idxs){
      sdata<-select_data(fzdata,id,1,1);#sdata1<-select_data(fzdata,id,2,1)
      #sdata<-rbind(sdata0,sdata1)
      fzdata[id,paste0(col,'_fillna')]<-mean(sdata[,col],na.rm = TRUE)
    }
  }
  return(fzdata)
}

############################################
find_anomaly<-function(data,col,alpha=0.05){
  col_na = paste0(col,'_fillna')
  idx = which(colnames(data)==col_na)
  tdata<-data.frame(time=data$Date_Time_PST,vname = data[,idx])
  d_ts<- as_tbl_time(tdata, time)
  tso<-d_ts%>%
    time_decompose(vname, method = "stl",trend = 10080*4,frequency= 1440) %>%    #
    anomalize(remainder, method = "iqr", alpha = alpha, max_anoms = 0.025) #iqr #
  rdata<- tsclean(tso$remainder)
  tso['anomaly2']<- ifelse(rdata!=tso$remainder,'Yes','No')
  data[paste0(col,'_outlier')]= ifelse((tso$anomaly=='Yes')&(tso$anomaly2=='Yes'),'Yes','No')#tso$anomaly
  data[paste0(col,'_corrected_outlier')]=tso$season+tso$trend+rdata
  return(data)
}

# find_anomaly<-function(data,col,alpha=0.05){
#   idx = which(colnames(data)==col)
#   tdata<-data.frame(time=data$Date_Time_PST,vname = data[,idx])
#   d_ts<- as_tbl_time(tdata, time)
#   tso<-d_ts%>%
#     time_decompose(vname, method = "stl") %>%    #
#     anomalize(remainder, method = "iqr", alpha = alpha, max_anoms = 0.05) #iqr #
#   rdata<- tsclean(tso$remainder)
#   tso['anomaly2']<- ifelse(rdata!=tso$remainder,'Yes','No')
#   data[paste0(col,'_outlier')]= ifelse((tso$anomaly=='Yes')&(tso$anomaly2=='Yes'),'Yes','No')#tso$anomaly
#   data[paste0(col,'_corrected_outlier')]=tso$season+tso$trend+rdata
#   return(data)
# }
############################################
ts_ggplot<-function(fdata,metadata,save=TRUE){
  data =fdata#$data; sdata<-fdata$sdata
  site<-unique(data$Site)
  dhobo<-metadata[metadata$Site_ID==site,grep('Depth_Hobo',names(metadata))]
  ylabels =c('Temperature(?C)','Dissolved Oxygen(mg/L)','Dissolved Oxygen Sturation(%)' )
  ynames = c('Temp(?C)','DO(mg/L)','Do sat(%)' )
  cols=c("Temp_degC","DO_mg_l","DO_sat")
  plot_list=list()#vector("list", length(cols))
  for (i in 1:length(cols)) {
    ## fill in na with the average of the previous and next day average at a specific hour
    idxs<-which(is.na(data[,cols[i]]))
    #####
    p1<-ggplot()+
      geom_point(data=data,aes(x=Date_Time_PST,y=data[,cols[i]], color=ynames[i]),size=1)+
      geom_line(data=data,aes(x=Date_Time_PST,y=data[,cols[i]], color=ynames[i]))+
      #geom_vline(xintercept = dhobo[grep(paste(c("Hobo_Start_Time",'Hobo_End_Time'), collapse = "|"),names(dhobo))], col = "red",)+
      geom_vline(xintercept = as.POSIXct(dhobo$Depth_Hobo_Start_Time_PST,"%Y-%m-%d %H:%M",tz='GMT'), col = "red",linetype="dotted")+
      geom_text(aes(x=as.POSIXct(dhobo$Depth_Hobo_Start_Time_PST,"%Y-%m-%d %H:%M",tz='GMT'), 
                    label="\nDepth_Hobo_Start", y=median(data[,cols[i]],na.rm=TRUE)), colour="red", angle=90)+
      geom_vline(xintercept = as.POSIXct(dhobo$Depth_Hobo_End_Time_PST,"%Y-%m-%d %H:%M",tz='GMT'), col = "red",linetype="dotted")+
      geom_text(aes(x=as.POSIXct(dhobo$Depth_Hobo_End_Time_PST,"%Y-%m-%d %H:%M",tz='GMT'), 
                    label="\nDepth_Hobo_End", y=median(data[,cols[i]],na.rm=TRUE)), colour="red", angle=90)+
      #geom_line(color='black') +
      labs(title=paste0("Time series plots with highlighted outliers (Site ",site,')'),x='DateTime (PST)',y=ylabels[i] )+
      scale_x_datetime(date_breaks = "7 day")
    if (length(idxs)>0){ ## plot the filled in NA values
      p1<-p1+geom_point(data=data[idxs,],aes(x=data$Date_Time_PST[idxs],y=data[,paste0(cols[i],'_fillna')][idxs], 
                                             color=paste0(cols[i],'_fillna(',round(length(idxs)/nrow(data)*100,1),'%)')),size=1)
      p1<-p1+geom_line(data=data[idxs,],aes(x=data$Date_Time_PST[idxs],y=data[,paste0(cols[i],'_fillna')][idxs], 
                                            color=paste0(cols[i],'_fillna(',round(length(idxs)/nrow(data)*100,1),'%)')))
    }
    ### get anomaly 
    tidx<-which((data[paste0(cols[i],'_outlier')]=='Yes')&(!is.na(data[,cols[i]])))
    if (length(tidx)>0){
      sdata[paste0(cols[i],'_noutlier')]<-length(tidx)
      p1<-p1+geom_point(data=data[tidx,],aes(x=data$Date_Time_PST[tidx],y=data[,cols[i]][tidx], 
                                             color=paste0(cols[i],'_outlier(#',length(tidx),')')),size=1.5)
      p1<-p1+geom_point(data=data[tidx,],aes(x=data$Date_Time_PST[tidx],y=data[,paste0(cols[i],'_corrected_outlier')][tidx], 
                                             color=paste0(cols[i],'_corrected_outlier')),size=1.5)
      
    }
    p1<-p1+ scale_colour_manual(values = c("black", "blue","red", "green"))+labs(color = "Variable")
    plot_list[[i]] = ggplot_gtable(ggplot_build(p1))
    #dev.off()
  }
  #ps<- do.call('grid.arrange',c(plot_list, ncol = 1))
  if(save==TRUE){
    ggsave(file.path(outdir,'Plots_fillna_outliers',
                     paste0('SSS_MiniDOT_',site,"_fillna_outliers.png")),
           plot=grid.arrange(grobs=plot_list,ncol=1), width = 12, height = 8, dpi = 300,device = "png") #grid.arrange(p1,p2, nrow=1)
  }

  return(grid.arrange(grobs=plot_list,ncol=1))
  
}



############################################
tsplot_ly<-function(fdata,save=TRUE){
  data =fdata#$data; sdata<-fdata$sdata
  site<-unique(data$Site)
  ylabels =c('Temperature(?C)','Dissolved Oxygen(mg/L)','Dissolved Oxygen Sturation(%)' )
  ynames = c('Temp(?C)','DO(mg/L)','Do sat(%)' )
  cols=c("Temp_degC","DO_mg_l","DO_sat")
  plot_list=list()#vector("list", length(cols))
  ylabels =c('Temperature(?C)','Dissolved Oxygen(mg/L)','Dissolved Oxygen Sturation(%)' )
  ynames = c('Temp(?C)','DO(mg/L)','Do sat(%)' )
  plot_list=list()#vector("list", length(cols))
  for (c in 1:length(cols)) {
    ## fill in na with the average of the previous and next day average at a specific hour
    idxs<-which(is.na(data[,cols[c]]))
    #####
    fig1 <- plot_ly(x = data$Date_Time_PST, y = data[,cols[c]], type = 'scatter', name = ynames[c],
                    mode = 'lines+markers', size = I(1),color = ~I('black'))%>%
      layout(xaxis = list(title = 'DateTime (PST)'), yaxis = list(title = ylabels[c]))
    
    #fna1<-which(is.na(fzdata[,cols[c]]))
    if (length(idxs)>0){ ## plot the filled in NA values
      # fig1 <- fig1 %>% add_markers(x = fzdata$Date_Time_PST[fna1], y = fzdata[,paste0(c,'_fillna')][fna1],name = "Temp_fillna",
      #                              size = I(1),color = ~I('red'))
      fig1 <- fig1 %>% add_trace(x = data$Date_Time_PST[idxs], y = data[,paste0(cols[c],'_fillna')][idxs],
                                 name = paste0(cols[c],'_fillna(',round(length(idxs)/nrow(data)*100,1),'%)'),
                                 type = 'scatter',mode = 'lines+markers',size = I(1),color = ~I('blue'))
    }
    ### get anomaly
    tidx<-which((data[paste0(cols[c],'_outlier')]=='Yes')&(!is.na(data[,cols[c]])))
    if (length(tidx)>0){
      fig1 <- fig1 %>% add_trace(x = data$Date_Time_PST[tidx], y = data[,cols[c]][tidx],
                                 name = paste0(cols[c],'_outlier(#',length(tidx),')'),
                                 type = 'scatter',mode = 'markers',size = I(1),color = ~I('red'))
      fig1 <- fig1 %>% add_trace(x = data$Date_Time_PST[tidx], y = data[,paste0(cols[c],'_fillna')][tidx],
                                 name = paste0(cols[c],'_replaced_outlier'),
                                 type = 'scatter',mode = 'markers',size = I(1),color = ~I('green'))
      
    }
    plot_list[[c]] = fig1
  }
  ##
  plots <- subplot(plot_list,  nrows = length(cols),shareX = TRUE,titleY = TRUE, titleX = TRUE) %>% #, margin = 0.01
    layout(title = list(text = paste0("Time series plots with highlighted outliers (Site ",site,')')),
         plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'))
  if(save==TRUE){
    withr::with_dir(file.path(outdir,'Plots_fillna_outliers'), htmlwidgets::saveWidget(plots, paste0('SSS_MiniDOT_',site,"_fillna_outliers.html")))
  }
  return(plots)

}

