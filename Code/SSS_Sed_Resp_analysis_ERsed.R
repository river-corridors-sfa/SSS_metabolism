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
sdata =cdata[cdata$ERsed_Square<=0,]
sapply(sdata, function(x) sum(is.na(x)))
################################################################################################
# Stepwise Regression for ERsed
# fit <- lm(DO_Slope ~ DIC + NPOC + TN + TSS+T_mean+TOT_BASIN_AREA+StreamOrde, data = na.omit(cdata))
# step <- stepAIC(fit, direction="both")
# step$anova # display results
yvar ='ERsed_Square'
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", #'NPOC',#'TN',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'D50_m',
          "hz_annual","Chlorophyll_A",'streamorde')
sdata = cdata[c(yvar,xvars)];#
sdata =sdata[sdata$ERsed_Square<=0,]


##
#log transform variables
for ( v in 1:length(xvars)){
  if(xvars[v] %in% c("totdasqkm",'TSS','Slope','D50_m',"Discharge",'Mean_Depth')){
    sdata[xvars[v]] <- log10(sdata[xvars[v]])
  }else if(xvars[v] %in% c('PctCrop2019Ws',"Chlorophyll_A")){
    sdata[xvars[v]] <- log10(sdata[,xvars[v]]+1)
  }
  # else{
  #   sdata[xvars[v]] <- scale(sdata[xvars[v]], center = TRUE, scale = TRUE)
  # }
}


#sdata =sdata[-which(sdata$ERsed_Square < -15),]
sdata0 =na.omit(sdata)
#define intercept-only model
intercept_only <- lm(ERsed_Square ~ 1, data=sdata0)

#define model with all predictors
all <- lm(ERsed_Square ~ ., data = sdata0)

################################################################################################
#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), steps=5000,trace=1)
forward$anova
forward$coefficients

#  lm fitting using selected variables from forward stepwise selection
sdata2 = sdata[c(yvar,c('Slope','Chlorophyll_A','streamorde','Mean_Depth','HOBO_Temp'))];#
sdata2 =na.omit(sdata2)
#ffit<- lm(ERsed_Square ~ totdasqkm+velocity_ms +AridityWs+Slope+ Minidot_Temperature, data = sdata)
ffit<- lm(ERsed_Square ~ Slope+streamorde+Mean_Depth+HOBO_Temp, data = sdata2)
#ffit<- lm(ERsed_Square ~ hz_annual+ D50_m+Slope+Mean_Depth+HOBO_Temp, data = sdata)
summary(ffit)

png(file.path(outdir,'ERsed',paste0('bestfit_regression_forward',".png")),
    width = 6, height = 6, units = 'in', res = 600)
par(mfrow=c(2,2)) 
plot(ffit)
dev.off()


#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), steps=5000, trace=1)
backward$anova
backward$coefficients

#  lm fitting using selected variables from backward stepwise selection
sdata3 = sdata[c(yvar,c("HOBO_Temp",'Mean_Depth', "AridityWs","PctMxFst2019Ws",
                        "hz_annual",'streamorde'))];#
sdata3 =na.omit(sdata3)
bfit<- lm(ERsed_Square ~ HOBO_Temp+Mean_Depth+AridityWs+PctMxFst2019Ws+hz_annual+streamorde, data = sdata3)
#bfit<- lm(ERsed_Square ~ totdasqkm+velocity_ms+AridityWs+Minidot_Temperature, data = sdata)
#bfit<- lm(DO_Slope ~TN +TOT_BASIN_AREA+ T_mean+StreamOrde+Transformations, data = data)
summary(bfit)

png(file.path(outdir,'ERsed',paste0('bestfit_regression_backward_log',".png")),
    width = 6, height = 6, units = 'in', res = 600)
par(mfrow=c(2,2)) 
plot(bfit)
dev.off()

# #Partial regression plots 
# png(file.path(outdir,paste0('avPlots_backward_bfit',".png")),
#     width = 6, height = 6, units = 'in', res = 600)
# avPlots(bfit,id=FALSE,layout=c(2,2)) 
# dev.off()
###########################
# Partial residual plots 

# png(file.path(outdir,paste0('crPlots_backward_bfit',".png")),
#     width = 6, height = 6, units = 'in', res = 600)
# par(mfrow=c(3,1),mgp=c(2,1,0),mar=c(3.1,4.1,2,1.5))
# crPlots(bfit,~totdasqkm,main='Partial-Residual Plots',
#         ylab=expression(paste("Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
#         smooth=FALSE,id=FALSE,layout=c(2,2))
# dev.off()

png(file.path(outdir,'ERsed',paste0('crPlots_bfit_variables_log',".png")),
    width = 6, height = 6, units = 'in', res = 600)
par(mfrow=c(2,2),mgp=c(2.2,1,0),mar=c(3.1,4.1,2,1.5))
crPlots(bfit,~totdasqkm,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        #xlab=expression("Total Drainage Area (km"^2*")"),
        xlab=expression("log(Total Drainage Area)"),
        smooth=FALSE,id=FALSE)
# crPlots(bfit,~Minidot_Temperature,
#         ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
#         xlab=expression("Temperature (°C)"),
#         smooth=FALSE,id=FALSE)
crPlots(bfit,~velocity_ms,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("Velocity (ms"^-1*")"),
        smooth=FALSE,id=FALSE)
crPlots(bfit,~AridityWs,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("Aridityws"),
        smooth=FALSE,id=FALSE)
crPlots(bfit,~Slope,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("Slope"),
        smooth=FALSE,id=FALSE)
dev.off()


png(file.path(outdir,'ERsed',paste0('crPlots_bfit_variables_3vars',".png")),
    width = 3, height = 8, units = 'in', res = 600)
par(mfrow=c(3,1),mgp=c(2.2,1,0),mar=c(3.1,4.1,2,1.5))
crPlots(bfit,~Slope,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("log(Slope)"),
        smooth=FALSE,id=FALSE)
crPlots(bfit,~D50_m,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("log(D50_m)"),
        smooth=FALSE,id=FALSE)
crPlots(bfit,~AridityWs,
        ylab=expression(paste("Partial Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("Aridityws"),
        smooth=FALSE,id=FALSE)
dev.off()

###########################
#Partial regression plots

png(file.path(outdir,'ERsed',paste0('avPlots_bfit_variables_log',".png")),
    width = 6, height = 6, units = 'in', res = 600)
par(mfrow=c(2,2),mgp=c(2.2,1,0),mar=c(3.1,4.1,2,1.5))
avPlots(bfit,~totdasqkm,main='',
        ylab=expression(paste("Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        #xlab=expression("Residuals - Total Drainage Area (km"^2*")"),
        xlab=expression("log(Total Drainage Area)"),
        id=FALSE)
# avPlots(bfit,~Minidot_Temperature,main='',
#         ylab=expression(paste("Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
#         xlab=expression("Residuals - Temperature (°C)"),
#         id=FALSE)
avPlots(bfit,~velocity_ms,main='',
        ylab=expression(paste("Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("Residuals - Velocity (km"^-1*")"),
        id=FALSE)
avPlots(bfit,~AridityWs,main='',
        ylab=expression(paste("Residuals - ","ER"[sed]*" (g O"[2]*" m"^2*" day"^-1*")")), 
        xlab=expression("Residuals - Aridityws"),
        id=FALSE)

dev.off()



################################################################################################
## random forest analysis 
library("randomForest")
# sdata1 =sdata
#sdata1$ERsed_Square[sdata1$ERsed_Square>0] = 0
yvar ='ERsed_Square'
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", #'NPOC',#'TN',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'D50_m',"hz_annual","Chlorophyll_A",'streamorde')
sdata = cdata[c(yvar,xvars)];#
sdata =sdata[sdata$ERsed_Square<=0,]
sdata =na.omit(sdata)

mtry <- tuneRF(sdata[,-1],sdata[,1], ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

set.seed(42)
rf_fit <- randomForest(ERsed_Square ~ ., ntree=100,maxnodes=4,nodesize=5,mtry=6, data=sdata, importance=TRUE) #nPerm=3,
predicted <- unname(predict(rf_fit, data=cdata))
R2 <- 1 - (sum((sdata$ERsed_Square-predicted)^2)/sum((sdata$ERsed_Square-mean(sdata$ERsed_Square))^2))
R2


png(file.path(outdir,'ERsed',paste0('rf_importance_no_transform_add','.png')), 
    width = 8, height = 6, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(12.5,3.1,2.1,1))
#rimp <- importance(rf_fit)
vimp <- setNames(as.data.frame(rf_fit$importance)$IncNodePurity, row.names(as.data.frame(rf_fit$importance)))
vimp<- sort(vimp,decreasing = TRUE)
vimp

narg<-c("Temperature","Depth","Slope" ,"Velocity" ,"Discharge","Aridity", "TSS", 
        "Drainage_Area", "Pct_Forest","Pct_Crop",'D50',
        'Hflux','ChlA','Streamorde')
for (v in 1:length(xvars)){
  idx <- grep(xvars[v],names(vimp))
  names(vimp)[idx]<-narg[v]
}
col.colors <- c(Temperature='#00FF66', Depth='#CC00FF',Slope='#FF0000',Velocity="#0066FF",
                Discharge="#00FF66",Aridity='#CC00FF', TSS ='#00FF66',
                Drainage_Area="#FF0000",Pct_Forest='#CCFF00',Pct_Crop ="#CCFF00", D50='#CCFF00',
                Hflux='#FF0000',ChlA='#00FF66',Streamorde='#00FF66')


order.names <- names(vimp)
order.colors <- col.colors[order.names] #rainbow(5)
barplot(vimp/sum(vimp),col =order.colors , horiz = FALSE,las=3,cex.lab=1.5, cex.axis=1.5, 
        cex.main=2,cex.names=1.5,ylab="Relative Importance")#,main=paste0("RF Feature Importance (PoreWater)"))
dev.off()








