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
png(file.path(outdir,'ERwater',paste0('exploratory_variables_correlation_matrix',".png")),
    width = 10, height = 10, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(cdata[c(yvar,vars)], histogram=TRUE, pch=19)
dev.off()

# remove positive ERwc
sdata =cdata[cdata$ERwc_Square<=0,]
################################################################################################
# Stepwise Regression for ERwc
# fit <- lm(DO_Slope ~ DIC + NPOC + TN + TSS+T_mean+TOT_BASIN_AREA+StreamOrde, data = na.omit(cdata))
# step <- stepAIC(fit, direction="both")
# step$anova # display results
yvar ='ERwc_Square'
xvars = c("HOBO_Temp","Slope","Velocity" ,"TSS", "Discharge","AridityWs",#'NPOC','TN',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'D50_m')
sdata = cdata[c(yvar,xvars)];#
sdata =sdata[sdata$ERwc_Square<=0,]
#sdata =na.omit(sdata)

##
#log transform variables
for ( v in 1:length(xvars)){
  if(xvars[v] %in% c("totdasqkm",'TSS','Slope','D50_m')){
    sdata[xvars[v]] <- log10(sdata[xvars[v]])
  }else if(xvars[v] %in% c('PctCrop2019Ws')){
    sdata[xvars[v]] <- log10(sdata[,xvars[v]]+1)
  }
  # else{
  #   sdata[xvars[v]] <- scale(sdata[xvars[v]], center = TRUE, scale = TRUE)
  # }
}


#sdata =sdata[-which(sdata$ERwc_Square < -15),]

#define intercept-only model
intercept_only <- lm(ERwc_Square ~ 1, data=sdata)

#define model with all predictors
all <- lm(ERwc_Square ~ ., data = sdata)

################################################################################################
#perform forward stepwise regression
forward <- step(intercept_only, direction='both', scope=formula(all), steps=5000,trace=1)
forward$anova
forward$coefficients

#  lm fitting using selected variables from forward stepwise selection
#ffit<- lm(ERwc_Square ~ totdasqkm+velocity_ms +AridityWs+Slope+ Minidot_Temperature, data = sdata)
ffit<- lm(ERwc_Square ~ Slope+ D50_m, data = sdata)
#ffit<- lm(ERwc_Square ~ AridityWs+ Minidot_Temperature, data = sdata)
summary(ffit)

png(file.path(outdir,'ERwater',paste0('bestfit_regression_forward',".png")),
    width = 6, height = 6, units = 'in', res = 600)
par(mfrow=c(2,2)) 
plot(ffit)
dev.off()


#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=1)
backward$anova
backward$coefficients

#  lm fitting using selected variables from backward stepwise selection
bfit<- lm(ERwc_Square ~ Slope+D50_m+AridityWs, data = sdata)
#bfit<- lm(ERwc_Square ~ totdasqkm+velocity_ms+AridityWs+Minidot_Temperature, data = sdata)
#bfit<- lm(DO_Slope ~TN +TOT_BASIN_AREA+ T_mean+StreamOrde+Transformations, data = data)
summary(bfit)

png(file.path(outdir,'ERwater',paste0('bestfit_regression_backward_log',".png")),
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

png(file.path(outdir,'ERwater',paste0('crPlots_bfit_variables_log',".png")),
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


png(file.path(outdir,'ERwater',paste0('crPlots_bfit_variables_3vars',".png")),
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

png(file.path(outdir,'ERwater',paste0('avPlots_bfit_variables_log',".png")),
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
#sdata1$ERwc_Square[sdata1$ERwc_Square>0] = 0
sdata = cdata[c(yvar,xvars)];#
sdata =sdata[sdata$ERwc_Square<=0,]
sdata =na.omit(sdata)

mtry <- tuneRF(sdata[,-1],sdata[,1], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

set.seed(123)
rf_fit <- randomForest(ERwc_Square ~ ., ntree=500,maxnodes=4,mtry=best.m, data=sdata, importance=TRUE) #nPerm=3,
predicted <- unname(predict(rf_fit, data=cdata))
R2 <- 1 - (sum((sdata$ERwc_Square-predicted)^2)/sum((sdata$ERwc_Square-mean(sdata$ERwc_Square))^2))
R2


png(file.path(outdir,'ERwater',paste0('rf_importance_no_transform','.png')), 
    width = 8, height = 6, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(12.5,3.1,2.1,1))
#rimp <- importance(rf_fit)
vimp <- setNames(as.data.frame(rf_fit$importance)$IncNodePurity, row.names(as.data.frame(rf_fit$importance)))
vimp<- sort(vimp,decreasing = TRUE)
vimp

narg<-c("Temperature","Slope" ,"Velocity" ,"Discharge","TSS", "Total_Drainage_Area", 
        "Pct_Forest","Pct_Crop","Aridity",'D50')
for (v in 1:length(xvars)){
  idx <- grep(xvars[v],names(vimp))
  names(vimp)[idx]<-narg[v]
}
col.colors <- c(Temperature='#CC00FF', TSS='#00FF66',Velocity="#0066FF",
                Total_Drainage_Area="#FF0000",Slope='#FF0000',Discharge="#00FF66",
                Pct_Forest='#CCFF00',Pct_Crop ="#CCFF00", Aridity='#CC00FF',D50='#CCFF00')


order.names <- names(vimp)
order.colors <- col.colors[order.names] #rainbow(5)
barplot(vimp/sum(vimp),col =order.colors , horiz = FALSE,las=3,cex.lab=1.5, cex.axis=1.5, 
        cex.main=2,cex.names=1.5,ylab="Relative Importance")#,main=paste0("RF Feature Importance (PoreWater)"))
dev.off()








