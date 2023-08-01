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
sapply(cdata, function(x) sum(is.na(x)))
################################################################################################
# Stepwise Regression for ERsed
# fit <- lm(DO_Slope ~ DIC + NPOC + TN + TSS+T_mean+TOT_BASIN_AREA+StreamOrde, data = na.omit(cdata))
# step <- stepAIC(fit, direction="both")
# step$anova # display results
yvar ='ERsed_Square'
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", #'NPOC',#'TN',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'PctShrb2019Ws','D50_m',
          "hz_annual","Chlorophyll_A",'streamorde','GPP_Square')
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
#ffit<- lm(ERsed_Square ~ totdasqkm+velocity_ms +AridityWs+Slope+ Minidot_Temperature, data = sdata)
ffit<- lm(ERsed_Square ~ Slope+streamorde+Mean_Depth+HOBO_Temp, data = sdata)
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
bfit<- lm(ERsed_Square ~ HOBO_Temp+Mean_Depth+Slope+totdasqkm+D50_m+hz_annual+Chlorophyll_A+GPP_Square, data = sdata)
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
library(reprtree)
library(randomForestExplainer)
library(treeshap)
library(RColorBrewer)
library(iml)

# sdata1 =sdata
#sdata1$ERsed_Square[sdata1$ERsed_Square>0] = 0
yvar ='ERsed_Square'
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", 'NPOC',#'TN',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'PctShrb2019Ws','D50_m',
          "hz_annual","Chlorophyll_A",'streamorde','GPP_Square')
sdata = cdata[c(yvar,xvars)];#
sites = cdata$Site_ID[as.numeric(row.names(sdata))] 
names(sdata)[-1]<-c("Temperature",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", 'NPOC',#'TN',
                    "Drainage_Area","PctMxFst","PctCrop",'PctShrb','D50_m',
                    "Hflux","ChlA",'streamorde','GPP')
sdata =sdata[sdata$ERsed_Square<=0,]
sdata =na.omit(sdata)

palettes <- c(brewer.pal(9,name = 'Set1'),brewer.pal(length(xvars)-9,name = 'Set3'))
colors<-data.frame(color = palettes, xvars=c("Temperature",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", 'NPOC',#'TN',
                                                  "Drainage_Area","PctMxFst","PctCrop",'PctShrb','D50_m',
                                                  "Hflux","ChlA",'streamorde','GPP'))

mtry <- tuneRF(sdata[,-1],sdata[,1], ntreeTry=1000,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
##########################################
##  full data and all variables
set.seed(11)
rf_fit <- randomForest(ERsed_Square ~ ., ntree=100,nodesize=5, #maxnodes=5,
                       mtry=6, data=sdata, importance=TRUE, do.trace=100) #nPerm=3,
predicted <- unname(predict(rf_fit, data=sdata))
R2 <- 1 - (sum((sdata$ERsed_Square-predicted)^2)/sum((sdata$ERsed_Square-mean(sdata$ERsed_Square))^2))
R2
plot(sdata$ERsed_Square,predicted)

png(file.path(outdir,'ERsed',paste0('rf_importance_no_transform_all','.png')), 
    width = 8, height = 6, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(10.5,3.1,2.1,1))
#rimp <- importance(rf_fit)
#vimp <- setNames(as.data.frame(rf_fit$importance)$IncNodePurity, row.names(as.data.frame(rf_fit$importance)))
vimp<-data.frame(vimp=as.data.frame(rf_fit$importance)$IncNodePurity, 
                 xvars=row.names(as.data.frame(rf_fit$importance)))
vimp<- Reduce(function(x, y) merge(x, y,by ='xvars',all.x =TRUE), list(vimp,colors))
#vimp<- sort(vimp,decreasing = TRUE)
vimp<-vimp[order(vimp$vimp,decreasing = TRUE),]
barplot(vimp$vimp/sum(vimp$vimp),col =vimp$color , names.arg=vimp$xvars,
        horiz = FALSE,las=3,cex.lab=1.5, cex.axis=1.5, 
        cex.main=2,cex.names=1.5,ylab="Relative Importance")#,main=paste0("RF Feature Importance (PoreWater)"))
dev.off()


## plot tree in random forest
tree<- getTree(rf_fit, k=1, labelVar=FALSE)

#explain_forest(rf_fit, interactions = TRUE, data = sdata)
png(file.path(outdir,'ERsed',paste0('rf_importance_tree_rep_all','.png')), 
    width = 7, height = 5, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.1,2.1,1))
rf_fit$forest$xbestsplit<-round(rf_fit$forest$xbestsplit,3)
rf_fit$forest$nodepred <-round(rf_fit$forest$nodepred,2)
#reprtree:::plot.getTree(rf_fit,k=90, depth = 10)
reprtree:::plot.reprtree(ReprTree(rf_fit, sdata, metric='d2'),depth=10)
dev.off()

png(file.path(outdir,'ERsed',paste0('rf_importance_tree_2_all','.png')), 
    width = 7, height = 5, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.1,2.1,1))
rf_fit$forest$xbestsplit<-round(rf_fit$forest$xbestsplit,3)
rf_fit$forest$nodepred <-round(rf_fit$forest$nodepred,2)
reprtree:::plot.getTree(rf_fit,k=2, depth = 10)
#reprtree:::plot.reprtree(ReprTree(rf_fit, sdata, metric='d2'),depth=10)
dev.off()


# Create the explainer object
#explainer <- RandomForestExplainer(rf_fit, sdata)
unified_model <- randomForest.unify(rf_fit, sdata)
shaps <- treeshap(unified_model, sdata, interactions = TRUE)
plot_contribution(shaps, obs = 35)
#plot_feature_dependence(shaps, 'Temperature')
library(ggrepel)
for (var in names(sdata)[-1]){
  p<-plot_feature_dependence(shaps, var)+ theme_classic() + 
    theme(
      axis.line.x = element_line(colour = "grey50"),
      axis.line.y = element_line(colour = "grey50"))+
    geom_label_repel(aes(label = sites),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') 
    # geom_text_repel(aes(label = sites),
    #                 color='red',segment.curvature = -1e-20,point.padding = 0.5,
    #                 nudge_x = .25,nudge_y = .25,max.overlaps=40,size=2,
    #                 min.segment.length = 0, seed = 42, box.padding = 0.25)
  ggsave(file.path(outdir,'ERsed','shap_all',paste0('shap_feature_dependence_',var,'_label.png')),
         p,device = "png",width = 6, height = 4,dpi=300)
}

##########################################
## drop 3 fastest ER rates and included all variables
sdata =sdata[(sdata$ERsed_Square<=0)&(sdata$ERsed_Square>(-15)),]

set.seed(11)
rf_fit <- randomForest(ERsed_Square ~ ., ntree=100,nodesize=5, #maxnodes=5,
                       mtry=6, data=sdata, importance=TRUE, do.trace=100) #nPerm=3,
predicted <- unname(predict(rf_fit, data=sdata))
R2 <- 1 - (sum((sdata$ERsed_Square-predicted)^2)/sum((sdata$ERsed_Square-mean(sdata$ERsed_Square))^2))
R2

png(file.path(outdir,'ERsed',paste0('rf_importance_no_transform_drop_3ER','.png')), 
    width = 8, height = 6, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(10.5,3.1,2.1,1))
#rimp <- importance(rf_fit)
#vimp <- setNames(as.data.frame(rf_fit$importance)$IncNodePurity, row.names(as.data.frame(rf_fit$importance)))
vimp<-data.frame(vimp=as.data.frame(rf_fit$importance)$IncNodePurity, 
                 xvars=row.names(as.data.frame(rf_fit$importance)))
vimp<- Reduce(function(x, y) merge(x, y,by ='xvars',all.x =TRUE), list(vimp,colors))
#vimp<- sort(vimp,decreasing = TRUE)
vimp<-vimp[order(vimp$vimp,decreasing = TRUE),]
vimp
barplot(vimp$vimp/sum(vimp$vimp),col =vimp$color , names.arg=vimp$xvars,
        horiz = FALSE,las=3,cex.lab=1.5, cex.axis=1.5, 
        cex.main=2,cex.names=1.5,ylab="Relative Importance")#,main=paste0("RF Feature Importance (PoreWater)"))
dev.off()


## plot tree in random forest
tree<- getTree(rf_fit, k=1, labelVar=FALSE)

#explain_forest(rf_fit, interactions = TRUE, data = sdata)
png(file.path(outdir,'ERsed',paste0('rf_importance_tree_rep_drop_3ER','.png')), 
    width = 7, height = 5, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.1,2.1,1))
rf_fit$forest$xbestsplit<-round(rf_fit$forest$xbestsplit,3)
rf_fit$forest$nodepred <-round(rf_fit$forest$nodepred,2)
#reprtree:::plot.getTree(rf_fit,k=90, depth = 10)
reprtree:::plot.reprtree(ReprTree(rf_fit, sdata, metric='d2'),depth=10)
dev.off()

png(file.path(outdir,'ERsed',paste0('rf_importance_tree_2_drop_3ER','.png')), 
    width = 7, height = 5, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.1,2.1,1))
rf_fit$forest$xbestsplit<-round(rf_fit$forest$xbestsplit,3)
rf_fit$forest$nodepred <-round(rf_fit$forest$nodepred,2)
reprtree:::plot.getTree(rf_fit,k=2, depth = 10)
#reprtree:::plot.reprtree(ReprTree(rf_fit, sdata, metric='d2'),depth=10)
dev.off()
##########################################
## Drop GPP in the RF model
yvar ='ERsed_Square'
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", 'NPOC',#'TN',
          "totdasqkm","PctMxFst2019Ws","PctCrop2019Ws",'PctShrb2019Ws','D50_m',
          "hz_annual","Chlorophyll_A",'streamorde')
sdata = cdata[c(yvar,xvars)];#
names(sdata)[-1]<-c("Temperature",'Mean_Depth',"Slope","Velocity" , "AridityWs","TSS","Discharge", 'NPOC',#'TN',
                    "Drainage_Area","PctMxFst","PctCrop",'PctShrb','D50_m',
                    "Hflux","ChlA",'streamorde')
sdata =sdata[sdata$ERsed_Square<=0,]
sdata =na.omit(sdata)

set.seed(11)
rf_fit <- randomForest(ERsed_Square ~ ., ntree=100,nodesize=5, #maxnodes=5,
                       mtry=5, data=sdata, importance=TRUE, do.trace=100) #nPerm=3,
predicted <- unname(predict(rf_fit, data=sdata))
R2 <- 1 - (sum((sdata$ERsed_Square-predicted)^2)/sum((sdata$ERsed_Square-mean(sdata$ERsed_Square))^2))
R2

png(file.path(outdir,'ERsed',paste0('rf_importance_no_transform_drop_GPP','.png')), 
    width = 8, height = 6, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(10.5,3.1,2.1,1))
#rimp <- importance(rf_fit)
#vimp <- setNames(as.data.frame(rf_fit$importance)$IncNodePurity, row.names(as.data.frame(rf_fit$importance)))
vimp<-data.frame(vimp=as.data.frame(rf_fit$importance)$IncNodePurity, 
                 xvars=row.names(as.data.frame(rf_fit$importance)))
vimp<- Reduce(function(x, y) merge(x, y,by ='xvars',all.x =TRUE), list(vimp,colors))
#vimp<- sort(vimp,decreasing = TRUE)
vimp<-vimp[order(vimp$vimp,decreasing = TRUE),]
vimp
barplot(vimp$vimp/sum(vimp$vimp),col =vimp$color , names.arg=vimp$xvars,
        horiz = FALSE,las=3,cex.lab=1.5, cex.axis=1.5, 
        cex.main=2,cex.names=1.5,ylab="Relative Importance")#,main=paste0("RF Feature Importance (PoreWater)"))
dev.off()


## plot tree in random forest
tree<- getTree(rf_fit, k=1, labelVar=FALSE)

#explain_forest(rf_fit, interactions = TRUE, data = sdata)
png(file.path(outdir,'ERsed',paste0('rf_importance_tree_rep_drop_GPP','.png')), 
    width = 7, height = 5, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.1,2.1,1))
rf_fit$forest$xbestsplit<-round(rf_fit$forest$xbestsplit,3)
rf_fit$forest$nodepred <-round(rf_fit$forest$nodepred,2)
#reprtree:::plot.getTree(rf_fit,k=90, depth = 10)
reprtree:::plot.reprtree(ReprTree(rf_fit, sdata, metric='d2'),depth=10)
dev.off()


png(file.path(outdir,'ERsed',paste0('rf_importance_tree_2_drop_GPP','.png')), 
    width = 7, height = 5, units = 'in', res = 600)
par(mgp=c(2,0.5,0),mar=c(3.5,3.1,2.1,1))
rf_fit$forest$xbestsplit<-round(rf_fit$forest$xbestsplit,3)
rf_fit$forest$nodepred <-round(rf_fit$forest$nodepred,2)
reprtree:::plot.getTree(rf_fit,k=2, depth = 10)
#reprtree:::plot.reprtree(ReprTree(rf_fit, sdata, metric='d2'),depth=10)
dev.off()


