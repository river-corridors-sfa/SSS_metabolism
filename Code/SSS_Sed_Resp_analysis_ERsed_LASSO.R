# RC2 spatial study - Multiple linear regression 
# ER_sed
# X Lin April 18 2023
################################################################################################
# Read in data 
################################################################################################
rm(list=ls(all=TRUE))
library(glmnet)
source('./COde/SSS_Sed_Resp_data_merging.R')
outdir<-'./MLR_Analysis_Figures'
##############################################################################################################
# read in data
cdata <- data_merge()
# add a coloumn 'ratio'
#cdata['Ratio'] <- cdata$Mean_Depth/cdata$D50_m
cdata$TN[is.na(cdata$TN)]<-min(cdata$TN,na.rm=TRUE)/2
rownames(cdata) <- cdata$Site_ID
# Fill in average ERsed for Site T03 (originally NA) 
cdata$ERsed_Square[cdata$Site_ID=='T03'] <- cdata$ERtotal_Square[cdata$Site_ID=='T03']-mean(cdata$ERwc_Square,na.rm=TRUE)
# remove positive ERsed
sdata =cdata[cdata$ERsed_Square<=0,]
sapply(cdata, function(x) sum(is.na(x)))
################################################################################################
# lasso regression

yvar ='ERsed_Square'
xvars = c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" ,"Discharge","TSS", 'TN','NPOC',
          "totdasqkm","PctFst","PctAg",'PctShrb2019Ws',"AridityWs",  
          'D50_m',"hz_annual","Chlorophyll_A",'streamorde','GPP_Square') #"PctMxFst2019Ws","PctCrop2019Ws"
sdata = cdata[c(yvar,xvars)];#
sdata =sdata[sdata$ERsed_Square<=0,]
#############################################################
#log transform variables
ldata<- sdata[c(yvar,xvars)]
vars <-names(ldata)
#log transform variables
for ( v in 1:length(vars)){
  if(vars[v] %in% c("ERsed_Square")){
    ldata[vars[v]] <- log10(abs(ldata[vars[v]])+1)
  }else if (vars[v] %in% c('hz_annual',"AridityWs","HOBO_Temp","PctFst")){
    ldata[vars[v]] <- ldata[vars[v]]
  }
  else if(vars[v] %in% c("Chlorophyll_A","GPP_Square","PctAg")){
    ldata[vars[v]] <- log10(ldata[,vars[v]]+1)
  }else{
    ldata[vars[v]] <- log10(ldata[vars[v]])
  }
}
#

################################################################################################
#sdata =sdata[-which(sdata$ERsed_Square < -15),]
xvars2 <- c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" ,"Discharge", 'NPOC', #"TSS",
            "totdasqkm","PctFst",'PctShrb2019Ws',"AridityWs",  
            'D50_m',"hz_annual","Chlorophyll_A",'streamorde','GPP_Square')
ldata2<-ldata[c(yvar,xvars2)]
ldata2 <-na.omit(ldata2)
#define intercept-only model
intercept_only <- lm(ERsed_Square ~ 1, data=ldata2)

#define model with all predictors
all <- lm(ERsed_Square ~ ., data = ldata2)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), steps=5000,trace=1)
forward$anova
forward$coefficients
summary(forward)
#  lm fitting using selected variables from forward stepwise selection
#ffit<- lm(ERsed_Square ~ totdasqkm+velocity_ms +AridityWs+Slope+ Minidot_Temperature, data = sdata)
ffit<- lm(ERsed_Square ~ GPP_Square+Slope+totdasqkm+Mean_Depth+Chlorophyll_A, data = ldata2)
#ffit<- lm(ERsed_Square ~ hz_spring+ D50_m+Slope+Mean_Depth+HOBO_Temp, data = sdata)
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
summary(backward)


################################################################################################
# lasso regression 
xvars2 <- c("HOBO_Temp",'Mean_Depth',"Slope","Velocity" ,"Discharge", 'NPOC', #"TSS",
            "totdasqkm","PctFst",'PctShrb2019Ws',"AridityWs",  
            'D50_m',"hz_annual","Chlorophyll_A",'streamorde','GPP_Square')
ldata2<-ldata[c(yvar,xvars2)]
ldata2 <-na.omit(ldata2)

# identifying best lamda
lambdas_to_try <- 10^seq(-3, 7, length.out = 100)
set.seed(100)
Xs <-as.matrix(ldata2[,xvars2])
ys <-as.matrix(ldata2[,yvar])
lasso_cv <- cv.glmnet(Xs, ys, alpha = 1, lambda = lambdas_to_try,nfolds = 3)
plot(lasso_cv)
#r2 <-lasso_cv$glmnet.fit$dev.ratio[which(lasso_cv$glmnet.fit$lambda==lasso_cv$lambda.min)]
optimal_lambda <- lasso_cv$lambda.min
## Rebuilding the model with best lamda value identified
best_model <- glmnet(Xs, ys, lambda=optimal_lambda, family='gaussian', intercept = F, alpha=1) 
coef(best_model)
best_model$beta
lasso_pred <- predict(best_model, s = optimal_lambda, newx = Xs)
R2 <- 1 - (sum((ys-lasso_pred)^2)/sum((ys-mean(ys))^2))
R2
# bfit<- lm(ERsed_Square ~ GPP_Square+Slope+hz_spring+Chlorophyll_A+PctFst+AridityWs, data = ldata2)
# summary(bfit)
################################################################################################
# normalized variables after log transform
sldata2 <- scale(ldata2, center = TRUE, scale = TRUE)
# check that we get mean of 0 and sd of 1
#colMeans(sldata2)  # faster version of apply(scaled.dat, 2, mean)
#apply(sldata2, 2, sd)
# identifying best lamda
lambdas_to_try <- 10^seq(-3, 7, length.out = 100)
set.seed(100)
Xs <-as.matrix(sldata2[,xvars2])
ys <-as.matrix(sldata2[,yvar])
lasso_cv <- cv.glmnet(Xs, ys, alpha = 1, lambda = lambdas_to_try,nfolds = 3)
plot(lasso_cv)
#r2 <-lasso_cv$glmnet.fit$dev.ratio[which(lasso_cv$glmnet.fit$lambda==lasso_cv$lambda.min)]
optimal_lambda <- lasso_cv$lambda.min
## Rebuilding the model with best lamda value identified
best_model <- glmnet(Xs, ys, lambda=optimal_lambda, family='gaussian', intercept = F, alpha=1) 
coef(best_model)
best_model$beta
lasso_pred <- predict(best_model, s = optimal_lambda, newx = Xs)
R2 <- 1 - (sum((ys-lasso_pred)^2)/sum((ys-mean(ys))^2))
R2

################################################################################################
# robust regression 
# Robust regression deals with influential observations and outliers.
#sdata2 <-na.omit(sdata)
sdata2 <-ldata2
# inputs selscted by forward/backward selection
rr.huber <- rlm(ERsed_Square ~ GPP_Square+Slope+hz_spring+Chlorophyll_A+PctFst+AridityWs, 
                data = sdata2, psi = psi.huber,maxit=50)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(rr.huber, las = 1)
summary(rr.huber)

rlm_pred <- predict(rr.huber, newx = Xs)
R2 <- 1 - (sum((ys-rlm_pred)^2)/sum((ys-mean(ys))^2))
R2
plot(forward$fitted.values, rlm_pred)


# odata <- odata%>%arrange(median(ERwc))%>%
#   mutate(Site_ID = factor(Site_ID, levels = unique(Site_ID)))
ldata2['rlm_pred'] <-rlm_pred
ldata2['lasso_pred'] <-lasso_pred
ldata3 <- ldata2[order(ldata2$ERsed_Square),]
sites<-row.names(ldata3)
ldata3['rank'] <-c(1:nrow(ldata3))

png(file.path(outdir,'ERsed',paste0('robust_huber_lasso_vars',".png")),
    width = 6, height = 4, units = 'in', res = 600)
par(mfrow=c(1,1),mgp=c(2.2,1,0),mar=c(3.1,4.1,2,1.5))
plot(ldata3$rank,ldata3$ERsed_Square,type = 'b', 
     xlab='Index',
     ylab=expression("ER"[sed]*" (mg O"[2]*" L"^-1*"day"^-1*")"))
lines(ldata3$rank,ldata3$rlm_pred,type = 'b', col="blue") # lowess line (x,y)
lines(ldata3$rank,ldata3$lasso_pred,type = 'b', col="red") # lowess line (x,y)
sid = which(abs(ldata3$ERsed_Square-ldata3$rlm_pred)>0.15)
text(ldata3$rank[sid], ldata3$rlm_pred[sid], sites[sid],cex=0.65, pos=3)
legend("topleft", legend=c("ERsed", "ERsed (robust) R2=0.61","ERsed (lasso) R2=0.58"),
       col=c('black',"red", "blue"),box.lty=0, lty=1, cex=0.8)
dev.off()



# 
rr.huber <- rlm(ERsed_Square ~ GPP_Square + Mean_Depth + totdasqkm + Chlorophyll_A + Slope, 
                data = ldata2, psi = psi.huber,maxit=50)
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(rr.huber, las = 1)
summary(rr.huber)

rlm_pred <- predict(rr.huber, newx = Xs)
R2 <- 1 - (sum((ys-rlm_pred)^2)/sum((ys-mean(ys))^2))
R2

# odata <- odata%>%arrange(median(ERwc))%>%
#   mutate(Site_ID = factor(Site_ID, levels = unique(Site_ID)))
ldata2['rlm_pred'] <-rlm_pred
ldata2['lm_pred'] <-forward$fitted.values
ldata3 <- ldata2[order(ldata2$ERsed_Square),]
sites<-row.names(ldata3)
ldata3['rank'] <-c(1:nrow(ldata3))

png(file.path(outdir,'ERsed',paste0('robust_huber_forward_vars',".png")),
    width = 6, height = 4, units = 'in', res = 600)
par(mfrow=c(1,1),mgp=c(2.2,1,0),mar=c(3.1,4.1,2,1.5))
plot(ldata3$rank,ldata3$ERsed_Square,type = 'b', 
     xlab='Index',
     ylab=expression("ER"[sed]*" (mg O"[2]*" L"^-1*"day"^-1*")"))
lines(ldata3$rank,ldata3$rlm_pred,type = 'b', col="blue") # lowess line (x,y)
lines(ldata3$rank,ldata3$lm_pred,type = 'b', col="red") # lowess line (x,y)
sid = which(abs(ldata3$ERsed_Square-ldata3$rlm_pred)>0.15)
text(ldata3$rank[sid], ldata3$rlm_pred[sid], sites[sid],cex=0.65, pos=3)
legend("topleft", legend=c("ERsed", "ERsed (robust) R2=0.67","ERsed (forward) R2=0.67"),
       col=c('black',"red", "blue"),box.lty=0, lty=1, cex=0.8)
dev.off()


# hweights <- data.frame(ERsed_Square = sdata2$ERsed_Square, resid = rr.huber$resid, weight = rr.huber$w)
# hweights2 <- hweights[order(rr.huber$w), ]
# #Tukey bisquare
# rr.bisquare <- rlm(ERsed_Square ~., data=sdata2,psi = psi.bisquare,maxit=50)
# summary(rr.bisquare)
# biweights <- data.frame(ERsed_Square = sdata2$ERsed_Square, resid = rr.bisquare$resid, weight = rr.bisquare$w)
# biweights2 <- biweights[order(rr.bisquare$w), ]
# correlation matrix
png(file.path(outdir,'ERsed',paste0('exploratory_variables_correlation_matrix_log_vars',".png")),
    width = 12, height = 8, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(ldata2, histogram=TRUE, pch=19)
dev.off()

png(file.path(outdir,'ERsed',paste0('exploratory_variables_correlation_matrix_log_normalized_vars',".png")),
    width = 12, height = 8, units = 'in', res = 600)
#par(mfrow=c(2,2)) 
chart.Correlation(sldata2, histogram=TRUE, pch=19)
dev.off()



cdata2<-cdata[which(cdata$Site_ID%in%rownames(ldata2)),]
cdata2<-cdata2[c(yvar,xvars2)]
for (var in c(yvar,xvars2)){
  png(file.path(outdir,'ERsed','histograms',paste0(var,'_hist3',".png")),
      width = 6, height = 2, units = 'in', res = 600)
  par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3.4,3.4,1,1.5))
  hist(cdata2[,var],breaks=10,xlab=var,main='')
  hist(ldata2[,var],breaks=10,xlab=paste0(var,'_log'),main='')
  hist(sldata2[,var],breaks=10,xlab=paste0(var,'_log_nor'),main='')
  dev.off()
}

