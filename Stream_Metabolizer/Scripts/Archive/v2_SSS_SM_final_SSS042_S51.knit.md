---
title: "stream metabolizer SSS template"
author: "kaufman"
date: "2024-03-06"
output: html_document
#output_file: 'NA'
#params:
#  PARENT_ID: 'NA'
#  SITE_ID: 'NA'

---


##Matt Kaufman, matthew.kaufman@pnnl.gov, Pacific Northwest National Laboratory
##This code is the a single-site file for a special case for stream metabolizer for one site in the 2022 spatial study dataset. It takes in time-series dissolved oxygen, depth, and barometric pressure data, as well as single-point location data and an estimate of k600. From all of that, it estimates GPP, ER, and k600 on a daily basis over the course of the deployment. In general, if there is a lot of correlation between k600 and ER results, it is acceptable to use the deployment-period-average values, but the day-to-day variation should not be trusted.


## 1. Install packages and loading libraries
If this is your first time running this code or streamMetabolizer, make sure to install the appropriate packages below. uncomment them in the chunk before Knitting.


```r
#install.packages(remotes); library(remotes)
# remotes::install_github('appling/unitted', force = TRUE)
# remotes::install_github("USGS-R/streamMetabolizer", force = TRUE)
#install.packages("rstan", dependencies = FALSE)
#install.packages(devtools)

# If you have trouble installing rstan, try the installation codes below: 

#devtools::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan", force = TRUE)
#install.packages("rstan", type = "source")

# Run the line below if you have trouble installing devtools
#devtools::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan")
```

### Loading libraries


## 2. Setting up the data
Set working directory and read in the data and change units to match the needs of stream metabolizer.
Note: Make sure to always double check that your date time column is in the date time format and not as character


```r
#-------------------------------------
#PARENT_ID='' #this is where you would specify a site if you were not running this as a loop
#-------------------------------------

print("PARENT_ID: ")
```

```
## [1] "PARENT_ID: "
```

```r
PARENT_ID<-'SSS042'

print("SITE_ID: ")
```

```
## [1] "SITE_ID: "
```

```r
SITE_ID<-'S51'
```


```r
#bp<-bpcalc(29.9, 489/3.28)# elevation at satus (alt in meters) #---------------------------------------------------------------------

data.path = "Inputs/"

dat = read.csv(paste0(data.path,'Sensor_Files/v2_',PARENT_ID,"_Temp_DO_Press_Depth.csv"),header=T,skip=8)
 
 K600estimates=read.csv(paste0(data.path,'v2_SSS_K600.csv'),header=T,skip=3)
 K600estimate<-K600estimates[K600estimates$Site_ID==SITE_ID,2]
 
print("K600 estimate: ")
```

```
## [1] "K600 estimate: "
```

```r
K600estimate
```

```
## [1] 65.78
```

```r
 output.path="Outputs/"

 #---------------------------------------------------------------------------------------------
  file.name = paste('v2_SSS_SM_',PARENT_ID,'_',SITE_ID,'_final',sep='')
  #dat = na.omit(dat)
  #dat=dat[450:33311-300,] #-------------------------------------------------------------------------------------------------------------
 # Change date time format  

#DOWNSAMPLE
samplingmins=15
dat = dat[seq(1, nrow(dat), samplingmins), ]


#colnames(dat)[10]="Unix.Timestamp" 
#dat$timeUTC<-as_datetime(dat$Unix.Timestamp)

#data is always collected in pacific-standard-time, so conversion to UTC is +8 hours
dat$timeUTC<-as.POSIXct(dat$DateTime)+hours(8)
dat$timeUTC<-force_tz(dat$timeUTC,tzone='UTC')
dat$solar.time<-convert_UTC_to_solartime(dat$timeUTC, longitude= dat$Longitude[1], time.type="mean solar")


#----------------------------------


dat$light<- calc_light(dat$solar.time, latitude=dat$Latitude[1], longitude=dat$Longitude[1], max.PAR =2300, attach.units = F) #------------------

# cal_DO_stat requieres barometric pressure in millibars, or a unitted object of barometric pressure.
dat$DO.sat=calc_DO_sat(dat$Temperature,dat$Pressure, model = 'garcia-benson') 


# Selecting the data types that are needed for stream metabolizer and changing header names. Running the model with K600_pooling = normal does not require discharge input

dat = dat %>% dplyr::select("solar.time" = solar.time,"DO.obs" = Dissolved_Oxygen,"DO.sat" = DO.sat,"temp.water" = Temperature,"light" = light,"depth" = Depth)
```

Check the number of cores you have in your computer. Based on the number that it prints, set the number of cores you want to dedicate to the metabolism run. It is recommended to set 2-4 cores less than you have in your computer to minimize the chances of R crashing. It is also recommended to select a pair number for your run. 


```r
parallel::detectCores()
```

```
## [1] 12
```

## 3. Inspect Data
<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/inspect-1.png" width="672" /><img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/inspect-2.png" width="672" /><img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/inspect-3.png" width="672" />

## 4. Configure the model
We will select a Bayesian model. Then we will configure the specs of the model depending on the needs of our run.    
You can play around with the number of iterations (e.g., 100 burnin iterations , also called warm up) and 50 saved steps. You can adjust the number of iterations based on the convergence of the convergence of the model.  
For example, you may start with 1000 and 500 and then up the numbers to 2000 and 1000 if the model results don't seem to converge. 

If you have already adjusted the number of steps multiple times and your model fits are still not good (e.g., negative GPP values) you might have to consider changing other specs in the model. Some examples of variables that you may change are: GPP_daily_lower = 0.01,ER_daily_upper = -0.01. However, you should consult the Help for more information.

Use the command plot_distribs if you want to observe the distribution of the specs if they were changed. 



```r
# Set the model

bayes_name = mm_name(type='bayes',
                     pool_K600='normal', 
                     err_obs_iid=TRUE, 
                     err_proc_iid=TRUE)
bayes_name
```

```
## [1] "b_Kn_oipi_tr_plrckm.stan"
```

```r
 # Options for pool K600 are binned, linear, none and normal. If normal is specified, discharge doesn't need to be provided

# Changing the specs
bayes_specs = specs(bayes_name, K600_daily_meanlog_meanlog=log(K600estimate), K600_daily_meanlog_sdlog=0.7, K600_daily_sdlog_sigma=0.05, burnin_steps=1000, 
                  saved_steps=1000)
```

## 5. Fit the model and save the results
Fitting the model might take hours or days depending on the dataset.
Some times R crashes while you are running the model so you want to make sure to save your results in each run. Create an output folder inside of the path where you are storing the data, the output path will update automatically here once you change your data path in step 2. 

Some key parameters to look at in the mm output are:  
- $daily (includes metabolism estimates) 
- $overall (includes error information) 
- $KQ_overall (included the relationship between K600-Q)


```r
mm = metab(bayes_specs, data=dat)# 
#load("~/GitHub/gitlab/SSS_metabolism/initial_SM_testing/test_15min.RData")

#Extracting the data from the model output the outputs are in a S4
#class of data and you'll need to operators to extract the daily
#time series of estimates
#get_fit(mm) %>%
  #lapply(names)

# Saving key data
#output.path = paste0(data.path,"/Output/")
preds = mm@fit$daily
preds_output<-preds
#str(preds)
write.csv(preds_output,paste0(output.path,'v2_',PARENT_ID,"_",SITE_ID,"_SM_final_daily_prediction_results.csv"),row.names = FALSE) 

instant = mm@fit$inst
instant_output<-instant
#str(instant)
write.csv(instant_output,paste0(output.path,'v2_',PARENT_ID,"_",SITE_ID,"_SM_final_instant_fit_results.csv"),row.names = FALSE)

Overall = mm@fit$overall
Overall_output<-Overall
#str(Overall)
write.csv(Overall_output,paste0(output.path,'v2_',PARENT_ID,"_",SITE_ID,"_SM_final_overall_fit_results.csv"),row.names = FALSE)

full = predict_DO(mm)
full_output<-full
#str(KQ)
write.csv(full_output,paste0(output.path,'v2_',PARENT_ID,"_",SITE_ID,"_SM_final_full_prediction_results.csv"),row.names = FALSE)


#get_data(mm)# Shows a table with all the data + DO modeled
#get_data_daily(mm) #daily fitting data. It shows values for Q for now
#get_params(mm)
```

## 6. Inspect GPP, ER and K600

### Daily predictions of modeled GPP and ER

The goal is for the predictions (lines) and observations (points) to be very similar. 


```
## # A tibble: 36 × 10
##    date          GPP GPP.lower GPP.upper      ER ER.lower ER.upper msgs.fit 
##    <date>      <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl> <chr>    
##  1 2022-07-26 NA       NA          NA    NA        NA       NA     "w     E"
##  2 2022-07-27  3.28     1.13        6.33  1.75      1.37     2.10  "w      "
##  3 2022-07-28  1.21    -0.0171      2.69  1.24      0.864    1.64  "w      "
##  4 2022-07-29  1.12    -0.162       2.45  0.644     0.175    1.13  "w      "
##  5 2022-07-30  0.532   -0.547       1.68  1.11      0.650    1.67  "w      "
##  6 2022-07-31  0.512   -0.547       1.67  0.985     0.530    1.45  "w      "
##  7 2022-08-01  2.96     2.29        3.82 -0.0359   -0.437    0.335 "w      "
##  8 2022-08-02  0.448   -0.332       1.53  1.23      0.799    1.66  "w      "
##  9 2022-08-03  0.555   -0.324       1.81  0.983     0.508    1.44  "w      "
## 10 2022-08-04  0.705   -0.153       2.01  1.14      0.657    1.56  "w      "
## # ℹ 26 more rows
## # ℹ 2 more variables: warnings <chr>, errors <chr>
```

<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/inspect1-1.png" width="672" />

```
## # A tibble: 36 × 9
##    date       GPP.daily GPP.daily.sd ER.daily ER.daily.sd K600.daily
##    <date>         <dbl>        <dbl>    <dbl>       <dbl>      <dbl>
##  1 2022-07-26    NA           NA      NA           NA           NA  
##  2 2022-07-27     3.40         1.34    1.74         0.187       42.4
##  3 2022-07-28     1.25         0.684   1.24         0.198       21.5
##  4 2022-07-29     1.14         0.656   0.650        0.240       17.7
##  5 2022-07-30     0.538        0.551   1.12         0.250       13.5
##  6 2022-07-31     0.540        0.543   0.985        0.228       13.0
##  7 2022-08-01     2.99         0.401  -0.0391       0.200       29.1
##  8 2022-08-02     0.488        0.481   1.23         0.224       12.9
##  9 2022-08-03     0.613        0.550   0.982        0.242       14.6
## 10 2022-08-04     0.768        0.570   1.13         0.231       14.6
## # ℹ 26 more rows
## # ℹ 3 more variables: K600.daily.sd <dbl>, warnings <chr>, errors <chr>
```

```
##         date          solar.time DO.obs   DO.sat depth temp.water light DO.mod
## 1 2022-07-26 2022-07-26 23:58:06   9.71 9.708931  0.54     13.947     0     NA
## 2 2022-07-26 2022-07-27 00:13:06   9.75 9.750186  0.55     13.755     0     NA
## 3 2022-07-26 2022-07-27 00:28:06   9.79 9.787843  0.55     13.581     0     NA
## 4 2022-07-26 2022-07-27 00:43:06   9.82 9.820295  0.54     13.432     0     NA
## 5 2022-07-26 2022-07-27 00:58:06   9.87 9.863326  0.55     13.231     0     NA
## 6 2022-07-26 2022-07-27 01:13:06   9.91 9.897167  0.56     13.073     0     NA
```

<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/inspect1-2.png" width="672" />

Ideally, good model results should have n_eff > 100 and Rhat < = 1.1. Below is a summary of these metrics for daily GPP, ER and K600. 



```
## # A tibble: 36 × 6
##    GPP_Rhat ER_Rhat DO_R2_Rhat GPP_daily_Rhat ER_daily_Rhat K600_daily_Rhat
##       <dbl>   <dbl>      <dbl>          <dbl>         <dbl>           <dbl>
##  1    NA      NA         NA             NA            NA              NA   
##  2     1.05    1.02       1.52           1.05          1.02            1.05
##  3     1.05    1.04       1.50           1.05          1.04            1.04
##  4     1.14    1.10       1.55           1.14          1.10            1.13
##  5     1.07    1.05       1.52           1.07          1.05            1.07
##  6     1.05    1.03       1.52           1.05          1.03            1.05
##  7     1.04    1.04       1.55           1.04          1.04            1.03
##  8     1.05    1.05       1.51           1.05          1.05            1.04
##  9     1.02    1.02       1.51           1.02          1.02            1.02
## 10     1.07    1.06       1.52           1.07          1.06            1.06
## # ℹ 26 more rows
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.004   1.011   1.021   1.028   1.039   1.096       4
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.011   1.023   1.047   1.049   1.068   1.139       4
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.011   1.023   1.041   1.048   1.064   1.132       4
```

```
## # A tibble: 36 × 6
##    GPP_n_eff ER_n_eff DO_R2_n_eff GPP_daily_n_eff ER_daily_n_eff
##        <dbl>    <dbl>       <dbl>           <dbl>          <dbl>
##  1      NA       NA         NA               NA             NA  
##  2      72.7    307.         5.79            72.7          307. 
##  3      63.9    180.         5.74            63.9          180. 
##  4      33.6     51.9        5.46            33.6           51.9
##  5      54.9     55.9        5.58            54.9           55.9
##  6      64.5     98.7        5.47            64.5           98.7
##  7     173.     215.         5.56           173.           215. 
##  8     135.     126.         5.68           135.           126. 
##  9     107.     115.         5.64           107.           115. 
## 10      71.9    119.         5.81            71.9          119. 
## # ℹ 26 more rows
## # ℹ 1 more variable: K600_daily_n_eff <dbl>
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   51.85  125.68  191.88  187.31  228.95  318.66       4
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   33.61   72.54  123.21  115.92  155.05  231.11       4
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   34.58   76.30  123.32  120.08  157.06  219.29       4
```

## 7. Inspect errors and their standard deviations

err_obs_iid_Rhat should have a value < 1.1. Additionally, in a conversation with Allison Appling she mentioned that they have seen pretty frequently err_obs_iid_sigma_Rhats much greater than 1.05, and mentioned it on the [JGR 2018 paper](https://doi.org/10.1002/2017JG004140). This is OK for that particular parameter because the values that the model continues to consider are usually quite similar in absolute magnitudes. 

### Inspect err_obs_iid_Rhat and err_obs_iid_sigma_Rhat

```r
#err_obs_iid_Rhat
get_fit(mm)$inst %>%
  select(ends_with('Rhat'))
```

```
## # A tibble: 3,072 × 2
##    err_obs_iid_Rhat err_proc_iid_Rhat
##               <dbl>             <dbl>
##  1            0.999              1.01
##  2            1.00               1.00
##  3            1.00               1.00
##  4            0.999              1.01
##  5            1.00               1.01
##  6            0.999              1.00
##  7            1.00               1.00
##  8            1.00               1.00
##  9            1.00               1.01
## 10            1.00               1.01
## # ℹ 3,062 more rows
```

```r
# err_obs_iid_sigma_Rhat. See comment from Appling
get_fit(mm)$overall %>%
  select(ends_with('Rhat'))
```

```
## # A tibble: 1 × 3
##   err_obs_iid_sigma_Rhat err_proc_iid_sigma_Rhat lp___Rhat
##                    <dbl>                   <dbl>     <dbl>
## 1                   1.79                    1.03      2.08
```

```r
get_fit(mm)$inst %>%
  select(ends_with('n_eff'))
```

```
## # A tibble: 3,072 × 2
##    err_obs_iid_n_eff err_proc_iid_n_eff
##                <dbl>              <dbl>
##  1             6345.               364.
##  2             6248.               314.
##  3             6256.               362.
##  4             6122.               416.
##  5             6340.               411.
##  6             7145.               359.
##  7             6529.               413.
##  8             6168.               396.
##  9             6442.               240.
## 10             6177.               299.
## # ℹ 3,062 more rows
```

```r
get_fit(mm)$overall %>%
  select(ends_with('n_eff'))
```

```
## # A tibble: 1 × 3
##   err_obs_iid_sigma_n_eff err_proc_iid_sigma_n_eff lp___n_eff
##                     <dbl>                    <dbl>      <dbl>
## 1                    5.55                     179.       6.01
```

```r
summary(instant$err_obs_iid_n_eff)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   420.4  6177.4  6572.2  6584.4  6983.1  9102.2
```

```r
summary(instant$err_obs_iid_Rhat)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.9990  0.9994  0.9996  0.9997  0.9999  1.0150
```

```r
summary(instant$err_proc_iid_n_eff)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   56.58  283.60  424.49  886.70  849.95 5191.90      32
```

```r
summary(instant$err_proc_iid_Rhat)  
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.9993  1.0036  1.0078  1.0117  1.0155  1.0986      32
```

```r
summary(Overall$err_obs_iid_sigma_Rhat)# There is only one err_obs_iid_sigma_Rhat per run
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.787   1.787   1.787   1.787   1.787   1.787
```

```r
summary(Overall$err_proc_iid_sigma_Rhat)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.035   1.035   1.035   1.035   1.035   1.035
```

```r
summary(Overall$err_obs_iid_sigma_n_eff)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   5.554   5.554   5.554   5.554   5.554   5.554
```

```r
summary(Overall$err_proc_iid_sigma_n_eff)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   179.4   179.4   179.4   179.4   179.4   179.4
```

## 8. Inspect the relationships between variables

### Relationship between k600 and Q: we do not have Q in this experiment, so use depth as a proxy

The first step is to plot k600 and depth. To find this relationship we have to calculate the mean depth per day and then plot against k600.


```r
    dat$solar.time = as.Date(dat$solar.time)
    meanDday = aggregate(dat["depth"],by = dat["solar.time"],mean)
    meanDday=meanDday[meanDday$solar.time %in% preds$date,]
    K600D = lm(preds$K600_daily_mean~meanDday$depth)

    plot(preds$K600_daily_mean,meanDday$depth,
         ylab="D_daily_mean",xlab="Daily_mean_K600")
    abline(lm(meanDday$depth~preds$K600_daily_mean))
      legend("topright", bty="n", legend=paste("R2 =", format(summary(K600D)$r.squared, digits=4)))
legend("top", bty="n", legend=paste("p = ", format(summary(K600D)$coefficients[8], digits=4)))
```

<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/ins-1.png" width="672" />

```r
    summary(K600D)
```

```
## 
## Call:
## lm(formula = preds$K600_daily_mean ~ meanDday$depth)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.776  -2.614  -1.042   1.871  14.910 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -46.116      9.939  -4.640 6.42e-05 ***
## meanDday$depth  149.625     21.760   6.876 1.24e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.765 on 30 degrees of freedom
##   (4 observations deleted due to missingness)
## Multiple R-squared:  0.6118,	Adjusted R-squared:  0.5989 
## F-statistic: 47.28 on 1 and 30 DF,  p-value: 1.241e-07
```

### Relationship between k600 and ER

This relationship is key. A primary metric is to look for covariance between ER and K.  These have high equifinality which means that any combination of ER or K can provide an equally good fit to the model. If these covary strongly (r=0.6 or higher) then that may not be good for examining controls on variation in ER. If you find **no relationship**, that is a **very good** thing. If there is a relationship, that is if R2 is high, it means that the model can't parse reaeration from respiration. In this case, ER and k600 are working in opposite for O2, so ideally there is no relationship in the model outputs. If they covary, you can still use the period-avarage data, but day-to-day variation may not be trustworthy.


```r
K600ER = lm(K600_daily_mean~ER_mean, data=preds)
   
    plot(preds$K600_daily_mean,preds$ER_mean,ylab="ER_mean",xlab="Daily_mean_K600")
    abline(lm(ER_mean~K600_daily_mean, data=preds))
    legend("topright", bty="n", legend=paste("R2 =", format(summary(K600ER)$r.squared, digits=4)))
legend("top", bty="n", legend=paste("p = ", format(summary(K600ER)$coefficients[8], digits=4)))
```

<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/ins plot-1.png" width="672" />

```r
 summary(K600ER)
```

```
## 
## Call:
## lm(formula = K600_daily_mean ~ ER_mean, data = preds)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.797  -6.620  -2.212   3.823  22.490 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)   14.299      5.015   2.851  0.00781 **
## ER_mean        7.620      4.798   1.588  0.12277   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.887 on 30 degrees of freedom
##   (4 observations deleted due to missingness)
## Multiple R-squared:  0.07754,	Adjusted R-squared:  0.04679 
## F-statistic: 2.522 on 1 and 30 DF,  p-value: 0.1228
```

### Relationship between k600 and GPP
High GPP is needed to estimate K well, so K should get more variable as GPP decreases.


```r
K600GPP = lm(GPP_mean~K600_daily_mean, data=preds)

    
    plot(preds$K600_daily_mean,preds$GPP_mean,ylab="GPP_mean",xlab="Daily_mean_K600")
    abline(lm(GPP_mean~K600_daily_mean, data=preds))
    legend("topright", bty="n", legend=paste("R2 =", format(summary(K600GPP)$r.squared, digits=4)))
legend("top", bty="n", legend=paste("p = ", format(summary(K600GPP)$coefficients[8], digits=4)))
```

<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/ins plot2-1.png" width="672" />

```r
    summary(K600GPP)
```

```
## 
## Call:
## lm(formula = GPP_mean ~ K600_daily_mean, data = preds)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.80074 -0.16770 -0.03457  0.21409  0.50237 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -0.902989   0.129766  -6.959 9.92e-08 ***
## K600_daily_mean  0.120466   0.005492  21.934  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2784 on 30 degrees of freedom
##   (4 observations deleted due to missingness)
## Multiple R-squared:  0.9413,	Adjusted R-squared:  0.9393 
## F-statistic: 481.1 on 1 and 30 DF,  p-value: < 2.2e-16
```

### Time series daily mean k600 
Because this work was carried out in generally consistent weather and flow conditions, we expect day-to-day variation in k600 to be small


```r
    preds$date = as.Date(preds$date)
  
    plot(preds$date,preds$K600_daily_mean,
         xlab="Date",ylab="Daily_mean_K600")
```

<img src="v2_SSS_SM_final_SSS042_S51_files/figure-html/ins4-1.png" width="672" />
