
data.path="C:/Users/kauf093/OneDrive - PNNL/Documents/GitHub/gitlab/SSS_metabolism/initial_SM_testing"
setwd(data.path)
K600estimates=read.csv('k600.csv',header=T)
for(i in 1:length(K600estimates[,2])) { 
#for(i in 1:2) { 
  

SITEID<-K600estimates[i,2]
htmlfilename=paste('SMET_021723_',SITEID,'_.HTML',sep="")
  
rmarkdown::render("smet_021723_template.Rmd",
                  output_file = htmlfilename,
                  params = list(
                    SITE=SITEID
))

}