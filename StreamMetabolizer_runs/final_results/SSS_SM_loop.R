##Matt Kaufman matthew.kaufman@pnnl.gov Pacific Northwest National Laboratory
##This script loads the list of SSS sites from the published metadata and loops through them all,
##running the stream metabolizer template for each one

metadata.path="C:/Users/kauf093/OneDrive - PNNL/Documents/GitHub/SSS_metabolism/StreamMetabolizer_runs/final_results/SSS_Data_Package/"

output.path="C:/Users/kauf093/OneDrive - PNNL/Documents/GitHub/SSS_metabolism/StreamMetabolizer_runs/final_results/"
setwd(output.path)
metadata=read.csv(paste(metadata.path,'SSS_Metadata_IGSN-Mapping.csv',sep=''),skip=1,header=T)
metadata <- metadata[grepl("Water", metadata$Sample_Name, ignore.case = TRUE),]

SITE_LIST<-data.frame(substring(metadata$Sample_Name,0,6),metadata$Locality)
colnames(SITE_LIST)<-c('PARENT_ID','SITE_ID')
for(i in 1:length(SITE_LIST[,1])) { 
#for(i in 1:2) { 
  

PARENT_ID<-SITE_LIST[i,1]
SITE_ID<-SITE_LIST[i,2]
htmlfilename=paste(output.path,PARENT_ID,'_',SITE_ID,'_SM_output.HTML',sep="")
  
rmarkdown::render("SSS_SM_final_template.Rmd",
                  output_file = htmlfilename,
                  params = list(
                    PARENT_ID=PARENT_ID,
                    SITE_ID=SITE_ID
))

}