## This is an example of pulling SIPP Panel 2008's 16 waves data. Unzip and manipulate it in R.

library(readr)
library(SAScii)
library(data.table)
# library(rstudioapi) # load it
# current_path <- getActiveDocumentContext()$path 
# setwd(dirname(current_path ))


## pull, unzip and save the core files
save<-function(i){
  url<-paste("https://thedataweb.rm.census.gov/pub/sipp/2008/l08puw",i,".zip",sep = '')
  path<-tempfile(fileext = '.zip')
  download.file(url,path,mode = 'wb')
  unzip(zipfile = path,exdir='A place you wanna save the raw data')
}
for(i in 1:16) save(i)


## process the SAS input statement from Census, thanks the SAScii package from Anthony Damico
sasinput2008.url<-'https://thedataweb.rm.census.gov/pub/sipp/2008/l08puw1.sas'
sasinput2008<-parse.SAScii(sasinput2008.url , beginline = 5 )
sasinput2008$end<-cumsum(sasinput2008$width)
sasinput2008$start<-sasinput2008$end-sasinput2008$width+1

## select the variable you need in your research
## my current research is about education attainment, focus on high quality certificate.
## here, I selected "age, gender, education attainment, income" and obviously the "weight, panel, wave, month".
var<-c('SPANEL',
       'SWAVE',
       'MONTHCODE',
       'WPFINWGT',
       'TAGE','ESEX','TFIPSST','EEDUCATE','TPTOTINC')
sasinput2008<-sasinput2008[sasinput2008$varname %in% var,]


## manipulate sipp data
i=1
df=data.frame()
while(i<=16){
  location<-paste0('C:/Users/jwang/Downloads/LWC/sipp/unziped/l08puw',i,'.dat')
  wave<-read_fwf(location,
                 fwf_positions(c(sasinput2008$start),c(sasinput2008$end),c(sasinput2008$varname)))
  df=rbind(df,wave)
  i=i+1
}

df<-apply(df,2,as.numeric)
cert<-df%>%
  group_by(SWAVE,TFIPSST,ESEX,EEDUCATE)%>%
  summarise(weight=sum(WPFINWGT))%>%
  group_by(SWAVE,TFIPSST,ESEX)%>%
  mutate(percent=weight/sum(weight))
cert<-cert[cert$EEDUCATE==41,]  

write.csv(df,file = ' ',row.names = FALSE)
write.csv(cert,file = ' ',row.names = FALSE)

## use the same methodology to have other panels
## example Panel 2014
sipp2014wave2.location<-"https://www2.census.gov/programs-surveys/sipp/data/datasets/2014/w2/pu2014w2_dat.zip"
sipp2014.sasinput<-'https://thedataweb.rm.census.gov/pub/sipp/2014/pu2014w1.sas'
sipp2014wave1.location="http://thedataweb.rm.census.gov/pub/sipp/2014/pu2014w1_dat.zip" 
  
  
