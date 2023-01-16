library(stringr)
library(ggplot2)
library(lubridate)
library(scales)
library(tibbletime)
library(dplyr)
library(zoo)
library(quantmod)
library(pracma)
library(patchwork)
library(tidyverse)
library(stats)
library(TSA)
library(openair)
library(WaveletComp)
library(lme4)
library(MASS)
library(gam)


root<-"C:/Users/fried/OneDrive/Senior Year backup/Github/OpenValvometry/OpenValvometry/" #set your root directory
setwd(paste0(root,"/raw data"))
data1<-scan("example clamdata.txt",what = "character",sep = "\n") %>%  #import the single column arduino data
  matrix(ncol = 5,byrow = TRUE)%>%
  as.data.frame()

colnames(data1)=c("datetime","Sensor1","Sensor2", "Sensor3", "Sensor4") #rename columns

time1<-data1 %>% #change matrix to data frame
  mutate(Sensor1 = str_remove(Sensor1,"Sensor 1: "),
         Sensor2 = str_remove(Sensor2,"Sensor 2: "),
         Sensor3 = str_remove(Sensor3,"Sensor 3: "),
         Sensor4 = str_remove(Sensor4,"Sensor 4: "),
         datetime = ymd_hms(paste(year(datetime),month(datetime),day(datetime)," ",hour(datetime),":",minute(datetime),":",second(datetime))))%>%
  distinct()%>%
  arrange(datetime)%>%
  as_tbl_time(index = datetime)%>%
  mutate(Sensor1 = as.numeric(Sensor1),
         Sensor2 = as.numeric(Sensor2),
         Sensor3 = as.numeric(Sensor3))
  

#importing past data (comment out lines 60-65 and uncomment line 66 if you don't have past data)
setwd(paste0(root,"/processed data"))
load("pastdata.rData")
pd$Sensor4<-NA

#merging past data
alltime<-rbind(pd,time1) %>%
  distinct()%>%
  arrange(datetime) %>%
  mutate(date = as_date(datetime)) %>%
  as_tbl_time(index = datetime)

#create sensor 3 daily mean/sd datasheet
meansd3<-alltime %>%
  collapse_by("day") %>%
  group_by(date)%>%
  summarise(mean=mean(Sensor3,is.NA=FALSE),sd=sd(Sensor3))
colnames(meansd3)<-c("date","mean.S3","sd.S3")

meansd1<-alltime %>%
  collapse_by("day") %>%
  group_by(date)%>%
  summarise(mean=mean(Sensor1,is.NA=FALSE),sd=sd(Sensor1))
colnames(meansd1)<-c("date","mean.S1","sd.S1")

meansd2<-alltime %>%
  collapse_by("day") %>%
  group_by(date)%>%
  summarise(mean=mean(Sensor2,is.NA=FALSE),sd=sd(Sensor2))
colnames(meansd2)<-c("date","mean.S2","sd.S2")

alltimed<-alltime %>%
  merge(meansd3,by="date")%>%
  merge(meansd2,by="date")%>%
  merge(meansd1,by="date")%>%
  mutate(zscore3 = (Sensor3-mean.S3)/sd.S3,
         zscore2 = (Sensor2-mean.S2)/sd.S2,
         zscore1 = (Sensor1-mean.S1)/sd.S1)%>%
  as_tbl_time(index=datetime)%>%
  mutate(entrynumber = seq(1,length(datetime),by=1))

#create index of peaks in the dataset 
peaks2.3<-findpeaks(alltimed$zscore3,nups=2,ndowns=2,zero="+") %>%
  data.frame()
colnames(peaks2.3)<- c("peakval3","entrynumber","peakstart3","peakend3") #rename columns
peaks2.3$closurelength3<-(peaks2.3$peakend3-peaks2.3$peakstart3)*5 #column with closure lengths

peaks2.2<-findpeaks(alltimed$zscore2,nups=2,ndowns=2,zero="+") %>%
  data.frame()
colnames(peaks2.2)<- c("peakval2","entrynumber","peakstart2","peakend2") #rename columns
peaks2.2$closurelength2<-(peaks2.2$peakend2-peaks2.2$peakstart2)*5 #column with closure lengths

peaks2.1<-findpeaks(alltimed$zscore1,nups=2,ndowns=2,zero="+") %>%
  data.frame()
colnames(peaks2.1)<- c("peakval1","entrynumber","peakstart1","peakend1") #rename columns
peaks2.1$closurelength1<-(peaks2.1$peakend1-peaks2.1$peakstart1)*5 #column with closure lengths

peaksm1t<-peaks2.1 %>%
  merge(alltimed,by="entrynumber")%>%
  as_tbl_time(index = "datetime")
peaksm2t<-peaks2.2 %>%
  merge(alltimed,by="entrynumber")%>%
  as_tbl_time(index = "datetime")
peaksm3t<-peaks2.3 %>%
  merge(alltimed,by="entrynumber")%>%
  as_tbl_time(index = "datetime")

#minima datasheet
minzs3<-alltimed %>%
  collapse_by("day") %>%
  group_by(date)%>%
  summarise(min=min(zscore3),max=max(zscore3))

alltimed<-alltimed %>%
  merge(minzs3,by = "date")%>%
  mutate(percentclosed = (zscore3-min)/(max-min),
         entrynumber = seq(1,length(datetime),by=1))%>%
  as_tbl_time(index=datetime)

closuresperday1<-peaksm1t %>%
  group_by(date)%>%
  summarise(length(entrynumber))
colnames(closuresperday1)<-c("date","count")
closuresperday1$date<-as.Date(closuresperday1$date)

closuresperday2<-peaksm2t %>%
  group_by(date)%>%
  summarise(length(entrynumber))
colnames(closuresperday2)<-c("date","count")
closuresperday2$date<-as.Date(closuresperday2$date)

closuresperday3<-peaksm3t %>%
  group_by(date)%>%
  summarise(length(entrynumber))
colnames(closuresperday3)<-c("date","count")
closuresperday3$date<-as.Date(closuresperday3$date)

