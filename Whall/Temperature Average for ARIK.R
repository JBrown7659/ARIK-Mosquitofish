#reference https://rpubs.com/cgb-grupstra/moorea-hobo-20190314

#Load required libraries
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(pollen)
library(data.table)

# set working directory to ensure R can find the file we wish to import

stackByTable("NEON_temp-surfacewater.zip")
wat.temp<-read.csv("NEON_temp-surfacewater/stackedFiles/TSW_30min.csv")
head(wat.temp)

# Are there quality flags in your data? Count 'em up
sum(wat.temp$finalQF==1)  

# Are there NA's in your data? Count 'em up
sum(is.na(wat.temp$surfWaterTempMean))    
mean(wat.temp$surfWaterTempMean) 

# create new dataframe without NAs
wat.temp.NA<-wat.temp %>% drop_na(surfWaterTempMean)  

# did it work?
sum(is.na(wat.temp.NA$surfWaterTempMean))    

# View the date range
range(wat.temp.NA$startDateTime)   
# what format are they in?
str(wat.temp.NA$startDateTime)      

# Convert character data to date and time.
timeDate<-as.POSIXct(wat.temp.NA$startDateTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")   
str(timeDate)

#separate date and time into different columns
watLongDate<-tidyr::separate(wat.temp.NA,'startDateTime',into=c('longdate','time'),sep='T')  

#remove meansurfwater temp out of range - filter by QF=0?
subsetLongDate<-watLongDate %>% filter(surfWaterTempMean>=-20 & surfWaterTempMean<=40)

#graph surfmeanwatertemp points without outliers 
tempGraph1<-ggplot(data=subsetLongDate,mapping=aes(x=as.Date(longdate,format="%Y-%m-%d"),y=surfWaterTempMean))+geom_point(color = "darkorchid4") +
  labs(title = "ARIK Temp",
       y = "Temp",
       x = "Date") + theme_bw(base_size = 15)
tempGraph1

#graph surfmeanwatertemp points with outliers
tempGraph2<-ggplot(data=watLongDate,mapping=aes(x=as.Date(longdate,format="%Y-%m-%d"),y=surfWaterTempMean))+geom_point(color = "darkorchid4") +
  labs(title = "ARIK Temp",
       y = "Temp",
       x = "Date") + theme_bw(base_size = 15)
tempGraph2


#graph mean water temp with outliers
watFull<-watLongDate%>%tidyr::separate('longdate',into=c('year','month','day'),sep='-',remove=FALSE)
watfullmean<-watFull%>%group_by(year,month,day,longdate)%>%summarise(meantemp=mean(surfWaterTempMean))
head(watfullmean)
watfullmean$month<-factor(watfullmean$month,levels=c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08"))
meanplot <- ggplot(watfullmean,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meantemp))+
  geom_smooth(aes(colour=meantemp))+
  theme_bw()+
  labs(title= "ARIK site Monthly temperature means", y="Monthly mean temperature (°C) with 95% CI", x="Date")
meanplot

#graph mean water temp without outliers
subwatFull<-subsetLongDate%>%tidyr::separate('longdate',into=c('year','month','day'),sep='-',remove=FALSE)
subwatfullmean<-subwatFull%>%group_by(year, month, day, longdate)%>%summarise(meantemp=mean(surfWaterTempMean),meanmax=mean(surfWaterTempMaximum),meanmin=mean(surfWaterTempMinimum))
head(subwatfullmean)
subwatfullmean$month<-factor(subwatfullmean$month,levels=c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08"))
submeanplot <- ggplot(subwatfullmean,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meantemp))+
  geom_point()+geom_line()+geom_smooth(aes(colour=meantemp))+
  theme_bw()+
  labs(title= "ARIK site Monthly temperature means", y="Monthly mean temperature (°C) with 95% CI", x="Date")
submeanplot

#growing degree days with pollen package (TESTING)
#Type "B" - The heat units are calculated based on the difference between the mean daily temperature and the threshold (tbase). In the case when the value of tmin is lower than tbase, then it is replaced by tbase
#Type "C" - same as type "B" and when the value of tmax is larger than tbase_max, then it is replaced by tbase_max
#Type "D"- same as type "B" and when the value of tmax is larger than tbase_max, then no heat units are added

df_plot1<-pivot_longer(subwatfullmean,meanmax:meanmin)
p1<-ggplot(df_plot1)+geom_line(aes(group=1,longdate,value,color=name))
p1
subwatfullmean$type_b <- gdd(tmax = subwatfullmean$meanmax, tmin = subwatfullmean$meanmin, 
                       tbase = 10, type = "B")
subwatfullmean$type_c <- gdd(tmax = subwatfullmean$meanmax, tmin = subwatfullmean$meanmin, 
                       tbase = 10, tbase_max = 15, type = "C")
subwatfullmean$type_d <- gdd(tmax = subwatfullmean$meanmax, tmin = subwatfullmean$meanmin, 
                       tbase = 10, tbase_max = 15, type = "D")
head(subwatfullmean)
df_plot2<-pivot_longer(subwatfullmean,type_c)
p2<-ggplot(df_plot2)+geom_line(aes(group=1,year,value,color=name))
p2

dd1<-((subwatfullmean$meanmax+subwatfullmean$meanmin)/2)+10
subwatfullmean$year<-factor(subwatfullmean$year,levels=c("2016", "2017", "2018", "2019", "2020"))
qplot(subwatfullmean$day,dd1)
ggplot(subwatfullmean,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=dd1))+geom_smooth(aes(color=dd1))                                                                    
ggplot(subwatfullmean,mapping=aes(x=as.Date(month, format="%Y-%m-%d"), y=dd1))+geom_smooth(aes(color=dd1))                                                                    
