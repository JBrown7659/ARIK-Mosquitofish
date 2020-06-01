#reference https://rpubs.com/cgb-grupstra/moorea-hobo-20190314

#Load required libraries
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyr)

# set working directory to ensure R can find the file we wish to import

stackByTable("NEON_precipitation.zip")
Prec<-read.csv("NEON_precipitation/stackedFiles/PRIPRE_30min.csv")
head(Prec)

# Are there quality flags in your data? Count 'em up
sum(Prec$finalQF==1)  

# Are there NA's in your data? Count 'em up
sum(is.na(Prec$priPrecipBulk))    
mean(Prec$priPrecipBulk) 

# create new dataframe without NAs
Prec.NA<-Prec %>% drop_na(priPrecipBulk)  

# did it work?
sum(is.na(Prec.NA$priPrecipBulk))    
mean(Prec.NA$priPrecipBulk)

# View the date range
range(Prec.NA$startDateTime)   
# what format are they in?
str(Prec.NA$startDateTime)      

# Convert character data to date and time.
timeDate<-as.POSIXct(Prec.NA$startDateTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")   
str(timeDate)

#separate date and time into different columns
PrecipLongDate<-tidyr::separate(Prec.NA,'startDateTime',into=c('longdate','time'),sep='T')  

#graph priPrecipBulk points with outliers
Prec1<-ggplot(data=PrecipLongDate,mapping=aes(x=as.Date(longdate,format="%Y-%m-%d"),y=priPrecipBulk))+geom_point(color = "darkorchid4") +
  labs(title = "ARIK Prec",
       y = "Precip",
       x = "Date") + theme_bw(base_size = 15)
Prec1

#graph mean Prec elevation without outliers
Precmean<-PrecipLongDate%>%tidyr::separate('longdate',into=c('year','month','day'),sep='-',remove=FALSE)
Precmean1<-Precmean%>%group_by(year, month, day, longdate)%>%summarise(meanprecip=mean(priPrecipBulk))
head(Precmean1)
Precmean1$month<-factor(Precmean1$month,levels=c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08"))
Precmeanplot <- ggplot(Precmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanprecip))+
  geom_smooth(aes(colour=meanprecip))+
  theme_bw()+
  labs(title= "ARIK site mean precipitation", y="Mean precip with 95% CI", x="Date")
Precmeanplot


