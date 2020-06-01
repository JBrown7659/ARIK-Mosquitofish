#reference https://rpubs.com/cgb-grupstra/moorea-hobo-20190314

#Load required libraries
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyr)

# set working directory to ensure R can find the file we wish to import

stackByTable("NEON_elev-surfacewater.zip")
SW.elev<-read.csv("NEON_elev-surfacewater/stackedFiles/EOS_30_min.csv")
head(SW.elev)

# Are there quality flags in your data? Count 'em up
sum(SW.elev$finalQF==1)  

# Are there NA's in your data? Count 'em up
sum(is.na(SW.elev$surfacewaterElevMean))    
mean(SW.elev$surfacewaterElevMean) 

# create new dataframe without NAs
SW.NA<-SW.elev %>% drop_na(surfacewaterElevMean)  

# did it work?
sum(is.na(SW.NA$surfacewaterElevMean))    
mean(SW.NA$surfacewaterElevMean)

# View the date range
range(SW.NA$startDateTime)   
# what format are they in?
str(SW.NA$startDateTime)      

# Convert character data to date and time.
timeDate<-as.POSIXct(SW.NA$startDateTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")   
str(timeDate)

#separate date and time into different columns
SWLongDate<-tidyr::separate(SW.NA,'startDateTime',into=c('longdate','time'),sep='T')  

#graph surfacewaterelevmean points with outliers
SW1.elev<-ggplot(data=SWLongDate,mapping=aes(x=as.Date(longdate,format="%Y-%m-%d"),y=surfacewaterElevMean))+geom_point(color = "darkorchid4") +
  labs(title = "ARIK SW elev",
       y = "elev",
       x = "Date") + theme_bw(base_size = 15)
SW1.elev

#graph mean SW elevation without outliers
SWmean<-SWLongDate%>%tidyr::separate('longdate',into=c('year','month','day'),sep='-',remove=FALSE)
SWmean1<-SWmean%>%group_by(year, month, day, longdate)%>%summarise(meanelev=mean(surfacewaterElevMean))
head(SWmean1)
SWmean1$month<-factor(SWmean1$month,levels=c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08"))
SWmeanplot <- ggplot(SWmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanelev))+
  geom_smooth(aes(colour=meanelev))+
  theme_bw()+
  labs(title= "ARIK site mean monthly SW elevation", y="Mean monthly SW elevation with 95% CI", x="Date")
SWmeanplot

#Graph GW along with SW
GSWmeanplot <- ggplot(SWmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanelev))+
  geom_smooth(aes(colour='SW'))+geom_smooth(GWmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanelev,colour='GW'))+
  theme_bw()+
  labs(title= "ARIK mean monthly GW & SW elevation", y="Mean monthly water elevation with 95% CI", x="Date")
GSWmeanplot
