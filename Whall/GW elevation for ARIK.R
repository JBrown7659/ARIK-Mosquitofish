#reference https://rpubs.com/cgb-grupstra/moorea-hobo-20190314
#Load required libraries
library(neonUtilities)
library(ggplot2)
library(dplyr)
library(tidyr)

# set working directory to ensure R can find the file we wish to import

stackByTable("NEON_elev-groundwater.zip")
gw.elev<-read.csv("NEON_elev-groundwater/stackedFiles/EOG_30_min.csv")
head(gw.elev)

# Are there quality flags in your data? Count 'em up
sum(gw.elev$finalQF==1)  

# Are there NA's in your data? Count 'em up
sum(is.na(gw.elev$groundwaterElevMean))    
mean(gw.elev$groundwaterElevMean) 

# create new dataframe without NAs
gw.elev.NA<-gw.elev %>% drop_na(groundwaterElevMean)  

# did it work?
sum(is.na(gw.elev$groundwaterElevMean))    
mean(gw.elev.NA$groundwaterElevMean)

# View the date range
range(gw.elev.NA$startDateTime)   
# what format are they in?
str(gw.elev.NA$startDateTime)      

# Convert character data to date and time.
timeDate<-as.POSIXct(gw.elev.NA$startDateTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")   
str(timeDate)

#separate date and time into different columns
GWLongDate<-tidyr::separate(gw.elev.NA,'startDateTime',into=c('longdate','time'),sep='T')

#remove GW elevation out of range
subsetGW<-GWLongDate %>% filter(groundwaterElevMean>=1000 & groundwaterElevMean<=1300)

gw.elev.lev<-ggplot(data=subsetGW,mapping=aes(x=as.Date(longdate,format="%Y-%m-%d"),y=groundwaterElevMean))+geom_point(color = "darkorchid4") +
  labs(title = "ARIK GW elevation",
       y = "GW Elevation",
       x = "Date") + theme_bw(base_size = 15)
gw.elev.lev

#graph mean GW elevation without outliers
GWmean<-subsetGW%>%tidyr::separate('longdate',into=c('year','month','day'),sep='-',remove=FALSE)
GWmean1<-GWmean%>%group_by(year, month, day, longdate)%>%summarise(meanelev=mean(groundwaterElevMean))
head(GWmean1)
GWmean$month<-factor(GWmean$month,levels=c("09", "10", "11", "12", "01", "02", "03", "04", "05", "06", "07", "08"))
GWmeanplot <- ggplot(GWmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanelev))+
  geom_smooth(aes(colour=meanelev))+
  theme_bw()+
  labs(title= "ARIK site mean monthly GW elevation", y="Mean monthly GW elevation with 95% CI", x="Date")
GWmeanplot

#Graph GW along with SW
GSWmeanplot <- ggplot(SWmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanelev))+
  geom_smooth(aes(colour='SW'))+geom_smooth(GWmean1,mapping=aes(x=as.Date(longdate, format="%Y-%m-%d"), y=meanelev,colour='GW'))+
  theme_bw()+
  labs(title= "ARIK mean monthly GW & SW elevation", y="Mean monthly water elevation with 95% CI", x="Date")
GSWmeanplot