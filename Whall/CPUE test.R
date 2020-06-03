## ARIK mosquitofish project 
#### Wayne Hall
## Last Updated: June 03, 2020 

library("lubridate")
library("dplyr") 
library("tidyr")
library("neonUtilities")
library("ggplot")
library("ggthemes")

# Calculating CPUE for mosquitofish (GAMAFF) specifically 
# First, isolate the GAMAFF and POESP2 taxon ID from the bulkCount data 
# NOTE: All POESP species were mistakenly identified as different from Mosquitofish prior to 2018 
bulk.count <- read.csv("NEON_count-fish/stackedFiles/fsh_bulkCount.csv") 
bulk.count <- bulk.count[bulk.count$passNumber=='1',] 
bulk.count <- filter(bulk.count, as.numeric(taxonID) > "2") # This makes sure that POESP2 and GAMAFF are included in the bulkCount data

#Do the same for the perFish data table 
per.fsh <- read.csv("NEON_count-fish/stackedFiles/fsh_perFish.csv")
per.fsh <- per.fsh[per.fsh$passNumber=='1',]

# Do the same for the perPass data table 
per.pass <- read.csv(("NEON_count-fish/stackedFiles/fsh_perPass.csv"))
per.pass <- per.pass[per.fsh$passNumber=='1',]

################################################
#change date format for bulk.count data table
bulk.count$date<-as.Date(bulk.count$passStartTime,'Y%-%m-%d')
fsh.bulk.timeDate<-as.POSIXct(bulk.count$passStartTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
fsh.bulk.LongDate<-tidyr::separate(bulk.count,'passStartTime',into=c('longdate','time'),sep='T')
fsh.bulk.LongDateyear<-tidyr::separate(fsh.bulk.LongDate,'longdate',into=c('year','month','day'),sep='-')

#change date format for the perFish data table 
fsh.per.timeDate<-as.POSIXct(per.fsh$passStartTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
fsh.per.LongDate<-tidyr::separate(per.fsh,'passStartTime',into=c('longdate','time'),sep='T')
fsh.per.LongDateyear<-tidyr::separate(fsh.per.LongDate,'longdate',into=c('year','month','day'),sep='-')

#change date format for the perPass data table 
fsh.pass.timeDate<-as.POSIXct(per.pass$passStartTime,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
fsh.pass.LongDate<-tidyr::separate(per.pass,'passStartTime',into=c('longdate','time'),sep='T')
fsh.pass.LongDateyear<-tidyr::separate(fsh.pass.LongDate,'longdate',into=c('year','month','day'),sep='-')

################################################
#new data.frame summary - combine Bulk.count and Per.Fsh
fsh.bulk.LongDateyear$abundance<-fsh.bulk.LongDateyear$bulkFishCount
fsh_abu<-fsh.per.LongDateyear%>%group_by(year,month,taxonID,namedLocation,eventID)%>%summarise(abundance=n())

fsh.total2<-merge(fsh_abu,fsh.bulk.LongDateyear,
        by=c('abundance','year','month','taxonID','namedLocation','eventID'),
        all=TRUE)
fsh.total2.mutate<-fsh.total2%>%
        mutate(taxonID=replace(taxonID,taxonID=='POESP1','GAMAFF'))%>%
        mutate(taxonID=replace(taxonID,taxonID=='POESP2','GAMAFF'))
fsh.total2.summary<-fsh.total2.mutate%>%
        group_by(year,month,taxonID)%>%
        summarise(fun=sum(abundance))%>%
        filter(taxonID%in%c('GAMAFF'))

#new data.frame to include efTime
fsh.total3<-fsh.pass.LongDateyear%>%
        group_by(year,month,passNumber)%>%
        summarise(efTIME=sum(efTime))%>%
        filter(year%in%c('2017','2018','2019'))%>%
        filter(passNumber%in%c(1))

#merge counts with eftime
fsh_final<-merge(fsh.total3,fsh.total2.summary,by=c('year','month'),all=TRUE)
fsh_final<-fsh_final%>%drop_na(taxonID)

#Calculate CPUE by month 
cpue<-function(x,y){x/y}
fsh_final$CPUE<-as.numeric(cpue(x=fsh_final$fun,y=fsh_final$efTIME))
table(fsh_final$CPUE)
fsh_final_grpdate<-unite(fsh_final,date,year:month,sep='-')

#plot CPUE by year
ggplot(fsh_final_grpdate, aes(x=date,y=CPUE,colour=date))+ 
  geom_bar(stat='identity', position = 'dodge')+
  geom_text(aes(label = round(CPUE,3)),show.legend = FALSE,size=3) +
  ggtitle("GAMAFF CPUE per sampling period") + 
  xlab("Sampling Period for GAMAFF") + ylab("CPUE")+theme(axis.text.x = element_text(size = 7), text = element_text(size = 8))



##########################################################################################################
# Calculating CPUE for all species  
fsh.all<-merge(fsh_abu,fsh.bulk.LongDateyear,
                  by=c('abundance','year','month','taxonID','namedLocation','eventID'),
                  all=TRUE)
fsh.all=unite(fsh.all,date,year:month,sep='-')


fsh.all.summary<-fsh.all%>%
  group_by(date,taxonID)%>%
  summarise(fun=sum(abundance))

#new data.frame to include efTime
fsh.all.ef<-fsh.pass.LongDateyear%>%
  group_by(year,month,passNumber)%>%
  summarise(efTIME=sum(efTime))%>%
  filter(passNumber%in%c(1))
fsh.all.ef=unite(fsh.all.ef,date,year:month,sep='-')

#merge counts with eftime
fsh.all.final<-merge(fsh.all.ef,fsh.all.summary,by=c('date'),all=TRUE)
fsh.all.final<-fsh.all.final%>%drop_na(taxonID)

#Calculate CPUE by month 
cpue<-function(x,y){x/y}
fsh.all.final$CPUE<-as.numeric(cpue(x=fsh.all.final$fun,y=fsh.all.final$efTIME))
table(fsh.all.final$CPUE)

#plot CPUE by year
ggplot(fsh.all.final, aes(x=taxonID, y = CPUE,colour=taxonID)) + 
  geom_bar(stat='identity', position = 'dodge',aes(color=taxonID))+
  geom_text(aes(label = round(CPUE,3)),show.legend = FALSE,size=2,angle=60) +
  ggtitle("CPUE per sampling period") +
  xlab("species code") + ylab("CPUE") +facet_wrap(~date,scales='free')+theme(axis.text.x = element_text(size = 7,angle = 60), text = element_text(size = 8))


