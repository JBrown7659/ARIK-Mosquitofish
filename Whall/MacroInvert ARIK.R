#Load required libraries
library(neonUtilities)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(dplyr)
library(tidyr)
library(stringr)

stackByTable("NEON_count-macroinvertebrates.zip")
inverts_persample<-read.csv("NEON_count-macroinvertebrates/stackedFiles/inv_persample.csv")
inverts_field<-read.csv("NEON_count-macroinvertebrates/stackedFiles/inv_fieldData.csv")
inverts_pervial<-read.csv("NEON_count-macroinvertebrates/stackedFiles/inv_pervial.csv")
inverts_taxpro<-read.csv("NEON_count-macroinvertebrates/stackedFiles/inv_taxonomyProcessed.csv")

#change date format
inverts_taxpro$date<-as.Date(inverts_taxpro$collectDate,'Y%-%m-%d')
timeDate<-as.POSIXct(inverts_taxpro$date,format="%Y-%m-%dT%H:%M:%SZ",tz="GMT")
LongDate<-tidyr::separate(inverts_taxpro,'collectDate',into=c('longdate','time'),sep='T')  #separate date and time into different columns
LongDateyear<-tidyr::separate(LongDate,'longdate',into=c('year','month','day'),sep='-')

#new data.frame summary
inv_abu<-LongDateyear%>%group_by(year,acceptedTaxonID, month)%>%summarise(abundance=n())

#plot capture inverts by year
ggplot(inv_abu, aes(x = acceptedTaxonID, y = abundance)) + 
  geom_bar(stat='identity', position = 'dodge',aes(color=acceptedTaxonID)) + 
  ggtitle("Yearly ARIK Invert Capture") +
  xlab("species code") + ylab("total count") +facet_wrap(~year)+guides(color="none")


#plot capture inverts by year with greater than 5 individuals
top_inv<-inv_abu%>% filter(abundance>=5)

ggplot(top_inv, aes(x = acceptedTaxonID, y = abundance)) + 
  geom_bar(stat='identity', position = 'dodge') + 
  facet_wrap(~year)+theme(axis.text.x = element_text(size = 7), text = element_text(size = 10)) +
  xlab("Species Code") +
  ylab("Total Count") +
  ggtitle("Total Captures of macroinverts (5+): 2017-2019")

#remove leading 0 in month to categorize by season
top_inv$month<-str_replace(top_inv$month,"^0+","")
InvSeason<- top_inv %>% 
  mutate(season = 
           ifelse(month %in% c(12, 1, 2), "Winter",
                  ifelse(month %in% c(3, 4, 5), "Spring",
                         ifelse(month %in% c(6, 7, 8), "Summer",
                                ifelse(month %in% c(9, 10, 11), "Fall", "Error")))))

#plot capture inverts by year and season with greater than 5 individuals
InvSeasonPlot <- ggplot(InvSeason, aes(acceptedTaxonID, abundance)) +
  geom_bar(stat='identity', position = 'dodge',aes(color=acceptedTaxonID)) +
  ggtitle("ARIK Invert Capture based on seasons") +
  xlab("species code") + ylab("total count") +
  theme(plot.title = element_text(lineheight=.8, face="bold",
                                  size = 20)) +
  theme(text = element_text(size=10))+facet_grid(year~season)+theme_bw()+guides(color="none")

InvSeasonPlot

