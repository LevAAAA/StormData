library(dplyr)
library(ggplot2)

filename<-"StormData.csv.bz2"

# Download file

if(!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, filename, method = "curl")
}

StormData<-read.csv("StormData.csv.bz2", header = TRUE)

StormData_by_evtype<-group_by(StormData, EVTYPE)

StormData_sum_by_evtype<-summarise(StormData_by_evtype,  
                                  SumFATALITIES=sum(FATALITIES), SumINJURIES=sum(INJURIES), 
                                  TotalHealth=SumFATALITIES+SumINJURIES)

StormData_sum_by_evtype<-StormData_sum_by_evtype[order(StormData_sum_by_evtype$TotalHealth, decreasing = TRUE), ]

StormData_sum_by_evtype_top10<-head(StormData_sum_by_evtype, 10)

StormData_sum_by_evtype_top10

ggplot(StormData_sum_by_evtype_top10, aes(x=EVTYPE, y=TotalHealth, fill=EVTYPE)) +
  geom_bar(stat="identity") +
  ggtitle("Top 10 casualties due to severe weather events in US") +
  xlab("Events") +
  ylab("Total casualties") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))


StormData_by_evtype<-mutate(StormData_by_evtype, PropDamage = ifelse(PROPDMGEXP == "", PROPDMG, 0 ))

StormData_by_evtype$PropDamage<-with(StormData_by_evtype, 
                                     ifelse(PROPDMGEXP == "K", PROPDMG*1000+PropDamage, PropDamage))

StormData_by_evtype$PropDamage<-with(StormData_by_evtype, 
                                     ifelse(PROPDMGEXP == "M", PROPDMG*1000000+PropDamage, PropDamage ))

StormData_by_evtype$PropDamage<-with(StormData_by_evtype, 
                                     ifelse(PROPDMGEXP == "B", PROPDMG*1000000000+PropDamage, PropDamage ))


StormData_by_evtype<-mutate(StormData_by_evtype, CropDamage = ifelse(CROPDMGEXP == "", CROPDMG, 0 ))

StormData_by_evtype$CropDamage<-with(StormData_by_evtype, 
                                     ifelse(CROPDMGEXP == "K", CROPDMG*1000+CropDamage, CropDamage))

StormData_by_evtype$CropDamage<-with(StormData_by_evtype, 
                                     ifelse(CROPDMGEXP == "M", CROPDMG*1000000+CropDamage, CropDamage ))

StormData_by_evtype$CropDamage<-with(StormData_by_evtype, 
                                     ifelse(CROPDMGEXP == "B", CROPDMG*1000000000+CropDamage, CropDamage ))


StormData_sum_damage_by_evtype<-summarise(StormData_by_evtype,  
                                   SumPropDamage=sum(PropDamage), SumCropDamage=sum(CropDamage),
                                   TotalDamage=SumPropDamage+SumCropDamage)

StormData_sum_damage_by_evtype<-StormData_sum_damage_by_evtype[order(StormData_sum_damage_by_evtype$TotalDamage, decreasing = TRUE), ]


StormData_sum_damage_by_evtype_top10<-head(StormData_sum_damage_by_evtype, 10)

StormData_sum_damage_by_evtype_top10

ggplot(StormData_sum_damage_by_evtype_top10, aes(x=EVTYPE, y=TotalDamage, fill=EVTYPE)) +
  geom_bar(stat="identity") +
  ggtitle("Top 10 weather economic consequences events in US") +
  xlab("Events") +
  ylab("Total damage in millions USD") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5))
