library(maps)      
library(plotly)  
library(ggmap)    
library(rgeos)     
library(maptools)  
library(plyr) 
library(sp)
library(ggplot2)
setwd('/Users/julianna/Documents/HKU/Social media analytics/assignment3')

#manipulate the data
HKIsland<-read.csv('result_hk.csv',header=TRUE,fill=TRUE)
names(HKIsland)<-c("id","address","station","Candidate1","Candidate4","total")
for (x in 1:nrow(HKIsland)){
  HKIsland$rate[x]<- max(HKIsland$Candidate1[x],HKIsland$Candidate4[x])/HKIsland$total[x]
  if(HKIsland$Candidate1[x] < HKIsland$Candidate4[x]){
    HKIsland$winner[x] <- "AU Nok-hin"
    HKIsland$party[x] <- "Pan-democrat"
  }
  else{
    HKIsland$winner[x] <-"CHAN Judy Kapui"
    HKIsland$party[x] <- "Pro-establishment"
  }
}
HKIsland

KW<-read.csv('result_kw.csv',header=TRUE,fill=TRUE)
names(KW)<-c("id","address","station","Candidate1","Candidate2","total")
for (x in 1:nrow(KW)){
  KW$rate[x]<- max(KW$Candidate1[x],KW$Candidate2[x])/KW$total[x]
  if(KW$Candidate1[x] < KW$Candidate2[x]){
    KW$winner[x] <- "YIU Chung-yim"
    KW$party[x] <- "Pan-democrat"
  }
  else{
    KW$winner[x] <-"CHENG Wing-shun Vincent"
    KW$party[x] <- "Pro-establishment"
  }
}
KW

NT<-read.csv('result_nt.csv',header=TRUE,fill=TRUE)
names(NT)<-c("id","address","station","Candidate2","Candidate4","Candidate6","total")
for (x in 1:nrow(NT)){
  NT$rate[x]<- max(NT$Candidate2[x],NT$Candidate4[x],NT$Candidate6[x])/NT$total[x]
  if(NT$Candidate2[x] == max(NT$Candidate2[x],NT$Candidate4[x],NT$Candidate6[x])){
    NT$winner[x] <- "FONG Kwok-shan Christine"
    NT$party[x] <- "Neither"
  }
  else if (NT$Candidate4[x] == max(NT$Candidate2[x],NT$Candidate4[x],NT$Candidate6[x])){
    NT$winner[x] <-"TANG Ka-piu"
    NT$party[x] <- "Pro-establishment"
  }
  else {
    NT$winner[x] <-"FAN Gary Kwok-wai"
    NT$party[x] <- "Pan-democrat"
  }
}
NT

#Get the goelocation of each station

geocode_hk <- data.frame()
for(it in HKIsland$address){
  position<-geocode(it)
  geocode_hk<-rbind(geocode_hk,position)
  Sys.sleep(2)
}
HKIsland$lon<-geocode_hk$lon
HKIsland$lat<-geocode_hk$lat

geocode_kw <- data.frame()
for(it in KW$address){
  position<-geocode(it)
  geocode_kw<-rbind(geocode_kw,position)
  Sys.sleep(2)
}
KW$lon<-geocode_kw$lon
KW$lat<-geocode_kw$lat

geocode_nt <- data.frame()

addlocation<-function(DF){
  DF$lon<-rep(NA,nrow(DF))
  DF$lat<-rep(NA,nrow(DF))
  for(it in DF$address){
    position<-geocode(it)
    i<- 0
    while(is.na(position) & i<10){
      position<-geocode(it)
      i<-i+1
      Sys.sleep(2)
    }
    DF[DF$address==it,]$lon<-position$lon
    DF[DF$address==it,]$lat<-position$lon
  }
  return(DF)
}

for(it in NT$address){
  position<-geocode(it)
  geocode_nt<-rbind(geocode_nt,position)
  while(is.na(geocode_nt))
  Sys.sleep(2)
}
NT$lon<-geocode_nt$lon
NT$lat<-geocode_nt$lat


#Load map
hkmap = readRDS("HKG_adm1.rds")

ListName = data.frame(Id=hkmap$ID_1, Code=hkmap$HASC_1, Name=hkmap$NAME_1)
ListName$Code = gsub('HK.', '', as.character(ListName$Code))
RegionHK = c("CW", "EA", "SO", "WC")
RegionKL = c("SS", "KC", "KU", "WT", "YT")
ListName$Region = 'NT'
ListName$Region[ListName$Code %in% RegionHK] = 'HK'
ListName$Region[ListName$Code %in% RegionKL] = 'KL'
ListName = ListName[order(ListName$Region),]
ListName$NewID = seq(1,dim(ListName)[1])
ListName[,c("NewID", "Region", "Code", "Name")]

hkmapdf = fortify(hkmap)
hkmapdf = merge(hkmapdf, ListName, by.x="id", by.y="Id")

p <-ggplot(hkmapdf, aes(long, lat, group=group)) +
  geom_polygon(fill="white", colour="gray") +
  layer(data = subset(hkmapdf, Region=="HK"),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = "HK Island")) +
  layer(data = subset(hkmapdf, Code=="YT"),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'KW West')) +
  layer(data = subset(hkmapdf, Code=='KC'),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'KW West')) +
  layer(data = subset(hkmapdf, Code=='SS'),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'KW West')) +
  layer(data = subset(hkmapdf, Code=='NO'),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'NT East')) +
  layer(data = subset(hkmapdf, Code=='TP'),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'NT East')) +
  layer(data = subset(hkmapdf, Code=='SK'),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'NT East')) +
  layer(data = subset(hkmapdf, Code=='ST'),
        geom = "polygon",
        stat = "identity", 
        position = "identity",
        mapping = aes(x=long, y=lat, group=group, fill = 'NT East')) +
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values=c("#E8B647","#F6C555","#FAD689"))

p

#Add a NA colum named "group" to the data for some strang reason
for (x in 1:nrow(HKIsland)){
  HKIsland$group[x]<- NA
}

for (x in 1:nrow(KW)){
  KW$group[x]<- NA
}

for (x in 1:nrow(NT)){
  NT$group[x]<- NA
}

p1<-p + geom_point(data=HKIsland,aes(lon, lat, shape = party,colour = party)) +
  geom_point(data=KW,aes(lon, lat, shape = party,colour = party)) +
  geom_point(data=NT,aes(lon, lat, shape = party,colour = party)) +
  scale_shape(solid = FALSE)
p1
ggmap(get_map('K'))

#to build a bar chart for the three districts
total_votes_pan<-c(sum(HKIsland$Candidate1),sum(KW$Candidate1),sum(NT$Candidate6))
total_votes_pro<-c(sum(HKIsland$Candidate4),sum(KW$Candidate2),sum(NT$Candidate4))
district<-c("HKIsland","Kowloon West","New Territory East")

pan<-data.frame(district,total_votes_pan)
pro<-data.frame(district,total_votes_pro)

library(plotly)
Sys.setenv("plotly_username"="JuliannaWQY")
Sys.setenv("plotly_api_key"="zaEo3WhZ1ZuGVdGDxuy6")

p3 <- plot_ly(x = pan$district, y = pan$total_votes_pan, name = "Pan-democrats", type = 'bar', name = 'Votes')
p3 <- add_trace(p3, y = pro$total_votes_pro, name = "Pro-establishment")
layout(p3, title = "", xaxis = list(title = "District"), yaxis = list (title = "Total votes"))
p3
api_create(p3, filename = "assign3_final")

#_______________________________________________________________________________

ggplotly()

thismap <- get_map("Kowloon, Hong Kong") # requires Internet 
ggmap(thismap)
