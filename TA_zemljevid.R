tabela<-read.csv(file="TA.csv", header=TRUE, sep=";" ,stringsAsFactors=FALSE, dec=",", row.names=NULL)
tabela$Lat <- NULL
tabela$Long <- NULL
tabela2<-read.csv("C:/Users/Jerry/Documents/ekoF/DataM/Tripadvisor project/Tripadvisor_project/hoteli_lokacija_.txt", header=TRUE, sep=";", quote = "", fill = TRUE, stringsAsFactors=FALSE, check.names=FALSE )
library("dplyr", lib.loc="~/R/win-library/3.4")
library(ggmap)
library(ggplot2)

tabela3<-inner_join(tabela,tabela2,by="Hotel") 
print(head(tabela3))
print(head(tabela))
print(head(tabela2))
map<-get_map(location='Slovenia', zoom=8, maptype = "terrain", source='google',color='color')

ggmap(map) +
  geom_point(data = tabela3, aes(x = Long, y = Lat, fill = "red", alpha = 0.8), size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)



data_sea<- filter(tabela3, Location =="Seaside")
data_sea
data_ski<- filter(tabela3, Location =="Ski_resort")
dataski
data_spa<- filter(tabela3, Location =="Spa")
dataspa


ggmap(map) +
  geom_point(data = data_ski, aes(x = Long, y = Lat, fill = "red", alpha = 0.8), size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + geom_point(data = data_sea, aes(x = Long, y = Lat, fill = "yellow", alpha = 0.8), size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + geom_point(data = data_spa, aes(x = Long, y = Lat, fill = "blue", alpha = 0.8), size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

