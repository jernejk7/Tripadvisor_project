library(ggplot2)
library(dplyr)

tabela<-read.table("C:/Users/Jerry/Documents/ekoF/DataM/Tripadvisor project/Tripadvisor_project/hoteliTA.txt", header=TRUE, sep="|",quote = "" ,stringsAsFactors=FALSE, fill = TRUE, check.names=FALSE )

#Povprečne ocene po kategorijah
povp<-tabela %>% group_by(Location) %>% summarise(Mean=mean(rating)) %>% arrange(desc(Mean))
print(povp)

#Skupno povprečje 
vpovp<-tabela %>% summarise(Total_mean=mean(rating)) 
print(vpovp)

#Število ocen po kategorijah 
stocen<-tabela %>% group_by(Location) %>% summarise(NumberOfRatings=n()) %>% arrange(desc(NumberOfRatings))
print(stocen)

#Modus ocen
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modocen<-tabela %>% group_by(Location) %>% summarise(Modus=Mode(rating)) %>% arrange(desc(Modus))
print(modocen)

#najnižja ocena 
stocen<-tabela %>% group_by(Location) %>% summarise(MINN=min(rating)) %>% arrange(desc(MINN))
print(stocen)



