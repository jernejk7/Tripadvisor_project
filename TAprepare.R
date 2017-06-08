tabela1<-read.table("C:/Users/Jerry/Documents/ekoF/DataM/Tripadvisor project/Tripadvisor_project/TAOutput_.txt", header=TRUE, sep="|",quote="" ,stringsAsFactors=FALSE, fill = TRUE, check.names=FALSE )


#tabela2<-read.csv("C:/Users/Jerry/Documents/ekoF/DataM/Tripadvisor project/Tripadvisor_project/hoteli_lok.csv", header=TRUE, sep=";", stringsAsFactors=FALSE)
tabela2<-read.table("C:/Users/Jerry/Documents/ekoF/DataM/Tripadvisor project/Tripadvisor_project/hoteli_lok_.txt", header=TRUE, sep=";", quote = "", fill = TRUE, stringsAsFactors=FALSE, check.names=FALSE )

library("dplyr", lib.loc="~/R/win-library/3.4")


#inner join 
tabela3<-inner_join(tabela1,tabela2,by="Hotel") 
#tabela4<-merge(x=tabela1, y=tabela2, by="Hotel")

print(head(tabela1))
print(tabela2)
print(head(tabela3))
print(tail(tabela3))
summary(tabela3)
unique(print(tabela3$Hotel))

#write.csv2(tabela3,"hoteliTA", row.names=FALSE, qoute=FALSE)
write.table(tabela3,"hoteliTA",sep ="|", row.names=FALSE ,quote=F)
