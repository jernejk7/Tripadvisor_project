slovar<-read.csv("./baza//urban_dictionary.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
pslovar <- slovar$word

####slovar$beseda1 <- sub("[A-Za-z]+).*", "\\1", slovar$word)
slovar$beseda2 <- sub("(\\w+).*", "\\1", slovar$word)
x <- slovar$beseda2

####slovar$beseda <- strsplit(slovar$word,"") [[1]][1]


####print(head(pslovar))
####print (pslovar)


####print(head(slovar$beseda))
####print(slovar$beseda)

####print(head(slovar$beseda1))

print(head(slovar$beseda2))
print(slovar$beseda2)


write.csv2(slovar$beseda2, file = "besede3", quote = FALSE, row.names=FALSE)

#####row.names = FALSE, col.names=FALSE



y <- x[beseda2]
