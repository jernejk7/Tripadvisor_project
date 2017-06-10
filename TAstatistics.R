library(ggplot2)
library(dplyr)

#tabela<-read.table("C:/Users/Jerry/Documents/ekoF/DataM/Tripadvisor project/Tripadvisor_project/hoteliTA.txt", header=TRUE, sep="|",quote = "" ,stringsAsFactors=FALSE, fill = TRUE, check.names=FALSE )
tabela<-read.csv(file="TA.csv", header=TRUE, sep=";" ,stringsAsFactors=FALSE, dec=",", row.names=NULL)

#Delitev na skupine
sea<- filter(tabela, Location=="Seaside") 
ski<- filter(tabela, Location=="Ski_resort")
spa<- filter(tabela, Location=="Spa")

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

#text mining---------------------------------------------------------------------------------------
library(tidytext)
library(tidyr)
library(scales)
library(stringr)
library(wordcloud)
library(reshape2)

tekst<- unnest_tokens(tabela, word, fullrev, drop=TRUE)

#izbris stop words
data("stop_words")
custom_stop_words <- bind_rows(data_frame(word = c("hotel"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

tekst <- tekst %>% anti_join(custom_stop_words)

#št najpogostejših besed v bazi  ....potrebno izbrisati besedo hotel
tekst %>% count(word, sort=TRUE)

#graf najpogostejših besed 
tekst %>%  count(word, sort = TRUE) %>% filter(n > 400) %>%  mutate(word = reorder(word, n)) %>% 
ggplot(aes(word, n)) + geom_col() +  xlab(NULL) + coord_flip()

#tekst %>% count (Location, word) %>% group_by(Location) %>% mutate(proportion = n / sum(n)) %>%
#select (-n) %>% spread(Location, proportion) %>% gather (Location, proportion, "Seaside":"Ski_resort")

#tokenize in stop_word po skupinah-----------------------------------------------------------------------------------------
tsea<-unnest_tokens(filter(tabela, Location=="Seaside"), word, fullrev, drop=TRUE) %>% anti_join(custom_stop_words)
tski<-unnest_tokens(filter(tabela, Location=="Ski_resort"), word, fullrev, drop=TRUE) %>% anti_join(custom_stop_words)
tspa<-unnest_tokens(filter(tabela, Location=="Spa"), word, fullrev, drop=TRUE)%>% anti_join(custom_stop_words)

#najpogostejše besedepo skupinah 
tsea %>% count(word, sort = TRUE)
tsea %>% count(word) %>% with(wordcloud(word, n, max.words=100))

tski %>% count(word, sort = TRUE)
tski %>% count(word) %>% with(wordcloud(word, n, max.words=100))

tspa %>% count(word, sort = TRUE)
tspa %>% count(word) %>% with(wordcloud(word, n, max.words=100))

#graf najpogostejših besed po skupinah
tsea %>%  count(word, sort = TRUE) %>% filter(n > 275) %>%  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() +  xlab(NULL) + coord_flip()
tski %>%  count(word, sort = TRUE) %>% filter(n > 169) %>%  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() +  xlab(NULL) + coord_flip()
tspa %>%  count(word, sort = TRUE) %>% filter(n > 100) %>%  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() +  xlab(NULL) + coord_flip()

#primerjava med kategorijami 
freq<- bind_rows( mutate(tsea, Location = "Seaside"),
                  mutate(tski, Location = "Ski_resort"),
                  mutate(tspa, Location = "Spa")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(Location, word) %>%
  group_by(Location) %>% 
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(Location, proportion) %>% 
  gather(Location, proportion, `Seaside`:`Ski_resort`)


# prikaz na grafu 
ggplot(freq, aes(x = proportion, y = `Spa`, color = abs(`Spa` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~Location, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Spa", x = NULL)

#Words that are close to the line in these plots have similar frequencies in both sets of texts, 
#Words that are far from the line are words that are found more in one set of texts than another.
#Bolj kot so točke skupaj diagonalni črti večja je podobnost, bolj kot so točke razpršene manjša je podobnost.

#Corelation test between SPa & Ski_resort
cor.test(data = freq[freq$Location == "Ski_resort",], 
         ~ proportion + `Spa`)
#Corelation test between SPa & Seaside
cor.test(data = freq[freq$Location == "Seaside",], 
         ~ proportion + `Spa`)
#Corelation test between  Ski resort & Seaside   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cor.test(data = freq[freq$Location == "Seaside",], 
         ~ proportion + `Ski_resort`)

# Sentiment -----------------------------------------------------------------------------------------------------

# morje 
bing_wc_sea <- tsea %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_wc_sea

bing_wc_sea %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tsea %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var= "n", fill = 0) %>% 
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),
                   max.words=100)


# smucisca
bing_wc_ski <- tski %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_wc_ski

bing_wc_ski %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tski %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var= "n", fill = 0) %>% 
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),
                   max.words=100)


# toplice 

bing_wc_spa <- tspa %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% ungroup()
bing_wc_spa

bing_wc_spa %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tspa %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var= "n", fill = 0) %>% 
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),
                   max.words=100)


# celotna baza  

bing_wc_all<- tekst %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort= TRUE) %>% ungroup()
bing_wc_all

bing_wc_all %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tekst %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var= "n", fill = 0) %>% 
  comparison.cloud(colors=c("#F8766D", "#00BFC4"),
                   max.words=100)

#term frequency-----------------------------------------------------------------------------------------------------------------


besedeh<-unnest_tokens(tabela, word, fullrev, drop=TRUE) %>%
  count(Location, word, sort=TRUE) %>% ungroup() %>% group_by(Location) 
print(besedeh)

#stopwords
mystopwords<-data_frame(word = c("piran","tartini","kempinski", "0430", "0435", "043e", "0442",
                                 "koper", "lek", "skipass", "habakuk", "kompas", "olimia", "rimske",
                                 "rogaska", "lasko", "rimski", "thermana", "Slatina", "thermalia", "roga�ka"))
besedeh<- anti_join(besedeh, mystopwords, by= "word")

besedet<- besedeh %>% group_by(Location) %>% summarize(total=sum(n))

besedeh<-left_join(besedeh, besedet)
besedeh

#Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common

besedeh<-besedeh %>% bind_tf_idf(word, Location, n)
besedeh

    #tf-idf are zero for these extremely common words

besedeh %>% select(-total) %>% arrange(desc(tf_idf))

gbesedeh <-besedeh %>% arrange(desc(tf_idf))%>% 
  mutate(word = factor(word, levels = rev(unique(word))))%>% 
  ungroup()

gbesedeh %>% top_n(20) %>% 
  ggplot(aes(word, tf_idf, fill = Location))+ 
  geom_col() + labs(x = NULL, y = "tf-idf") + 
  coord_flip()


# graf za vsako skupino 
gbesedeh %>% 
  group_by(Location) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = Location)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Location, ncol = 2, scales = "free") +
  coord_flip()
