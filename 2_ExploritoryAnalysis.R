### Loading Data ############################################################
data <- read.csv(paste0(getwd(),"/twitterData/SampleData.csv"))
data$text <- as.character(data$text)
### Summary Statistics ########################################################
data(stop_words) ## this is list of words that make 
wordcount <- stri_count_words(data$text)
data.frame('Total words' = sum(wordcount), 'Average Wordsin Tweet' = mean(wordcount),
           'Total without stopwords' = nrow(stop))
##Total word = 112,197, Average Tweet = 22.4, Total words without Stops = 51,885
textdf <- data %>% 
              unnest_tokens(word, text)
textdf %>% 
  count(word, sort = T)

stop <-  textdf %>% 
                anti_join(stop_words)
stop %>% 
  count(word, sort = T)


### Plotting most common word breakdown ######################################
x <- stop %>% 
  count(word, sort =T)
top20Words <- head(x$word, 20)
  
stop %>% 
  filter(word %in% top20Words) %>% 
  count(Classification, word, sort = T) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = Classification))+
  geom_col(position = "dodge", colour = "Black")+
  xlab("Top 20 most Common Words")+
  ylab("Frequency")+
 theme(text = element_text(size=20),axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position = c(0.93, 0.06), legend.title = element_text(size = 15))+
  coord_flip()
TopWordsPlot <- last_plot()
ggsave(paste0(getwd(),"/EDAPlots/TopWords.png"), plot = TopWordsPlot,
       width = 16, height = 13.14)
### Two-word phrases ########################################################

word_2 <- data %>% 
               unnest_tokens(bigram, text, token = "ngrams", n = 2)
## Most popular two word phrases 
word_2 %>% 
  count(bigram, sort = T)
## Need to remove stopwords

Sep_Bigram <- word_2 %>% 
                separate(bigram, c("first","second"), sep = " ") %>% 
                filter(!first %in% stop_words$word) %>% 
                filter(!second %in% stop_words$word)
Bigram <- Sep_Bigram%>%
              unite(bigram, first, second, sep = " ")

x <- Bigram %>% 
      count(bigram, sort = T)
Top20bigrams <- head(x$bigram, n = 20)


Bigram %>% 
  filter(bigram %in% Top20bigrams) %>% 
  count(Classification, bigram, sort = T) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(bigram, n, fill = Classification))+
  geom_col(position = "dodge", colour = "Black")+
  xlab("Top 20 Bigrams")+
  ylab("Frequency")+
  theme(text = element_text(size=20),axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position = c(0.93, 0.06), legend.title = element_text(size = 15))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  coord_flip()
Bigramplot <- last_plot()
ggsave(paste0(getwd(),"/EDAPlots/TwoWords.png"), plot = Bigramplot, 
       width = 16, height = 13.14)
### Three-word phrases #######################################################
word_3 <- data %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3)
word_3 %>% 
  count(trigram, sort = T)
## Need to remove stopwords
Sep_trigram <- word_3 %>% 
  separate(trigram, c("first","second", "third"), sep = " ") %>% 
  filter(!first %in% stop_words$word) %>% 
  filter(!second %in% stop_words$word) %>% 
  filter(!third %in% stop_words$word) 
trigram <- Sep_trigram%>%
  unite(trigram, first, second,third, sep = " ")
x <- trigram %>% 
  count(trigram, sort = T)
Top20trigrams <- head(x$trigram, n = 20)
trigram %>% 
  filter(trigram %in% Top20trigrams) %>% 
  count(Classification, trigram, sort = T) %>% 
  mutate(trigram = reorder(trigram, n)) %>% 
  ggplot(aes(trigram, n, fill = Classification))+
  geom_col(position = "dodge", colour = "Black")+
  xlab("Top 20 Trigrams")+
  ylab("Frequency")+
  theme(text = element_text(size=20),axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.position = c(0.93, 0.06), legend.title = element_text(size = 15))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  coord_flip()
trigramplot <- last_plot()
ggsave(paste0(getwd(),"/EDAPlots/ThreeWords.png"),plot = trigramplot, 
       width = 16, height = 13.14)
