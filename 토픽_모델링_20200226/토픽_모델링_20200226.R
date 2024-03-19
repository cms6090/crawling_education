# Topic modeling for Toyota USA data

rm(list=ls())
getwd()
save.image()
load(".RData")
## 패키지 설치 파일 (한 번 실행 후 재실행 필요 없음)
# install.packages("readxl")
# install.packages("tm")
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("tibble")
# install.packages("wordcloud")
# install.packages("stringr")


# 패키지 실행
library(readxl)
library(stringr)
library(dplyr)
library(tm)    
library(readr)
library(tibble)
library(tidytext)     # for using unnest_tokens()
library(topicmodels)  # for using LDA()
library(ggplot2)       # 그래프 그리는 패키지

# Excel 파일 불러오기
DATA=read_excel("page_197052454200_2018_02_24_17_27_15_raw.xlsx")

# 필요한 컬럼 Document_id, type, post_massage 선택
DATA1 <- DATA[c(1,2,6)]

# 대문자 -> 소문자로
DATA1$post_message <- tolower(DATA1$post_message)

# 숫자 제거
# DATA1$post_message <- gsub("[[:digit:]]", "", DATA1$post_message)

# 특수문자 제거
DATA1$post_message <- gsub("[[:punct:]]", "", DATA1$post_message)

# 앞뒤 공백 제거
DATA1$post_message <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", DATA1$post_message)

WORDS = read.csv("prepositions.csv", encoding = "utf-8")
WORDS <- unlist(WORDS)
View(WORDS)

# From Post Message, Split into words (단어 추출)
by_word <- DATA1 %>% unnest_tokens(output=word, input=post_message, token = "words")


# Find document-word counts 
## 전치사, 관대사, 불용어 정리   #tibble constructs a data frame 
custom_stop_words <- bind_rows(
                    tibble(word = 
                     c("!","@","#","$","^","&","*","=","+","",".",
                       "_","-","{}","[]","()",";",":","'",",.",
                       "1","2","3","4","5","6","7","8","9",
                       "/","?","led","ġ","ơ","ǡ","ȡ",
                       "셳","셱","셫","셶","셲","뙂","쫆","봳","삎",
                       "쉺","ㄳ","럠","몟뤌몟뤌","몡뤌몡뤌","봧","봨",
                       "봴","봵","삂","삆","삩","셝","셪","셲삞","쏷",
                       "쪨","쫞","쫤","찼","찾","챠","챰","첬",
                       "toyota","jr","car ","usa"," ","don","doesn","000"),
                     lexicon = c("custom")), 
                     tibble(word=as.character(WORDS), lexicon = c("custom")),
                     stop_words)


word_counts <- by_word %>%
  anti_join(custom_stop_words) %>%        # custom_stop_word와 매치되지 않는 단어들을 남겨라 
 # mutate(word=tolower(word)) %>%             # 대문자를 소문자로 변환
  count(Document_ID, word, sort = TRUE) %>%    # 빈도가 많은것부터 정렬
  ungroup()
View(word_counts)

# word_counts의 blank 이거나 http인 word변수 지우기

word_counts=word_counts %>% filter(!word %in% "" & !substr(word, start=1, stop=4) %in% "http") 

# 원변수의 단어를 어근으로 사용함
dic= Corpus(VectorSource(word_counts$word))  # Corpus{tm}

#어근을 찾아 변화
word_counts$word = stemDocument(word_counts$word, language = "english")

# #--- 어근으로 원래 단어 유추 
word_counts$word = stemCompletion(word_counts$word, dictionary = dic)

word_counts = word_counts %>% filter(!word %in% "")

#stop_words
## 워드 클라우드 그리기

library(wordcloud)

set.seed(12345)
pal=brewer.pal(8, "Dark2")
x11()
# 워드 클라우드를 그리면서 외부로 저장
png(filename = "wordcloud.png", width = 800, height = 700)
word_counts %>%
  count(word) %>%
  with(wordcloud(words=word, freq=n, scale=c(5,1),min.freq = 2, max.words = 100, 
                 random.order=FALSE, colors=pal)) 
dev.off()


## LDA토픽 분석
Title_dtm <- word_counts %>%
  cast_dtm(document=Document_ID, term=word, value=n)

myDtm<-t((as.matrix(Title_dtm))) 

data_dist <- dist(myDtm, method="euclidean") 

k.max <- 10 
wss <- sapply(1:k.max,  
              function(k){kmeans(data_dist, k, nstart=50,iter.max = 15 )$tot.withinss}) 
wss 
x11()
png(filename = "n_topics.png", width = 800, height = 600)
plot(1:k.max, wss, 
     type="b", pch = 19, frame = FALSE,  
     xlab="Number of clusters K", 
     ylab="Total within-clusters sum of squares") 
dev.off()

# Topic의 수는 5개로 진행함
# In the default of LDA, alpha=50/k, beta=0.1 or TRUE.
lda <- LDA(Title_dtm, k = 5, control=list(seed=1234,alpha=10,
                                          estimate.beta=0.1)) # find 5 topics

## LDA토픽 분석 후 모델에서 "베타"라는 방법을 사용하여 토픽별 단어별 빈도를 도출한 파일
topics <- tidy(lda, matrix = "beta")   # 5 topics

# 외부 파일로 저장하는 코드
write.csv(topics, "topics.csv", row.names = F)

## topic별 wordcloud + 외부 파일로 저장

for (i in 1:5){
  obs_topicS <- filter(topics, topics$topic == i)
  obs_topicS$Freq <- obs_topicS$beta * 10000
  
  path <- paste0("wordcloud_topic5_", i, ".png")
  png(filename = path, width = 800, height = 600)
  
  wordcloud(words=obs_topicS$term, freq = obs_topicS$Freq, scale=c(3,1), 
            max.words = 100, colors=pal, random.order=FALSE)
  dev.off()
}


## 단어(word)들이 topic으로 부터 생성될 확률(beta)이 높은 topic별 20개를 뽑기

# 5 topic
top_terms <- topics %>%
  group_by(topic) %>%
  filter(!(topic==3 & term=="ve")) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

png(filename = "Topic_20.png", width = 800, height = 600)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(x=term, y=beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + ggtitle("Frequency of TOP 20 WORDS(TOPIC 5)")
  ggsave("Topic_20.pdf")
dev.off()  

library(tidyr) # for using spread() 

## 단어별 토픽별 확률분포를 정리한 파일

# 5 topic
beta_spread <- topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta)

# 외부 파일로 저장
write.csv(beta_spread, "beta_spread.csv", row.names = F)

## Document(Title)-Topic Probabilities

## 각 document(sKey)에서 topic이 선정될 확률(gamma)을 구함
obs_gamma <- tidy(lda, matrix = "gamma")
obs_gamma <- obs_gamma %>% rename(Document_ID = document) 
obs_gamma$Document_ID=as.numeric(obs_gamma$Document_ID)
str(obs_gamma)
obs_gamma %>% arrange(Document_ID)

write.csv(obs_gamma, "obs_gamma.csv", row.names = F)

select_topic <-  obs_gamma %>%  
  group_by(Document_ID)  %>% 
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(Document_ID)

write.csv(select_topic, "select_topic.csv", row.names = F)


## tf-idf 분석

word_tf_idf = word_counts  %>% bind_tf_idf(term=word, document = Document_ID, n=n)

# left_join(x=word_tf_idf, y=select_topic, by=c("Document_ID")) 
word_topic_tf_idf = word_tf_idf %>% 
  left_join(select_topic, by=c("Document_ID")) 

write.csv(word_topic_tf_idf, "word_topic_tf_idf.csv", row.names = F)

x11()

topic_t <- function(a) {
  if (a==1) c="Topic=1"
  else if (a==2) c="Topic=2"
  else if (a==3) c="Topic=3"  
  else if (a==4) c="Topic=4"    
  else   c="Topic=5"
  return(c)
}

# Topic 안에 같은 단어가 다른 Document_ID에 존재하고 있음=> sum했음

top_terms <- word_topic_tf_idf %>%
  select(c('topic', 'word', 'tf_idf')) %>%
  group_by(topic, word) %>%
  summarise(tf_idf=sum(tf_idf)) %>%
  ungroup() %>%
  group_by(topic) %>%
  arrange(desc(tf_idf)) %>%
  top_n(15) %>%
  ungroup() %>%
  arrange(topic)

png(filename = "Topic_tf_idf.png", width = 800, height = 600)
top_terms %>%
  mutate(topic_g=mapply(topic_t, topic), term = reorder(word, tf_idf)) %>%
  ggplot(aes(x=term, y=tf_idf, fill = factor(topic_g))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_g, scales = "free") +
  coord_flip()
dev.off()

##  감성분석

library(textdata)
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


word_sentiment <- word_counts %>%
  inner_join(get_sentiments("bing"), by=c("word")) %>%
  count(Document_ID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
?spread

write.csv(word_sentiment, "word_sentiment.csv", row.names = F)

topic_sentiment <- word_topic_tf_idf %>%
  mutate(topic_g=mapply(topic_t, topic)) %>%
  inner_join(get_sentiments("bing"), by=c("word")) %>%
  count(topic_g, index=Document_ID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  rename(Document_ID=index)

x11()
png(filename = "Topic_sentiment_bing.png", width = 800, height = 600)
ggplot(topic_sentiment, aes(x=Document_ID, y=sentiment, fill =topic_g )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic_g, ncol = 2, scales = "free_x")
dev.off()  
?facet_wrap

topic_sentiment_afinn <- word_topic_tf_idf %>%
  mutate(topic_g=mapply(topic_t, topic)) %>%
  inner_join(get_sentiments("afinn"), by=c("word")) %>%
  group_by(topic_g, index=Document_ID) %>%
  summarise(sentiment = sum(value)) 

write.csv(topic_sentiment_afinn, "topic_sentiment_afinn.csv", row.names = F)
x11()
png(filename = "Topic_sentiment_afinn.png", width = 800, height = 600)
ggplot(topic_sentiment_afinn, aes(x=index, y=sentiment, fill =topic_g )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic_g, ncol = 2, scales = "free_x")
dev.off() 

# Most Common Positive and Negative Words

bing_word_counts <- word_counts %>%
  inner_join(get_sentiments("bing"), by=c("word")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

png(filename = "word_contribution.png", width = 800, height = 600)
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(n=10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x=word, y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
dev.off()  

install.packages("reshape2")
library(reshape2) # for using acast() 
x11()
png(filename = "common_senti_word.png", width = 800, height = 600)
bing_word_counts %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100, random.order=FALSE)
dev.off()  

## Bi-gram and Correaltions

library(tidytext)

# Split into bi-gram words (단어 추출)
bi_words = DATA1 %>% 
     unnest_tokens(output=bigram, input=post_message, token = "ngrams", n = 2)

# Counting and filtering bi-grams
bi_words %>%
  count(bigram, sort = TRUE)

library(tidyr) # for using separate() and  unite()

bigrams_separated <- bi_words %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)  %>%
  filter(!substr(word1, start=1, stop=4) %in% "http") %>%
  filter(!substr(word2, start=1, stop=4) %in% "http") %>%
  filter(!is.na(word1)) %>%
  filter(!is.na(word2))

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)  


bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# Analyze bi-grams

bigram_tf_idf <- bigrams_united %>%
  count(Document_ID, bigram) %>%
  bind_tf_idf(term=bigram, document=Document_ID, n=n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(select_topic, by=c("Document_ID"))  %>%
  arrange(desc(tf_idf)) 

x11()
bigram_tf_idf %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(topic) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(x=bigram, y=tf_idf, fill = topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~topic, ncol = 2, scales = "free") +
  coord_flip()

bi_terms <- bigram_tf_idf %>%
  select(c('topic', 'bigram', 'tf_idf')) %>%
  group_by(topic) %>%
  arrange(desc(tf_idf)) %>%
  top_n(10) %>%
  ungroup() %>%
  arrange(topic)

## 크기 순서대로 안됨 
png(filename = "bi_topic_tf_idf.png", width = 800, height = 600)
bi_terms %>%
  mutate(topic_g=mapply(topic_t, topic), term = reorder(bigram, tf_idf)) %>%
  ggplot(aes(x=term, y=tf_idf, fill = factor(topic_g))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_g, scales = "free") +
  coord_flip()
dev.off()  

# Visualizing a network of bigrams with ggraph

install.packages("igraph")
library(igraph)   # for using graph_from_data_frame()

# original counts
bigram_counts

# filter for only relatively common combinations

bigram_graph <- bigram_counts %>%
  filter(n > 2) %>%
  graph_from_data_frame()

bigram_graph

install.packages("ggraph")
library(ggraph)
set.seed(2017)

x11()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.10, "inches"))

png(filename = "bi_network.png", width = 800, height = 600)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, color ="black",
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "blue", size = 2) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
dev.off() 



# Counting and correlating among sections

section_words <- word_counts %>%
  mutate(section =Document_ID)  


table(word_counts$Document_ID)

install.packages("widyr")
library(widyr) # for using pairwise_count() and pairwise_cor()

?pairwise_count

# count words co-occuring within sections

word_pairs <- word_counts %>%
  pairwise_count(item=word, feature=Document_ID, sort = TRUE)

word_pairs


# Pairwise correlation

# we need to filter for at least relatively common words first

word_cors <- word_counts %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(item=word, feature=Document_ID, sort = TRUE)

word_cors

word_cors %>%
  filter(item1 == "tire")


word_cors %>%
  filter(item1 %in% c("key", "tire", "truck", "pride")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(x=item2, y=correlation, fill =item1)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "blue", size = 2) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

?geom_edge_link

