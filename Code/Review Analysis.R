---
  title: "Review Analysis"
author: "Kaiyan"
date: "`r Sys.Date()`"
output:
  rmdformats::readthedown:
  self_contained: true
thumbnails: true
lightbox: true
gallery: false
highlight: tango
df_print: paged
---
  # 1.
  
  ```{r}
data <- read.csv("../Data/yelp_review_filtered_breakfast.csv")
head(data)
```


```{r}
library(stringr)
library(tidytext)
library(tidyr)
library(dplyr)
library(topicmodels)
library(ggplot2)
library(stopwords)
library(readr)
library(tidyverse)
```




```{r}
#stopwords
my_stopwords <- strsplit(read_file("gist_stopwords.txt"),",")[[1]]
stop_words_all <- rbind(stop_words,data.frame("word"=my_stopwords,"lexicon"=rep("user",length(my_stopwords))))

# split into words
data_word <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_all)
```

# doc: take each row as a document (42194 docs)

```{r}
#remove common words? ski[p]
all_document_data_word_count <- data_word %>%
  count(word, sort = TRUE)

data_word_count <- data_word %>%
  count(review_id, word, sort = TRUE)

review_id_dtm <- data_word_count %>%
  cast_dtm(review_id, word, n)
review_id_dtm
```


```{r}
food <- read.table("../Data/food.txt")
food <- food$V1

all_document_data_word_count %>% 
  filter(word %in% food)%>% 
  filter(word %in% common_words==FALSE) %>%
  write_csv("../food_in_all_reviews.csv")

```


## lda
```{r}
review_id_lda <- LDA(review_id_dtm, k = 4, control = list(seed = 1234))

review_id_topics <- tidy(review_id_lda, matrix = "beta")

review_id_topics


review_id_topics$topic <- sub('1','Food_Quality',review_id_topics$topic)
review_id_topics$topic <- sub('2','Serving_Speed',review_id_topics$topic)
review_id_topics$topic <- sub('3','Staff_Attitude',review_id_topics$topic)
review_id_topics$topic <- sub('4','Customer_Loyalty',review_id_topics$topic)
```

```{r}
# foods
top_terms_food <- review_id_topics %>% 
  filter(term %in% food) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# ggplot
top_terms_food  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free",nrow = 2) +
  scale_y_reordered()+ 
  ggtitle("Top-10 frequent foods in each topic")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=17,face="bold"),
        plot.title = element_text(size=20))+ 
  labs(x = "per-topic-per-word probabilities(beta)")

```

```{r}
review_id_topics %>% 
  filter(term %in% food) %>% 
  write_csv("../review_food_topics.csv")
```

```{r}
# others
common_words <- c('food','pretty','bit','restaurant','breakfast','looked','feel','lot','eat')

top_terms_others <- review_id_topics %>% 
  filter(term %in% food == FALSE) %>%
  filter(term %in% common_words == FALSE) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# ggplot
top_terms_others %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free",nrow = 2) +
  scale_y_reordered()+ 
  ggtitle("Top-10 frequent words in each topic")+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=17,face="bold"),
        plot.title = element_text(size=20))+ 
  labs(x = "per-topic-per-word probabilities(beta)")

```




```{r}
review_id_topics

write_csv(review_id_topics,"review_id_topics.csv")
```



```{r}
review_id_gamma <- tidy(review_id_lda, matrix = "gamma")

# add other id cols
library(reshape2)

review_id_gamma_wide <- review_id_gamma %>%
  dcast(document ~ topic, value.var="gamma")

colnames(review_id_gamma_wide) <- c("review_id",'Food_Quality','Serving_Speed','Staff_Attitude','Customer_Loyalty')
review_id_gamma_wide

yelp_review_add_gamma <- merge(data,review_id_gamma_wide,by="review_id")

write.csv(yelp_review_add_gamma,"../Data/yelp_review_add_gamma.csv")

yelp_review_add_gamma
```



```{r}
colnames(review_id_gamma)[1] <-"review_id"
review_id_gamma

yelp_review_add_gamma_long <- merge(data,review_id_gamma,by="review_id")
yelp_review_add_gamma_long


yelp_review_add_gamma_long$stars <- sub('1','1-star',yelp_review_add_gamma_long$stars)
yelp_review_add_gamma_long$stars <- sub('2','2-stars',yelp_review_add_gamma_long$stars)
yelp_review_add_gamma_long$stars <- sub('3','3-stars',yelp_review_add_gamma_long$stars)
yelp_review_add_gamma_long$stars <- sub('4','4-stars',yelp_review_add_gamma_long$stars)
yelp_review_add_gamma_long$stars <- sub('5','5-stars',yelp_review_add_gamma_long$stars)

yelp_review_add_gamma_long$stars<- factor(yelp_review_add_gamma_long$stars,levels = c('1-star','2-stars','3-stars','4-stars','5-stars'))

yelp_review_add_gamma_long$topic <- sub('1','Food_Quality',yelp_review_add_gamma_long$topic)
yelp_review_add_gamma_long$topic <- sub('2','Serving_Speed',yelp_review_add_gamma_long$topic)
yelp_review_add_gamma_long$topic <- sub('3','Staff_Attitude',yelp_review_add_gamma_long$topic)
yelp_review_add_gamma_long$topic <- sub('4','Customer_Loyalty',yelp_review_add_gamma_long$topic)

write.csv(yelp_review_add_gamma_long,"yelp_review_add_gamma_long.csv")

```

```{r}
user_filter_sample <- unique(data$business_id)[1:5]

yelp_review_add_gamma_long %>%
  ## filter by business/location/tag...
  filter(business_id %in% user_filter_sample) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ stars) +
  labs(x = "topic", y = expression(gamma))+ 
  ggtitle("Topics distribution in user_defined restaurant")+ 
  theme(axis.text.x=element_text(angle=45, hjust=1))


```
