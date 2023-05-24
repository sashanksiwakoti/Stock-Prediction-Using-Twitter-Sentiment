library(data.table)
library(janitor)
library(dplyr)
library(stringr)
library(tidytext)
library(sentimentr)
library(tm)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
main <- read.csv('Tweet.csv') %>%
        clean_names()%>%
        data.table()
company_tweet <- read.csv('Company_Tweet.csv') %>%
        clean_names()
company_values <- read.csv('companyValues.csv') %>%
        clean_names()

#Merging company_tweet & company_values on the common col name 'tweet_id' using left join.
merge_file <- left_join(main, company_tweet, by = "tweet_id")
head(merge_file)
dim(merge_file)
#viewing unique writer and thier frequency of tweet 
table(merge_file$writer)

#Total enagagement Tweet(So we will add all total of comment, number, and like to a new column which has a new name total_engangement.)
merge_file<- merge_file %>%
        mutate(total_eangagement=
                       merge_file$comment_num + merge_file$retweet_num + merge_file$like_num)

summary(merge_file$total_eangagement)
#To obtain high-impact tweets, we ,will drop rows which has a value less than engangement_threshold count.
#engagement_threshold is a heuristic parameter that we chose.
#engagement_threshold = 40
merge_file<- merge_file %>% filter(total_eangagement>40)
dim(merge_file)

#converting post date in tweet_df and day_date in company value to date time format
class(merge_file$post_date)
merge_file <- merge_file %>% 
        mutate(day_date =
                       as.Date(merge_file$post_date / 60 / 60 / 24, origin = '1970-01-01'))
#Because we just care about day of post then compare with stock data in day. So we will get create new columne which names "day_date".

# to work on the data, We only keep the value of company_value if it is greater than start_day and less than end_day of tweet_df
start_day<-merge_file$day_date[1:1]
last_day<-merge_file$day_date[54441:54441]

stock_data_filtered<- company_values %>% filter(day_date>=start_day & day_date<=last_day)
print(stock_data_filtered[order(stock_data_filtered$day_date, decreasing = TRUE), ]   )

#Data Cleaning 
colnames(merge_file)
updated_main<- merge_file %>% select(tweet_id, writer, body, ticker_symbol, total_eangagement, day_date)


#Text preprocessing for Body tweet in tweet_df
#in this project, we only use the lowercase processing. Because we will use Affin algorithm which has text processing itself and use the whole sentences to gain sentiment score.
#To remove all the punctuation characters:
updated_main<-updated_main %>% 
        mutate(body=str_replace_all(updated_main$body, "[[:punct:]]", " "))
#To remove all the non-alphanumeric characters:
updated_main<-updated_main %>% 
        mutate(body=str_replace_all(updated_main$body, "[^[:alnum:]]", " "))
updated_main=updated_main %>% mutate(body=tolower(updated_main$body))

#tweets_collection <- Corpus(VectorSource(updated_main$body))
#expanding our tweets into individual words
words_data <- updated_main %>% select(body)  %>% 
        unnest_tokens(word, body)
words_data %>% count(word, sort = TRUE)
#filtering out stop words
words_data <- words_data %>% filter(!word %in% c('https','http', 't.co', 'he\'s', 'i\'m', 'it\'s'))
words_data2 <- words_data %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)
head(words_data2, n = 100000)

#wordclouding the most frequently used word sorted by top influencing tweet.
wordcloud(words = words_data2$word, freq = words_data2$n, 
          min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

#Sentiment Analysis at Word Level Using Bing Lexicon
words_data2 %>%
        inner_join(get_sentiments("bing")) %>%
        count(sentiment, sort = TRUE)
#We can see here that the majority of words are considered negative. If we want to gather a sense of what words in our data are being categorized as positive or negative, we can take a peak using a comparison word cloud (and exclude any profanity using the sentimentr library).
profanity_list <- unique(tolower(lexicon::profanity_alvarez))
words_data %>% filter(!word %in% c('https', 't.co', 'he\'s', 'i\'m', 'it\'s', profanity_list)) %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("red", "blue"),
                         max.words = 100)

#meanSentiment  
tweet_sentences_data<-sentiment(get_sentences(updated_main$body)) %>% 
        group_by(updated_main$day_date) %>% rename('day_date'='updated_main$day_date') %>%
        summarize(meanSentiment = mean(sentiment))
#merging it with the main table
Tweet_with_sentiment <- left_join(updated_main, tweet_sentences_data, by = "day_date")
Tweet_with_sentiment<-Tweet_with_sentiment %>% mutate(sentiments= case_when(
        (meanSentiment > 0.05) ~ "Positive",
        between(meanSentiment, -0.05, 0.05) ~ "Neutral",
        (meanSentiment < 0.05) ~ "Negative",
        TRUE ~ NA_character_
))

#Data Analysis

#Sentiment Score of Apple
Apple<-Tweet_with_sentiment %>% filter(ticker_symbol=='AAPL')
hist(Apple$meanSentiment)
summary(Apple$meanSentiment)
tableApple = as.table(table(Apple$sentiments))
pie(tableApple)

#Sentiment Score of Amazon
Amazon<-Tweet_with_sentiment %>% filter(ticker_symbol=='AMZN')
hist(Amazon$meanSentiment)
tableAMZN = as.table(table(Amazon$sentiments))
summary(Amazon$meanSentiment)
pie(tableAMZN)

#Sentiment Score of Google
Google<-Tweet_with_sentiment %>% filter(ticker_symbol=='GOOG')
hist(Google$meanSentiment)
summary(Google$meanSentiment)
ggplot(Tweet_with_sentiment, aes(day_date, meanSentiment))+geom_line()+geom_point()
tableGoog = as.table(table(Google$sentiments))
pie(tableGoog)

#Sentiment Score of Tesla
Tesla<-Tweet_with_sentiment %>% filter(ticker_symbol=='TSLA')
hist(Tesla$meanSentiment)
tableTesla = as.table(table(Tesla$sentiments))
summary(Tesla$meanSentiment)
pie(tableTesla)

#Sentiment Score of Microsoft
Microsoft<-Tweet_with_sentiment %>% filter(ticker_symbol=='MSFT')
hist(Microsoft$meanSentiment)
tableMicrosoft = as.table(table(Microsoft$sentiments))
summary(Microsoft$meanSentiment)
pie(tableMicrosoft)

#Sentiment overtime function
# Data generation

#for company closing values
mean(company_values$close_value)
sd(company_values$close_value)
table(company_values$close_value)
summary(company_values$close_value)
company_values<-company_values%>%mutate(Compnorm=pnorm(company_values$close_value,455.167,469.3246, log.p = FALSE))

#for sentiment analysis normal distribution
mean(Tweet_with_sentiment$meanSentiment)
sd(Tweet_with_sentiment$meanSentiment)
summary(Tweet_with_sentiment$meanSentiment)
Snmtnorm <- pnorm(Tweet_with_sentiment$meanSentiment,0.05710433,0.2590514, log.p = FALSE)

#testing the amazon value over the years
colnames(Tweet_with_sentiment)
#Amazon Corelation
Amazon_value<-company_values %>% filter(ticker_symbol=='AMZN')
A1<-ggplot(Amazon_value, aes(day_date, Compnorm))+geom_line()+geom_point()
A2<-ggplot(data=Amazon, aes(x=day_date, y=meanSentiment))+geom_line()
ggarrange(A1, A2, 
          labels = c("A", "B"),
           nrow = 2)
#Tesla
Tesla_value<-company_values %>% filter(ticker_symbol=='TSLA')
T1<-ggplot(Tesla_value, aes(day_date, Compnorm))+geom_line()+geom_point()
T2<-ggplot(data=Tesla, aes(x=day_date, y=meanSentiment))+geom_line()
ggarrange(T1, T2, 
          labels = c("A", "B"),
          nrow = 2)

#GoOGL
GOOGL_value<-company_values %>% filter(ticker_symbol=='GOOGL')
G1<-ggplot(GOOGL_value, aes(day_date, Compnorm))+geom_line()+geom_point()
G2<-ggplot(data=Google, aes(x=day_date, y=meanSentiment))+geom_line()
ggarrange(G1, G2, 
          labels = c("A", "B"),
          nrow = 2)

#Microsoft
MSFT_value<-company_values %>% filter(ticker_symbol=='MSFT')
M1<-ggplot(MSFT_value, aes(day_date, Compnorm))+geom_line()+geom_point()
M2<-ggplot(data=Microsoft, aes(x=day_date, y=meanSentiment))+geom_line()
ggarrange(M1, M2, 
          labels = c("A", "B"),
          nrow = 2)

#Discussion

#In general, the prices of shares of the two big companies Microsoft and Tesla are affected by Twitter if they tend to be positive or negative. The remaining 3 stocks of Apple, Amazon and Google have no or very little volatility compared to the news.

#This can be seen on the chart as the popularity of the social network Twitter has affected the value of the stock. If there isn't a lot of engagement or the Twitter page isn't active, Twitter doesn't affect pricing either.

