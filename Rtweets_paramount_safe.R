#Loading rtweet & dplyr libraries
library(rtweet)
library(dplyr)
library(stringr)
library(utils)
library(tidytext) 
library(ggplot2)
library(wordcloud)




# Accessing my Twitter API which allows me to retrieve tweets
# key & secret are redacted for privacy
appname <- "MontyBlathers"
key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# Query for tweets containing "ParamountPlus", limiting the query to 10,000 tweets, and excluding retweets
paramount_df <- search_tweets("ParamountPlus", n = 10000, include_rts = FALSE)
View(paramount_df)

# Saving the results as a CSV file
save_as_csv( paramount_df, file_name="paramount_plus.csv", prepend_ids = TRUE, fileEncoding = "UTF-8")


#######Data Cleaning################
#  I'm going to clean my data by removing characters that are unnecessary for analysis
# The following lines of code will be using regex so I can locate and match text easily
# \S is a regex character that matches any non-white space character(i.e. everything not a tab, space, etc)
# * is a regex character that will continue to match after its found one character that is in the position of the * 

#  I am removing hyperlinks. 
paramount_df$text <- gsub("https\\S*", "", paramount_df$text)

#I am removing mentions( those begin with an @)
paramount_df$text <- gsub("@\\S*", "", paramount_df$text)

#I am removing ampersands
paramount_df$text <- gsub("amp", "", paramount_df$text)

#I am removing new lines & carriage returns
paramount_df$text <- gsub("[\r\n]", "", paramount_df$text)

# I am removing punctuation
paramount_df$text <- gsub("[[:punct:]]", "", paramount_df$text)

#I'm removing ???????? which is a single right quotation mark
paramount_df$text <- gsub("????????", "", paramount_df$text)
paramount_df$text <- gsub("?????", "", paramount_df$text)



paramount_df$text <- (gsub("paramount", "", paramount_df$text))
paramount_df$text <- (gsub("paramountplus", "", paramount_df$text))
paramount_df$text <- (gsub("Paramount", "", paramount_df$text))

#I'm removing stop words from the tweets
paramount_tweets <- paramount_df %>%
                    select(text) %>%
                    unnest_tokens(word,text)
paramount_tweets <- paramount_tweets %>%
                    anti_join(stop_words)

View(paramount_df)

#Now, I can make a bar chart of the most frequent words used in the Tweets
paramount_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "blue") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in tweets about Paramount Plus")





############ Wordckoud of Hashtags##############
paramount_df$hashtags <- as.character(paramount_df$hashtags)
paramount_df$hashtags <- gsub("c\\(", "", paramount_df$hashtags)
set.seed(1234)
wordcloud(paramount_df$hashtags, min.freq=5,  scale=c(3.5, .5), random.order=FALSE, random.color = TRUE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2")) 

#I looked up Wynonna Earp and found out that it is a show that was recently cancelled. 
#Fans call themselves Earpers and they are mass tweeting any streaming platform to greenlight a 5th season for the show




save_as_csv( paramount_df, file_name="paramount_plus_clean.csv", prepend_ids = TRUE, fileEncoding = "UTF-8")

                        
# I'm creating a new dataframe that only has tweet data about tweets containing "Wynonna"
 
Wynonna_tweets_data <- filter(paramount_df, grepl("Wynonna", text, fixed = TRUE))
# Has 2,318 tweets, I want to know how many users were tweeting to Paramount Plus about Wynonna
length(unique(Wynonna_tweets_data$user_id))
# 634 users used were tweeting while"ParamountPlus" and trying to convince Paramount about Wynonna


#I'm creating a new dataframe that doesn't have tweet data about tweets containing "Wynonna"
JustParamount_tweets_data <- filter(paramount_df, ! grepl("Wynonna", text, fixed = TRUE))
  
JustParamount_tweets <- JustParamount_tweets_data %>%
  select(text) %>%
  unnest_tokens(word,text)
JustParamount_tweets <- JustParamount_tweets %>%
  anti_join(stop_words)

#Now, I can make a bar chart of the most frequent words used in the Tweets for just Paramount tweets
JustParamount_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "blue") +
  xlab(NULL) +
  theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in tweets about Paramount Plus",
       subtitle = "Without tweets containing Wynonna")

ggsave("most_freq_words_tweets.png", width = 10, height=11, units="in" )

############## Wordcloud of Hashtags#################
JustParamount_tweets_data$hashtags <- as.character(JustParamount_tweets_data$hashtags)
JustParamount_tweets_data$hashtags <- gsub("c\\(", "", JustParamount_tweets_data$hashtags)
set.seed(1234)
wordcloud(JustParamount_tweets_data$hashtags, min.freq=5,  scale=c(5, 1), random.order=FALSE, random.color = TRUE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))                           



########Total Sentiment Analysis##############                                   
#I want to perform a sentiment analysis, so I'm loading the syuzhet package

library(syuzhet)

#So, I'm going to convert the tweets to ASCII
ASCII_tweets <- iconv(JustParamount_tweets, from= "UTF-8", to = "ASCII", sub="bytes")

# I'm creating the bar graph of the sentiment Analysis
ew_sentiment<-get_nrc_sentiment((ASCII_tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total Sentiment Based on Scores")+
  theme_minimal()   

ggsave("Sentiment_tweets.png", width = 10, height=11, units="in" )

#Database PeakPlus has the following JustParamount_tweets_data variables: created_at, screen_name, text, & source
PeakPlus = select (JustParamount_tweets_data, 3:6)



#########Determining positive and negative tweets##########

library(SentimentAnalysis)

#Analyzing the sentiment of each tweet
Peak_sentiment <- analyzeSentiment(PeakPlus$text)
#Selecting the Sentiment word count, and the sentiement scores according to the following sentiment dictionaries:DictionaryGI,DictionaryHE, DictionaryLM & DictionaryQDAP 
Peak_sentiment <- dplyr::select(Peak_sentiment, 
                                WordCount, SentimentGI, SentimentHE, SentimentLM, SentimentQDAP)

# Getting the mean sentiment score for each Tweet
Peak_sentiment <- dplyr::mutate(Peak_sentiment, 
                                 mean_sentiment = rowMeans(Peak_sentiment[,-1]))

# Selecting only WordCount & mean_sentiment to be in the Peak_sentiment dataframe
Peak_sentiment <- dplyr::select(Peak_sentiment, 
                                 WordCount, 
                                 mean_sentiment)

# Adding the Peak_Sentiment dataframe to the PeakPlus dataframe
PeakPlus <- cbind.data.frame(PeakPlus, Peak_sentiment)


# Converting mean sentiment scores into "positive" or "negative"
PeakPlus$sentiment <- convertToBinaryResponse(PeakPlus$mean_sentiment)


#Gathering the postive tweets into a dataframe
Positive_PeakPlus <- filter(PeakPlus, grepl("positive", PeakPlus$sentiment, fixed = TRUE ))

#Gathering the negative tweets into a dataframe
Negative_PeakPlus <- filter(PeakPlus, grepl("negative", PeakPlus$sentiment, fixed = TRUE ) )


View(Negative_PeakPlus)
######### Positive Graphs##################

# Seperating tweets by indivdual words & then removing stop words
Positive_tweets <- Positive_PeakPlus %>%
  select(text) %>%
  unnest_tokens(word,text)
Positive_tweets <- Positive_tweets %>%
  anti_join(stop_words)


# Positive tweets most frequent words
Positive_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "blue") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
   xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in Positive tweets about Paramount Plus"
      )

ggsave("Positive_tweets.png", width = 10, height=11, units="in" )
#########Negative Graphs#########
# Doing the same to the negative tweets
Negative_tweets <- Negative_PeakPlus %>%
  select(text) %>%
  unnest_tokens(word,text)
Negative_tweets <- Negative_tweets %>%
  anti_join(stop_words)

Negative_tweets <- tokens(Negative_tweets)

Negative_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "light blue") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in Negative tweets about Paramount Plus"
  )

ggsave("Negative_tweets.png", width = 10, height=11, units="in" )













