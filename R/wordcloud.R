# Following the tutorial available online: 
# https://cognitivedatascientist.com/2016/03/16/twitter-sentiment-analysis-in-r-14/
# accessed on 15/04/2018

load(<FILE WITH TWEETS>)

t <- strip_retweets(tweets)
df1 <- twListToDF(t)
df1 <- as.data.table(df)

load(<ANOTHER FILE WITH TWEETS")


t <- strip_retweets(tweets)
df2 <- twListToDF(t)
df2 <- as.data.table(df)

df <- rbind(df, df2)

rm(tweets, t)
rm(df1, df2)

library(tm)

# create a vector source, which interprets each element of its argument as a document
v <- VectorSource(df$text)

# create an object of class 'corpus'; a collection of documents containing NL text
docs <- Corpus(v)

# convert corpus to plain text
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, content_transformer(function(x) iconv(x, to='ASCII', sub='byte')))
docs <- tm_map(docs, content_transformer(function(x) tolower(x)))
docs <- tm_map(docs, removeWords, stopwords('en'))

# remove URLs
stripURL = function(x) {
  gsub("www[^[:space:]]+|htt[^[:space:]]+", "", x)
}
docs <- tm_map(docs, content_transformer(stripURL))

docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

df$tweet <- sapply(docs, as.character)
# df$text <- NULL

# remove rows with duplicate tweets
require(dplyr)
df_tweets <- df %>% distinct(., tweet)

library(wordcloud)
col=brewer.pal(8, 'Set1')

text2cloud <- df_tweets$tweet
corp <- Corpus(VectorSource(text2cloud))
print(wordcloud(corp, max.words=75, random.color=F, 
                random.order=F, colors=col))
