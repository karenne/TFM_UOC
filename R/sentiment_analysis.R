
# Downloading ranking of words 
# available online http://crr.ugent.be/programs-data/word-ratings
# accessed on 15/04/2018

strFile <- <FILE WITH DATA>

data_rank <- read.csv(strFile)

colnames(data_rank)

require(stringr)
# by-tweet averages: for each row of the original df, take the mean of each numeric measure
# across all words in that tweet that appear in the valence, arousal and dominance lexicon
measures <- df_tweets %>% 
  rowwise() %>% 
  do({
    tws <- unlist(str_split(.$tweet, boundary("word")))
    dplyr::filter(data_rank[,c('Word', 'V.Mean.Sum', 'A.Mean.Sum', 'D.Mean.Sum')], Word %in% tws) %>%
      summarise_all(funs(mean)) %>%
      as.data.frame()
  })
codedTweets <- bind_cols(df_tweets, measures)
codedTweets[,'Word':=NULL]

head(codedTweets[order(V.Mean.Sum, decreasing = T),],10)
head(codedTweets[order(A.Mean.Sum, decreasing = T),],10)
head(codedTweets[order(D.Mean.Sum, decreasing = T),],10)

head(codedTweets[!is.na(V.Mean.Sum),][order(V.Mean.Sum, decreasing = F),],10)
head(codedTweets[!is.na(A.Mean.Sum),][order(A.Mean.Sum, decreasing = F),],10)
head(codedTweets[!is.na(D.Mean.Sum),][order(D.Mean.Sum, decreasing = F),],10)

require(lexicon)
data(dodds_sentiment)
head(dodds_sentiment)

measures <- df_tweets %>% 
  rowwise() %>% 
  do({
    tws <- unlist(str_split(.$tweet, boundary("word")))
    dplyr::filter(dodds_sentiment[,c('word', 'happiness_rank')], word %in% tws) %>%
      summarise_all(funs(mean)) %>%
      as.data.frame()
  })
codedTweets <- bind_cols(df_tweets, measures)
codedTweets[,'word':=NULL]

head(codedTweets[order(happiness_rank, decreasing = T),],50)
head(codedTweets[!is.na(happiness_rank),][order(happiness_rank, decreasing = F),],10)

data(hash_sentiment_inquirer)


measures <- df %>% 
  rowwise() %>% 
  do({
    tws <- unlist(str_split(.$text, boundary("word")))
    dplyr::filter(hash_sentiment_inquirer[,c('x', 'y')], x %in% tws) %>%
      summarise_all(funs(mean)) %>%
      as.data.frame()
  })
codedTweets <- bind_cols(df, measures)
codedTweets[,'x':=NULL]

head(codedTweets[order(y, decreasing = T),],10)
head(codedTweets[!is.na(y),][order(y, decreasing = F),],10)
