
#~~~~~~~~~~~~~~~~~~~~~~~~~~ FIRST APPROACH ON SENTIMENT ANALYSIS

# Downloading ranking of words 
# available online http://crr.ugent.be/programs-data/word-ratings
# accessed on 15/04/2018

strFile <- 'D:/UOC/tfm/data/Ratings_Warriner_et_al.csv'

data_rank <- read.csv(strFile)

colnames(data_rank)
head(data_rank[, c('Word', 'V.Mean.Sum', 'A.Mean.Sum', 'D.Mean.Sum')])

require(stringr)
# by-tweet averages: for each row of the original df, take the mean of each numeric measure
# across all words in that tweet that appear in the valence, arousal and dominance lexicon

#~# REMARK. It is better to use directly the words without processing to avoid wrong transformations. 

measures <- df %>% 
  rowwise() %>% 
  do({
    tws <- unlist(str_split(.$text, boundary("word")))
    dplyr::filter(data_rank[,c('Word', 'V.Mean.Sum', 'A.Mean.Sum', 'D.Mean.Sum')], Word %in% tws) %>%
      summarise_all(funs(mean)) %>%
      as.data.table()
  })

codedTweets <- bind_cols(data.table(text = df$text), measures)
codedTweets[,Word:=NULL]

head(codedTweets[order(V.Mean.Sum, decreasing = T),],10)
head(codedTweets[order(A.Mean.Sum, decreasing = T),],10)
head(codedTweets[order(D.Mean.Sum, decreasing = T),],10)

head(codedTweets[!is.na(V.Mean.Sum),][order(V.Mean.Sum, decreasing = F),],10)
head(codedTweets[!is.na(A.Mean.Sum),][order(A.Mean.Sum, decreasing = F),],10)
head(codedTweets[!is.na(D.Mean.Sum),][order(D.Mean.Sum, decreasing = F),],10)

require(lexicon)
data(dodds_sentiment)
head(dodds_sentiment[,c('word', 'happiness_average', 'happiness_rank')])

measures <- df %>% 
  rowwise() %>% 
  do({
    tws <- unlist(str_split(.$text, boundary("word")))
    dplyr::filter(dodds_sentiment[,c('word', 'happiness_rank')], word %in% tws) %>%
      summarise_all(funs(mean)) %>%
      as.data.table()
  })
codedTweets <- bind_cols(data.table(text = df$text), measures)
codedTweets[,word:=NULL]

head(codedTweets[order(happiness_rank, decreasing = T),],10)
head(codedTweets[!is.na(happiness_rank),][order(happiness_rank, decreasing = F),],10)

data(hash_sentiment_inquirer)
head(hash_sentiment_inquirer)

measures <- df %>% 
  rowwise() %>% 
  do({
    tws <- unlist(str_split(.$text, boundary("word")))
    dplyr::filter(hash_sentiment_inquirer[,c('x', 'y')], x %in% tws) %>%
      summarise_all(funs(mean)) %>%
      as.data.table()
  })
codedTweets <- bind_cols(data.table(text = df$text), measures)
codedTweets[,x:=NULL]

head(codedTweets[order(y, decreasing = T),],10)
head(codedTweets[!is.na(y),][order(y, decreasing = F),],10)

# Combine the datasets 
merge_rank <- merge(data_rank[,c('Word', 'V.Mean.Sum', 'A.Mean.Sum', 'D.Mean.Sum')], 
                    dodds_sentiment[,c('word', 'happiness_rank', 'happiness_average')],
                    by.x = 'Word', by.y = 'word', all = T)
merge_rank <- merge(merge_rank, hash_sentiment_inquirer, by.x = 'Word', by.y = 'x', all = T)

tws_filt <-  function (x) {
  tws <- unlist(str_split(x, boundary("word")))
  words <- dplyr::filter(merge_rank, Word %in% tws)
  df <- data.table(txt = x, mean_val = mean(words$V.Mean.Sum, na.rm = T), 
                   sum_val = sum(words$V.Mean.Sum, na.rm = T),
                   mean_dom = mean(words$D.Mean.Sum, na.rm = T), 
                   sum_dom = sum(words$D.Mean.Sum, na.rm = T), 
                   mean_hap = mean(words$happiness_rank, na.rm = T),
                   sum_hap = sum(words$happiness_rank, na.rm = T),
                   mean_hpav = mean(words$happiness_average, na.rm = T), 
                   sum_hpav = sum(words$happiness_average, na.rm = T), 
                   mean_y = mean(words$y, na.rm = T),
                   sum_y = sum(words$y, na.rm = T))
  return(list(df))
  }

ds_means <- as.data.table(tws_filt(df$tweet[1]))

for (x in df$tweet[2:length(df$tweet)]){
  ds_means <- rbind(ds_means, as.data.table(tws_filt(x)))
}

ds_means[,mean_hap:=-mean_hap]
ds_means[,sum_hap:=-sum_hap]


# ds_means[!is.na(mean_y),][order(ds_means[!is.na(mean_y),mean_y]),]
# ds_means[!is.na(sum_y),][order(ds_means[!is.na(sum_y),sum_y]),]
# ds_means[!is.na(sum_val),][order(ds_means[!is.na(sum_val),sum_val]),]
# ds_means[!is.na(sum_hap),][order(ds_means[!is.na(sum_hap),sum_hap], decreasing = T),]
# ds_means[!is.na(mean_val),][order(ds_means[!is.na(mean_val),mean_val]),]


norm_fun <- function (x) {
  sigma <- sd(x, na.rm = T)
  mu <- mean(x, na.rm = T)
  xnorm <- (x - mu) / sigma
  return(xnorm)
}

ds_means_norm <- cbind(ds_means$txt, 
                       apply(ds_means[,2:(ncol(ds_means) - 1), with = F], 2, norm_fun),
                       ds_means$mean_y)

head(ds_means_norm)

mean_tot <-
  unlist(apply(ds_means_norm[,2:ncol(ds_means_norm)], 1, 
                                     function (x) mean(as.numeric(x), na.rm = T)))

ds_means_norm <- as.data.table(ds_means_norm )
ds_means_norm[,mean_tot := mean_tot]

head(ds_means_norm[!is.na(mean_tot),][order(ds_means_norm[!is.na(mean_tot),mean_tot]),],50)


### ELABORACIÓN DE UN ALGORITMO NO SUPERVISADO PARA ENCONTRAR TWEETS QUE PUEDEN SER CYBERBULLYING

# Incorporamos el kpi de sentiment analysis creado:
df$means_tot <- ds_means_norm$mean_tot

# Miramos otras variables que sean validas.

head(sort(table(df$screenName), decreasing = T))

df$time <- strftime(df$created, format="%H", tz = 'GMT')
df[!is.na(means_tot),][order(df[!is.na(means_tot),means_tot])]  

require(ggplot2)

df[, replied := ifelse(is.na(replyToSN),0,1)]

ggplot(data = df) +
  geom_density(aes(means_tot, fill = factor(replied)), alpha = 0.3)

ggplot(data = df) +
  geom_boxplot(aes(x = factor(time), y = means_tot, color = factor(time)
               ), alpha = 0.3)

df[retweetCount!= 0,][order(df[retweetCount!= 0,means_tot])]

length(df[retweetCount > 2,retweetCount])
quantile(df[,retweetCount], c(0.25,0.5,0.75, 0.9, 0.95))

df[,rtwct := cut(retweetCount, c(0,3, 8, 15, Inf), include.lowest = T)]

cor(df[retweetCount < 90 & retweetCount > 2 ,retweetCount], df[retweetCount < 90 & retweetCount > 2  ,means_tot])

ggplot(data = df) +
  geom_density(aes(means_tot, fill = factor(rtwct)), alpha = 0.3)


ggplot(data = df) +
  geom_point(aes(x = retweetCount,y = means_tot, color = rtwct), alpha = 0.3) +
  scale_x_log10()

##  Asumir que el mean es la única variable que corresponde a un posible cyberbullying

df_neg <- df[means_tot < -1,]

head(sort(table(df$replyToSID), decreasing = T))

df[id %in% replyToSID,]

ggplot(data = df_neg) +
  geom_density(aes(replied))

ggplot(data = df_neg) +
  geom_density(aes(time), fill = 'blue', alpha = 0.5)

df[grepl('sex', tweet)]
df[grepl('fuck', tweet)]

col=brewer.pal(8, 'Set1')

text2cloud <- df_neg$tweet
corp <- Corpus(VectorSource(text2cloud))
print(wordcloud(corp, max.words=75, random.color=F, 
                random.order=F, colors=col))

# df_neg$text


df_neg[!is.na(means_tot),][order(df_neg[!is.na(means_tot), means_tot]), c('text', 'means_tot'), with = FALSE]

df_neg[means_tot %in% sample(df_neg$means_tot, size = 5), c('text', 'means_tot'), with = FALSE]

df_pos <- df[means_tot >= -1]
df_pos[means_tot %in% sample(df_pos$means_tot, size = 5), c('text', 'means_tot'), with = FALSE]


ggplot(data = df) + 
  geom_density(aes(means_tot), fill = 'turquoise', alpha = 0.7)

