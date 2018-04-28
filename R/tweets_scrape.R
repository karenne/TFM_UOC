library(twitteR)
library(data.table)

#directorio que utilizamos 
dir <- <DIRECTORY>

# script donde se declaran las credenciales
source(paste0(dir, "/scripts/access_twit_api.R"))

tweets <- searchTwitter(' ', n=10000, lang = 'en')
save(tweets, file = "D:/datascience2help/tweets2.RData")
