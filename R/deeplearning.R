## We try NLP first and CNN second

## Classifying tweets with deep learning

df[,is_neg := ifelse(means_tot < -1, 1, 0)]

df_dl <- df[,c('time', 'replied', 'favoriteCount', 'retweetCount', 'tweet','is_neg'), with = F]

# sample_df <- df_dl[rownames(df_dl) %in% sample(rownames(df_dl), size = 1000)]

sum(df_dl$is_neg)

# https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html (accessed on 13/05/2018)

library(text2vec)
library(data.table)
library(magrittr)

# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(df_dl$tweet, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = rownames(df_dl), 
                  progressbar = FALSE)

vocab = create_vocabulary(it_train)

vocab = vocab[vocab$term_count > 1,]
vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))

dtm_train <- cbind(dtm_train, as.numeric(as.matrix(df_dl[,c(1:4), with = F])))
dim(dtm_train)

## following tutorial https://www.datacamp.com/community/tutorials/keras-r-deep-learning
## accessed on 13/05/2018

# devtools::install_github("rstudio/keras")
require(tensorflow)
# install_tensorflow()
require(keras)
model <- keras_model_sequential()

model %>%
  layer_dense(units = 8, activation = 'relu',
              input_shape = 7667) %>%
  layer_dense(units = 2, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(
  dtm_train,
  to_categorical(df_dl$is_neg),
  epochs = 50,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
# Plot the model loss of the training data
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col="green")

# Add legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))


classes <- model %>% predict_classes(dtm_train, batch_size = 128)
pred <- cbind(df_dl, classes)
head(pred[V2 == 1 & is_neg == 0,],10)
head(pred[V2 == 0 & is_neg == 1,],10)

### Convolutional networks following https://keras.rstudio.com/articles/examples/cifar10_cnn.html

model <- keras_model_sequential()

model %>%
  # Start with hidden 2D convolutional layer being fed 32x32 pixel images (K: find equivalent for text...)
  layer_conv_1d(filters = 8, kernel_size = 3,
                padding = "same",
              input_shape = c(7667,1)) %>%
  layer_activation("relu") %>%

  # Flatten max filtered output into feature vector
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(8) %>%
  layer_activation("relu") %>%

  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(2) %>%
  layer_activation("softmax")

opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(
  array(dtm_train, dim = c(dim(dtm_train),1)),
  to_categorical(df_dl$is_neg),
  epochs = 5,
  batch_size = 5,
  validation_split = 0.2
)

plot(history)
# Plot the model loss of the training data
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col="green")

# Add legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))


classes <- model %>%
  predict_classes(array(dtm_train, dim = c(dim(dtm_train),1)),
                  batch_size = 128)

pred <- cbind(df_dl, classes)
head(pred[V2 == 1 & is_neg == 0,],10)
head(pred[V2 == 0 & is_neg == 1,],10)

pred[V2 == 1,]

