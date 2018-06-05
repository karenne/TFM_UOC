### Applying reinforcement learning

## The model applied is as follows, to make the agent learn, every time we use the algorithm 
## it will return five examples of predicted values as negative and five predicted values as positive
## that were neither positive or negative and we have to say if we agree or not with the result
## if we agree this is a point, if we don't it is 0. If the punctuation is higher than 5 then the new train 
## would be the prediction from the network


df[,is_neg := ifelse(means_tot < -0.5, 1, 0)]

df_dl <- df[,c('time', 'replied', 'favoriteCount', 'retweetCount', 'tweet', 'text', 'is_neg'), with = F]

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



norm_fun <- function (x) {
  sigma <- sd(x, na.rm = T)
  mu <- mean(x, na.rm = T)
  xnorm <- (x - mu) / sigma
  return(xnorm)
}

cols_to_add <- df_dl[,c(1:4), with = F]

cols_to_add <- apply(cols_to_add, 2, function (x) norm_fun(as.numeric(x)))

dtm_train <- cbind(dtm_train, as.numeric(as.matrix(cols_to_add)))

val_array <- rep(0, nrow(dtm_train))
#dtm_train2 <- cbind(dtm_train, val_array)

#dim(dtm_train2)
dim(dtm_train)

### Convolutional networks following https://keras.rstudio.com/articles/examples/cifar10_cnn.html
require(tensorflow)
# install_tensorflow()
require(keras)


cnn_model <- function (dtm_train, target, epochs){
  model <- keras_model_sequential()
  
  model %>%
    # Start with hidden 2D convolutional layer being fed 32x32 pixel images (K: find equivalent for text...)
    layer_conv_1d(filters = 8, kernel_size = 3, 
                  padding = "same",
                  input_shape = c(dim(dtm_train)[2],1)) %>%
    layer_activation("relu") %>%
    
    # Flatten max filtered output into feature vector 
    # and feed into dense layer
    layer_flatten() %>%
    layer_dense(8) %>%
    layer_activation("relu") %>%
    
    # Outputs from dense layer are projected onto 10 unit output layer
    layer_dense(2) %>%
    layer_activation("softmax")
  
  # opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  
  history <- model %>% fit(
    array(dtm_train, dim = c(dim(dtm_train),1)),
    to_categorical(target),
    epochs = epochs,
    batch_size = 5,
    validation_split = 0.2
  )
  return(model)
}

# plot(history)
# # Plot the model loss of the training data
# plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
# 
# # Plot the model loss of the test data
# lines(history$metrics$val_loss, col="green")
# 
# # Add legend
# legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))


target = df_dl[, c('text', 'is_neg'), with = FALSE]
model <- cnn_model(dtm_train, target$is_neg, epochs = 1)
classes <- model %>%
  predict_classes(array(dtm_train, dim = c(dim(dtm_train),1)),
                  batch_size = 128)

pred <- cbind(target, classes)

read_punct <- function(txt)
{ 
  n <- readline(prompt=paste("Do you think this sentence can be cyberbullying? (1 if yes, 0 if no): \n", 
                             txt))
  return(as.integer(n))
}

k = 0

for (pr in sample(pred[V2 == 1 & is_neg == 0,text],5)){
  rmark = read_punct(pr)
  while (is.na(rmark)) rmark = read_punct(pr)
  k <- k + rmark
  
  if (rmark == 1) {
    target[text == pr, is_neg := 1]  
  } else {
    target[text == pr, is_neg := 0]  
    }
}


for (pr in sample(pred[V2 == 0 & is_neg == 1,text],5)){
  rmark = read_punct(pr)
  while (is.na(rmark)) rmark = read_punct(pr)
  k <- k + ifelse(rmark == 1, 0, 1)
  if (rmark == 1) {
    target[text == pr, is_neg := 1]  
  } else {
    target[text == pr, is_neg := 0]  
  }
}

val_array <- target$is_neg * (10 - k)/10 + classes * k/10 
target[, is_neg := ifelse(val_array > 0.5, 1, 0)]

n_iter = 9
i = 1
ep = 1
while (i <= n_iter | k == 10) {
  if (k <= 5){
    ep = ep + 1
    
  } 
  
    # dtm_train2 <- cbind(dtm_train, as.matrix(val_array))
    model <- cnn_model(dtm_train, target$is_neg, epochs = ep)
    classes <- model %>%
      predict_classes(array(dtm_train, dim = c(dim(dtm_train),1)),
                      batch_size = 128)
    
    pred <- target[,V2 := classes]
    
    while (dim(pred[V2 == 0])[1] == 0) {
      model <- cnn_model(dtm_train, target$is_neg, epochs = ep - 1)
      classes <- model %>%
        predict_classes(array(dtm_train, dim = c(dim(dtm_train),1)),
                        batch_size = 128)
    
      pred <- target[,V2 := classes]
      }
    
    
    k = 0
    
    for (pr in sample(pred[V2 == 1 & is_neg == 0,text],5)){
      rmark = read_punct(pr)
      k <- k + rmark
      if (rmark == 1) {
        target[text == pr, is_neg := 1]  
      } else {
        target[text == pr, is_neg := 0]  
      }
    }
    
    
    for (pr in sample(pred[V2 == 0 & is_neg == 1,text],5)){
      rmark = read_punct(pr)
      k <- k + ifelse(rmark == 1, 0, 1)
      if (rmark == 1) {
        target[text == pr, is_neg := 1]  
      } else {
        target[text == pr, is_neg := 0]  
      }
    }
    
    val_array <- val_array * (10 - k)/10 + classes * k/10 
    target[, is_neg := ifelse(val_array > 0.5, 1, 0)]
  
  i = i + 1 
}


final_df <- cbind(df_dl, val_array)
final_df[val_array > 0.9 & is_neg == 0, text]
final_df[val_array <=0.9 & is_neg == 1, text]


sample(final_df[val_array > 0.9, text], 10)
