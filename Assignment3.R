data.train <- read.csv("http://ricknouwen.org/moviereview.training.frame",
                       sep=",", header=TRUE)
str(data.train)
## 'data.frame':    22402 obs. of  3 variables:
##  $ term     : Factor w/ 11201 levels "aaron","abandon",..: 1 1 2 2 3 3 4 4 5 5 ...
##  $ freq     : int  9 5 16 8 22 19 5 0 10 0 ...
##  $ sentiment: int  1 0 1 0 1 0 1 0 1 0 ...

data.test <- read.csv("http://ricknouwen.org/movie.testing.frame",
                      sep=",",
                      colClasses=c("character","character","integer"),
                      header=TRUE)

#classifier
classify <- function(wordlist){
  #likelihood function
  ## a function that takes a word as input and outputs the likelihood per class
  totalLikelihoodPos <-1
  totalLikelihoodNeg <-1
  for (w in wordlist){
    freqs <- data.train$freq[data.train$term==w]
    totalPos <- sum(data.train[data.train$sentiment == 1,]$freq)
    totalNeg <- sum(data.train[data.train$sentiment == 0,]$freq)
    likelihoodPos <- freqs[1]/totalFreq
    likelihoodNeg <- freqs[2]/totalFreq
    totalLikelihoodPos <- totalLikelihoodPos* likelihoodPos
    totalLikelihoodNeg <- totalLikelihoodNeg* likelihoodNeg
  }
  
  prior.scores <- sapply(c(0,1),function (n) {
    sum(data.train$freq[data.train$sentiment==n])/sum(data.train$freq)} )
  
  totalLikelihood <- c(totalLikelihoodPos* prior.scores[1] ,totalLikelihoodNeg* prior.scores[2])
  result <- which.max(totalLikelihood)
  print(totalLikelihood)
  if (result == 1){
    print("Positive sentiment")
  }
  else {
    print("Negative sentiment")
  }
}
blabla <- c("hello", "world")
classify(blabla)

#tokenizer
#install.packages("tokenizers")
library(tokenizers)
tokenize_words("mY name \n is :-) MICHAEL Caine")

#subset
intersect(unlist(tokenize_words(data.test[22,]$txt)),data.train$term)
