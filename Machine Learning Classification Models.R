#Machine Learning Model For Spam Detection

library(caret)
library(randomForest)
library(lattice)
library(tm)
library(e1071)


dat <- read.csv("Spam Classification.csv")
dat$CLASS <- as.factor(dat$CLASS)

levels(dat$CLASS) <- c("Real", "Spam")
levels(dat$CLASS)

corpus = VCorpus(VectorSource(dat$CONTENT))
as.character(corpus[[1]])

corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
as.character(corpus[[1]])

dtm = DocumentTermMatrix(corpus)
dtm

dtm = removeSparseTerms(dtm, .999)
dim(dtm)
inspect(dtm[40:50, 10:15])

convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

datasetNB <- apply(dtm, 2, convert_count)
dataset <- as.data.frame(as.matrix(datasetNB))

freq<- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
tail(freq, 10)

findFreqTerms(dtm, lowfreq=60)

dataset$class <- dat$CLASS
is.factor(dataset$class)
str(dataset$cl)

set.seed(100)
split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
train_set = dataset[split == 1,]
test_set = dataset[split == 2,] 

prop.table(table(train_set$class))

is.factor(train_set$class)

#Random Forest Classifier
rf_classifier = randomForest(x = train_set[-978],
                            y = train_set$class,
                            ntree = 300)

rf_classifier

rf_pred = predict(rf_classifier, newdata = test_set[-978])
confusionMatrix(table(rf_pred,test_set$class))

#Naive Bayes Classifier
control <- trainControl(method="repeatedcv", number=10, repeats=3)
system.time( classifier_nb <- naiveBayes(train_set, train_set$class, laplace = 1,
                                         trControl = control,tuneLength = 7) )

nb_pred = predict(classifier_nb, type = 'class', newdata = test_set)

confusionMatrix(nb_pred,test_set$class)


