library(data.table)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(knitr)
library(e1071)

link_train <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
link_test <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainRaw <- read.csv(url(link_train), na.strings=c("NA","#DIV/0!",""))
testRaw <- read.csv(url(link_test), na.strings=c("NA","#DIV/0!",""))


#splitting data 70/30 on classe variable for testing model accuracy before applying to testRaw dataset.
set.seed(42)
train_split <- createDataPartition(trainRaw$classe, p =0.7, list = FALSE)
train_1 <- trainRaw[train_split,]
train_2 <- trainRaw[-train_split,]
dim(train_1); dim(train_2)

#removing varaibles with near zero variance with the R-function nearZeroVar
nzv <- nearZeroVar(train_1, saveMetrics=TRUE)
train_1 <- train_1[,nzv$nzv==FALSE]
train_2 <- train_2[,nzv$nzv==FALSE]
dim(train_1); dim(train_2)

#identifying clutter variables that do not have sufficient data to build into models
#these are columns with highest NA counts. na_count is a vector that contains TRUE if the column has no NAs. 
#these are the columns we want to incorporate into our model building
discardColumns <- sapply(train_1, function(x) mean(is.na(x))) > 0.95
train_1 <- train_1[, discardColumns==F]
train_2 <- train_2[, discardColumns==F]
dim(train_1); dim(train_2)

#dropping these five irrelevant columns
knitr::kable(train_1[1:5,1:5])
train_1 <- train_1[, -c(1:5)]
train_2 <- train_2[, -c(1:5)]
dim(train_1); dim(train_2)

##Modeling
#using the train_1 dataframe, I constructed three models using Random forest. 
#1. Random Forest
#2. Generalized Boosted Regression (gbm)
#3. Linear discriminate analysis
#these three models will be built using train_1 and predicted on train_2
mod_rf <- randomForest(classe ~ ., data = train_1, method = "rf")
mod_gbm <- randomForest(classe ~ ., data = train_1, method = "gbm")
mod_lda <- randomForest(classe ~ ., data = train_1, method = "lda")
p_rf <- predict(mod_rf, train_2)
p_gbm <- predict(mod_gbm, train_2)
p_lda <- predict(mod_lda, train_2)
#confusionMatrix(train_2$classe, p_rf)
#confusionMatrix(train_2$classe, p_gbm)
#confusionMatrix(train_2$classe, p_lda)
accuracy_rf <- postResample(p_rf, train_2$classe)
accuracy_gbm <- postResample(p_gbm, train_2$classe)
accuracy_lda <- postResample(p_lda, train_2$classe)
accuracy_rf; accuracy_gbm; accuracy_lda
#from the accuracy output, we can see the lda model has the highest accuracy and lowest out-of-sample error rate
mod_lda #our OOB estimate of error rate: 0.3%
plot(mod_lda)
#confusion matrix of the model we've decided to build out
confusionMatrix(train_2$classe, p_lda)
#predict on test set based on these results
result_lda <- predict(mod_lda, testRaw)
result_lda

#txt file containing results
lapply(result_lda, function(x) write.table( data.frame(x), 'results.txt'  , append= T, sep=',' ))

