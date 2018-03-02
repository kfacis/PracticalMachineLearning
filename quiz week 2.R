library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
head(concrete)


library(Hmisc)
cutFlyAsh <- cut2(training$FlyAsh,g=3)
cutAge <- cut2(training$FlyAsh,g=5)
p1 <- qplot(CompressiveStrength,cutFlyAsh, data=training,fill=cutFlyAsh)
p1   
?cut2 ##--cut continuous variables into factors 

#########2. Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each of the variables in the data set (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). What do you notice in these plots?
#plotting everything but compressivestrength - by index to hopefully identify variable correlated 
names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
        theme_bw()
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)
ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
        theme_bw()
featurePlot(x = training[, names], y = cutCS, plot = "box")

####3. 
p1 <- qplot(Superplasticizer,data=training,plot = "hist")
ggplot(data = training, aes(Superplasticizer)) + geom_density()
variable <- log(training$Superplasticizer)

####4. Find all the predictor variables in the training set that begin with IL. 
        #Perform principal components on these variables with the preProcess() function from the caret package. 
        #Calculate the number of principal components needed to capture 90% of the variance. How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]];training = adData[ 
        inTrain,]
testing = adData[-inTrain,]
colnames(training)[58:69] #colnames starting with IL
#use Caret to preprocess
preProc <- preProcess(adData[,58:69],method="pca",thresh = 0.8) #80% is a standard threshold
preProc$rotation #since our threshold is set at 80, and 7 columns are returned, we need 7 components

#####5, Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
        #Build two predictive models, one using the predictors as they are and one using PCA with principal components 
        #explaining 80% of the variance in the predictors. Use method="glm" in the train function.
        #What is the accuracy of each method in the test set? Which is more accurate?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
## grep the predictors starting with 'IL'
IL_str <- grep("^IL", colnames(training), value = TRUE)
## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]
#PCA
modelFit <- train(diagnosis ~., data = training, method = "glm") #, preProcess = "pca")
predictions <- predict(modelFit, newdata = testing)
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1$overall[1])
colnames(training)

modelFit1 <- train(diagnosis ~ ., method = "glm", preProcess = "pca", data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit1,testing))
print(C2$overall[1])
