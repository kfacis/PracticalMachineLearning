####Course 8
Sensitivity: Pr(positive test | disease)
Specificity: Pr(negative test | no disease)
Positive Predictive Value: Pr(disease | positive test)
Negative Predictive Value: Pr(no disease | negative test)
Accuracy: Pr(correct outcome)

ROC Curves: receiver operating characteristic
Plotting sensitivity vs specificity
area under curve represents strength of prediction
0.5 = random guessing = 45 degree line
1 is perfect classifier
above .8 is 'good'

Cross validation: tool for identifying predictors


## SPAM Example: Data splitting
        library(caret); library(kernlab); data(spam)
        inTrain <- createDataPartition(y=spam$type,
                                       p=0.75, list=FALSE)
        training <- spam[inTrain,]
        testing <- spam[-inTrain,]
        dim(training)
## SPAM Example: K-fold
        set.seed(125)
        folds <- createFolds(y=spam$type,k=10,
                             list=TRUE,returnTrain=TRUE)
        sapply(folds,length)
        folds[[1]][1:10]
## SPAM Example: Return test
        set.seed(32323)
        folds <- createFolds(y=spam$type,k=10,
                             list=TRUE,returnTrain=FALSE)
        sapply(folds,length)
        folds[[1]][1:10]
## SPAM Example: Resampling
        set.seed(32323)
        folds <- createResample(y=spam$type,times=10,
                                list=TRUE)
        sapply(folds,length)
        folds[[1]][1:10]
## SPAM Example: Time Slices
        set.seed(32323)
        tme <- 1:1000
        folds <- createTimeSlices(y=tme,initialWindow=20,
                                  horizon=10) ###predicting window of 20 samples, want to predict 10 future ones
        names(folds)
        folds$train[[1]]
        folds$test[[1]]
        
##############Train options
preProcess = later lecture
weights = up/down weight obs (for unbalanced data)
metric = default is 'Accuracy' for categorical, RMSE for continuous
maximize = 
trControl = large no. of other parameters set with trainControl
tuneGrid = 
tuneLength = 

##############trainControl options        
method: for resampling; bootstrap or crossvalidation
        boot = bootstrapping
        boot632 = boot w/ adjustment
        cv = cross validation
        repeatedcv = repeated cross validation
        LOOCV = leave one out cross validation
number: no of times to repeat resampling
        for boot/cross validation
        no of subsamples to take
repeats: no of times to repeat entire process of method resampling
p: size of training set
initialWindow:timecourse data tells size of original dataset, no of time points in training data
horizon: no of time points predicting
verboseInter:
seeds: for all different resampling layers; use w/ large samples and high no of predicters
        most rerun with random resampling of data...seeding each resample is useful for parallel fits
allowParallel
        
####Plotting predictors example
library(ISLR); library(ggplot2); library(caret); library(gridExtra);
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)
##plot all features against each other
featurePlot(x=training[,c("age","education","jobclass")], ###variables against wage
            y = training$wage, ##outcome
            plot="pairs") ##3:11 in Plotting predictors
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
## Add regression smoothers (*ggplot2* package)
        qq <- qplot(age,wage,colour=education,data=training)
        qq +  geom_smooth(method='lm',formula=y~x)

## cut2, making factors (*Hmisc* package)
        library(Hmisc)
        cutWage <- cut2(training$wage,g=3)
        table(cutWage) 
        p1 <- qplot(cutWage,age, data=training,fill=cutWage,
                    geom=c("boxplot"))
        p1        
#add points to box plots
        p2 <- qplot(cutWage,age, data=training,fill=cutWage,
                    geom=c("boxplot","jitter"))
        grid.arrange(p1,p2,ncol=2)
#can use the cut variable to look at tables of data
        t1 <- table(cutWage,training$jobclass) ##more industrial jobs in low wage factor
        t1
        prop.table(t1,1) 
##density plot
        qplot(wage,colour=education,data=training,geom="density")
###make plots ONLY in the training data
        --look for imbalance in outcomes/predictors
        --outliers
        --groups of points not explained by a predictor
        --skewed variables
        
#####Basic Preprocessing
Why preprocss?
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length") ###skewed to low ave run length
#standardize data to mean = 0 ----> I think this is what scale does
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)
        #same results with.....
        preObj <- preProcess(training[,-58],method=c("center","scale"))
        trainCapAveS <- predict(preObj,training[,-58])$capitalAve
        
        ##need to do the same to the test set
        testCapAveS <- predict(preObj,testing[,-58])$capitalAve
        
        ##can also pass preProcess commands to the train function
        modelFit <- train(type ~.,data=training,
                          preProcess=c("center","scale"),method="glm")
#Box-Cox transforms: take continuous data and transform  to normal data
        preObj <- preProcess(training[,-58],method=c("BoxCox"))
        trainCapAveS <- predict(preObj,training[,-58])$capitalAve
        par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)
#impute data for NAs
# Impute and standardize
        training$capAve <- training$capitalAve
        selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
        training$capAve[selectNA] <- NA
        preObj <- preProcess(training[,-58],method="knnImpute")
        capAve <- predict(preObj,training[,-58])$capAve ###subset of missing values
                
        # Standardize true values
        capAveTruth <- training$capitalAve
        capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
        quantile(capAve - capAveTruth)
        quantile((capAve - capAveTruth)[selectNA])
        quantile((capAve - capAveTruth)[!selectNA])
##Covaritaes - features that are a summary of other variables
        1. Raw data to covariate
        2. transform tidy covariates --> ONLY done in training set
        Why?
                -turn categorical variable to quantitative
                -
        #Run this to see which variables have little variability and will therefore not be good predictors
        nsv <- nearZeroVar(training,saveMetrics=TRUE)
library(splines) #for curvy lines
        bsBasis <- bs(training$age,df=3) 
        bsBasis     

##Correlated predictors
        library(caret); library(kernlab); data(spam)
        inTrain <- createDataPartition(y=spam$type,
                                       p=0.75, list=FALSE)
        training <- spam[inTrain,]
        testing <- spam[-inTrain,]
        
        M <- abs(cor(training[,-58])) #all variables besides outcome, i.e. costAdj, and taking correlation of all other values
        diag(M) <- 0 #removing the column of 1 where all variables w/itself have correlation of 1
        which(M > 0.8,arr.ind=T) #which of these variables of a correlation > 0.8
        #highly correlated
        names(spam)[c(34,32)]
        plot(spam[,34],spam[,32])
###benefits: 1) reduce noise by combining variables that are related adn 
                2)reduce number of predictors
##trying to figure out best combo of correlated variables that explains variability
###GOAL: 1) find a new set of variables that are uncorrelated and explain as much variance as possible 
        #2) put all variables in a matrix and find the best matrix w/fewest variables
#Reduce no of variables while maintaining accuracy
#(goal 1 = statistcial, 2 = data compression)
        #SOLUTIONS to goal
                #SVD- singular value decomposition
                #PCA: principal component analysis
smallSpam <- spam[,c(34,32)] #these are the two columns that are correlated
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
#rotation matrix
prComp$rotation #sum columns for the principal component that explains the highest variability
#PCA on SPAM data
typeColor <- ((spam$type=="spam")*1 + 1) #color variable: equal to black if spam, equal to red if non-spam
prComp <- prcomp(log10(spam[,-58]+1)) #PCA on entire dataset (log10 + 1 to make gaussian)
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2") #PCAs of entire dataset. selecting 1 and 2 to plot
#use Caret to preprocess
        preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
        spamPC <- predict(preProc,log10(spam[,-58]+1))
        plot(spamPC[,1],spamPC[,2],col=typeColor)
#can also pre-process as part of the training process
        modelFit <- train(training$type ~., method = "glm", preProcess = "pca", data = training)
        
###Predicting w/regression
        library(caret);data(faithful); set.seed(333)
        inTrain <- createDataPartition(y=faithful$waiting,
                                       p=0.5, list=FALSE)
        trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
        head(trainFaith)      
####LONGHAND
        #eruption data vs waiting time
                plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
        #fit a linear model
                lm1 <- lm(eruptions ~ waiting,data=trainFaith)
                lines(trainFaith$waiting,lm1$fitted,lwd=3)
        #Predict a new value
                coef(lm1)[1] + coef(lm1)[2]*80
                newdata <- data.frame(waiting=80)
                predict(lm1,newdata)
        #plot predictions - training and test
                par(mfrow=c(1,2))
                plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
                lines(trainFaith$waiting,predict(lm1),lwd=3)
                plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
                lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)
        #training/test set errors
                # Calculate RMSE on training
                sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
                # Calculate RMSE on test
                sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))
        ## Prediction intervals
                pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
                ord <- order(testFaith$waiting)
                plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
                #intervals capturing confidence intervals
                matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)        
###Same process with Caret
        modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
        summary(modFit$finalModel)
###Predicting with regression, multiple covariates
        #AND how to explore predictors in a dataset ##########exploratory analysis
        library(ISLR); library(ggplot2); library(caret);
        data(Wage); Wage <- subset(Wage,select=-c(logwage))
        summary(Wage)
        inTrain <- createDataPartition(y=Wage$wage,
                                       p=0.7, list=FALSE)
        training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
        dim(training); dim(testing)
#featureplot
        featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage, plot="pairs")        
        ## Plot age versus wage
        qplot(age,wage,data=training)
        ## Plot age versus wage colour by jobclass
        qplot(age,wage,colour=jobclass,data=training)
        ## Plot age versus wage colour by education
        qplot(age,wage,colour=education,data=training)
#fit linear modle
        modFit<- train(wage ~ age + jobclass + education,
                       method = "lm",data=training)
        finMod <- modFit$finalModel
        print(modFit)        
## Diagnostics
        plot(finMod,1,pch=19,cex=0.5,col="#00000010")        
        ## Color by variables not used in the model
        qplot(finMod$fitted,finMod$residuals,colour=race,data=training)        
        
## If you want to use all covariates
        modFitAll<- train(wage ~ .,data=training,method="lm")
        pred <- predict(modFitAll, testing)
        qplot(wage,pred,data=testing)        

##Predicting with Trees        
        #pros - easy to interpret
        #cons - can lead to overfitting without pruning
                #-hard to estimate uncertainty
        ## Basic algorithm
        # 1. Start with all variables in one group
        # 2. Find the variable/split that best separates the outcomes
        # 3. Divide the data into two groups ("leaves") on that split ("node")
        # 4. Within each split, find the best variable/split that separates the outcomes
#measeure of impurity: 0 = perfect, 0.5 = none
        modFit <- train(Species ~ .,method="rpart",data=training)
        print(modFit$finalModel)
        plot(modFit$finalModel, uniform=TRUE, 
             main="Classification Tree")
        text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
        ## Prettier plots
        library(rattle)
        fancyRpartPlot(modFit$finalModel)
#caret package - train functions that already including bagging: set method = bagEarth, treebag, bagFDA
        #alt. use the bag function 
#random forest -- bootstrap our data and rebuild classification trees on each split
        1. Bootstrap samples
        2. At each split, bootstrap variables
        3. Grow multiple trees and vote
        #pros - accuracy
        #cons - speed, interpretability, overfitting
        library(caret)
        modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)
        modFit        
        ## Getting a single tree
        getTree(modFit$finalModel,k=2)
        
#boosting - one of the best out-of-the-box (w/ random forest) accurate classifiers        
        ## Wage example
        library(ISLR); data(Wage); library(ggplot2); library(caret);
        Wage <- subset(Wage,select=-c(logwage))
        inTrain <- createDataPartition(y=Wage$wage,
                                       p=0.7, list=FALSE)
        training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
        ## Fit the model
        modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
        print(modFit)
        qplot(predict(modFit,testing),wage,data=testing)
        
#####Week4
##Regularized regression - subset data to remove excess predictors
###***** more complicated models ALWAYS decreases training error. HOWEVER in the test set
        #the error will decrease to a point and then increase
        #caret methods are: ridge, lasso, relaxo
#combining predictors:
        # combine classifiers by average/voting
        #improves accuracy, but can reduce interpretability
        ## Approaches for combining classifiers
        
        1. Bagging, boosting, random forests
        * Usually combine similar classifiers
        2. Combining different classifiers
        * Model stacking
        * Model ensembling
## Example with Wage data
        library(ISLR); data(Wage); library(ggplot2); library(caret);
        Wage <- subset(Wage,select=-c(logwage))
        # Create a building data set and validation set
        inBuild <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
        validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
        inTrain <- createDataPartition(y=buildData$wage,p=0.7, list=FALSE)
        training <- buildData[inTrain,]; testing <- buildData[-inTrain,]
## Build two different models
        mod1 <- train(wage ~.,method="glm",data=training)
        mod2 <- train(wage ~.,method="rf",data=training, trControl = trainControl(method="cv"),number=3)
        ## Predict on the testing set 
        pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
        qplot(pred1,pred2,colour=wage,data=testing)
## Fit a model that combines predictors
        predDF <- data.frame(pred1,pred2,wage=testing$wage)
        combModFit <- train(wage ~.,method="gam",data=predDF)
        combPred <- predict(combModFit,predDF)
## Testing errors
        sqrt(sum((pred1-testing$wage)^2))
        sqrt(sum((pred2-testing$wage)^2))
        sqrt(sum((combPred-testing$wage)^2))
## Predict on validation data set
        pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
        predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
        combPredV <- predict(combModFit,predVDF)        
####Forecasting
## What is different?

* Data are dependent over time
* Specific pattern types
* Trends - long term increase or decrease
* Seasonal patterns - patterns related to time of week, month, year, etc.
* Cycles - patterns that rise and fall periodically
* Subsampling into training/test is more complicated
* Similar issues arise in spatial data 
* Dependency between nearby observations
* Location specific effects
* Typically goal is to predict one or more observations into the future. 
* All standard predictions can be used (with caution!)        
        
## Google finance data
        library(quantmod)
        from.dat <- as.Date("01/01/08", format="%m/%d/%y")
        to.dat <- as.Date("12/31/13", format="%m/%d/%y")
        getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
        head(GOOG)        
## Summarize monthly and store as time series
        mGoog <- to.monthly(GOOG)
        googOpen <- Op(mGoog)
        ts1 <- ts(googOpen,frequency=12)
        plot(ts1,xlab="Years+1", ylab="GOOG")        
## Decompose a time series into parts
        plot(decompose(ts1),xlab="Years+1")
## Training and test sets
        ts1Train <- window(ts1,start=1,end=5)
        ts1Test <- window(ts1,start=5,end=(7-0.01))
        ts1Train
## Simple moving average
        plot(ts1Train)
        lines(ma(ts1Train,order=3),col="red")
## Exponential smoothing-more heavily weight time points closer, less as they are further away
        ets1 <- ets(ts1Train,model="MMM")
        fcast <- forecast(ets1)
        plot(fcast); lines(ts1Test,col="red")        
        
############################################################ Week 3 and 4 quiz
        require(AppliedPredictiveModeling)
        require(caret)
        require(ElemStatLearn)
        require(pgmm)
        
        data(SAheart)
        set.seed(33833)
        train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
        trainSA = SAheart[train,]
        testSA = SAheart[-train,]
        set.seed(13234)
        fit <- glm(cdh ~ age, family = "binomial", data = SAheart)
        modelSA <- caret::train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
                                data = trainSA, method = "glm", family = "binomial")
        missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
        missClass(testSA$chd, predict(modelSA, newdata = testSA))
        missClass(trainSA$chd, predict(modelSA, newdata = trainSA))
        data(vowel.train)
        data(vowel.test)
        vowel.train$y <- as.factor(vowel.train$y)
        vowel.test$y <- as.factor(vowel.test$y)
        library(randomForest)
        modvowel <- randomForest(y ~ ., data = vowel.train, method = "rf")
        modvowel1 <- randomForest(y ~ ., data = vowel.train, method = "gbm")
        p <- predict(modvowel, vowel.test)
        p1 <- predict(modvowel1, vowel.test)
        confusionMatrix(p1, vowel.test$y)$overall[1]
        
        
        set.seed(3433)
        data(AlzheimerDisease)
        adData = data.frame(diagnosis,predictors)
        inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
        training = adData[ inTrain,]
        testing = adData[-inTrain,]
        mod_rf <- randomForest(diagnosis ~ ., data = training, method = "rf")
        mod_gbm <- randomForest(diagnosis ~ ., data = training, method = "gbm")
        mod_lda <- randomForest(diagnosis ~ ., data = training, method = "lda")
        p_rf <- predict(mod_rf, testing)
        p_gbm <- predict(mod_gbm, testing)
        p_lda <- predict(mod_lda, testing)
        pred_combine <- data.frame(p_rf, p_gbm,p_lda, diagnosis = testing$diagnosis)
        combineFit <- caret::train(diagnosis~., method = "rf", data = pred_combine)
        comb_pred <- predict(combineFit, pred_combine)
        accuracy(combineFit, testing$diagnosis)
        
        data(concrete)
        inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
        training = concrete[ inTrain,]
        testing = concrete[-inTrain,]
        set.seed(325)
        mod_lasso <- caret::train(CompressiveStrength ~ ., data = training, method = "lasso")
        library(elasticnet)
        plot.enet(mod_lasso$finalModel, xvar = "penalty", use.color = TRUE)
        colnames(concrete)
        require("e1071")
        model_svm <- svm(CompressiveStrength ~ . , training)
        pred <- predict(model_svm, testing)
        accuracy(pred_svm, testing$CompressiveStrength)