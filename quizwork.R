#quizwork
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
head(predictors)
head(diagnosis)
data(concrete)
testing <- mixtures[ -inTrain, ]
training <- mixtures[ inTrain, ]

#Q5
library(AppliedPredictiveModeling)
library(caret)
set.seed(3433)
data(AlzheimerDisease)
# adData <- data.frame(diagnosis, predictors[,grep("IL", names(predictors))])
adData <- data.frame(diagnosis, predictors)
inTrain <- createDataPartition(adData$diagnosis, p = 0.75)[[1]]
training <- adData[inTrain,]
testing <- adData[-inTrain,]
train(adData[,grep("^IL", names(predictors))], adData$diagnosis, method = "glm", preProcess = "pca")


#Q3.1
library(AppliedPredictiveModeling)
library(caret)
data(segmentationOriginal)
summary(segmentationOriginal )
segTran <- segmentationOriginal[segmentationOriginal$Case == "Train",]
set.seed(125)
segmdl <- train(Class ~ ., data=segTran, model = "rpart")


segTran.cart0 <- rpart(Class ~ ., segTran)
segTran.cart <- rpart(Class ~ FiberWidthCh1 
                      + VarIntenCh4 
                      + PerimStatusCh1 
                      + TotalIntenCh2, segTran)

plot(segTran.cart0); text(segTran.cart0, cex = 0.6)

#Q3.3
libary(pgmm)
data(olive)
olive <- Olive[, -1]
molive <- rpart(olive$Area ~ ., olive)
newdata  <- as.data.frame(t(colMeans(olive)))
predict(molive, newdata)

Q3.4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train <- sample(1:dim(SAheart)[1], size=dim(SAheart)[1]/2, replace = F)
trainSA <- SAheart[train,]
testSA <- SAheart[-train,]
summary(trainSA)
set.seed(13234)
frml <- "chd ~ age + alcohol + obesity + tobacco + typea + ldl"
modelSA <- glm( frml , data = trainSA, family = "binomial")
missClass <- function(values, prediction) {sum(((prediction > 0.5) * 1) != values)/length(values)}
missClass(trainSA$chd, predict(modelSA, trainSA))
missClass(testSA$chd, predict(modelSA, testSA))

#Q3.5
data(vowel.train)
data(vowel.test)
set.seed(33833)
RFFIT <- train(y ~ ., data = vowel.train, method="rf")
varImp(RFFIT)

#Q4.1
data(vowel.train)
data(vowel.test)
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
rf.mod <- train(vowel.train[,-1], vowel.train$y, method = "rf")
gbm.mod <- train(vowel.train[,-1], vowel.train$y, method = "gbm")














