library(caret)
dataTypes <- c("NULL", "character", rep("NULL", 3)
               )#5
               "character", "integer", rep("numeric", 6), "NULL", #14
               "numeric", "numeric", "NULL", rep("numeric",71),   #88
             "NULL", rep("numeric",2), "NULL", rep("numeric",34), #126
              "NULL", "numeric", "numeric", "NULL",               #130
               rep("numeric", 9), "NULL", rep("numeric" ,19),     #159
               "character"
)

PMLhead <- read.table(
  "C:\\Users\\steve\\Desktop\\Coursera\\MachLearn\\pml-training.csv",
  header = TRUE, sep = ",", na.strings = "#DIV/0!", nrows = 3,
  colClasses = c("NULL", "character", rep("NULL",3), "character", rep("numeric", 4), #6
                  rep("NULL", 3), 
                   rep("character",147))
)

PMLfull <- read.table(
  "C:\\Users\\steve\\Desktop\\Coursera\\MachLearn\\pml-training.csv",
  header = TRUE, sep = ",", na.strings = "#DIV/0!", 
)
PMLuseful <- PMLfull[, c(2, 6:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:160 )]
PMLuseful$classe <- as.factor(PMLuseful$classe)
PMLuseful$user_name <- as.factor(PMLuseful$user_name)

PMLtest <- read.table(
  "C:\\Users\\steve\\Desktop\\Coursera\\MachLearn\\pml-testing.csv",
  header = TRUE, sep = ",")[, c(2, 6:11, 37:49, 60:68, 84:86, 102, 113:124, 140, 151:160 )]
PMLtest$user_name <- as.factor(PMLtest$user_name)
PMLtest$problem_id

one.pred <- predict(one.mdl, PMLtest)
plot(one.pred)

PML <- PMLuseful[, -c(2,3)]
two.mdl <-train(classe ~ ., PML, method = "rf")
rfVarList <- varImp(two.mdl)
two.pred <- predict(two.mdl, PMLtest[, -c(2,3)])
plot(two.pred)
two.pred


knn.mdl <- knn3(classe ~ ., data = PML, k=5)
knn.pred <- predict(knn.mdl, PMLtest[, -c(2,3)])
knn.pred

