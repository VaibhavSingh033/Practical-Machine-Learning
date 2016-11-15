f1 <- function()
{  
    library(caret)
    library(randomForest)
    library(rpart)
    train_in <- read.csv("pml-training.csv")
    validation <- read.csv('pml-testing.csv', header=T)
    set.seed(127)
    training_sample <- createDataPartition(y=train_in$classe, p=0.7, list=FALSE)
    training <- train_in[training_sample, ]
    testing <- train_in[-training_sample, ]
    dim(training)
    dim(testing)
    NZV <- nearZeroVar(training)
    training <- training[, -NZV]
    testing  <- testing[, -NZV]
    dim(training)
    dim(testing)
    #Now removing variables that are mostly NA
    AllNA    <- sapply(training, function(x) mean(is.na(x))) > 0.95
    training <- training[, AllNA==FALSE]
    testing  <- testing[, AllNA==FALSE]
    dim(training)
    dim(testing)
    controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
    modFitRandForest <- train(classe ~ ., data=training, method="rf",trControl=controlRF)
    predictRandForest <- predict(modFitRandForest, newdata=testing)
    confMatRandForest <- confusionMatrix(predictRandForest, TestSet$classe)
    confMatRandForest
}
