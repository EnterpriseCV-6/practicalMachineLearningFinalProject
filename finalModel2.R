#Final Model Build
#Sets Directory, then loads data and required packages
setwd("~/Desktop/MichaelStuff/courseraRProgramingDirectory/machineLearningFinal")
exerciseData <- read.csv("train.csv")
finalTestData <- read.csv("test.csv")
library(caret)

#Sets seed, so project can be reproduced
set.seed(1993)

#Breaks data into testing and training set.
inTrain <- createDataPartition(y = exerciseData$classe, p = .8,list = FALSE)
training <- exerciseData[inTrain,]
testing <- exerciseData[-inTrain,]

#Removes NA data
goodCol <- 1
for(i in 2:160)
{
        if(sum(is.na(training[,i])) == 0)
        {
                goodCol <- c(goodCol,i)
        }
}
training <- training[,goodCol]
testing <- testing[,goodCol]
finalTestData <- finalTestData[,goodCol]

#Removes data index and person's name as these things do not help with prediction
training <- training[c(-1,-2)]
testing <- testing[c(-1,-2)]
finalTestData <- finalTestData[c(-1,-2)]

#Removes near-zero values, but insures the classe is kept
nsv <- nearZeroVar(training)
training <- training[-nsv]
testing <- testing[-nsv]
finalTestData <- finalTestData[-nsv]
numericValue <- sapply(training, is.numeric)
testing <- testing[c(numericValue,dim(training)[2])]

#creates final model
finalTestData <- finalTestData[c(numericValue,dim(training)[2])]
training <- training[c(numericValue,dim(training)[2])]
modelFit <- train(training$classe ~., method = "gbm", preProcess = "pca", data=training)
