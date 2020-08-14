# Projectfianl
Machine Learning Project at Coursera
rm(list = ls())
library(caret)
library(doParallel)
set.seed(21335210)
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv','pml-training.csv')
download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv','pml-test.csv')
trainingSrc = read.csv('pml-training.csv' , na.strings=c ("NA","#DIV/0!",""))
testSrc = read.csv('pml-test.csv' , na.strings=c("NA", "#DIV/0!",""))

goodVars = which((colSums(!is.na(trainingSrc)) >= 0.6*nrow(trainingSrc)))
trainingSrc = trainingSrc[,goodVars]
testSrc = testSrc[,goodVars]

testSrc = testSrc[-ncol(testSrc)]
testSrc$new_window = factor(testSrc$new_window, levels=c("no","yes"))

trainingSrc = trainingSrc[,-c(1,5)]
testSrc = testSrc[,-c(1,5)]

inTraining = createDataPartition(trainingSrc$classe, p = 0.6, list = FALSE)
training = trainingSrc[inTraining,]
testing = trainingSrc[-inTraining,]

class = training$classe
data = training[-ncol(training)]

registerDoParallel()
rf = train(data, class, method="parRF", tuneGrid=data.frame(mtry=3), trControl=trainControl(method="none"))

rf

plot(varImp(rf))

testingPredictions = predict(rf, newdata=testing)
confMatrix = confusionMatrix(testingPredictions, testing$classe)
confMatrix

confMatrix$overall[1]

# pm1_write_files = function(x){
#   n = length(x)
#   for (i in 1:n){
#     filename = paste0("problem_id_", i, ".txt")
#     write.table(x[i], file=filename, quote=FALSE, row.names = FALSE, col.names = FALSE)
#   }
# }
# 
# answers = predict(rf, testSrc)
# pm1_write_files(answers)
