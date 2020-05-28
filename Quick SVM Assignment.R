##########################################
#IST 387
#Emily Korzendorfer


dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!


library(ggplot2)
install.packages("kernlab")
library(kernlab)
install.packages("caret")
library(caret)

diamondsDF <- diamonds
goodDiamonds <- diamonds[diamonds$cut == "Ideal" | diamonds$cut == "Premium",]
as.numeric(goodDiamonds$clarity, goodDiamonds$color)
as.factor(as.character(goodDiamonds$cut))

View(goodDiamonds)
table(goodDiamonds$cut)
#Premium = 13791
#Ideal = 21551
randomIndex <- sample(1:dim(goodDiamonds)[1])
range(randomIndex)
#1 35242
cutdimond <- floor(2 * dim(goodDiamonds)[1]/3)
cutdimond
#23561

trainData <- goodDiamonds[randomIndex[1:cutdimond],]
testData <- goodDiamonds[randomIndex[(cutdimond+1):dim(goodDiamonds)[1]],]
dim(trainData) #23561 10
dim(testData)  #11781 10
svmOutput <- ksvm(cut ~., data=trainData, kernel = "rbfdot", kpar="automatic", C=5, cross= , prob.model=TRUE)
svmOutput
#ksvmm cut ~ command tells us what dataset we are using
#data = train data means we are using the train data dataset
#c = 5 illustrates the cost of cosntraints violation (default = 1)
#if you want a large seperation margin, you could have many mistakes, 
#if you give a smaller value to C than you get fewer mistakes at the cost of a lower margin 
#Therefore we set c = 5 to prevent mistakes at the cost of a smaller margin
#Cross means that if a integer value k>0 is specified, a k-fold cross validation on the training data is performed to assess the quality of the model: the accuracy rate for classification and the Mean Squared Error for regression
svmPred <- predict(svmOutput, testData)

str(svmPred)
head(svmPred)
#good fair fair fair fair good
matrix <- data.frame(testData[,10], .default(svmPred, 1, )
table(matrix)