#Put your data in and name it something simple
h1<-water
head(h1)
summary(h1)

#Begin
for (i in 1:3){
  # Set an arbitrary seed to reorganize your data
  seed<-69
  set.seed(seed)
  seedling<-seed+1
  accuracies<-c()
  
  # This will partition 75% of the data points into a training set and 25% into the test set
  trainIndex <- createDataPartition(y=training$classe, p=0.75, list=FALSE)
  trainingSet<- training[trainIndex,]
  testingSet<- training[-trainIndex,]
  modelFit <- randomForest(classe ~., data = trainingSet)
  prediction <- predict(modelFit, testingSet)
  testingSet$rightPred <- prediction == testingSet$classe
  t<-table(prediction, testingSet$classe)
  print(t)
  accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
  accuracies <- c(accuracies,accuracy)
  print(accuracy)
}

h1id<-sample(2,nrow(h1),prob=c(0.7,0.3),replace=TRUE)
h1train<-h1[h1id==1,]
h1test<-h1[h1id==2,]

# Optimized value of mtry 
h1mtry<-tuneRF(h1train,h1train$Water,stepFactor=1.2,improve=0.01,trace=T,plot=T)
# Error rate = 0 is the best, increasing ER is bad

# If you receive the error "NA not permitted in predictors" the following will find the column then row, respectively
sapply(h1train, function(x) sum(is.na(x)))
which(is.na(h1train$Water))

# Two types of randomForests exist, Classification (binomial), and regression (multinomial)
h1forest<-randomForest(Water~.,data=h1train)
h1forest
# Output: when x and y axes agree, it indicates a correct prediction, when they disagree it means it was incorrect
# OOB estimate of error is calculated from the correct and incorrect predictions

importance(h1forest)

varImpPlot(h1forest)

# Now time to use your training model

h12<-predict(h1forest,newdata=h1test,type="response")
h12

# When comparing Yes/No, confusionMatrix shows you the discrepancies and efficacy of your model
confusionMatrix(table(predictedh1,AAandCPh1test$Dry))

# To create a feature plot
Tophits<-c("RH","Exposure","Recovery","BF")
featurePlot(x=h1[,Tophits],y=h1$Water,plot="pairs")

# Focus on some individual plots that appear to have interesting trends
qplot(BF,Recovery,color=Water,data=h1)+
  geom_jitter()

qplot(BF,Exposure,color=Water,data=h1)+
  geom_jitter()

qplot(BF,RH,color=Water,data=h1)+
  geom_jitter()

qplot(RH,Exposure,color=Water,data=h1)+
  geom_jitter()
# ...etc

# Histograms help view normal distributions
par(mfrow=c(1,2))
hist(h1$Water,main="Water")


modelFit <- randomForest(classe ~., data = training)
nrow(testing)
## [1] 20
prediction <- predict(modelFit, testing)
prediction
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E

importants <- c()
for (i in 1:4){
  x <- sort(modelFit$importance[,i], dec = TRUE)
  y=modelFit$importance[,i]
  plot(y, main = paste("Measure", i), ylab="Importance")
  abline(h=0.075,col="green",lty=3, lwd = 2)
  importants <- c(importants, names(y[y>.075]))
  print(names(y[y>.075]))
}

sort(importants, dec = TRUE)

importants<-unique(importants)
importants

#Let's fit a model using only the "important" predictors and guesstimate its accuracy by trying it out on the test set


smallerModelFit <-randomForest(classe ~ roll_belt + pitch_belt + yaw_belt +
                                 magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z
                               + roll_forearm + pitch_forearm + roll_dumbbell +
                                 accel_dumbbell_y + accel_forearm_x,
                               data = trainingSet, importance = TRUE)
print(smallerModelFit)
smallerModelFit <-randomForest(classe ~ roll_belt + pitch_belt + yaw_belt + magnet_dumbbell_x + magnet_dumbbell_y + 
                                 magnet_dumbbell_z
                               + roll_forearm + pitch_forearm + roll_dumbbell +
                                 accel_dumbbell_y + accel_forearm_x,
                               data = trainingSet, importance = TRUE)
print(smallerModelFit)

smallerPrediction <- predict(smallerModelFit, testingSet)
testingSet$smallerRightPred <- smallerPrediction == testingSet$classe
t<-table(smallerPrediction, testingSet$classe)
t

accuracy <- sum(testingSet$smallerRightPred)/nrow(testingSet)
accuracy

set.seed(1976)
treeModel <- rpart(classe ~ roll_belt + pitch_belt + yaw_belt +magnet_dumbbell_x
                   +magnet_dumbbell_y+magnet_dumbbell_z + roll_forearm +
                     pitch_forearm + roll_dumbbell + accel_dumbbell_y + accel_forearm_x,
                   method="class", data=trainingSet)

printcp(treeModel) # display the results

plotcp(treeModel) # visualize cross-validation results

# plot tree
plot(treeModel, uniform=TRUE,
     main="Classification Tree for Weight Lifting Exercises Dataset")
text(treeModel, use.n=TRUE, all=TRUE, cex=.45)

treePredValues <- predict(treeModel, testingSet)
treePrediction <- c()
for (k in 1:nrow(testingSet)) {
  treePrediction <- c(treePrediction, names(which.max(treePredValues[k,])))
}
testingSet$treeRightPred <-treePrediction == testingSet$classe
t<-table(treePrediction, testingSet$classe)
t

accuracy <- sum(testingSet$treeRightPred)/nrow(testingSet)
accuracy


data(iris)
str(iris)
table(iris$Species)
head(iris)

# Should shuffle first
set.seed(12345)
g<-runif(nrow(iris))
irisr<-iris[order(g),]
str(irisr)

# This is for creating your training set
m3<- rpart(Species ~ ., data=irisr[1:100,], method="class")
m3

# Draw the tree
rpart.plot(m3)

summary(m3)

# Time to test the training set
p3<-predict(m3,irisr[101:150,],type="class")
table(irisr[101:150,5],predicted=p3)
# Look diagonally to see the correct matches
