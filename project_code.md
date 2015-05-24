library("randomForest")

## Read in data
test = read.csv("pml-testing.csv")
pml.training = read.csv("pml-testing.csv")
test = pml.testing
train2 = pml.training

## Change blanks to NA, then remove columns with large # of NAs
train2[pml.training==""]=NA
train = train2[,colSums(is.na(train2))<.8*nrow(train2)]

## Fit using a random forest
RFfit = randomForest(classe~., data=train, ntree=500)
accuracy = sum(RFfit$predicted==train$classe)/nrow(train)
print(accuracy)

## Plot feature importance and error
varImpPlot(RFfit,main="Variable Importance as Calculated by randomForest",pch=20,col="blue")
plot(RFfit,main="Error as a Function of Trees")

## Cross validation
rfcv.fit = rfcv(train[,1:53],train[,54],cv.fold = 4)
rfcv.fit$error.cv

predict(RFfit,test)
