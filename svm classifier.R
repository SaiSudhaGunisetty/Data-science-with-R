library(kernlab)
library(e1071)
library(ggplot2)

x<- matrix(rnorm(20*2), ncol=2)
y <- c(rep(0,10), rep(1,10))
x[y==1, ] <- x[y==1, ]+3/2
df <- data.frame(x=x, y=as.factor(y))

# ggplot(data=df, aes(x= x.2, y= x.1, color= y shape= y))+geom_point(size= 2)

svmfit <- svm(y~., data = df, kernel= "radial")
plot(svmfit, df)
print(svmfit)
summary(svmfit)


############SVM Classifier###########
library(caret)
library(kernlab)
# library(ROCR)

#Importinng the data
data("segmentationData")
str(segmentationData)


# checking the dimensions
dim(segmentationData)

# checking the  output variable whether it is balanced or imbalanced dataset
table(segmentationData$Class)
round(prop.table(table(segmentationData$Class))*100, digits=0)

 
round(table(segmentationData$Class)/nrow(segmentationData)*100, digits= 0)

# data partitioning
ind <- createDataPartition(segmentationData$Class, p=0.7, list = F)
train <- segmentationData[ind, ]
test <- segmentationData[-ind, ]

# remove meaninngless variables

names(train)
trainx <- train[, 3:61] 
testx <- test[, 3:61]

# model building forr svm classifier
library(e1071)
svm <- svm(Class~. , data =trainx, kernel = "linear", probability=TRUE)
summary(svm)
print(svm)
plot(svm)

# remove output variable
pred <- predict(svm, testx[, -1])
predtr <- predict(svm, trainx[, -1])

# Accuracy
mean ( pred==testx$Class)
mean(predtr==trainx$Class)


library(e1071)
svm <- svm(Class~. , data =trainx, kernel = "sigmoid", probability=TRUE)
summary(svm)
print(svm)
plot(svm)

# remove output variable
pred <- predict(svm, testx[, -1])
predtr <- predict(svm, trainx[, -1])

# Accuracy
mean ( pred==testx$Class)
mean(predtr==trainx$Class)

library(e1071)
svm <- svm(Class~. , data =trainx, kernel = "poly", probability=TRUE)
summary(svm)
print(svm)
plot(svm)

# remove output variable
pred <- predict(svm, testx[, -1])
predtr <- predict(svm, trainx[, -1])

# Accuracy
mean ( pred==testx$Class)
mean(predtr==trainx$Class)


library(e1071)
svm <- svm(Class~. , data =trainx, kernel = "radial", probability=TRUE)
summary(svm)
print(svm)
plot(svm)

# remove output variable
pred <- predict(svm, testx[, -1])
predtr <- predict(svm, trainx[, -1])

# Accuracy
mean ( pred==testx$Class)
mean(predtr==trainx$Class)