# Decision Trees

# Importing the dataset
mydata <- read.csv("C:/Users/Noomit yagna/Downloads/Microsoft.SkypeApp_kzf8qxf38zg5c!App/All/german_credit.csv")
str(mydata)
names(mydata)
dim(mydata)

# converting thee output variable as factor for classification problem
mydata$Creditability <- as.factor(mydata$Creditability)

# Split the data into train and test
library(caret) 
indep <- createDataPartition(mydata$Creditability, p=0.8, list= F)
train <- mydata[indep, ]
test <- mydata[-indep, ]

# Build a Decision tree
library(rpart)
mytree <- rpart(Creditability~., data= train, method= "class")
plot(mytree)
text(mytree)

# Prediction

predicted <- predict(mytree, type= "class")
actual <- train$Creditability

# check for accuracy
mean(actual==predicted)

# Improve the model performance by applying the hyperparameter as cp_value(complexity parameter)
printcp(mytree)
mytree$cptable
which.min(mytree$cptable [, 'xerror'])
bestcp <- mytree$cptable[which.min(mytree$cptable[,"xerror"]),"CP"]
prunedtree <- prune(mytree, cp= bestcp)
plot(prunedtree)
text(prunedtree)

predict <- predict(prunedtree, type= "class")
mean(actual==predict)

library(Metrics)
auc(actual, predict)
f1(actual, predict)
ce(actual, predict)
precision(actual, predicted)
recall(actual, predicted)
cm <- table(actual, predict)
cm<- t(cm)
precision <- cm[2,2]/sum(cm[2,])
recall <- cm[2,2]/sum(cm[,2])
f_measure <- 2*precision*recall/precision+recall

