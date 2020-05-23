# load the data
mh_data <- read.csv(file.choose(), header=T)
str(mh_data)
names(mh_data) <- c("vendorname", "ModelName", "MYCT", "MMIN", "MMAX","CACH", "CHMIN", "CHMAX", "PRP","ERP")
# data preparation
data= subset(mh_data, select= -c("vendorname", "ModelName"))
data=mh_data[-1, ]
data1=mh_data[-2, ]
str(data2)
names(data2)
# numeric conversion
data2$MYCT <- as.numeric(data2$MYCT)
data2$MMIN <- as.numeric(data2$MMIN) 
data2$MMAX <- as.numeric(data2$MMAX)
data2$CACH <- as.numeric(data2$CACH)
data2$CHMIN <- as.numeric(data2$CHMIN)
data2$CHMAX <- as.numeric(data2$CHMAX)
data2$PRP <- as.numeric(data2$PRP)
str(data2)
names(data2)
cor<- cor(data2)
plot(cor)

# data partition for traing and testing
set.seed(12345)
data_splt <- sample(1:nrow(data2), 0.8*nrow(data2))
train_data <- data2[data_splt, ]
test_data <- data2[-data_splt,]
nrow(train_data)
nrow(test_data)

# model building MLR
mlr <- lm(ERP~., data= train_data)
summary(mlr)
# residuals
errors <- residuals(mlr)
mean(errors)
# shapiro test for residual normality distribution
shapiro.test(errors)
par(mfrow=c(2,2))
plot(mlr)

# bp test for homoscadasticity
library(lmtest)
bptest(mlr)

# multicolinearity checking
library(car)
vif(mlr)

# PRP is multicolinear
data_mlr <- data2[, -7]

# check for outliers
outlierTest(mlr)

# drop the outliers
data_mlrfn <- data_mlr[-c(9,156,198), ]

# model after multicolinearity and outlier removal
# Build a model for MLR
ml_hw_model <- lm(ERP~., data= data_mlrfn)
summary(ml_hw_model)

# assumptions:
--------------
# residuals
errs <- residuals(ml_hw_model)
mean(errs)
# shapiro test for residual normality distribution
shapiro.test(errs)
par(mfrow=c(2,2))
plot(ml_hw_model)

# bp test for homoscadasticity
library(lmtest)
bptest(ml_hw_model)


# stepwise mlr
library(MASS)
hw_mdl <- stepAIC(ml_hw_model)
summary(hw_mdl)

cbind(summary(ml_hw_model), summary(hw_mdl))

# predictions
trn_pred <- predict(hw_mdl, train_data) 
ts_pred <- predict(hw_mdl, test_data)

# model evaluation
rmse <- function(x,y){
  error= x-y
  error2= error^2
  merror2= mean(error2)
  rmse= sqrt(merror2)
  return (rmse)
  }

mape <- function(x,y){
  error = x-y
  aberror = abs(error)
  percentage = aberror/x
  mape = mean(percentage)
  return (mape)
  }
 # RMSE
rmse(train_data$ERP, trn_pred)
rmse(test_data$ERP, ts_pred)
# MAPE
mape(train_data$ERP, trn_pred)
mape(test_data$ERP, ts_pred)
# Accuracy
1-mape(trn_pred, train_data$ERP)
1-mape(ts_pred, test_data$ERP)
# correlation
cor(trn_pred, train_data$ERP)
cor(ts_pred, test_data$ERP)
