# SIMPLE LINEAR REGRESSION WITH R
==================================
# importing the data 
machine <- read.csv("C:/Users/INDIA/Downloads/machine.data", header=FALSE)

# Drop the character columns which has no importance 
machine <- machine[, c(-1,-2)]

# Data understanding
machine
names(machine) <- c("MYCT", "MMIN", "MMAX","CACH", "CHMIN", "CHMAX", "PRP","ERP")
str(machine)

# Data summerization
summary(machine)

# Data understanding through box plot for finding any outliers in the dataset
boxplot(machine)

# Datatype conversions according to our requirements
machine_dt <- as.data.frame(lapply(machine[1:8], as.numeric))

# checking the structure of dataset
str(machine_dt)

# Assumption=I
===============
# input and output variables are linearly related or not checking through 
# correlation (it explains about the relationship and strength of the data).
cor(machine_dt$ERP, machine_dt$MMAX)

# visualize the same correlation with scatter plot
plot(machine)

# DATA PARTITIONING AND MODEL BUILDING
=======================================
  
# split the data into training and testing
model_dt <- sample(1:nrow(machine_dt), 0.8*nrow(machine_dt))
train_dt <- machine_dt[model_dt, ]
test_dt <- machine_dt[-model_dt, ]

# model building
slr_model <- lm(ERP~PRP, data= train_dt)
summary(slr_model)

# Assumption=II
================
#here we will check whether the residuals are close to zero or not
# residuals
error <- residuals(slr_model)
mean(error)

# Assumption=III
================
  
#Residuals are normally distributed or not check by using shapiro test and Q-Q plot
shapiro.test(error) 
par(mfrow=c(2,2))
plot(slr_model)

# Assumption=IV
================
# Residuals are homoscadastic orhetero scadastic check by using bptest
install.packages("lmtest")
library(lmtest)
bptest(slr_model)

# Assumption=V
================
# Outliers in the data are detected by using cooksdistance
cookD <- cooks.distance(slr_model)
plot(cookD)
influential_obs <- as.numeric(names(cookD)[(cookD > (15/nrow(train_dt)))])
train_dt_f <- train_dt[- influential_obs]

final_model <- lm(ERP~PRP, data=train_dt_f)
summary(final_model)
plot(final_model)

# PREDICTIONS
==============
tr_pred <- predict(final_model, train_dt_f)
ts_pred <- predict(final_model, test_dt)

# MODEL EVALUATION
===================
RMSE <- function(x,y){
  error= x-y
  error2=error^2
  merror2= mean(error2)
  RMSE= sqrt(merror2)
  return(RMSE)
}  

MAPE <- function(x,y){
  error= x-y
  abserror= abs(error)
  percentage= abserror/x
  MAPE= mean(percentage)
  return(MAPE)
}

RMSE(train_dt_f$ERP, tr_pred)
RMSE(test_dt$ERP, ts_pred)

MAPE(train_dt_f$ERP, tr_pred)
MAPE(test_dt$ERP, ts_pred)

1-MAPE(train_dt_f$ERP, tr_pred)
1-MAPE(test_dt$ERP, ts_pred)

cor(train_dt_f$ERP, tr_pred)
cor(test_dt$ERP, ts_pred)