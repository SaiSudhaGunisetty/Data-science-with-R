# loading the data
clg_data <- read.csv(file.choose(), header=T)
str(clg_data)
names(clg_data)
nrow(clg_data)
# convert the data to factors
clg_data$admit <- as.factor(clg_data$admit)
clg_data$rank <- as.factor(clg_data$rank)
str(clg_data)
names(clg_data)
View(clg_data)

# cross tabulation
xtabs(~., data = clg_data)
xtabs(~admit+rank, data = clg_data)

# data partitioning
set.seed(789654123)
admit <- sample(2, nrow(clg_data), replace=T, prob= c(0.8,0.2))
tr <- clg_data[admit==1,]
A=nrow(tr)
ts <- clg_data[admit==2,]
B=nrow(ts)
A+B

# build a model
clg_model <- glm(admit~., data= tr, family = binomial("logit"))
summary(clg_model)

# drop the least significant variables
clg_mod <- glm(admit~gre+rank, data= tr, family = binomial("logit"))
summary(clg_mod)

# multicolinearity
library(car)
vif(clg_model)

# stepwise logistic regression
library(MASS)
step_model <- stepAIC(clg_model)
summary(step_model)
cbind(summary(clg_model),summary(clg_mod),summary(step_model))

# predictions
# train predictions
tr_p1 <- predict(clg_mod, newdata = tr, type = "response")
tr_class_predict <- ifelse(tr_p1>0.5, 1, 0)
# test predictions
ts_p2 <- predict(clg_mod, newdata = ts, type = "response")
ts_class_predict <- ifelse(ts_p2>0.5, 1, 0)

# Model evaluation with classification metrics
========================
  
# confusion metrics
tr_tab <- table(predicted= tr_class_predict, actual=tr$admit)
ts_tab <- table(predicted=ts_class_predict, actual= ts$admit)

#Precision
(precision_tr <- tr_tab[2,2]/sum(tr_tab[2,]))
(precision_ts <- ts_tab[2,2]/sum(ts_tab[2,]))

# Recall
(recall_tr <- tr_tab[2,2]/sum(tr_tab[,2]))
(recall_ts <- ts_tab[2,2]/sum(ts_tab[,2]))

# F-Score
(f = 2 * precision_tr * recall_tr / (precision_tr + recall_tr))
(f = 2 * precision_ts * recall_ts / (precision_ts + recall_ts))

# Accuracy
mean(tr_class_predict==tr$admit)
mean(ts_class_predict==ts$admit)

# Goodness of fit test
with(clg_mod, pchisq(null.deviance-deviance, 
                     df.null-df.residual, lower.tail = F))

#ROC Curve

library(ROCR)
# For train
tr_pred = predict(clg_mod, newdata=tr, type="response")
pred <- prediction(tr_pred, tr$admit)
perf <- performance(pred, measure = "prec", x.measure = "rec")
par(mfrow = c(2,2))
# Plot Precision and Recall
plot(perf, main = "Precision-Recall Curve for col admisn on train", lwd = 2)
# Plot the ROC curve
perf_val2 <- performance(pred, "tpr", "fpr")
plot(perf_val2, col = "green", lwd = 1.5)
abline(a=0,b=1,lwd=2,lty=2)

# For test
ts_pred = predict(clg_mod, newdata=ts, type="response")
pred_ts <- prediction(ts_pred, ts$admit)
perf_ts <- performance(pred_ts, measure = "prec", x.measure = "rec")

# Plot Precision and Recall
plot(perf_ts, main = "Precision-Recall Curve for col admisn on test", lwd = 2)
# Plot the ROC curve
perf_val3 <- performance(pred_ts, "tpr", "fpr")
plot(perf_val3, col = "red", lwd = 2)
abline(a=0,b=1,lwd=2,lty=2)