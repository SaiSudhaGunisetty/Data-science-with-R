# k-NEAREST NEIGHBOUR CLASSIFIER IN R
=====================================
  
# Importing the data into r
heart <- read.table("D:/GSSdata science/Datascience with R/data science codes from 
                    srikanth sir/heart.dat", quote="\"", comment.char="")
names(heart) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL", "SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR", "THAL", "OUTPUT")

#Understanding the data
str(heart)
summary(heart)
boxplot(heart)
head(heart,10)
tail(heart,2)

#Knowing the proportion of distribution of output variable
table(heart$OUTPUT)
round(prop.table(table(heart$OUTPUT))*100, digits = 2)

#output variable as factor
heart$OUTPUT <- factor(heart$OUTPUT, levels = c("0","1"),
                         labels = c("negative","positive"))

# bringing the all variables into one scale because scaling 
#is important in KNN classifier
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#checking the normalization function
x<- c(1:10)
normalize(x)

#Scaling fuction applying on data
ncol(heart)
# Labels provided for the existing categories
heart[[14]] <- c("Negative", "Positive")
heart_nor <- as.data.frame(lapply(heart[1:13], normalize))

# function applied or not confirmed using summary
summary(heart_nor)

# splitting the data into train and test
ind <- sample(1:nrow(heart_nor), 0.8*nrow(heart_nor))
train_knn <- heart_nor[ind, ]
nrow(train_knn)
test_knn <- heart_nor[-ind, ]
nrow(test_knn)

# Actuals used here as class labels
train_knn_actuals <-heart[ind, 14]
test_knn_actuals <- heart[-ind,  14]

# Model building with KNN classifier
library(class)
k=sqrt(nrow(train_knn))
knn_model <- knn(train_knn, test_knn, train_knn_actuals, k=15)

# Model checking with K-value with looping 
Accur = rep(0,27)
for (i in 1:27) {
  classification = knn.cv(train_knn, train_knn_actuals, i)
  Accur[i] = sum(classification == train_knn_actuals)/length(train_knn_actuals)
}
which.max(Accur)

# Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = test_knn_actuals, y = knn_model,prop.chisq=FALSE)
mean(test_knn_actuals == knn_model)



