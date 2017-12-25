############################ SVM Letter Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to correctly identify the digit (between 0-9) written in an image. 

#####################################################################################

# 2. Data Understanding: 
# Number of Instances: 60,000
# Number of Attributes: 785 

#3. Data Preparation: 


#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)


#Loading Data

mnist_data <- read.csv("mnist_train.csv",header=F)
mnist_test <- read.csv("mnist_test.csv",header=F)

#Understanding Dimensions

dim(mnist_data)

#Structure of the dataset

str(mnist_data)

#checking first few rows

head(mnist_data)

#Exploring the data

summary(mnist_data)

#checking missing value - no missing values

sapply(mnist_data, function(x) sum(is.na(x)))

# Changing the first column name in train and test datasets
colnames(mnist_data)[1] <- "Digit"
colnames(mnist_test)[1] <- "Digit"

#Making our target class to factor

mnist_data$Digit<-factor(mnist_data$Digit)

# Creating a new sample data - by taking 1/30 ratio - Sample Size - 1986 obs

mnist_sample = sample.split( mnist_data, SplitRatio = 1/30, group = NULL )
mnist_train = mnist_data[mnist_sample, ]

#4.Constructing Model

#4.1 Using Linear Kernel
Model_linear <- ksvm(Digit~ ., data = mnist_train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, mnist_test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,mnist_test$Digit)

#Accuracy : 0.9054         
#95% CI : (0.8995, 0.9111)
#No Information Rate : 0.1135         
#P-Value [Acc > NIR] : < 2.2e-16      


#4.2 Using RBF Kernel
Model_RBF <- ksvm(Digit~ ., data = mnist_train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_test$Digit)

#Accuracy : 0.9349          
#95% CI : (0.9299, 0.9397)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16 

############   5 Hyperparameter tuning and Cross Validation #####################

# Using the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

set.seed(7)

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))


fit.svm <- train(Digit~., data=mnist_train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)


print(fit.svm)

plot(fit.svm)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm, mnist_test)
confusionMatrix(evaluate_non_linear, mnist_test$Digit)

#Accuracy : 0.101          
#95% CI : (0.0952, 0.1071)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : 1


# The accuracy for non-linear svm model is just around 10% , which proves that this
# is a linear data. So it would be best to consider Linear model for this data which
# is giving accuracy around 90%
