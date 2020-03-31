library(caTools)
library(rpart)
library(e1071)
library(dplyr)
library(rpart)
library(ggplot2)

#Import data and check first few rows
data = read.csv('heart.csv')
head(data)

#Checking the dimensions and structure of the dataset
dim(data)
str(data)
summary(data)

#DATA PROCESSING
#Converting to Factors

heart = data %>%
  mutate(sex = as.factor(sex),
         cp = as.factor(cp),
         fbs = as.factor(fbs),
         restecg = as.factor(restecg),
         exang = as.factor(exang),
         slope = as.factor(slope),
         ca = as.factor(ca),
         thal = as.factor(thal),
         target = as.factor(target))

str(heart)

#DATA PARTITIONING
# I usually like the sample.split function from the caTools packages 
# as it does the sampling and spliting in a pretty simple and easy way

split = sample.split(heart$target, SplitRatio = 0.8)
train_set = subset(heart, split == TRUE)
test_set = subset(heart, split == FALSE)

#Visualizing the training set
ggplot(data = train_set, aes(x = ï..age, y= thalach, color = target)) +
  geom_point()+
  scale_color_manual(values = c('red', 'black'))

#1. Training a Classification Tree model
classifier = rpart(target ~ ., data = train_set,
                   method = 'class')

#Predicting training set with classification tree model
pred1 = predict(classifier, train_set, type = 'class')
mean(train_set$target == pred1)

#Predicting test_set and measuring its accuracy
pred = predict(classifier, test_set, type = 'class')
mean(test_set$target == pred)

#2. Training the data with Support Vector Machine
svm_model = svm(target~., train_set,
                type = 'C-classification',
                Kernel = 'linear')
#Predicting and measuring the accuracy of svm with linear kernel on training set
pred_svm = predict(svm_model, train_set)
mean(pred_svm == train_set$target)

#Predicting and measuring the accuracy of svm with linear kernel on test set
pred_test_svm = predict(svm_model, test_set)
mean(pred_test_svm == test_set$target)

#Training on a polynomial kernel of degree 2
k2_model = svm(target ~., data = train_set, type = 'C-classification', kernel = 'polynomial', degree = 2)
k2_pred = predict(k2_model, train_set)
mean(k2_pred == train_set$target)

#training on an RBF kernel
tune = tune.svm(target ~., data = train_set, type = 'C-classification', kernel = 'radial',
                          cost = 10^(-1/2), gamma = c(0.1,1,10), coef0 = c(0.1,1,10))

rbf_model_tune = svm(target ~., data = train_set, type = 'C-classification', kernel = 'radial',
                     cost = tune$best.parameters$cost, gamma = tune$best.parameters$gamma, 
                     coef0  = tune$best.parameters$coef0)


rbf_pred = predict(rbf_model_tune, train_set)
mean(rbf_pred == train_set$target)

test_pred = predict(rbf_model_tune, test_set)
mean(test_pred == test_set$target)
