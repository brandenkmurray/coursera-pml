# Activity Classification Project
Branden Murray  

---------------


######Project Description
*Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).*  


*Data*   

*The training data for this project are available here:* 

*https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv*

*The test data are available here:*  

*https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv*  

*The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.*  

-----------------

First, we will load the necessary packages and data into R. And then check the summary of the training set to get an idea of what the data looks like.

```r
# Load packages
library(caret)
library(randomForest)
set.seed(999)
setwd("C:/Users/Branden/Documents/coursera/ml")
# Load the datasets
pml_train <- read.csv("./pml-training.csv", na.strings=c(NA,"#DIV/0!",""))
pml_test <- read.csv("./pml-testing.csv", na.strings=c(NA,"#DIV/0!",""))
summary(pml_train)
```

A look at the summary reveals that many features have a large number of NA values. A loop is created to determine which columns have more than 90% of NA's and those columns are removed from the training and test sets. Also removed are the first 7 columns which contain the index, names of the subjects, and timestamps. These features will not be useful in predicting future subjects.  


```r
# Determine columns that are missing a lot of data
cols <- NULL
for (i in 1:dim(pml_train)[2]){
  nacount <- sum(is.na(pml_train[,i]))
  collen <- length(pml_train[,i])
  cols[i] <-  ifelse(nacount/collen > .9, i, 0)
}

# Remove columns with a lot of missing data and 7 columns containing index, usernames, and timestamps
pml_tr_reduced <- pml_train[,-c(1:7,cols)]
pml_test_reduced <- pml_test[,-c(1:7,cols)]
```


The training set is the split further into a sub training and sub test set so that we can test our model.  


```r
# Split the training set into a training & test set
inTrain <- createDataPartition(pml_tr_reduced$classe, p=.8, list=FALSE)
pml_sub_train <- pml_tr_reduced[inTrain,]
pml_sub_test <- pml_tr_reduced[-inTrain,]
```

The controls are set to perform a 5 fold cross-validation. A random forest model is then trained on the pml_sub_train dataset.  


```r
# Create controls for cross validation
ctrl <- trainControl(method="cv", number=5)
# Use the train function to do cross-validation and tune parameters
rfFit.cv <- train(classe ~ ., data=pml_sub_train, method="rf", trControl=ctrl)
print(rfFit.cv, digits=4)
```

```
## Random Forest 
## 
## 15699 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## 
## Summary of sample sizes: 12560, 12558, 12560, 12559, 12559 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa   Accuracy SD  Kappa SD
##    2    0.9923    0.9902  0.001677     0.002121
##   27    0.9927    0.9908  0.001644     0.002080
##   52    0.9855    0.9817  0.002925     0.003699
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

The results of the cross-validation show that sampling 27 variables at each split will produce the best accuracy. The data is plotted below:


![plot of chunk unnamed-chunk-5](./activityproject_files/figure-html/unnamed-chunk-5.png) 


The cross-validated accuracy of the model is 99.27%. The model is the used to predict values for the sub test set. A confusion matrix is then created and the accuracy and kappa values are extracted.


```r
# Use the result of the cross-validation fit to predict classes for the test set
test_pred <- predict(rfFit.cv, pml_sub_test)
# Create a confusion matrix
confusionMatrix(test_pred, pml_sub_test$classe)$overall[1:2]
```

```
## Accuracy    Kappa 
##   0.9959   0.9948
```

An accuracy value of 99.59% and a Kappa value of 99.48% indicate that that the model predicted the test set extremely well.  

A model is then built on the entire training set, which is then used to predict the 20 values in the test set. A function borrowed from the course submission page is then used to create output files to be used to submit the predictions for grading.


```r
# Fit a random forest model on the entire reduced training set 
rfFit <- randomForest(classe ~ ., data=pml_tr_reduced, mtry=rfFit.cv$bestTune$mtry, importance=TRUE)
pml_test_predict <- predict(rfFit, pml_test_reduced)
pml_test_predict
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pml_test_predict)
```

After submitting the files for grading, 20 out of 20 of the predicted values were correct.
  
  
  
  
  
  
  
  
  
