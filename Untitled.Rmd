---
title: "Activity Data Prediction"
output: pdf_document
---
From Coursera:

##Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

##Data 


The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 
##Data Processing
```{r load library, set seed}
library(ggplot2)
library(lattice)
library(caret)
library(stats)
library(rpart)
library(RColorBrewer)
library(rattle)
library(rpart.plot)
library(e1071)
library(randomForest)

set.seed(930)

```

```{r load data}


download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv','pml-training.csv', method = "curl")

download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
','pml-testing.csv', method = "curl")

# read data
train<-read.csv("./pml-training.csv", na.strings = c("NA", "#DIV/0!",""))
test<-read.csv("./pml-testing.csv", na.strings = c("NA", "#DIV/0!",""))
```
Omiting variables with over 60% NA
```{r omit NA >60%}

omit <- which((colSums(!is.na(train)) >= 0.6*nrow(train)))
train.omit <- train[,omit]
test.omit <- test[,omit]
```


Partition training data 60:40 into 'train_part' and 'test_part' for purposes of model building

```{r partition data}
train_div  <- createDataPartition(train.omit$classe, p = 0.6, list = FALSE)
train_part    <- train.omit[train_div, ]
test_part     <- train.omit[-train_div, ]
```
##Decision Tree
Let's start the model building process with a decision tree algorithim.
```{r }
class<-train_part$classe

dtree_mod <- rpart(classe ~ ., data=train_part, method="class")
fancyRpartPlot(dtree_mod)

``` 

```{r prediciton}
prediction <- predict(dtree_mod, test_part, type = "class")
```

```{r confusion matrix}
confusionMatrix(prediction, test_part$classe)
```
The confusion matrix looks pretty good. Acccuracy is at 99.97%. However, let's see if we can improve prediciton accuracy by generating a larger amount of bootstrapped trees, i.e. Random Forest.

## Random Forest:  Generating a larger number of bootstrapped trees

Model accuracy is now at 100%
```{r random forest}
random_forest<-randomForest(classe~., data = train_part)
```

```{r predict rf}
predictition_RF<- predict(random_forest, test_part, type = "class")
```

```{r confusion matrix randomforest}
confusionMatrix(predictition_RF, test_part$classe)
```




##Submit results for test set
```{r}
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictition_RF)

```
