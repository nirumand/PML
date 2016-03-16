# Practical Machine Learning Course Project
Reza Nirumand  
March 12, 2016  



##Summary  
A couple of researchers wanted to see if they could detect a wrong activity using some sensors information for a particular physical activity.So they classified the overall activities into 5 groups A,B,C,D,E which corresponds to the way of doing a specific activity. 
In this Course Project we will try to re-produce the same results using different Machine learning methods. To build and test a model, we have splitted the provided training set to 70%training set and 30% test set. Since there has been lots of features in the original set and due to hardware limitations we needed to reduce the features before we train the model.
Then We have trained and built the model using the new pre-processed training and random forest algorithm. Finally we have predicted the classe of the activity (A,...,E) for the test set. The Achieved Accuracy for predicting the test set is ~ 97% which is acceptable for the purpose of this project. basically What we did:  
1- remove unused columns  
2- remove summary rows (section 5.1 of the paper)  
3- pre-process/reduce feature using PCA  
4- slicing data to have train,test,validation sets  
5- predict pre-processed train and test set using PCA  
6- train model with random forest algorithm  
7- check accuracy on test set. Optimizing model
8- predicting validation set.

##Data
The data which is provided by the assignment ([Train set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and [Test set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv))for this project comes originally from following [paper](http://groupware.les.inf.puc-rio.br/har):  
*Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.*

to build our model we have used following:  
- R version 3.2.3 (2015-12-10)  
- caret_6.0-64   
- randomForest_4.6-12 caret_6.0-64  
- OS: Windows 10 x64  

Please note due to bugs under doParallel package also , re-producibility issues on doMC package(it does not support windows), i have decided not to use parallel execution and also reduced the number of trees on random forest algorithm.  

### Prepration  
As mentioned before , there are two datasets to work on: train and test set. We will use the **test** set as the **validation** set.
We will extract our test set from training set for the purpose of optimization. We will first load the data from the given url and then will do some data cleansing. That means, we will remove the columns which will not be included on prediction from both train and test dataset. Afterwards we will remove the columns which do not have any value or have near zero variance. Finally we have 52 features to predict the category "classe".


```r
###Loading required packages###
library(caret)
library(randomForest)
set.seed(123) 
```


```r
### downloading the training and testing set from the url provided by course project
if(!file.exists("pml-training.csv")) 
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-training.csv")}
if(!file.exists("pml-testing.csv"))
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-testing.csv")}

### importing the downloaded files.
input.train<-read.csv("pml-training.csv",na.strings = c("NA","#DIV/0!",""))
input.test<-read.csv("pml-testing.csv",na.strings = c("NA","#DIV/0!",""))

### removing predication non-releated columns. 
# removing 7 first columns, such as X, username, new_window..
# removing summarized observations as per section 5.1 of the paper 
prPro.train<-subset(x=input.train,new_window=="no",select=-c(1:7)) 
notNaColumns<-names(which(colSums(is.na(prPro.train))<=0)) # Identifing columns which are not empty.
prPro.train<-prPro.train[notNaColumns]  # select only columns with above condition.


prPro.validation <-input.test[notNaColumns[notNaColumns!="classe"]] #removing the same columns from validation set.

### checking if any column has near zero variability
nzv<-nearZeroVar(prPro.train, saveMetrics=TRUE)

paste("Number of columns with NA values: " , sum(nzv$nzv))  ## 
```


###Splitting dataset  
To be able to test and optimize our model we will split the training data set into two data set: model.training  and model.testing. We will build our model from the data set model.training and will test our model using model.testing dataset for the accuracy.


```r
inTrain<-createDataPartition(y=prPro.train$classe,p = .7,list=FALSE) ##randomly partition data into two data sets ,training and testing
model.training<-prPro.train[inTrain,]   ##to build the model
model.testing<-prPro.train[-inTrain,]   ##to test and tune the model
rbind("Original Train set"=dim(input.train),"Original Test set"=dim(input.test),Training=dim(model.training),Testing=dim(model.testing)) 
```

```
##                     [,1] [,2]
## Original Train set 19622  160
## Original Test set     20  160
## Training           13453   53
## Testing             5763   53
```

###Explatory Analysis and pre-processing. 
In This section we will try to get an insight into data in order to reduce the number of features. Lets see if there is a correlation between features:


```r
findCorrelation(cor(model.training[,-53]),cutoff = .8)
```

```
##  [1] 10  1  9 36  8  2 34 21 25 18
```
As shown above there a couple of columns which are correlated. To remove effect of correlated columns, we will use pca to extract the most important features which have the most information. Also i should note that, This will be very useful when using algorithms such as random forest. Since High number of features causes very tall Tree hence very long execution time. 


```r
model.train.preProc<-preProcess(model.training[,-53],method=c("scale","center","pca")) ## we remove column 53 which is our predicted value. 
model.train.PC<-predict(model.train.preProc,model.training[,-53])  ## summarizing features value using pca on model.train
model.testing.PC<-predict(model.train.preProc,model.testing[-53])  ## applying the same to the testing set.

rbind("training set"=dim(model.training),"testing set (from input.traini)"=dim(model.testing),"pre-processed training set"=dim(model.train.PC),"pre-processed testing set"=dim(model.testing.PC)) 
```

```
##                                  [,1] [,2]
## training set                    13453   53
## testing set (from input.traini)  5763   53
## pre-processed training set      13453   26
## pre-processed testing set        5763   26
```

So after using the pca, there are no more correlated features.

```r
findCorrelation(cor(model.train.PC),cutoff = .8)
```

```
## integer(0)
```

##Model Building with cross-validation
Due to high accuracy of the random forest algorithm for classification problems, we will use it on this project of the prediction.
We have also used cross-validation to have better accuracy for the prediction. 
I have tested the algorithm with many different parameters such as  ntree=500 ,number=10 or 5. I noticed it does not have much effect on the model accuracy, rather it just increases the execution time. As mentioned above, due to compatibility problem , i am sharing non-parallel execution code which may take longer to execute but will definitely has a result, in contrast to using doMC and doParallel packages, which can throw some exceptions.


```r
trCont<-trainControl(method="cv") # creating control object for the random forest algorithm.

startTime<-Sys.time() #for troubleshooting purposes we will capture time.

rfModelFit<-train(model.training$classe ~ ., data=model.train.PC, method="rf", prox=TRUE, trControl=trCont,tuneGrid=expand.grid(mtry = 10),number=3,ntree=100) ## fitting the model

endTime<-Sys.time()

model.testing.predict<-predict(rfModelFit,model.testing.PC)
```


##Results  
To calculate accuracy and out of sample error we need to calculate confusion matrix:

```r
confMatrix<-confusionMatrix(model.testing$classe,model.testing.predict) 
```

By testing our model on testing set we have achieved 97.4% accuracy and 2.6% out of sample error.


```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1619   10    5    6    1
##          B   23 1071   18    1    2
##          C    3   20  970   10    2
##          D    0    3   26  915    0
##          E    2    3    7    8 1038
## 
## Overall Statistics
##                                        
##                Accuracy : 0.974        
##                  95% CI : (0.97, 0.978)
##     No Information Rate : 0.286        
##     P-Value [Acc > NIR] : < 2e-16      
##                                        
##                   Kappa : 0.967        
##  Mcnemar's Test P-Value : 0.000554     
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.983    0.967    0.945    0.973    0.995
## Specificity             0.995    0.991    0.993    0.994    0.996
## Pos Pred Value          0.987    0.961    0.965    0.969    0.981
## Neg Pred Value          0.993    0.992    0.988    0.995    0.999
## Prevalence              0.286    0.192    0.178    0.163    0.181
## Detection Rate          0.281    0.186    0.168    0.159    0.180
## Detection Prevalence    0.285    0.193    0.174    0.164    0.184
## Balanced Accuracy       0.989    0.979    0.969    0.984    0.995
```

This is a good result for the sake of learning from this project hence We will use it as the final mode to predict the validation set(the input.test set). To do so we need to first apply pre-process function to the validation-set and then predict outcomes.

Regarding *out of sample error* we can achieve better accuracy and less out of sample error, but it requires a good hardware for tuning and testing multiple models. At this point i am satisfied with the result. 


##Test (Quiz)
To answer course project quiz, we will use the final model to predict the validation-set.

```r
validation.PC<-predict(model.train.preProc,prPro.validation) ## first we pre-process our validation set to apply pca.
predict(rfModelFit,validation.PC)
```

```
##  [1] B A B A A B D B A A A C B A E E A B B B
## Levels: A B C D E
```
