#a very first looks shows that there are lots of columns which are n/a . to be precise from 19622 observations 19216 have not data. 
# we may use above columns to identify the observations with specific category. 
colSums(is.na(input.train)) 
names(which(colSums(is.na(input.train))>0))  #gives out the name of columns which mostly do not have data.

notNullRecords<-is.na(input.train$max_roll_belt) #creating filter for the columns
input.train.obswithData<-input.train[!notNullRecords,] # identifing observations with data.

###Loading required packages###
library(caret)
library(rpart)
library(randomForest)

set.seed(123)

### downloading the training and testing set 
if(!file.exists("pml-training.csv")) 
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-training.csv")}
if(!file.exists("pml-testing.csv"))
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-testing.csv")}

### importing the downloaded files

input.train<-read.csv("pml-training.csv",na.strings = c("NA","#DIV/0!",""))
input.test<-read.csv("pml-testing.csv",na.strings = c("NA","#DIV/0!",""))

##preprocessing
prPro.train<-input.train[-c(1:7)] #removing 7 first columns, such as X, username, new_window...

notNaColumns<-names(which(colSums(is.na(prPro.train))<=0)) # Identifing columns which are not empty.
prPro.train<-prPro.train[notNaColumns]  # select only columns with above condition.

prPro.validation <-input.test[notNaColumns[notNaColumns!="classe"]] #removing the same columns from test. 

nzv<-nearZeroVar(prPro.train, saveMetrics=TRUE) ## no column to be removed because of zero variation

### slicing the data


inTrain<-createDataPartition(y=prPro.train$classe,p = .7,list=FALSE)
model.training<-prPro.train[inTrain,]
model.testing<-prPro.train[-inTrain,]



#Build model
rf<-train(classe ~ ., data=model.training, method="rf", prox=TRUE, ntree=500)
