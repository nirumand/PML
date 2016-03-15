
###Loading required packages###
library(caret)
library(randomForest)

set.seed(123)

### downloading the training and testing set 
if(!file.exists("pml-training.csv")) 
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-training.csv")}
if(!file.exists("pml-testing.csv"))
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-testing.csv")}

### importing the downloaded files

g.train<-read.csv("pml-training.csv",na.strings = c("NA","#DIV/0!",""))
g.test<-read.csv("pml-testing.csv",na.strings = c("NA","#DIV/0!",""))
g.train<-g.train

##preprocessing
prPro.train<-g.train[-c(1:7)] #removing 7 first columns, such as X, username, new_window...

notNaColumns<-names(which(colSums(is.na(prPro.train))<=0)) # Identifing columns which are not empty.
prPro.train<-prPro.train[notNaColumns]  # select only columns with above condition.

prPro.validation <-g.test[notNaColumns[notNaColumns!="classe"]] #removing the same columns from test. 

nzv<-nearZeroVar(prPro.train, saveMetrics=TRUE) ## no column to be removed because of zero variation

### slicing the data


inTrain<-createDataPartition(y=prPro.train$classe,p = .7,list=FALSE)
model.training<-prPro.train[inTrain,]
model.testing<-prPro.train[-inTrain,]



#Build model
model.train.preProc<-preProcess(model.training[,-53],method=c("scale","center","pca"))
model.train.PC<-predict(model.train.preProc,model.training[,-53])

rfModelFit<-train(model.training$classe ~ ., data=model.train.PC, method="rf", prox=TRUE, trControl = trainControl(method="cv"),number=10,ntree=500)

model.testing.PC<-predict(model.train.preProc,model.testing[-53])

# compare results
confMatrix<-confusionMatrix(model.testing$classe,predict(rfModelFit,model.testing.PC))

confMatrix

> prPro.validation <-g.test[notNaColumns[notNaColumns!="classe"]]
> validation.PC<-predict(model.train.preProc,prPro.validation)
> predict(rfModelFit,validation.PC)
[1] B A C A A E D B A A B C B A E E A B B B


