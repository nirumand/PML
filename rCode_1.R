library(caret)
library(randomForest)
require(doParallel)
set.seed(123) 

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


prPro.validation <-input.test[notNaColumns[notNaColumns!="classe"]] #removing the same columns from test.

### checking if any column has near zero variability
nzv<-nearZeroVar(prPro.train, saveMetrics=TRUE)
sum(nzv$nzv) ## it shows that there is no more column removal availble.

inTrain<-createDataPartition(y=prPro.train$classe,p = .7,list=FALSE) ##randomly partition data into two data sets ,training and testing
model.training<-prPro.train[inTrain,]   ##to build the model
model.testing<-prPro.train[-inTrain,]   ##to test and tune the model
rbind("Original Train set"=dim(input.train),"Original Test set"=dim(input.test),Training=dim(model.training),Testing=dim(model.testing)) 


findCorrelation(cor(model.training[,-53]),cutoff = .8)

model.train.preProc<-preProcess(model.training[,-53],method=c("scale","center","pca")) ## we remove column 53 which is our predicted value. 
model.train.PC<-predict(model.train.preProc,model.training[,-53])  ## summarizing features value using pca on model.train
model.testing.PC<-predict(model.train.preProc,model.testing[-53])  ## applying the same to the testing set.

rbind("training set"=dim(model.training),"testing set (from input.traini)"=dim(model.testing),"pre-processed training set"=dim(model.train.PC),"pre-processed testing set"=dim(model.testing.PC)) 

findCorrelation(cor(model.train.PC),cutoff = .8)

########
trCont<-trainControl(method="cv",allowParallel=TRUE)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

startTime<-Sys.time()
rfModelFit<-train(model.training$classe ~ ., data=model.train.PC, method="parRF", prox=TRUE, trControl=trCont,tuneGrid=expand.grid(mtry = 10),number=3,ntree=500,do.trace=TRUE)
endTime<-Sys.time()

stopCluster(cl)

print(endTime-startTime,digit=3) ##showing execution time
print(rfModelFit$finalModel,digits=3)

mode.testing.predict<-predict(rfModelFit,model.testing.PC)
#============
#===========
confMatrix<-confusionMatrix(model.testing$classe,mode.testing.predict);print(confMatrix,digits=3)
