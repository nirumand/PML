# Practical Machine Learning Course Project
Reza Nirumand  
March 12, 2016  



##Summary,

##Data
The data which is provided by the assignment ([Train set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and [Test set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv))for this project comes originaly from following [paper](http://groupware.les.inf.puc-rio.br/har):  
*Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.*



###Envirnoment and Data prepration  
We will first load the data from the given url and then will do some data cleansing:

```r
###Loading required packages###
library(caret)
library(rpart)

### downloading the training and testing set 
if(!file.exists("pml-training.csv")) 
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-training.csv")}
if(!file.exists("pml-testing.csv"))
{   download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",destfile ="pml-testing.csv")}
  
### importing the downloaded files
input.train<-read.csv("pml-training.csv")
input.test<-read.csv("pml-testing.csv")
```


###Spliting dataset
how you used cross validation, 
###Explatory Analysis

##Model Building
Here i will explain how i built your model.


question->data->feature->algorithms

##Results
You will also use your prediction model to predict 20 different test cases.
what you think the expected out of sample error is, and why you made the choices you did. 
out of sample error = error resulted from applying your prediction algorithm to a new data set
– also known as generalization error
– out of sample error most important as it better evaluates how the model should perform

Problems: Since the number of test cases in Test set by the assignment is too low, there could be some accurace problem with the prediction. So to avoid that, i will split my trainning data set to two data set (70% Trainning and 30% test set) and will consider the test set provided by the assignment as the validation set.
generalization error

###Error measures and explanation


-------------------------------
-------------------------------

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

![](index_files/figure-html/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
