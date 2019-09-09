---
title: "Titanic Passenger Survival Prediction"
output: html_document
---

```{r setup, echo=FALSE, message=FALSE}
setwd("C:/Users/SHASHWAT/Documents/R tutorial 9")
train_data<-read.csv("train.csv")
test_data<-read.csv('test.csv')
```
### Understanding the Data:
```{r}
summary(train_data) #column summaries
str(train_data) #structure of the data
```

# **Part 1**  
Cleaning the data by removing missing values:
```{r}
test_data[is.na(test_data$Age),'Age']<-mean(test_data[,'Age'],na.rm=TRUE)
train_data[is.na(train_data$Age),'Age']<-mean(train_data[,'Age'],na.rm=TRUE)

train_data$Survived<-factor(train_data$Survived)


```
  
***

# **Part 2**  

#### Categorizing the various attributes:

```{r}
#Categorizing the Fare prices

train_data[train_data$Fare<(15),'Fare']<-"low fare"
train_data[(train_data$Fare>15)&(train_data$Fare<30),'Fare']<-"medium fare"
train_data[(train_data$Fare!='low fare'&train_data$Fare!='medium fare'),'Fare']<-"high fare"
train_data$Fare<-factor(train_data$Fare)

test_data[is.na(test_data$Fare),'Fare']<-mean((test_data[,'Fare']), na.rm=T)
test_data[test_data$Fare<(15),'Fare']<-"low fare"
test_data[(test_data$Fare>15)&(test_data$Fare<30),'Fare']<-"medium fare"
test_data[(test_data$Fare!='low fare'&test_data$Fare!='medium fare'),'Fare']<-"high fare"
test_data$Fare<-factor(test_data$Fare)

#Categorizing the Age of passengers

train_data[train_data$Age<(10),'Age']<-"child"
train_data[(train_data$Age>10)&(train_data$Age<25),'Age']<-"young adult"
train_data[(train_data$Age>25)&(train_data$Age<45),'Age']<-"adult"
train_data[(train_data$Age!='child'&train_data$Age!='young adult'&train_data$Age!='adult'),'Age']<-"senior"
train_data$Age<-factor(train_data$Age)

test_data[test_data$Age<(10),'Age']<-"child"
test_data[(test_data$Age>10)&(test_data$Age<25),'Age']<-"young adult"
test_data[(test_data$Age>25)&(test_data$Age<45),'Age']<-"adult"
test_data[(test_data$Age!='child'&test_data$Age!='young adult'&test_data$Age!='adult'),'Age']<-"senior"
test_data$Age<-factor(test_data$Age)

#Categorizing family sizes

train_data$FamSize<-train_data$SibSp+train_data$Parch
train_data$FamSize<-factor(train_data$FamSize)
test_data$FamSize<-test_data$SibSp+test_data$Parch
test_data$FamSize<-factor(test_data$FamSize)

```
***
  
# **Part 3**  

### Dependency of the survival of the passengers on given parameters
  
#### Studying the Dependency of Sex on their survival

```{r fig.cap="Figure 1:This is a plot to showing the survival of Titanic passengers according to their sex."}
# Dependency of Survival on Sex
library(ggplot2)
plot_sex<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Sex))+facet_grid(.~Sex)
plot_sex
```
  
#### Studying the Dependency of ticket fare on their survival  

```{r fig.cap="Figure 2:This is a plot to showing the survival of Titanic passengers according to their ticket fare."}

plot_fare<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Fare))+facet_grid(.~Fare)
plot_fare
```
  
#### Studying the Dependency of Age on their survival  
```{r fig.cap="Figure 3:This is a plot to showing the survival of Titanic passengers according to their Age."}
plot_Age<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Age))+facet_grid(.~Age)
plot_Age

```
  
#### Studying the Dependency of family size on their survival 
```{r fig.cap="Figure 4:This is a plot to showing the survival of Titanic passengers according to their family size."}
plot_Fam<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=FamSize))+facet_grid(.~FamSize)
plot_Fam

```
  
#### Removing unnecessary features  
```{r}
train_data<-train_data[,c(-4,-7,-8,-9,-11,-12)]
test_data<-test_data[,c(-3,-6,-7,-8,-10,-11)]
```

### Splitting in training and test sets  
```{r}
library(caTools)
set.seed(123)
split = sample.split(train_data$Survived, SplitRatio = 0.80)
training_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)

```
  
### Creating the classifier  
```{r}
library(randomForest)

classifier = randomForest(x = training_set[,c(-1,-2)],
                          y = training_set$Survived,
                          ntree = 25)
```
  
### Creating the vector of predictions for test set and confusion matrix  
```{r}
y_pred = predict(classifier, newdata = test_set[,c(-1,-2)])
cm = table(test_set[, 2], y_pred)

```
  
    
The confusion matrix obtained is as follows:
```{r}
cm
```
  
#### Measuring the Precision and recall of the model:  

*We know that precision of a model is calculated as*  
$Precision=TP/(TP+FP)$   
*Therefore* $Precision=50/(50+14)=0.78125$    
  
*again,* $Recall=TP/(TP+FN)$  
*Therefore* $Recall=50/(50+18)=0.735$  

# **Part 4**
  
#### Training the model on the Train data  
```{r}
classifier1 = randomForest(x = train_data[,c(-1,-2)],
                          y = train_data$Survived,
                          ntree = 25)

```
  
#### Predicting the Test data values  
```{r}
y_pred1 = predict(classifier1, newdata = test_data[,c(-1)])
```
  
#### Creating the result dataframe  
```{r}
results<-data.frame("PassengerId"=test_data$PassengerId,"Survived"=y_pred1)  

```
  
#### Viewing top 10 predictions from the result dataframe
```{r}
head(results,10)
```

*END*
