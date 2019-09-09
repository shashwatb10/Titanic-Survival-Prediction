setwd('~/R tutorial 9')
library(ggplot2)

train_data<-read.csv("train.csv")
test_data<-read.csv('test.csv')

test_data[is.na(test_data$Age),'Age']<-mean(test_data[,'Age'],na.rm=TRUE)
train_data[is.na(train_data$Age),'Age']<-mean(train_data[,'Age'],na.rm=TRUE)

train_data$Survived<-factor(train_data$Survived)

# Dependency of Survival on Sex
plot_sex<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Sex))+facet_grid(.~Sex)
plot_sex

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

# Dependency of Survival on Fare of ticket
plot_fare<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Fare))+facet_grid(.~Fare)
plot_fare


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

# Dependency of Survival on Age
plot_Age<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Age))+facet_grid(.~Age)
plot_Age

#Categorizing family sizes

train_data$FamSize<-train_data$SibSp+train_data$Parch
train_data$FamSize<-factor(train_data$FamSize)
test_data$FamSize<-test_data$SibSp+test_data$Parch
test_data$FamSize<-factor(test_data$FamSize)

# Dependency of Survival on Family Size
plot_Fam<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=FamSize))+facet_grid(.~FamSize)
plot_Fam

train_data$Pclass<-factor(train_data$Pclass)
test_data$Pclass<-factor(test_data$Pclass)

# Dependency of Survival on Family Size
plot_class<-ggplot(data=train_data, aes(x=Survived))+geom_bar(aes(fill=Pclass))+facet_grid(.~Pclass)
plot_class

#Removing unnecessary features
train_data<-train_data[,c(-4,-7,-8,-9,-11,-12)]
test_data<-test_data[,c(-3,-6,-7,-8,-10,-11)]

library(caTools)
set.seed(123)
split = sample.split(train_data$Survived, SplitRatio = 0.80)
training_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)


library(randomForest)

classifier = randomForest(x = train_data[,c(-1,-2)],
                          y = train_data$Survived,
                          ntree = 25)


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_data[,c(-1)])

# # Making the Confusion Matrix
# cm = table(training_set[, 2], y_pred)
# cm

# Predicting the Test set results
results<-data.frame("PassengerId"=test_data$PassengerId,"Survived"=y_pred)  

#Exporting the resultant dataframe
write.csv(results,'~//R tutorial 9//Results_2.csv')
