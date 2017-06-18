# Set working directory and import files

setwd("D:/rengalv_GIT/Kaggle-Titanic-in-R")

library(readr)
train <- read.csv("D:/rengalv_GIT/Kaggle-Titanic-in-R/train.csv",stringsAsFactors = TRUE)
View(train)

test <- read.csv("D:/rengalv_GIT/Kaggle-Titanic-in-R/test.csv")
View(test)

#Lets look at the summary of the number of people survived in the given dataset
table(train$Survived)

#To make it more intuitive, lets view it as a proportional value
prop.table(table(train$Survived))

#Lets look at a case wherein everyone dies
test$Survived <- rep(0,418)

#create a submission for this allperish case
submit <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit,file = "EveryoneDies.csv",row.names = FALSE)

#Gender Class model development
summary(train$Sex)
prop.table(table(train$Sex,train$Survived)) #Now this would give every row/ total numnber of rows
prop.table(table(train$Sex,train$Survived),1)#Would take into account the classes in row-wise

test$Survived[test$Sex == 'female'] <- 1
table(test$Survived)

#Submit considering all Women Survived
submit <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit,file = "AllWomenSurvived.csv",row.names = FALSE)

#Lets consider the Age as a factor for the prediction
summary(train$Age)
train$Child<-0
train$Child[train$Age < 18]<-1
table(train$Child)

#Compare the distribution of child and Sex
aggregate(Survived ~ Child + Sex, data= train, FUN = sum)#This considers only for Survived cases

aggregate(Survived ~ Child + Sex, data= train, FUN = length)#This considers all rows

#Now we want to get the proportion for each case, we need to write a function
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})


#Lets consider the Class and Ticket Fare to make predctions going forward
train$Fare2<- '30+'
train$Fare2[train$Fare >= 20 & train$Fare < 30]<- '20-30'
train$Fare2[train$Fare >= 10 & train$Fare < 20]<- '10-20'
train$Fare2[train$Fare < 10]<- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x)/length(x)})


#We find that the Female travelers in the third class with higer Fare didnt survive
test$Survived<-0
test$Survived[test$Sex == 'female']<-1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20 ] <-0

table(test$Survived)

submit <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit,file = "WomenInHigerClasseDead.csv",row.names = FALSE)

#Decision Trees
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)

#For getting these plot we need a newer version of R which can be installed using the follwing link:
# https://www.r-statistics.com/2013/03/updating-r-from-r-on-windows-using-the-installr-package/

#Need a better plotting technique
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

#Now we can try making a submission with this decision tree predction
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "DecisionTreePrediction_v1.csv", row.names = FALSE)
