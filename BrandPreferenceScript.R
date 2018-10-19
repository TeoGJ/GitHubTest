


library(readxl)
install.packages("caret",dependencies = TRUE)
library("caret")
library("rpart")
library("rpart.plot")
set.seed(123)
SurveyComplete<- read_xlsx("Survey_Key_and_Complete_Responses_excel.xlsx","Survey Results Complete")

#data exploration
#values type
SurveyComplete$credit<- as.numeric(SurveyComplete$credit)
SurveyComplete$brand<- as.factor(SurveyComplete$brand)
SurveyComplete$elevel<- as.character(SurveyComplete$elevel)
SurveyComplete$elevel<- as.numeric(SurveyComplete$elevel)



#missing values
summary(is.na(SurveyComplete))
is.na(SurveyComplete)


#plots&histograms
plot(y=SurveyComplete$credit,ylab = "Credit", x=SurveyComplete$age,xlab = "Age")
hist(SurveyComplete$credit, xlab = "credit")
hist(SurveyComplete$salary, xlab = "salary")
hist(SurveyComplete$car, xlab = "car")
hist(SurveyComplete$zipcode, xlab = "zipcode")
hist(SurveyComplete$elevel, xlab = "elevel")
hist(SurveyComplete$age, xlab = "age")
plot(y=SurveyComplete$brand, ylab = "brand", x=SurveyComplete$salary, xlab = "salary")
plot(y=SurveyComplete$brand, ylab="brand",x=SurveyComplete$age, xlab="age")
plot(y=SurveyComplete$brand, ylab="brand",x=SurveyComplete$elevel, xlab="education level")
plot(y=SurveyComplete$brand, ylab="brand",x=SurveyComplete$car, xlab="car")
plot(y=SurveyComplete$brand, ylab="brand",x=SurveyComplete$zipcode, xlab="ZIP code")
plot(y=SurveyComplete$brand, ylab="brand",x=SurveyComplete$credit,xlab="credit")
plot(y=SurveyComplete$salary, ylab="salary",x=SurveyComplete$zipcode,xlab="zipcode")
plot(y=SurveyComplete$age, x=SurveyComplete$car)



# comparing both data-sets. Are both intentional sampled?
hist(SurveyIncomplete$salary)
hist(SurveyComplete$salary)
hist(SurveyIncomplete$age)
hist(SurveyComplete$age)
hist(SurveyIncomplete$elevel)
hist(SurveyComplete$elevel)
hist(SurveyIncomplete$car)
hist(SurveyComplete$car)
hist(SurveyIncomplete$zipcode)
hist(SurveyComplete$zipcode)
hist(SurveyIncomplete$credit)
hist(SurveyComplete$credit)

#yes, both are


#practicing with ggplot
Plot1<- ggplot() + geom_histogram(data = SurveyComplete, aes(x = "salary", y = "age", color = "brand")) +
  xlab("Salary")  + ylab("Age") + theme(plot.title = element_text(hjust = 0.5))

Plot1

#normalizing attribute ranges
preProcessParams<- preProcess(SurveyComplete[,c("salary","age","credit")], method=c("center","scale"))
summary(preProcessParams)
transformed<- predict(preProcessParams,SurveyComplete[,c("salary","age","credit")])
SurveyCompleteNorm<- cbind(transformed,SurveyComplete[,c("elevel","car","zipcode","brand")])


preProcessParams2<- preProcess(SurveyIncomplete[,c("salary","age","credit")], method=c("center","scale"))
summary(preProcessParams2)
transformed2<- predict(preProcessParams,SurveyIncomplete[,c("salary","age","credit")])
SurveyIncompleteNorm<- cbind(transformed2,SurveyIncomplete[,c("elevel","car","zipcode")])




#creating training and test sets
inTraining <- createDataPartition(SurveyComplete$brand, p = .75, list = FALSE)
training<- SurveyComplete[inTraining,]
testing<- SurveyComplete[-inTraining,]

inTrainingNorm<- createDataPartition(SurveyCompleteNorm$brand, p = .75, list = FALSE)
trainingNorm<- SurveyCompleteNorm[inTrainingNorm,]
testingNorm<- SurveyCompleteNorm[-inTrainingNorm,]


summary(training)
summary(testing)


#cross validation
Control_SurCom<- trainControl(method="repeatedcv", number = 10, repeats = 3)
Control_SurCom


#decision tree
SurComTree<- rpart(brand~., data=training, method = "class", control = list(maxdepth = 3))
summary(SurComTree)
rpart.plot(SurComTree)


Pred1 <- predict(SurComTree, newdata = testing)
postResample(Pred1,testing$brand)
rpart.plot(SurComTree$finalModel)


#decision tree based on age
AgeTree<- train(age~., data=training, method="rpart")
rpart.plot(AgeTree$finalModel)


set.seed(123)
knn1<- train(brand~.,method="knn",data = training, tuneLength=22, preProcess=c("center","scale"))
knn2<- train(brand~salary+age,method="knn",data = training, tuneLength=5, preProcess=c("center","scale"))
kknn3.0<- train(brand~salary+age+car,method="knn",data = training, tuneLength=10, preProcess=c("center","scale"))

knn4<- train(brand~salary+age,method="knn",data = trainingNorm, tuneLength=5)
knn4



fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)


testpred_knn2<- predict(knn2,newdata = testing)
Predknn2<- predict(knn2,SurveyIncomplete)
Predknn2

testpred_knn4<- predict(knn4,newdata = testingNorm)
Predknn4<- predict(knn4,SurveyIncompleteNorm)
Predknn4
summary(Predknn4)


str(Predknn2)
plot(Predknn2)

str(Predknn4)
plot(Predknn4)

postResample(testpred_knn2,testing$brand)
postResample(testpred_knn4,testingNorm$brand)

mean(testpred_knn2 == testing$brand)

cm_knn2_testset <- confusionMatrix(testpred_knn2,as.factor(testing$brand))
cm_knn2_testset
confusionMatrix(data=testpred_knn2,testing)
confusionMatrix(testpred_knn2,testing$brand~salary+age)

cm_knn4_testset<- confusionMatrix(testpred_knn4,testingNorm$brand)
cm_knn4_testset

training$brand <- as.factor(training$brand)
rf1train = randomForest(brand~salary+age, data=training, ntree=50, proximity=T)
rf1train

testpredict_rf1<- predict(rf1train, newdata = testing)
cm_rf1_testset<- confusionMatrix(testpredict_rf1,as.factor(testing$brand))
cm_rf1_testset

PredRF1<- predict(rf1train, SurveyIncomplete)

PredRF1
summary(PredRF1)
plot(PredRF1)

NewSurvey<- cbind(SurveyIncomplete,PredRF1)
NewSurvey$brand<- NULL

#decision trees onto RF predictions
NewSurveyTree1<- rpart(NewSurvey$PredRF1~., data=NewSurvey, method = "class", control = list(maxdepth = 3))
rpart.plot(NewSurveyTree1)
summary(NewSurveyTree1)

NewSurveyTree2<- rpart(NewSurvey$PredRF1~., data=NewSurvey, method = "class", control = list(maxdepth = 5))
rpart.plot(NewSurveyTree2)
summary(NewSurveyTree2)

NewSurveyTree3<- rpart(NewSurvey$PredRF1~., data=NewSurvey, method = "class", control = list(maxdepth = 7))
rpart.plot(NewSurveyTree3)
summary(NewSurveyTree3)


#decision trees onto KNN predictions
NewSurvey2<- cbind(SurveyIncompleteNorm,Predknn4)
plot(NewSurvey2)

NewSurvey2Tree1<-rpart(NewSurvey2$Predknn4~., data=NewSurvey2, method = "class", control = list(maxdepth=3))
rpart.plot(NewSurvey2Tree1)
summary(NewSurvey2Tree1)

NewSurvey2Tree2<-rpart(NewSurvey2$Predknn4~., data=NewSurvey2, method = "class", control = list(maxdepth=5))
rpart.plot(NewSurvey2Tree2)
summary(NewSurvey2Tree2)

NewSurvey2Tree3<-rpart(NewSurvey2$Predknn4~., data=NewSurvey2, method = "class", control = list(maxdepth=7))
rpart.plot(NewSurvey2Tree3)
summary(NewSurvey2Tree3)

