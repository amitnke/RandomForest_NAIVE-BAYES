setwd("C:\\PERSONAL\\edureka\\Module-4")

DB<-read.csv("Diabetes.csv",head=T)

head(DB)

nrow(DB)

set.seed(2)
DB$ind<-sample(2,nrow(DB),replace=TRUE,prob=c(0.7,0.3))

head(DB)

trainData<-DB[(DB$ind==1),]
testData<-DB[(DB$ind==2),]

nrow(trainData)
nrow(testData)

library(rpart)

head(trainData)

dt<-rpart(Class.variable~Number.of.times.pregnant
				+Plasma.glucose.concentration
				+Diastolic.blood.pressure
				+Triceps.skin.fold.thickness
				+X2.Hour.serum.insulin
				+Body.mass.index
				+Diabetes.pedigree.function
				+Age..years., 
		data=trainData,
		control=rpart.control(minsplit=10))

dt

plot(dt)
text(dt)

####Predict####
pred<-predict(dt,testData,type=c("class"))
pred

cbind(as.character(testData$Class.variable),as.character(pred))

#####confusion matrix######
table(as.character(testData$Class.variable),as.character(pred))


####Predict-Prob#####
pred1<-predict(dt,testData,type=c("prob"))
pred1

head(pred)
head(pred1)

attributes(dt)

dt$variable.importance


#####New Prediction######
New<-read.csv("New.csv",head=T)
predict(dt,New,type=c("class"))


###########RANDOM FOREST###########################


setwd("C:\\PERSONAL\\edureka\\Module-4")

DB<-read.csv("Diabetes.csv",head=T)

head(DB)

nrow(DB)

set.seed(2)
DB$ind<-sample(2,nrow(DB),replace=TRUE,prob=c(0.7,0.3))

head(DB)

trainData<-DB[(DB$ind==1),]
testData<-DB[(DB$ind==2),]

nrow(trainData)
nrow(testData)

library(randomForest)

rf<-randomForest(Class.variable~Number.of.times.pregnant
				+Plasma.glucose.concentration
				+Diastolic.blood.pressure
				+Triceps.skin.fold.thickness
				+X2.Hour.serum.insulin
				+Body.mass.index
				+Diabetes.pedigree.function
				+Age..years., 
		data=trainData,ntree=600,mtry=4)


rf

attributes(rf)

rf$importance

predRF<-predict(rf,testData,type=c("class"))

#####confusion matrix######
table(as.character(testData$Class.variable),as.character(predRF))







####################NAIVE BAYES##################

setwd("C:\\PERSONAL\\edureka\\Module-4")

DB<-read.csv("Diabetes.csv",head=T)

head(DB)

nrow(DB)

set.seed(2)
DB$ind<-sample(2,nrow(DB),replace=TRUE,prob=c(0.7,0.3))

head(DB)

trainData<-DB[(DB$ind==1),]
testData<-DB[(DB$ind==2),]

nrow(trainData)
nrow(testData)

library(e1071)

NB<-naiveBayes(Class.variable~Number.of.times.pregnant
				+Plasma.glucose.concentration
				+Diastolic.blood.pressure
				+Triceps.skin.fold.thickness
				+X2.Hour.serum.insulin
				+Body.mass.index
				+Diabetes.pedigree.function
				+Age..years., 
		data=trainData)

attributes(NB)

NB$apriori
NB$tables


predNB<-predict(NB,testData,type=c("class"))



###Confusion Matrix####
table(testData$Class.variable,predNB)



head(trainData)

trainData1<-trainData[,-10]

head(trainData1)
NB1<-naiveBayes(Class.variable~., 
		data=trainData1)


predNB1<-predict(NB1,testData,type=c("class"))



###Confusion Matrix####
table(testData$Class.variable,predNB1)
















