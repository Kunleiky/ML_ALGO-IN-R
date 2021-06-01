#In this script, we use the Decision Tree Algorithm to fit a model for our data

setwd("C:\\Users\\User\\Desktop\\ADENIYI-reviewed") #Set the working directory so as to easily work the useful files
getwd() #confirm the working directory is set. 

library(rpart) #Import the library useful for implementing the model.

#Read in the data sets into R memory.
train<-read.csv("Train.csv") #Read in the Train data set 
test<-read.csv("Test.csv") #Read in the Test data set
head(train) #Check the first few rows of the Train data set 
str(train) #Check the structure of the Train data set
head(test) #Check the first few rows of the Test data set
str(test)
train_no_ward<-train[-1] #Remove the first column,i.e,ward, from the Train data set since it is not needed 
head(train_no_ward)
test_no_ward<-test[-1] #Remove the first column,i.e,ward, from the Test data set since it is not needed 
head(test_no_ward)

#model<- rpart(train_no_ward$total_households~.,method="anova",data=train_no_ward) #Run the regression
#model #Call the model
#print(model) #Print the regression
#predicted<-predict(model,test_no_ward,method=anova)

train_no_ward_no_tgt_pct<-train_no_ward[-3]
model_no_tgt_pct<- rpart(train_no_ward_no_tgt_pct$total_households~.,method="anova",data=train_no_ward_no_tgt_pct) #Removing the pct_target_vulnerable column to make prediction possible
print(model_no_tgt_pct)

#plot the predicted model on Decision Tree
plot(model_no_tgt_pct,uniform=TRUE,main="Decision Tree from Regresssion")
text(model_no_tgt_pct,use.n = TRUE,cex=.7)


predicted_no_tgt_pct<-predict(model_no_tgt_pct,test_no_ward,method=anova) #Make predictions
print(predicted_no_tgt_pct) #Print the predicted values






#RANDON FOREST
#In this script, we use the Random Forest Algorithm to fit a model for our data

setwd("C:\\Users\\User\\Desktop\\ADENIYI-reviewed") #Set the working directory so as to easily work the useful files
getwd() #confirm the working directory is set 

library(randomForest) #Import the library useful for implementing the model.
#Read in the data sets into R memory.

train<-read.csv("Train.csv") #Read in the Train data set 
test<-read.csv("Test.csv") #Read in the Test data set
head(train) #Check the first few rows of the Train data set 
str(train) #Check the structure of the Train data set
head(test) #Check the first few rows of the Test data set
str(test)
dim(train)
dim(test)
train_no_ward<-train[-1] #Remove the first column,i.e,ward, from the Train data set since it is not needed 
head(train_no_ward)
test_no_ward<-test[-1] #Remove the first column,i.e,ward, from the Test data set since it is not needed 
head(test_no_ward)

modelRF<-randomForest(train_no_ward$total_households~.,train_no_ward,ntree=500) #Run the regression 
modelRF #Call the model
print(modelRF) #Print the result

train_no_ward_no_tgt_pct<-train_no_ward[-3] #Remove the target_pct_vulnerable column also from the train data set to make prediction possible

modelpctRF<- randomForest(train_no_ward_no_tgt_pct$total_households~.,ntree=500,data=train_no_ward_no_tgt_pct) #Run the regression 
predictedpctRF<-predict(modelpctRF,test_no_ward)
print(predictedpctRF)
nrow(train)





#KNN

#In this script, we use the kNN Algorithm to fit a model for our data
setwd("C:\\Users\\User\\Desktop\\ADENIYI-reviewed") #Set the working directory so as to easily work the useful files
getwd() #confirm the working directory is set 

library(class) #Import the library useful for implementing the model.
#Read in the data sets into R memory.
library(caret)

train<-read.csv("Train.csv") #Read in the Train data set 
test<-read.csv("Test.csv") #Read in the Test data set
head(train) #Check the first few rows of the Train data set 
str(train) #Check the structure of the Train data set
head(test) #Check the first few rows of the Test data set
str(test)
train_no_ward<-train[-1] #Remove the first column,i.e,ward, from the Train data set since it is not needed 
head(train_no_ward)
test_no_ward<-test[-1] #Remove the first column,i.e,ward, from the Test data set since it is not needed 
head(test_no_ward)

x_train<-train_no_ward[-1] #This is to exclude the total_households column
x_test<-test_no_ward[-1]  #This is to exclude the total_households column
y_train<-train_no_ward[1]  #Includes only the total_households column
y_test<-test_no_ward[1]    #Includes only the total_households column

#As a rule of thumb, k is taken as the square root of the Train data set no. of observation

modelkNN<-knn(train = x_train,test = x_test,cl=y_train$total_households,k=57) #Run the regression

modelkNN #Call the model

print(modelkNN) #Print the result


predictedkNN<-predict(modelkNN,y_test ) #Make prediction 



dim(train)
dim(test)
nrow(train)
nrow(test)

train_no_ward_no_tgt_pct_vulnerable=train_no_ward[-3] #Remove the target_pct_vulnerable column to make prediction possible
head(train_no_ward_no_tgt_pct_vulnerable) #Confirm it is removed
#From here, we work without the target_pct_vulnerable column

x_train_no_tgt_pct_vulnerable<-train_no_ward_no_tgt_pct_vulnerable[-1] #This is to exclude the total_households column
x_test<-test_no_ward[-1]  #This is to exclude the total_households column
y_train_no_tgt_pct_vulnerable<-train_no_ward_no_tgt_pct_vulnerable[1]  #Includes only the total_households column
y_test<-test_no_ward[1]   #Includes only the total_households column

#As a rule of thumb, k is taken as the square root of the Train data set no. of observation

modelkNN_no_tgt_pct_vulnerable<-knn(train = x_train_no_tgt_pct_vulnerable,test = x_test,cl=y_train_no_tgt_pct_vulnerable$total_households,k=57) #Run the regression
modelkNN_no_tgt_pct_vulnerable
print(modelkNN_no_tgt_pct_vulnerable)

#confusionMatrix(factor(y_train_no_tgt_pct_vulnerable,levels=1:3174),factor(x_train_no_tgt_pct_vulnerable,levels=1:3174))





