#==============================================================================#
#                                LIBRARIES                                     #
#==============================================================================#
library(tidyverse)
library(readxl)
library(stringr)
library(purrr)
library(ggplot2)
library(janitor)
library(quanteda)
library(quanteda.textplots)
library(countrycode)
library(tm)
library(wordcloud)
library(wordcloud2)
library(MLeval)
library(funModeling)
library(kernlab)
library(caret)
#==============================================================================#
#                              PARALLEL PROCESSING                             #
#==============================================================================#

#Run these codes to enable parallel processing
library(parallel) 

no_cores <- detectCores() - 4# Calculate the number of cores (subtract 4, so you don't tie it up)

library(doParallel)

cl <- makePSOCKcluster(no_cores) # create the cluster for caret to use
registerDoParallel(cl)


## Run the machine learning code models
## Do not forget to release the cores afterwards (see the next section for details)



#use code below after you run models to make R Studio release cores
stopCluster(cl)
registerDoSEQ()

######################End of Parallel processing################################


#==============================================================================#
#                              DATA INITIALIZATION                             #
#==============================================================================#

hoteldata <- data.frame(read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv")) #puts CSV into a dataframe called data
hoteldata <- mutate_if(hoteldata, is.character, factor) #makes the categoricals as factors
hoteldata$IsCanceled <-  as.factor(hoteldata$IsCanceled) #makes the binary as factors
hoteldata$IsRepeatedGuest <-  as.factor(hoteldata$IsRepeatedGuest) #makes the binary as factors
levels(hoteldata$Meal) <- list(UndefinedSC=c("Undefined","SC"),BB=c("BB"),FB=c("FB"),HB=c("HB")) #combine Undefined and SC into one (since they're the same)
RepeatGuestData <- hoteldata %>%  filter(IsRepeatedGuest == 1)
FirstTimeGuestData <- hoteldata %>%  filter(IsRepeatedGuest == 0)
data <- hoteldata #making another copy of the data
#==============================================================================#
#                       SUPERVISED LEARNING MODELS                             #
#==============================================================================#

##########################Using svm.fold model##################################


set.seed(111) #setting a seed so all training is reproducable






################Repeat Guest Data ##############################
RGfiltered_data <- RepeatGuestData %>% select(-Country) # removing country because it is a (near) zero-variant data
levels(RGfiltered_data$IsCanceled) <- c("NO","YES")
levels(RGfiltered_data$IsRepeatedGuest) <- c("NO","YES")

RGtrainList <- createDataPartition(y=RGfiltered_data$IsCanceled, p=.70,list=FALSE) #creating a partition with a 70/30 split

RGtrainSet <- RGfiltered_data[RGtrainList,] #creating the training dataset for repeat guests
RGtestSet <- RGfiltered_data[-RGtrainList,] #creating the test dataset for repeat guests
########################################################################################



filtered_data <- data %>% select(-Country) # removing country because it is a (near) zero-variant data
levels(filtered_data$IsCanceled) <- c("NO","YES") #changing the 0s and 1s to No and Yes for easy of use (and a mleval package will work better if we want to use it)
levels(filtered_data$IsRepeatedGuest) <- c("NO","YES") #changing the 0s and 1s to No and Yes for easy of use (and a mleval package will work better if we want to use it)

trainList <- createDataPartition(y=filtered_data$IsCanceled, p=.70,list=FALSE)

trainSet <- filtered_data[trainList,] #creating the training dataset
testSet <- filtered_data[-trainList,] #creating the test dataset 

trctrl <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs = TRUE, savePredictions = T)



###########################SVM MODEL################################################
StartTime <- Sys.time() #getting the time right before the model is run 
model_svm <- train(IsCanceled ~., data=trainSet,method="svmRadial",trControl=trctrl,  preProcess=c("center", "scale"),tuneLength=3) 
StopTime <- Sys.time() #getting the time after the model is run
StopTime-StartTime #showing how long it took to run the model


predictValues_svm <-  predict(model_svm, newdata = testSet) #doing predictions and comparing to the test set
confusionMatrix(predictValues_svm, testSet$IsCanceled)

###############rpart()#######################
StartTime <- Sys.time() #getting the time right before the model is run 
model_rpart <- train(IsCanceled ~., data=trainSet,method="rpart",trControl=trctrl,  preProcess=c("center", "scale"),tuneLength=3) 
StopTime <- Sys.time() #getting the time after the model is run
StopTime-StartTime #showing how long it took to run the model



predictValues_rpart <-  predict(model_rpart, newdata = testSet)#doing predictions and comparing to the test set
confusionMatrix(predictValues_rpart, testSet$IsCanceled)

#### xgbtree####

StartTime <- Sys.time() #getting the time right before the model is run 
model_xgbtree <- train(IsCanceled ~., data=trainSet,method="xgbTree",trControl=trctrl,  preProcess=c("center", "scale"),tuneLength=3) 
StopTime <- Sys.time() #getting the time after the model is run
StopTime-StartTime #showing how long it took to run the model


predictValues_xgbtree <-  predict(model_xgbtree, newdata = testSet) #doing predictions and comparing to the test set
confusionMatrix(predictValues_xgbtree, testSet$IsCanceled)
varImp(model_xgbtree)


#######ranger/random forest####################
stopCluster(cl) #ranger model has built in parallezation 
registerDoSEQ() #so the parallel library code can stop here


StartTime <- Sys.time() #getting the time right before the model is run 
model_ranger <- train(IsCanceled ~., data=trainSet,method="ranger",trControl=trctrl,  preProcess=c("center", "scale"),tuneLength=3,  importance = 'permutation', num.threads = 12) 
StopTime <- Sys.time() #getting the time after the model is run
StopTime-StartTime #showing how long it took to run the model



predictValues_ranger <-  predict(model_ranger, newdata = testSet) #doing predictions and comparing to the test set
confusionMatrix(predictValues_ranger, testSet$IsCanceled)
varImp(model_ranger) #showing the 20 most important variables
varImp(model_ranger)$importance  #gives more than 20 variables


#running the model again, this time only for repeat guests

StartTime <- Sys.time() #getting the time right before the model is run 
model_ranger <- train(IsCanceled ~., data=RGtrainSet,method="ranger",trControl=trctrl,  preProcess=c("center", "scale"),tuneLength=3,  importance = 'permutation', num.threads = 12) 
StopTime <- Sys.time() #getting the time after the model is run
StopTime-StartTime #showing how long it took to run the model


predictValues_ranger <-  predict(model_ranger, newdata = RGtestSet) #doing predictions and comparing to the test set
confusionMatrix(predictValues_ranger, RGtestSet$IsCanceled) #
varImp(model_ranger) #showing the 20 most important variables

##################RG FOREST




