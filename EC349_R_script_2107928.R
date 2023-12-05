#Amogh Para, u2107928  





cat("\014")  
rm(list=ls())

setwd("C:/Users/amogh/OneDrive/Desktop/EC349 Repos/EC349 Project Actual/Data")

library(caret)
library(tidyverse)
library(randomForest) # for the random forest
library(rfUtilities) # for model improvement
library(jsonlite)

#Loading datasets
load("yelp_review_small.Rda")
load("yelp_user_small.Rda")
business_data <- stream_in(file("yelp_academic_dataset_business.json"))
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json"))
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json"))

#Data Exploration:
summary(review_data_small) #seeing summaries of datasets
summary(user_data_small)
summary(business_data)
summary(tip_data)
summary(checkin_data)

view(review_data_small)
view(user_data_small)
view(tip_data)
str(review_data_small) # checking structure and variable types of review_data_small
str(user_data_small) # checking structure and variable types of user_data_small
str(tip_data) # ..



#merging datasets
data2 <- merge(tip_data, review_data_small, by=("user_id"))
final_dataset <- merge(data2, user_data_small, by=("user_id"))

#changing date variables to date types
final_dataset$date.x <- as.Date(final_dataset$date.x)
final_dataset$date.y <- as.Date(final_dataset$date.y)
#changing starts to a factor variable
final_dataset$stars <- as.factor(final_dataset$stars)

#creating test and training datasets:
set.seed(1)
parts = createDataPartition(final_dataset$stars, p = 0.75, list = F)
train = final_dataset[parts, ] #creating training data
test = final_dataset[-parts, ] #creating test data
#creating random sample from training 
set.seed(1)
trainsample = train[sample(1:nrow(train), 15000,replace=FALSE),]

#training Random Forest model on training data sample
set.seed(1)
model_RF<-randomForest(stars~.,data=trainsample,ntree = 500, proximity = TRUE)
preds = predict(model_RF, test) #runs model on test data
mean(model_RF[["err.rate"]]) #computes error on test data

#summary of model
model_RF
RFMODEL <- plot(model_RF)

#importances of each model feature
importance_plot<-importance(model_RF)
importance_plot

#plot of Random Forest model OOB error against trees
plot(model_RF)

#find optimal number of variables to choose at split
tune<-tuneRF(trainsample[-7],trainsample$stars,stepFactor = 2,trace=TRUE,plot = TRUE)
tuneRF(trainsample[-7],trainsample$stars,stepFactor = 2,trace=TRUE,plot = TRUE) #excluding stars from first parameter
#above also plots diagram showing decrease in OOB error as mtry increases

#second model run
model_RF<-randomForest(stars~.,data=trainsample,ntree = 250,mtry=20, proximity = TRUE) #Runs random forest with 50% less trees and approx.15 more variables chosen per split
preds2 = predict(model_RF, test) #runs model on test data
mean(model_RF[["err.rate"]]) #computes error rate on test data


##Other Plots(used in markdown file)
review_data_small <- tibble::rowid_to_column(review_data_small, "Count")
stars_frequency<-ggplot(data=review_data_small, aes(x=stars,y=Count))+geom_bar(stat="identity")+ggtitle("Frequency of 'Stars'") #plots frequency of each number of star

#plots horizontal bar graph showing Mean decrease in Gini for the first Random Forest Model
importances<-barplot(importance_plot[, "MeanDecreaseGini"],names.arg = rownames(importance_plot), horiz = TRUE,col = "gray", main = "Feature Importance Plot",las = 1,cex.names = 0.35)
box()



#run model
#predict model on test data
#run model using more trees
#rfutilities thing to improve model
#run model using more mtry (mtry = 10)


