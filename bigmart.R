#Set working Directory
setwd('C:/Users/Administrator/Downloads/bigmart')
train <- read.csv('train_kOBLwZA.csv', na.strings = c(""))
test <- read.csv('test_t02dQwI.csv', na.strings = c(""))
#Audit the data
str(train)
sum(is.na(train))
#Impute Missing Values for Item_Weight
train$Item_Weight <- ifelse(is.na(train$Item_Weight), 
                            median(train$Item_Weight, na.rm = TRUE),
                            train$Item_Weight)
sum(is.na(train$Item_Weight))
#Handle Inconsistency for Item Fat Content
table(train$Item_Fat_Content)
train$Item_Fat_Content <- as.character(train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content =='LF',
                                 'Low Fat', train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content =='low fat',
                                 'Low Fat', train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content =='reg',
                                 'Regular', train$Item_Fat_Content)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
#Missing Values for Outlet_Size
table(train$Outlet_Size)
sum(is.na(train$Outlet_Size))
train$Outlet_Size <- as.character(train$Outlet_Size)
train$Outlet_Size <- ifelse(is.na(train$Outlet_Size), 'Medium',
                            train$Outlet_Size)
train$Outlet_Size <- as.factor(train$Outlet_Size)

train$YOB <- 2019 - train$Outlet_Establishment_Year

library(e1071)
skewness(train$Item_Weight)
boxplot(train$Item_Weight)
skewness(train$Item_Visibility)
boxplot(train$Item_Visibility)
skewness(train$Item_MRP)
boxplot(train$Item_MRP)
skewness(train$Item_Outlet_Sales)
boxplot(train$Item_Outlet_Sales)
skewness(train$YOB)
boxplot(train$YOB)
#######Univariate Analysis on Data#################
describe(train)
summary(train$Item_Visibility)
###########Bi-Variate Analysis on Data#################
numdf <- subset(train, select = c('Item_Outlet_Sales','YOB','Item_Visibility',
                                  'Item_MRP','Item_Weight'))
library(Hmisc)
cormat <-rcorr(as.matrix(numdf))
cormat <- as.data.frame(cormat$r)
############Multi-Variate Analysis on Data########
names(train)
model <- lm(Item_Outlet_Sales~Item_Weight+
              Item_Fat_Content+Item_Visibility+
              Item_Type+Item_MRP+Outlet_Size+
              Outlet_Location_Type+Outlet_Type+YOB, data = train)


summary(model)

train$preds_reg <- predict(model, train)
rmse <- sqrt(mean((train$Item_Outlet_Sales-train$preds_reg)^2))
print(rmse)

############################Random Forest Bigmart######################
library(randomForest)
rf <- randomForest(Item_Outlet_Sales~Item_Weight+
                     Item_Fat_Content+Item_Visibility+
                     Item_Type+Item_MRP+Outlet_Size+
                     Outlet_Location_Type+Outlet_Type+YOB, data = train)

train$preds_rf <- predict(rf, train)
rmse_rf <- sqrt(mean((train$Item_Outlet_Sales-train$preds_rf)^2))
print(rmse_rf)

#####################Test Data cleaning######################

test$Item_Weight <- ifelse(is.na(test$Item_Weight), 
                           median(test$Item_Weight, na.rm = TRUE),
                           test$Item_Weight)

test$Item_Fat_Content <- as.character(test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content =='LF',
                                'Low Fat', test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content =='low fat',
                                'Low Fat', test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content =='reg',
                                'Regular', test$Item_Fat_Content)
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)


test$Outlet_Size <- as.character(test$Outlet_Size)
test$Outlet_Size <- ifelse(is.na(test$Outlet_Size), 'Medium',
                           test$Outlet_Size)
test$Outlet_Size <- as.factor(test$Outlet_Size)

test$YOB <- 2019 - test$Outlet_Establishment_Year

test$Item_Outlet_Sales <- predict(rf, test)

write.csv(test, "finalfile.csv")








