#Setting the working directory
setwd('F:\\Bigmart')
#Import the train and test
train <- read.csv('train_kOBLwZA.csv', header = TRUE, sep = ',',
                  na.strings = c("NA","NaN", " "))
test <- read.csv('test_t02dQwI.csv', header = TRUE, sep = ',',
                 na.strings = c("NA", "NaN", " "))
#Audit the data
str(train)
#Summary
summary(train)
#Imputation of Missing values - Item_Weight
train$Item_Weight <- ifelse(is.na(train$Item_Weight),
                            median(train$Item_Weight,na.rm = TRUE),
                            train$Item_Weight)
sum(is.na(train$Item_Weight))
median(train$Item_Weight,na.rm = TRUE)
#Solve inconsistency of Item_Fat_Content
train$Item_Fat_Content <- as.character(train$Item_Fat_Content)
is.character(train$Item_Fat_Content)

train$Item_Fat_Content <- ifelse(train$Item_Fat_Content=='LF',
                                 'Low Fat',
                                 train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content=='low fat',
                                 'Low Fat', train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content=='reg',
                                 'Regular', train$Item_Fat_Content )
table(train$Item_Fat_Content)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
str(train)
summary(train)

train$Outlet_Size <- as.character(train$Outlet_Size)

train$Outlet_Size <- ifelse(train$Outlet_Size=='',
                            'Medium',
                            train$Outlet_Size)

train$Outlet_Size <- as.factor(train$Outlet_Size)
summary(train)

library(e1071)
skewness(train$Item_Weight)
boxplot(train$Item_Weight)

skewness(train$Item_Visibility)
boxplot(train$Item_Visibility)

skewness(train$Item_MRP)
boxplot(train$Item_MRP)

skewness(train$Item_Outlet_Sales)
boxplot(train$Item_Outlet_Sales)

tbl = table(train$Outlet_Location_Type, train$Outlet_Size)
#install.packages('MASS')
library(MASS)
chisq.test(tbl)


tbl1 <- table(train$Item_Fat_Content, train$Item_Type)
chisq.test(tbl1)
tbl1

tbl2 <- table(train$Outlet_Type, train$Outlet_Location_Type)
tbl2

chisq.test(tbl2)

cor(train$Item_Weight, train$Item_Visibility)
cor(train$Item_Weight, train$Item_MRP)
cor(train$Item_Visibility, train$Item_MRP)
cor(train$Item_MRP, train$Item_Outlet_Sales)
cor(train$Item_Weight, train$Item_Outlet_Sales)
cor(train$Item_Visibility, train$Item_Outlet_Sales)

#install.packages('Hmisc')
library(Hmisc)
describe(train)

plot(train$Item_MRP, train$Item_Outlet_Sales)
plot(train$Item_Weight, train$Item_MRP)
plot(train$Item_Visibility, train$Item_MRP)

train$Ln_Item_Weight <- log(train$Item_Weight)
skewness(train$Ln_Item_Weight)

train$LN_Sales <- log(train$Item_Outlet_Sales)
skewness(train$LN_Sales)
skewness(train$Item_Outlet_Sales)

train$Item_Weight_Scale <- scale(train$Item_Weight)
skewness(train$Item_Weight_Scale)
skewness(train$Item_Weight)

names(train)
#Creating a new variable
train$YOB <- 2019-train$Outlet_Establishment_Year
#Apply a regression
model <- lm(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_MRP+YOB, data = train)

summary(model)

train$preds_sales <- predict(model,train)

rmse_model <- sqrt(mean((train$Item_Outlet_Sales-train$preds_sales)^2))
print(rmse)
#Apply a regression with new variables
names(train)
model1 <- lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility
             +Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+YOB,
             data = train)


summary(model1)

train$preds_sales_model1 <- predict(model1, train)

rmse_model1 <- sqrt(mean((train$Item_Outlet_Sales-train$preds_sales_model1)^2))
print(rmse_model1)

################################Preparation of Test Data#####################



test$Item_Weight <- ifelse(is.na(test$Item_Weight),
                           median(test$Item_Weight,na.rm = TRUE),
                           test$Item_Weight)
sum(is.na(test$Item_Weight))
median(test$Item_Weight,na.rm = TRUE)
#Solve inconsistency of Item_Fat_Content
test$Item_Fat_Content <- as.character(test$Item_Fat_Content)
is.character(test$Item_Fat_Content)

test$Item_Fat_Content <- ifelse(test$Item_Fat_Content=='LF',
                                'Low Fat',
                                test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content=='low fat',
                                'Low Fat', test$Item_Fat_Content)
test$Item_Fat_Content <- ifelse(test$Item_Fat_Content=='reg',
                                'Regular', test$Item_Fat_Content )
table(test$Item_Fat_Content)
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)
str(test)
summary(test)

test$Outlet_Size <- as.character(test$Outlet_Size)

test$Outlet_Size <- ifelse(test$Outlet_Size=='',
                           'Medium',
                           test$Outlet_Size)

test$Outlet_Size <- as.factor(test$Outlet_Size)
test$YOB <- 2019 - test$Outlet_Establishment_Year

summary(test)

test$Sale_preds_reg <- predict(model1, test)