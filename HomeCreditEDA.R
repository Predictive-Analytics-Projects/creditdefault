#Set File Path
setwd("C:/Users/BOBBY/Documents/PREDICT/498/Datasets/Kaggle/HomeCredit/")
my.path <- "/Users/BOBBY/Documents/PREDICT/498/Datasets/Kaggle/HomeCredit/"

library(tidyverse)
library(xgboost)
library(magrittr)
library(data.table)
library(moments)
library(sqldf)
set.seed(0)

# Load the data
#---------------------------
cat("Loading data...\n")

bbalance <- fread("bureau_balance.csv",sep = ",",header = T)
#head(bbalance)
#str(bbalance)

bureau <- fread("bureau.csv",sep = ",",header = T);
cc_balance <- fread("credit_card_balance.csv",sep = ",",header = T);
payments <- fread("installments_payments.csv",sep = ",",header = T); 
pc_balance <- fread("POS_CASH_balance.csv",sep = ",",header = T);
app_prev <- fread("previous_application.csv",sep = ",",header = T);
app_train <- fread("application_train.csv",sep = ",",header = T); 
app_test <- fread("application_test.csv",sep = ",",header = T);
descript <- fread("HomeCredit_columns_description.csv",sep = ",",header = T);

# 1. Conduct exploratory data analysis on the data set prior to 
#    building classification and prediction models.

# bbalance
dim(bbalance)
# Data consist 27299925 observations of 3 variables
names(bbalance)
str(bbalance)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(bbalance,anyNA))
anyNA(bbalance[,-(2:3)])  #Great! No missing data

# bureau
dim(bureau)
# Data consist 1716428 observations of 17 variables
names(bureau)
str(bureau)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(bureau,anyNA)) #7 variables with missing data
anyNA(bureau[,-(2:17)])

# cc_balance
dim(cc_balance)
# Data consist 3840312 observations of 23 variables
names(cc_balance)
str(cc_balance)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(cc_balance,anyNA)) #7 variables with missing data
anyNA(cc_balance[,-(2:17)])  

# payments
dim(payments)
# Data consist 13605401 observations of 8 variables
names(payments)
str(payments)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(payments,anyNA)) #2 variables with missing data
anyNA(payments[,-(2:8)])  

# pc_balance
dim(pc_balance)
# Data consist 10001358 observations of 8 variables
names(pc_balance)
str(pc_balance)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(pc_balance,anyNA)) #2 variables with missing data
anyNA(pc_balance[,-(2:8)])  

# app_prev
dim(app_prev)
# Data consist 1670214 observations of 37 variables
names(app_prev)
str(app_prev)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(app_prev,anyNA)) #14 variables with missing data
anyNA(app_prev[,-(2:37)]) 

# app_train
dim(app_train)
# Data consist 307511 observations of 122 variables
names(app_train)
str(app_train)
# Check for NAs or missing data in predictor variables
which(sapply(app_train,anyNA)) #61 variables with missing data
anyNA(app_train[,-(2:122)]) 

# app_test
dim(app_test)
# Data consist 48744 observations of 121 variables
names(app_test)
str(app_test)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(app_test,anyNA)) #29 variables with missing data
anyNA(app_test[,-(2:121)]) 

# descript
dim(descript)
# Data consist 219 observations of 5 variables
names(descript)
str(descript)
# Check for NAs or missing data in predictor variables - exclude response variables (donr and damt)
which(sapply(descript,anyNA)) #2 variables with missing data
anyNA(descript[,-(2:5)]) 

############################
# Deeper Dive into app_train
summary(app_train)

# View breakdown of TARGET
table(app_train$TARGET)
#0      1 
#282686  24825 

# Bar Plot of Target
count_0 = sum(app_train$TARGET==0)
count_1 = sum(app_train$TARGET==1)
CountByTarget<- c(count_0, count_1)
Target <- c("0", "1")
xx <- barplot(CountByTarget, names = Target,
              xlab = "Target", ylab = "Count", main = "Record count by Target", ylim = c(0,320000))
text(x=xx, y = CountByTarget, label = CountByTarget, pos = 3, cex = 0.8, col = "red")

str(app_train)
table(app_train$NAME_CONTRACT_TYPE)

# View Target 1 breakdown by NAME_CONTRACT_TYPE
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$NAME_CONTRACT_TYPE)

pct_1 <- sum(app_train.target1$NAME_CONTRACT_TYPE=="Cash loans")/sum(table(app_train.target1$NAME_CONTRACT_TYPE))
pct_2 <- sum(app_train.target1$NAME_CONTRACT_TYPE=="Revolving loans")/sum(table(app_train.target1$NAME_CONTRACT_TYPE))
Pct <- c(pct_1, pct_2)
Sbjct <- c("Cash loans", "Revolving loans")
xx <- barplot(Pct, names = Sbjct,
              xlab = "NAME_CONTRACT_TYPE", ylab = "Percentage", main = "% of Late Payment by NAME_CONTRACT_TYPE", ylim = c(0,1))
text(x=xx, y = Pct, label = Pct, pos = 3, cex = 0.8, col = "red")

# View Target 1 breakdown by CODE_GENDER
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$CODE_GENDER)

pct_1 <- sum(app_train.target1$CODE_GENDER=="F")/sum(table(app_train.target1$CODE_GENDER))
pct_2 <- sum(app_train.target1$CODE_GENDER=="M")/sum(table(app_train.target1$CODE_GENDER))
Pct <- c(pct_1, pct_2)
Sbjct <- c("F", "M")
xx <- barplot(Pct, names = Sbjct,
              xlab = "CODE_GENDER", ylab = "Percentage", main = "% of Late Payment by CODE_GENDER", ylim = c(0,1))
text(x=xx, y = Pct, label = Pct, pos = 3, cex = 0.8, col = "red")

# View Target 1 breakdown by FLAG_OWN_CAR
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$FLAG_OWN_CAR)

pct_1 <- sum(app_train.target1$FLAG_OWN_CAR=="N")/sum(table(app_train.target1$FLAG_OWN_CAR))
pct_2 <- sum(app_train.target1$FLAG_OWN_CAR=="Y")/sum(table(app_train.target1$FLAG_OWN_CAR))
Pct <- c(pct_1, pct_2)
Sbjct <- c("N", "Y")
xx <- barplot(Pct, names = Sbjct,
              xlab = "FLAG_OWN_CAR", ylab = "Percentage", main = "% of Late Payment by FLAG_OWN_CAR", ylim = c(0,1))
text(x=xx, y = Pct, label = Pct, pos = 3, cex = 0.8, col = "red")

# View Target 1 breakdown by FLAG_OWN_REALTY
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$FLAG_OWN_REALTY)

pct_1 <- sum(app_train.target1$FLAG_OWN_REALTY=="N")/sum(table(app_train.target1$FLAG_OWN_REALTY))
pct_2 <- sum(app_train.target1$FLAG_OWN_REALTY=="Y")/sum(table(app_train.target1$FLAG_OWN_REALTY))
Pct <- c(pct_1, pct_2)
Sbjct <- c("N", "Y")
xx <- barplot(Pct, names = Sbjct,
              xlab = "FLAG_OWN_REALTY", ylab = "Percentage", main = "% of Late Payment by FLAG_OWN_REALTY", ylim = c(0,1))
text(x=xx, y = Pct, label = Pct, pos = 3, cex = 0.8, col = "red")


# View Target 1 breakdown by CNT_CHILDREN
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$CNT_CHILDREN)

hist(app_train.target1$CNT_CHILDREN)


# View Target 1 breakdown by NAME_EDUCATION_TYPE
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$NAME_EDUCATION_TYPE)

pct_1 <- sum(app_train.target1$NAME_EDUCATION_TYPE=="Academic degree")/sum(table(app_train.target1$NAME_EDUCATION_TYPE))
pct_2 <- sum(app_train.target1$NAME_EDUCATION_TYPE=="Higher education")/sum(table(app_train.target1$NAME_EDUCATION_TYPE))
pct_3 <- sum(app_train.target1$NAME_EDUCATION_TYPE=="Incomplete higher")/sum(table(app_train.target1$NAME_EDUCATION_TYPE))
pct_4 <- sum(app_train.target1$NAME_EDUCATION_TYPE=="Lower secondary")/sum(table(app_train.target1$NAME_EDUCATION_TYPE))
pct_5 <- sum(app_train.target1$NAME_EDUCATION_TYPE=="Secondary / secondary special")/sum(table(app_train.target1$NAME_EDUCATION_TYPE))

Pct <- c(pct_1, pct_2, pct_3, pct_4, pct_5)
Sbjct <- c("Academic degree", "Higher education", "Incomplete higher", "Lower secondary","Secondary / secondary special" )
xx <- barplot(Pct, names = Sbjct,
              xlab = "NAME_EDUCATION_TYPE", ylab = "Percentage", main = "% of Late Payment by NAME_EDUCATION_TYPE", ylim = c(0,.8))
text(x=xx, y = Pct, label = Pct, pos = 3, cex = 0.8, col = "red")

# View Target 1 breakdown by NAME_FAMILY_STATUS
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$NAME_FAMILY_STATUS)

pct_1 <- sum(app_train.target1$NAME_FAMILY_STATUS=="Civil marriage")/sum(table(app_train.target1$NAME_FAMILY_STATUS))
pct_2 <- sum(app_train.target1$NAME_FAMILY_STATUS=="Married")/sum(table(app_train.target1$NAME_FAMILY_STATUS))
pct_3 <- sum(app_train.target1$NAME_FAMILY_STATUS=="Separated")/sum(table(app_train.target1$NAME_FAMILY_STATUS))
pct_4 <- sum(app_train.target1$NAME_FAMILY_STATUS=="Single / not married")/sum(table(app_train.target1$NAME_FAMILY_STATUS))
pct_5 <- sum(app_train.target1$NAME_FAMILY_STATUS=="Widow")/sum(table(app_train.target1$NAME_FAMILY_STATUS))

Pct <- c(pct_1, pct_2, pct_3, pct_4, pct_5)
Sbjct <- c("Civil marriage", "Married","Separated","Single / not married","Widow")
xx <- barplot(Pct, names = Sbjct,
              xlab = "NAME_FAMILY_STATUS", ylab = "Percentage", main = "% of Late Payment by NAME_FAMILY_STATUS", ylim = c(0,1))
text(x=xx, y = Pct, labels = Pct, las=2, pos = 3, cex = 0.8, col = "red")

# View Target 1 breakdown by OCCUPATION_TYPE
app_train.target1<-app_train[app_train$TARGET==1,]

table(app_train.target1$OCCUPATION_TYPE)

pct_1 <- sum(app_train.target1$OCCUPATION_TYPE=="")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_2 <- sum(app_train.target1$OCCUPATION_TYPE=="Accountants")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_3 <- sum(app_train.target1$OCCUPATION_TYPE=="Cleaning staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_4 <- sum(app_train.target1$OCCUPATION_TYPE=="Cooking staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_5 <- sum(app_train.target1$OCCUPATION_TYPE=="Core staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_6 <- sum(app_train.target1$OCCUPATION_TYPE=="Drivers")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_7 <- sum(app_train.target1$OCCUPATION_TYPE=="High skill tech staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_8 <- sum(app_train.target1$OCCUPATION_TYPE=="HR staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_9 <- sum(app_train.target1$OCCUPATION_TYPE=="IT staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_10 <- sum(app_train.target1$OCCUPATION_TYPE=="Laborers")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_11 <- sum(app_train.target1$OCCUPATION_TYPE=="Low-skill Laborers")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_12 <- sum(app_train.target1$OCCUPATION_TYPE=="Managers")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_13 <- sum(app_train.target1$OCCUPATION_TYPE=="Medicine staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_14 <- sum(app_train.target1$OCCUPATION_TYPE=="Private service staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_15 <- sum(app_train.target1$OCCUPATION_TYPE=="Realty agents")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_16 <- sum(app_train.target1$OCCUPATION_TYPE=="Sales staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_17 <- sum(app_train.target1$OCCUPATION_TYPE=="Secretaries")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_18 <- sum(app_train.target1$OCCUPATION_TYPE=="Security staff")/sum(table(app_train.target1$OCCUPATION_TYPE))
pct_19 <- sum(app_train.target1$OCCUPATION_TYPE=="Waiters/barmen staff")/sum(table(app_train.target1$OCCUPATION_TYPE))

Pct <- c(pct_1, pct_2, pct_3, pct_4, pct_5, pct_6, pct_7, pct_8, pct_9, pct_10, pct_11, pct_12, pct_13, pct_14, pct_15, pct_16, pct_17, pct_18, pct_19)
Sbjct <- c("Others", "Accountants","Cleaning staff","Cooking staff","Core staff","Drivers","High skill tech staff","HR staff","IT staff","Laborers","Low-skill Laborers","Managers","Medicine staff","Private service staff","Realty agents","Sales staff","Secretaries","Security staff","Waiters/barmen staff")
xx <- barplot(Pct, names = Sbjct,
              xlab = "OCCUPATION_TYPE", ylab = "Percentage", main = "% of Late Payment by OCCUPATION_TYPE", ylim = c(0,1))
text(x=xx, y = Pct, label = Pct, pos = 3, cex = 0.8, col = "red")


boxplot(app_train.target0$EXT_SOURCE_1,app_train.target1$EXT_SOURCE_1,
        main = "Boxplot for EXT_SOURCE_1",
        at = c(1,2),
        names = c("On Time", "Late"),
        las = 2,
        col = c("blue","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)

boxplot(app_train.target0$EXT_SOURCE_2,app_train.target1$EXT_SOURCE_2,
        main = "Boxplot for EXT_SOURCE_2",
        at = c(1,2),
        names = c("On Time", "Late"),
        las = 2,
        col = c("blue","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)

boxplot(app_train.target0$EXT_SOURCE_3,app_train.target1$EXT_SOURCE_3,
        main = "Boxplot for EXT_SOURCE_3",
        at = c(1,2),
        names = c("On Time", "Late"),
        las = 2,
        col = c("blue","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)

boxplot(app_train.target0$AMT_INCOME_TOTAL,app_train.target1$AMT_INCOME_TOTAL,
        main = "Boxplot for AMT_INCOME_TOTAL",
        at = c(1,2),
        names = c("On Time", "Late"),
        las = 2,
        col = c("blue","red"),
        border = "brown",
        horizontal = FALSE,
        notch = TRUE
)

Sbjct <- names(table(app_train.target1$OCCUPATION_TYPE))
names(table(mtcars$cyl))

app_train.target0<-app_train[app_train$TARGET==0,]
summary(app_train$AMT_INCOME_TOTAL)
summary(app_train.target1$AMT_INCOME_TOTAL)
boxplot(app_train.target1$AMT_INCOME_TOTAL)
summary(app_train.target0$AMT_INCOME_TOTAL)

str(descript)
sqldf("SELECT DISTINCT Table FROM descript")