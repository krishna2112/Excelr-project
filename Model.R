x <- read.csv(file.choose())
View(x)
str(x)
attach(x)
summary(x)

xhalf <- x[c(1:400000),]
View(xhalf)

dt = sort(sample(nrow(xhalf), nrow(xhalf)*.6))
train<-x[dt,]
test<-x[-dt,]

View(train)
x1 <- train[,-c(1,3,4,6,7,8,14,18,19,26,27,28,29,30,32,33,34)]

xtest <- test[,-c(1,3,4,6,7,8,14,18,19,26,27,28,29,30,32,33,34)]

attach(x1)


###x1 <- train[,-c(2,5,11,15,22,23,36)]
###  c(--1,3,4,6,7,12,14,17,18,19,26,27,28,29,30,32,33,34,)


EDA.

## Replacing of null values with the mean value of the same column.

x2<-x1
x2[] <- lapply(x1, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x})


x2test<-xtest
x2test[] <- lapply(xtest, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x})



z_scores <- as.data.frame(sapply(x2, function(x2) (abs(x2-mean(x2))/sd(x2))))


xs <- as.scale()

#########
str(x2)


 x2$Experience <- as.factor(x2$Experience)
 x2$home_ownership <- as.factor(x2$home_ownership)
x2$verification_status <- as.factor(x2$verification_status)
x2$purpose <- as.factor(x2$purpose) 
x2$initial_list_status <- as.factor(x2$initial_list_status)
x2$application_type <- as.factor(x2$application_type)
 
str(x2)
scale(x2)

#########  Normalisation of the data..
#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply Min-Max normalization to first four columns in iris dataset
x2norm <- as.data.frame(lapply(x2[c(1,2,5,8:14,16,18,19)], min_max_norm))

#view first six rows of normalized iris dataset
head(x2norm)

#add back character variables
x2norm1 <- data.frame(x2norm,x2$Experience,x2$home_ownership,x2$verification_status,
                      x2$purpose,x2$initial_list_status,x2$application_type)

#view first six rows of iris_norm
head(x2norm1)

str(x2norm1)

#  Find the correlation b/w Output Scatter plot
pairs(x2)
library(psych)
pairs.panels(x2)

# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Cars)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(x2))
cor(x2)


###  PCA
cor(data)
pcaObj<-princomp(x2norm, cor = TRUE, scores = TRUE, covmat = NULL)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj) 



attach(x2)

####outliers

boxplot(x2)$out
library(ggstatsplot)
ggbetweenstats(x2,
               annual_inc, tot_curr_bal, outlier.tagging = TRUE)
## Graphical plots:

## 1.)Loan amount

hist(x2norm$loan_amnt, xlab="Loan Amount",main="Histogram of Loan Amount")

boxplot(x2norm$loan_amnt,ylab="Loan Amount", main="Boxplot for the Loan Amount")

plot(loan_amnt)

qqnorm(x2norm$loan_amnt)

library(ggplot2)
qplot(seq_along(loan_amnt), loan_amnt)
ggplot(mapping=aes(x=seq_along(loan_amnt), y=loan_amnt)) +
  geom_point()

summary(x2norm$loan_amnt)

quantile(x2norm$loan_amnt,c(0.25,0.5,0.75))

var(x2norm$loan_amnt)
sd(x2norm$loan_amnt)

## 2.)Annaul Income--- struc
hist(x2norm1$annual_inc, xlab="Annaul Income",main="Histogram of Annaul Income")

boxplot(x2norm1$annual_inc,ylab="Annaul Income", main="Boxplot for the Annaul Income")

qqnorm(x2$annual_inc)


View(x2norm1$annual_inc)
x2111 <- log(x2norm1$annual_inc)

hist(x2111)

boxplot(x2111)
head(x2111)
View(x2111)
qqnorm(x2111)

var(x2111)
summary(x2111)
sd(x2111)

boxplot(x2111)

var(x2$annual_inc)
sd(x2$annual_inc)


######## Rate of interest

hist(x2$Rate_of_intrst, xlab="ROI",main="Histogram of ROI")

boxplot(x2$Rate_of_intrst,ylab="ROI", main="Boxplot for the ROI")

qqnorm(x2$Rate_of_intrst)

summary(xRate_of_intrst)
var(x2$Rate_of_intrst)
sd(x2$Rate_of_intrst)


######### debt - income ratio-- struct

hist(x2norm1$debt_income_ratio, xlab="DIR",main="Histogram of DIR")

boxplot(x2norm1$debt_income_ratio,ylab="DIR", main="Boxplot for the DIR")

qqnorm(x2$debt_income_ratio)

summary(x2norm1$debt_income_ratio)
var(x2$debt_income_ratio)
sd(x2$debt_income_ratio)

x2DIR <- log(x2norm1$debt_income_ratio)
View(x2DIR)
head(x2DIR)
qqnorm(x2DIR)
var(x2DIR)
summary(x2DIR)
sd(x2DIR)

hist(x2DIR)
boxplot(x2DIR)

######delinq 2years--struc( remove)
hist(x2norm1$delinq_2yrs, xlab="DEL2y",main="Histogram of DEL2y")

boxplot(x2norm1$delinq_2yrs,ylab="DEL2y", main="Boxplot for the DEL2y")




##########experience
x2$Experience <- as.numeric(x2$Experience)
hist(x2$Experience, xlab="Experience",main="Histogram of Experience")

boxplot(x2$Experience,ylab="Experience", main="Boxplot for the Experienc")

qqnorm(x2$Experience)

summary(x2$Experience)
var(x2$Experience)
sd(x2$Experience)


####### inq last 6 months----   COnvert in to factor variable
View(x2$inq_last_6mths)
hist(x2$inq_last_6mths, xlab="inq 6 M",main="Histogram of inq 6 M")

boxplot(x2$inq_last_6mths,ylab="inq 6 M", main="Boxplot for the inq 6 M")

qqnorm(x2$inq_last_6mths)

summary(x2$Experience)
var(x2$Experience)
sd(x2$Experience)

x2last6 <- scale(x2$inq_last_6mths)
View(x2last6)
qqnorm(x2last6)
var(x2last6)
summary(x2last6)
sd(x2last6)
boxplot(x2last6)
hist(x2last6)

######numb Credit
hist(x2$numb_credit, xlab="NC",main="Histogram of NC")

boxplot(x2$numb_credit,ylab="NC", main="Boxplot for the NC")

outlier_values <- boxplot.stats(x2$numb_credit)$out  # outlier values.
boxplot(x2$numb_credit, main="NC", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)



qqnorm(x2$numb_credit)

summary(x2$numb_credit)
var(x2$numb_credit)
sd(x2$numb_credit)


###total collecting amount--- structu( remove this column) -- INF values 
hist(x2$tot_colle_amt, xlab="TCAm",main="Histogram of TCAm")

boxplot(x2$tot_colle_amt,ylab="TCAm", main="Boxplot for the TCAm")

qqnorm(x2$tot_colle_amt)

summary(x2$tot_colle_amt)
var(x2$tot_colle_amt)
sd(x2$tot_colle_amt)

x222 <- log(x2norm1$tot_colle_amt)

hist(x222)

boxplot(x222)
head(x222)
View(x222)
qqnorm(x2111)

var(x2111)
summary(x222)
sd(x2111)


#########total current balance-- Struct(  column)
hist(x2norm1$tot_curr_bal, xlab="TCB",main="Histogram of TCB")

boxplot(x2norm1$tot_curr_bal,ylab="TCB", main="Boxplot for the TCB")

qqnorm(x2$tot_curr_bal)

summary(x2$tot_curr_bal)
var(x2$tot_curr_bal)
sd(x2$tot_curr_bal)

x2tcb <- log(x2$tot_curr_bal)
View(x2tcb)
summary(x2tcb)
var(x2tcb)
sd(x2tcb)
boxplot(x2tcb)
hist(x2tcb)

######
####total  recovery inter
hist(x2norm1$total_rec_int, xlab="Tri",main="Histogram of Tri")

boxplot(x2norm1$total_rec_int,ylab="Tri", main="Boxplot for the Tri")
qqnorm(x2$total_rec_int)

summary(x2$total_rec_int)
var(x2$total_rec_int)
sd(x2$total_rec_int)




####total credits
hist(x2$total_credits, xlab="TCr",main="Histogram of TCr")

boxplot(x2$total_credits,ylab="TCr", main="Boxplot for the TCr")
qqnorm(x2$total_credits)

summary(x2$total_credits)
var(x2$total_credits)
sd(x2$total_credits)

qqnorm(x2$total_credits)

summary(x2$total_credits)
var(x2$total_credits)
sd(x2$total_credits)




## 3.)Relation b/w Loan amount & Annaul income.
library(ggplot2)
ggplot(data=x2, mapping = aes(x=loan_amnt, y=annual_inc)) +
  geom_point()+
  labs(ylab="Annaul Income",xlab="Loan Amount",title = "Relationship between Annaul Income $ Loan Amount")
######### Linearity

plot(x2$total.revol_bal~x2$Rate_of_intrst,data = x2)
plot(x2$total.revol_bal~x2$loan_amnt,data = x2)

##4.)Total revolving balance
hist(x2norm1$total.revol_bal, xlab="Revolving Balance",main="Histogram of Total revolving balance",xlim = )
hist(x2norm1$total.revol_bal, breaks = "Sturges",
     freq = 50,  col = "lightgray", border = NULL,
     main = paste("Histogram of Total revolving balance"),
     xlab = "Revolving Balance" )
View(x2$total.revol_bal)

boxplot(x2$total.revol_bal,ylab="Total revolving balance", main="Boxplot for the Total revolving balance")

qqnorm(x2$total.revol_bal)

summary(x2$total.revol_bal)

var(x2$total.revol_bal)
sd(x2$total.revol_bal)


## 5.) Relation b/w Loan amount & Total Revolving Balance.
library(ggplot2)
ggplot(data=x2, mapping = aes(x=loan_amnt, y=total.revol_bal)) +
  geom_point()+
  labs(ylab="Annaul Income",xlab="Total revolving balance",title = "Relationship between Annaul Income & Total revolving balance")

##6.) Total current balance

hist(x2$tot_curr_bal, xlab="Current balance",main="Histogram of Total current balance")

boxplot(x2$tot_curr_bal,ylab="Total current balance", main="Boxplot for the Total current balance")

qqnorm(x2$tot_curr_bal)

summary(x2$tot_curr_bal)
var(x2$tot_curr_bal)
sd(x2$tot_curr_bal)


##7.) relation b/w current balance & Total revolving balance

library(ggplot2)
ggplot(data=x2, mapping = aes(x=x2$total.revol_bal, y=x2$total.revol_bal)) +
  geom_point()+
  labs(ylab="Revolving balance",xlab="Total current balance",title = "Relationship between Current balance & Total revolving balance")


##8.) Terms

table(x$grade,x2$loan_amnt)

table(x$grade,x$home_ownership)
table(x$sub_grade,x$home_ownership)
table(x$State,x$home_ownership)

##9.) Relation b/w Qualitative variables & Revolving balance

library(ggplot2)

ggplot(x,  aes(x=x$grade,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")

ggplot(x,  aes(x=x$State,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")

ggplot(x,  aes(x=x$home_ownership,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")

ggplot(x,  aes(x=x$terms,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")

ggplot(x,  aes(x=x$Experience,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")

ggplot(x,  aes(x=x$verification_status,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")


ggplot(x,  aes(x=x$last_week_pay,  fill = x2$total.revol_bal)) + 
  geom_bar(position = "stack")




## cor 
cor(x2norm$loan_amnt,x2norm$total.revol_bal)

cor(x2norm$Rate_of_intrst,x2norm$total.revol_bal)

cor(x2norm$annual_inc,x2norm$total.revol_bal)

cor(x2norm$debt_income_ratio,x2norm$total.revol_bal)

cor(x2$tot_curr_bal,x2$total.revol_bal)

library(PerformanceAnalytics)
chart.Correlation(x2norm)

library(psych)
x3 <- x2[25000,]
pairs.panels(x3)


colnames(x)

######### Linarising of columns

library(ggplot2)
# Building histogram
ggplot(data=x2, aes(x2$total.revol_bal)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

hist(x2$total.revol_bal)

boxplot(x2$total.revol_bal)



























# Data partion for model building and testing


###  Model Buliding

## 1.)-- only numeric variables

model11 <- lm(x2norm$total.revol_bal~.,data = x2norm)
summary(model11)

library(car)

vif(model11)

pred <- predict(model11)
model11$residuals
sum(model11$residuals)

# Histogram to check the distribution of errors
hist(model11$residuals, color = "grey")

# caret package functions 

library(caret)

# R native funcitons
MAE(pred, x2norm$total.revol_bal)

RMSE(pred, x2norm$total.revol_bal)

####
sqrt(sum(model11$residuals^2)/nrow(x2norm))  #RMSE
sqrt(mean(model11$residuals^2))
confint(model11,level=0.95)
predict(model11,interval="predict")



## We can visualize it in a plot to check the difference visually.
x=1:length(x2$total.revol_bal)
plot(x, x2$total.revol_bal,pch=19, col="blue")
lines(x, pred, col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)



####### Testing

model11test <- lm(x2test$total.revol_bal~.,data = x2test)
summary(model11test)

library(car)

vif(model11test)

predtest <- predict(model11test)
model11test$residuals
sum(model11test$residuals)


library(caret)

# R native funcitons
MAE(predtest, total.re)
MSE(predicted, original)



# caret package functions 
RMSE(predtest, x2test$total.revol_bal)





sqrt(sum(model11test$residuals^2)/nrow(x2test))  #RMSE
sqrt(mean(model11test$residuals^2))
confint(model11,level=0.95)
predict(model11,interval="predict")



## We can visualize it in a plot to check the difference visually.
x=1:length(x2$total.revol_bal)
plot(x, x2$total.revol_bal,pch=19, col="blue")
lines(x, pred, col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)





## 1.)-- All variables

model121 <- lm(x2norm1$total.revol_bal~.,data = x2norm1)
summary(model121)

library(car)

vif(model121)

influencePlot(model121)

pred121 <- predict(model121)
model121$residuals
sum(model121$residuals)

# Histogram to check the distribution of errors
hist(model121$residuals, color = "grey")
plot(model121)


# caret package functions 

library(caret)

# R native funcitons
MAE(pred121, x2norm1$total.revol_bal)

RMSE(pred121, x2norm1$total.revol_bal)

####
sqrt(sum(model121$residuals^2)/nrow(x2norm1))  #RMSE
sqrt(mean(model121$residuals^2))
confint(model121,level=0.95)
predict(model121,interval="predict")



## We can visualize it in a plot to check the difference visually.
x=1:length(x2$total.revol_bal)
plot(x, x2$total.revol_bal,pch=19, col="blue")
lines(x, pred, col="red")
legend("topleft", legend = c("y-original", "y-predicted"),
       col = c("blue", "red"), pch = c(19,NA), lty = c(NA,1),  cex = 0.7)



####### Testing

model11test <- lm(x2test$total.revol_bal~.,data = x2test)
summary(model11test)

library(car)

vif(model11test)

predtest <- predict(model11test)
model11test$residuals
sum(model11test$residuals)


library(caret)

# R native funcitons
MAE(predtest, total.re)
MSE(predicted, original)



# caret package functions 
RMSE(predtest, x2test$total.revol_bal)





sqrt(sum(model11test$residuals^2)/nrow(x2test))  #RMSE
sqrt(mean(model11test$residuals^2))
confint(model11,level=0.95)
predict(model11,interval="predict")








































####Model new


modelnew <- lm(x2$total.revol_bal~x2$loan_amnt+x2$Rate_of_intrst+x2$Experience+
                 x2$home_ownership+x2$verification_status+x2$purpose+x2$numb_credit
               +x2$pub_rec+x2$total_credits+x2$initial_list_status+x2$total_rec_int
               +x2$application_type+x2111+x2DIR+x2last6+x2tcb)
summary(modelnew)

library(car)

vif(modelnew)


prednew <- predict(modelnew)
modelnew$residuals
sum(modelnew$residuals)


library(caret)

# R native funcitons
MAE(prednew, x2$total.revol_bal)


# caret package functions 
RMSE(prednew, x2$total.revol_bal)


sqrt(sum(modelnew$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(modelnew$residuals^2))
confint(modelnew,level=0.95)
predict(modelnew,interval="predict")


##
influencePlot(modelnew,id.n=3)
influenceIndexPlot(modelnew,id.n=3)
modelnew1.0 <- lm(x2$total.revol_bal~x2$loan_amnt+x2$Rate_of_intrst+x2$Experience+
                    x2$home_ownership+x2$verification_status+x2$purpose+x2$numb_credit
                  +x2$pub_rec+x2$total_credits+x2$initial_list_status+x2$total_rec_int
                  +x2$application_type+x2111+x2DIR+x2last6+x2tcb,data = x2[-c(89161,110327,137772)])

summary(modelnew1.0)
prednew1.0 <- predict(modelnew1.0)
modelnew1.0$residuals
sum(modelnew1.0$residuals)


library(caret)

# R native funcitons
MAE(prednew1.0, x2$total.revol_bal)


# caret package functions 
RMSE(prednew1.0, x2$total.revol_bal)


sqrt(sum(modelnew1.0$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(modelnew1.0$residuals^2))
confint(modelnew1.0,level=0.95)
predict(modelnew1.0,interval="predict")

##################
##model --
modelnew1.1 <- lm(x2$total.revol_bal~x2$loan_amnt+x2$Rate_of_intrst+
                   x2$verification_status+x2$numb_credit
                  +x2$pub_rec+x2$total_credits+x2$initial_list_status+x2$total_rec_int
                  +x2$application_type+x2111+x2DIR+x2last6+x2tcb,data = x2[-c(89161,110327,137772,10970,109386)])

summary(modelnew1.1)
prednew1.1 <- predict(modelnew1.1)
modelnew1.1$residuals
sum(modelnew1.1$residuals)

library(caret)

# R native funcitons
MAE(prednew1.1, x2$total.revol_bal)


# caret package functions 
RMSE(prednew1.1, x2$total.revol_bal)


sqrt(sum(modelnew1.1$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(modelnew1.1$residuals^2))
confint(modelnew1.1,level=0.95)
predict(modelnew1.1,interval="predict")




### model2

model12 <- lm(x2$total.revol_bal~x2$loan_amnt+x2$Rate_of_intrst)
summary(model12)

library(car)

vif(model12)

pred12 <- predict(model12)
model12$residuals
sum(model12$residuals)


library(caret)

# R native funcitons
MAE(pred12, x2$total.revol_bal)


# caret package functions 
RMSE(pred12, x2$total.revol_bal)


sqrt(sum(model12$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(model12$residuals^2))
confint(model12,level=0.95)
predict(model12,interval="predict")




### model3

model13 <- lm(x2norm1$total.revol_bal~x2norm1$loan_amnt+x2norm1$Rate_of_intrst+x2norm1$annual_inc)
summary(model13)

library(car)

vif(model13)

pred13 <- predict(model13)
View(pred13)
model13$residuals
sum(model13$residuals)

hist(model13$residuals)

library(caret)

# R native funcitons
MAE(pred13, x2norm1$total.revol_bal)


# caret package functions 
RMSE(pred13, x2norm1$total.revol_bal)


sqrt(sum(model13$residuals^2)/nrow(x2norm1))  #RMSE
sqrt(mean(model13$residuals^2))
confint(model13,level=0.95)
predict(model13,interval="predict")



### model4

model14 <- lm(x2norm$total.revol_bal~x2norm$debt_income_ratio)
summary(model14)

library(car)

vif(model14)

pred14 <- predict(model14)
model14$residuals
sum(model14$residuals)


library(caret)

# R native funcitons
MAE(pred14, x2$total.revol_bal)


# caret package functions 
RMSE(pred14, x2$total.revol_bal)


sqrt(sum(model14$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(model14$residuals^2))
confint(model14,level=0.95)
predict(model14,interval="predict")


qqnorm(x$loan_amnt)
qqline(x$loan_amnt)


#####model  5


model15 <- lm(x2norm1$total.revol_bal~x$last_week_pay+x$mths_since_last_delinq)
summary(model15)

library(car)

vif(model13)

pred13 <- predict(model13)
model13$residuals
sum(model13$residuals)


library(caret)

# R native funcitons
MAE(pred13, x2$total.revol_bal)


# caret package functions 
RMSE(pred13, x2$total.revol_bal)


sqrt(sum(model13$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(model13$residuals^2))
confint(model13,level=0.95)
predict(model13,interval="predict")



######

model16 <- lm(x2norm$total.revol_bal~,.da)
summary(model14)

library(car)

vif(model14)

pred14 <- predict(model14)
model14$residuals
sum(model14$residuals)


library(caret)

# R native funcitons
MAE(pred14, x2$total.revol_bal)


# caret package functions 
RMSE(pred14, x2$total.revol_bal)


sqrt(sum(model14$residuals^2)/nrow(x2))  #RMSE
sqrt(mean(model14$residuals^2))
confint(model14,level=0.95)
predict(model14,interval="predict")


qqnorm(x$loan_amnt)
qqline(x$loan_amnt)

