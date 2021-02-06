


##Imputation involves replacing missing values with "reasonable" guesses about what the values would have been if they had not been missing.

home1<-home
home1[] <- lapply(home, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x})



##relationship between two variables
##Bivariate graphs depict the relationship between two variables. The variables may be qualitative or quantitative. Let us look at different scenarios:
  
  
 ## Quantitative vs Quantitive


##Simple Scatter Plot:
  plot(data1$MONTHLY, data1$HRS.PER.WK, main="Relationship between Monthly Salary and Job Hours", xlab="Hours per week", ylab="Monthly Salary")


ggplot(data=data1, mapping = aes(x=HRS.PER.WK, y=MONTHLY)) +
  geom_point()+
  labs(ylab="Monthly Salary",xlab="Work Hours",title = "Relationship between Monthly Salary and Work Hours")


##To alter the x axis range in histogram
hist(dat, xaxt='n')
axis(side=1, at=seq(0,100, 10), labels=seq(0,1000,100))


##  Model Buliding

## 1.)

model3 <- lm(Price~.,data = y)
summary(model3)

library(car)
vif(model3)
avPlots(model3,id.n=0.2,id.cex=7)
influencePlot(model3,id.cex=3)

confint(model3,level=0.95)
pred3 <- predict(model3,interval="predict")
View(pred3)
write.csv(pred3,"corolla.csv")
getwd()