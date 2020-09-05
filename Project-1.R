x <- read.csv(file.choose())
View(x)
str(x)
attach(x)
boxplot(x$loan_amnt)
boxplot(x$Rate_of_intrst)
boxplot(x$annual_inc)

str(x$sub_grade)
y <- as.factor(x$sub_grade)
View(y)


x1 <- x[,c(2,5,11,15,16,17,18,19,20,21,22,23,25,26,27,28,29,30,34,35,36)]

x1 <- x[,c(2,5,11,15:23,25:30,34:36)]
View(x1)
