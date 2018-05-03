# Clearing environment variable
rm(list=ls())

setwd("/Users/manishrai/Desktop/UMN/Spring 2018/STAT 5021/Code")

dat=read.table(file="datasets20180328/gamb.txt")
head(dat)

# Exploratory data analysis
summary(dat)
mean(dat$income)
sd(dat$income)
# identifying the rows that are female
which(dat$gender == "F")
dat[which(dat$gender == "M"), ]
# This can also be done as
dat[dat$gender == "M", ]

# Summary by gender
by(data = dat, dat$gender, summary)

# Plotting
hist(dat$income, main="Incomes", xlab="Income in Pounds Per Week")
boxplot(dat$income, main = "Income", xlab="Males and females", ylab="Income in Pounds Per Week")
# produce side-by-side boxplots for incomes of males and females
boxplot(income ~ gender, data = dat, main = "Income", xlab="Males and females", ylab="Income in Pounds Per Week")

# QQ-plot can also be made for male incomes. we create a function called normalQQplot
normalQQplot=function(x.list, title="Normal QQ plot") {
  ## this function makes a Normal QQ plot of the measurements 
  ## in the vector x.list. title is the plot title. 
  probs=ppoints(length(x.list))
  d.percentiles=quantile(x.list, probs) 
  f.percentiles=qnorm(probs, mean=mean(x.list), sd=sd(x.list)) 
  plot(f.percentiles, d.percentiles, main=title,
  xlab="Fitted Normal percentile", ylab="Data percentile", type = 'o')
  abline(0,1)
}

# make a normal QQ-plot for male income
male.income = dat$income[which(dat$gender == 'M')]
normalQQplot(x.list = male.income, title = 'Normal QQ plot for male incomes')

# make a normal QQ-plot for female income
female.income = dat$income[which(dat$gender=='F')]
normalQQplot(x.list = female.income, title = "Normal QQ plot for female income")



################################################
plant=read.table("datasets20180328/plant.txt")
summary(plant)
stripchart(weight ~ condition, data=plant, vertical=TRUE, xlab="growth condition")

sort(by(plant$weight, plant$condition, mean))

mod=aov(weight ~ condition, data=plant)
summary(mod)
