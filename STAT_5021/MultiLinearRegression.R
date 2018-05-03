rm(list=ls())
library(ggplot2)
setwd("/Users/manishrai/Desktop/UMN/Spring 2018/STAT 5021/Code")

sat = read.table(file = "datasets20180328/sat.txt", header = TRUE)
head(sat)

pairs(sat)
# Scatter plot with labels as the values
plot(total ~ takers, data = sat,type = "n")
text(sat$takers, sat$total, labels = rownames(sat), cex = 0.8)

# Linear model with response is total and predictor are expenses, ratio, salary, takers
fit = lm(sat$total ~ sat$expend + sat$ratio + sat$salary + sat$takers)
summary(fit)
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics



# for the predictors as (6, 17.5, 35.948, 9) the response at 95% CI :
u0 = data.frame(expend = 6, ratio = 17.5, salary = 35.948, takers = 9)
predict(object = fit, newdata = u0, interval = "confidence")

# Checking assumptions about the error:
# Residual vs fitted plot
plot(residuals(fit) ~ fitted(fit))
abline(0, 0)

# residual vs predictor plot
par(mfrow = c(2, 2))
plot(residuals(fit) ~ (expend), data = sat)
abline(0, 0)
plot(residuals(fit) ~ (ratio), data = sat)
abline(0, 0)
plot(residuals(fit) ~ (salary), data = sat)
abline(0, 0)
plot(residuals(fit) ~ (takers), data = sat)
abline(0, 0)

# QQ plot of the residual
normalQQplot=function(x.list, title="Normal QQ plot") {
  ## this function makes a Normal QQ plot of the measurements ## in the vector x.list. title is the plot title. 
  probs=ppoints(length(x.list))
  d.percentiles=quantile(x.list, probs) 
  f.percentiles=qnorm(probs, mean=mean(x.list), sd=sd(x.list)) 
  plot(f.percentiles, d.percentiles, main=title, xlab="Fitted Normal percentile", ylab="Data percentile")
  abline(0,1)
}
normalQQplot(x.list=residuals(fit), title="Normal QQ plot of the residuals")
