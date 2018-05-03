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