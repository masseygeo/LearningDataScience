#####STA674-Assignment 1 ########


####Question 1

library(ggplot2)
y = c(12, 11.5, 14, 15, 15.4, 15.3, 17.5)
x = c(5:11)
plant = data.frame(x, y)


#There are simpler plotting functions, google scatterplot in R for details. 
#plot(y ~ x)
ggplot(plant, aes(x, y)) + geom_point(aes(x, y)) +  geom_smooth(method="lm",col=1) +
  labs(x = "Production", y = "Costs") +
    theme(text = element_text(size = 15)) + ggtitle("Plant")
plant.lm = lm(y~x, data = plant)
summary(plant.lm)


###Question 2
setwd("C:/Users/Melissa/Dropbox (Personal)/STA 674/homework") ###remember to change to your working directory. and it is front slash not back slash
div3 = read.csv("div3.csv", header = T)
div3.lm = lm(DIVYIELD~EPS, data = div3)
result.div3 = summary(div3.lm)
result.div3

salesad3 = read.csv("SALESAD3.csv", header = T)
salead3.lm = lm(SALES~ADV, data = salesad3)
result.salead3 = summary(salead3.lm)
result.salead3


###Q4
#a confidence interval for mean response at a given value of x.

n = 7 ###(number of observation)
xm = 8 ###(8 in 10000 units)
xbar = mean(x) 
ym = 7.1 + 0.91 * 8 #(intercept * beta_1 * 8)
ym # point estimate
MSE.plant.lm = anova(plant.lm)[2,3]  #mean square error taken from lineear model output

est.interval = function(x, yhat, xm, alpha = 0.05, MSE)  #creating a function to compute confidence interval for a value of y, on average
{
  n = length(x)
  xbar = mean(x)
  interval = yhat + c(-1, 1) * qt(alpha/2, n-2, lower.tail = F) * sqrt( MSE*  (1/n + (xm-xbar)^2/((n-1)*sd(x)^2 )) )
  return(interval)
}
est.interval(x, ym, xm,alpha = 0.05, MSE.plant.lm)



###b  prediction interval at a given value of x.

pre.interval = function(x, yhat, xm, alpha = 0.05, MSE)
{
  n = length(x)
  xbar = mean(x)
  interval = yhat + c(-1, 1) * qt(alpha/2, n-2, lower.tail = F) * sqrt(MSE *(1 + 1/n + (xm-xbar)^2/((n-1)*sd(x)^2 )))
  return(interval)
}
pre.interval(x, ym, xm, alpha = 0.05, MSE.plant.lm)
      

###5


Xm1 = 2 ###(2 is earning)
Xm2 = 5
EPS = div3$EPS

Ym1 = 2.0336 + 0.374 * Xm1
Ym1 # point estimate
Ym2 = 2.0336 + 0.374 * Xm2
Ym2 # point estimate
MSE.div3.lm = anova(div3.lm)[2, 3]  #mean square error

div3.2.pre = pre.interval(EPS, Ym1, Xm1, alpha = 0.05, MSE.div3.lm )
div3.2.pre  #prediction interval

div3.5.pre = pre.interval(EPS, Ym2, Xm2, alpha = 0.05, MSE.div3.lm)
div3.5.pre  #prediction interval




