setwd("C:/Users/Melissa/Dropbox (Personal)/STA 674/code for homework/Assignment 4/R code for assignment 4")
#Enter into your path

lettuce = read.csv("lettuce.csv", header = T)

boxplot(LETTUCE~NITROGEN, ylab = "Lettuce", xlab = "Nitrogen", data=lettuce)
n = as.factor(lettuce$NITROGEN)
result = aov(LETTUCE~n , data = lettuce)
summary(result)

model = lm(LETTUCE~n-1,data=lettuce)
summary(model)
plot(model)


confint(model)#Confidence intervals

res = model$residuals
qqnorm(res)
qqline(res)



