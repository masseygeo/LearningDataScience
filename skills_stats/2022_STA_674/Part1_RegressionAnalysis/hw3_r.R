#question1
#you need to install the olsrr package if you are using R downloaded onto your computer. 
#R will likely prompt you to install it.
#If you are using the Rstudio server (rstudio.as.uky.edu) do NOT install it.  It is already
#installed.

library(olsrr)
setwd("C:/Users/Melissa/Dropbox (Personal)/STA 674/code for homework/Assignment 3")
bass = read.csv("bass.csv", header = T)
#attach(bass)
bass.lm = lm(Avg_Mercury ~ Alkalinity + pH + Calcium + Chlorophyll, data = bass)
bass.all= ols_step_all_possible(bass.lm)
bass.all
plot(bass.all)   # careful, many plots will show up
#detach(bass)



#Question 2
library(MASS)
body = read.csv("body.csv", header = T)
# part (a): forward selection
bd.lm <- lm(weight ~ chest_diam + chest_depth + ankle_diam
            + waist_girth + wrist_girth + wrist_diam + age
            + height + gender, data = body)
summary(bd.lm)
bd.forward = ols_step_forward_aic(bd.lm)
bd.forward

plot(bd.forward)



# part (b): backward selection
bd.back = ols_step_backward_p(bd.lm)                # best model
bd.back
plot(bd.back)

step_backward = stepAIC(bd.lm, direction = "backward") 
step_backward$anova    
summary(step_backward) # details of the best model

# part (c): all possible model
body.all= ols_step_all_possible(bd.lm)
body.all
body_5 <- subset(body.all,n==5)   # check models with 5 variables
plot(body.all)


#question 3 
city = read.csv("city_temps.csv", header = T)
plot(High_F ~ Latitude, data=city)
city.lm =lm(High_F~Latitude, data = city)
summary(city.lm)

# use Cook's distance to detect influential observations
cooksd = cooks.distance(city.lm)
plot(cooksd)

summary(city.lm)
city.res = resid(city.lm)
plot(city$Latitude, city.res,
     ylab = "Residuals", xlab = "Latitude",
     main="Latitude vs. Residuals")
abline(0,0)

city.stres = rstandard(city.lm)
qqnorm(city.stres,
       ylab="Standardized Residual",
       xlab = "Normal Scores",
       main = "QQ Plot of Residuals")
qqline(city.stres)

#question 4
city_2 = city[-c(35,47), ]

plot(x = Latitude, y = High_F)
city_2.lm =lm(High_F~Latitude, data = city_2)
city_2.lm
summary(city_2.lm)


#question 5
earlyind1 = c(substr(city$City,0, 1) < "M") 
city_3 = data.frame(city, earlyind = earlyind1)

city_3.lm = lm(High_F~Latitude + earlyind, data = city_3)
summary(city_3.lm)

