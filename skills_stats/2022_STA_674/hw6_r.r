setwd("C:/Users/Melissa/Dropbox (Personal)/STA 674/code for homework/Assignment 6")
irrigation = read.csv("assignment 6.csv", header = T)

#Problem 1
library(emmeans)
library(STAT)
method = as.factor(irrigation$METHOD)
block = as.factor(irrigation$BLOCK)
weight = irrigation$WEIGHT
Irrigation = data.frame(method, block, weight)
Irrigation.lm = lm(weight~ method + block, data = Irrigation)
lsmeans(Irrigation.lm,"method")
groupmeans = c(300, 290, 224, 292, 291, 230)


#ANOVA with blocks
anova(Irrigation.lm)

# Power with blocks
library(pwr2)
pwr.2way(a = 6, b = 8, alpha = 0.05, 
         size.A = 1, size.B = 1, f.A = 0.489, f.B = 0.999)

#Problem 2

#ANOVA without blocks
Irrigation.lm2 = lm(weight~ method, data = Irrigation)
irrigation.anova = anova(Irrigation.lm2)
irrigation.anova
irrigation.aov = aov(Irrigation.lm2)
summary(irrigation.aov)
oneway.test(weight ~ method, data=Irrigation, var.equal = TRUE)

#Power without blocks

power.anova.test(groups = 6, n=8, between.var = var(groupmeans), within.var = 14390.3601, 
                 sig.level = 0.05, power = NULL)


#2d number of replicates needed to obtain same power including blocks 
#divide by number of blocks
#r = 2 * (qnorm(0.025, lower.tail = F) + qnorm(beta, lower.tail = F))^2 * (117/16.4)^2
#r

#another way to compute sample size for problem 2d
power.anova.test(groups=6,n=NULL,between.var= var(groupmeans), within.var = 14390.4, sig.level = 0.05, power = .665)
