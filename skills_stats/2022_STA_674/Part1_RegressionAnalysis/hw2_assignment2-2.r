#question1
setwd("C:/Users/Melissa/Dropbox (Personal)/STA 674/code for homework/Assignment 2") ###remember to change to your working directory. and it is front slash not back slash
mfgcost = read.csv("mfgcost.csv", header = T)

mfg.lm = lm(COST~PAPER+MACHINE+OVERHEAD+LABOR, data = mfgcost)
result.mfg = summary(mfg.lm)
result.mfg

ci = 2.47 +c(-1, 1)* qt(p = 0.025, df = 22, lower.tail = F) *0.46556
ci
#question 2

wheat = read.csv("wheat.csv", header = T)
n = dim(wheat)[1]
wheat.lm = lm(SHIPMENT~ EXCHRATE +PRICE , data = wheat)
result.wheat = summary(wheat.lm)
result.wheat


#question 3

#a
library(ggplot2)
RDS = read.csv("RDS.csv",header = T)
RDS_1 = data.frame(RDS,RDS$RD^2)
#attach(RDS_1)
Risk = RDS_1$RISK
Profit = RDS_1$PROFIT
Rd = RDS_1$RD


ggplot(RDS_1, aes(RISK, PROFIT)) + geom_point(aes(RISK, PROFIT)) +  geom_smooth(method="lm",col=1) +
  labs(x = "Risk", y = "Profits") +
  theme(text = element_text(size = 15)) + ggtitle("RDS")
ggplot(RDS_1, aes(RD, PROFIT)) + geom_point(aes(RD, PROFIT)) +  geom_smooth(method="lm",col=1) +
  labs(x = "RD", y = "Profits") +
  theme(text = element_text(size = 15)) + ggtitle("RDS")

#b
RDS_1.lm = lm(PROFIT~ RISK + RD, data = RDS_1)
RDS_1.result = summary(RDS_1.lm)
RDS_1.result

#c
R_2.lm = lm(PROFIT~ RISK+ RD + RDS.RD.2, data = RDS_1)

R_2.result = summary(R_2.lm)
R_2.result

res.RDS_1 = RDS_1.result$residuals
res.R_2 = R_2.result$residuals
res = data.frame( idx= c(1:length(res.RDS_1)),res.RDS_1, res.R_2)
ggplot(res, aes(idx, res.RDS_1)) + geom_point(aes(idx, res.RDS_1)) + 
  labs(x = "idx", y = "Residuals") +
  theme(text = element_text(size = 15)) + ggtitle("Residual plots linear model")

ggplot(res, aes(idx, res.R_2)) + geom_point(aes(idx, res.R_2)) + 
  labs(x = "idx", y = "Residuals") +
  theme(text = element_text(size = 15)) + ggtitle("Residual plots polynomial model")

#questions 4  and 5
bank = read.csv("bank.csv", header = T)

bank_1 = lm(SALARY ~ EDUCAT + EXPER + MONTHS + MALES, data = bank)
bank_2 = lm(SALARY ~ EDUCAT + EXPER + MONTHS + MALES + EDUCAT*EXPER, data = bank)

result_bank_1 = summary(bank_1)
result_bank_1
result_bank_2 = summary(bank_2)
result_bank_2

