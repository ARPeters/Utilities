#In Reference to...
#https://vimeo.com/harvardcatalyst/review/123625538/7fd460d231

####################################################
# Reading in Packages
####################################################
rm(list = ls())
library(RMediation)
library(lavaan)
library(mediation)
library(sandwich)
library(lavaan)
####################################################
# Reading in Data
####################################################
md <- read.csv("mediation.csv")

####################################################
# Creating new binary outcomes
####################################################
md$y2<-ifelse(md$y==1, 1, 0)
md$y3<-ifelse(md$y==5, 1, 0)


####################################################
# Model 1
####################################################
med_fit <- lm(m ~ x, data = md)
out_fit <- lm(y ~ m + x, data = md) #without interaction
med_out_1a <- mediate(med_fit, out_fit, boot = TRUE, treat="x", mediator="m", sims = 1000, data=md)
summary(med_out_1a)

#Getting confidence interval for Mediated Effect
ci_1a_delta <- medci(mu.x=coef(summary(med_fit))["x",1],se.x=coef(summary(med_fit))["x",2],mu.y=coef(summary(out_fit))["m",1], se.y=coef(summary(out_fit))["m",2],rho = vcov(out_fit)["x","m"],type = "asymp",alpha = .05) #delta se
ci_1a_delta
ci_1a_bootstrap <- medci(mu.x=coef(summary(med_fit))["x",1],se.x=coef(summary(med_fit))["x",2],mu.y=coef(summary(out_fit))["m",1], se.y=coef(summary(out_fit))["m",2],rho = vcov(out_fit)["x","m"],type = "MC",alpha = .05) #bootstrap se
ci_1a_bootstrap

#med_fit <- lm(m ~ x, data = md)
out_fit <- lm(y ~ m*x, data = md) #with interaction
med_out_1b <- mediate(med_fit, out_fit, boot = TRUE, treat="x", mediator="m", sims = 1000, data=md)
summary(med_out_1b)

####################################################
# Model 2
####################################################
#med_fit <- lm(m ~ x, data = md)
out_fit <- lm(y2 ~ m + x, data = md) #without interaction
med_out_2a <- mediate(med_fit, out_fit, boot = TRUE, treat="x", mediator="m", sims = 1000, data=md)
summary(med_out_2a)

#Getting confidence interval for Mediated Effect
ci_2a_delta <- medci(mu.x=coef(summary(med_fit))["x",1],se.x=coef(summary(med_fit))["x",2],mu.y=coef(summary(out_fit))["m",1], se.y=coef(summary(out_fit))["m",2],rho = vcov(out_fit)["x","m"],type = "asymp",alpha = .05) #delta se
ci_2a_delta
ci_2a_bootstrap <- medci(mu.x=coef(summary(med_fit))["x",1],se.x=coef(summary(med_fit))["x",2],mu.y=coef(summary(out_fit))["m",1], se.y=coef(summary(out_fit))["m",2],rho = vcov(out_fit)["x","m"],type = "MC",alpha = .05) #bootstrap se
ci_2a_bootstrap

#med_fit <- lm(m ~ x, data = md)
out_fit <- lm(y2 ~ m*x, data = md) #with interaction
med_out_2b <- mediate(med_fit, out_fit, boot = TRUE, treat="x", mediator="m", sims = 1000, data=md)
summary(med_out_2b)

####################################################
# Model 3
####################################################
#med_fit <- lm(m ~ x, data = md)
out_fit <- lm(y3 ~ m + x, data = md) #without interaction
med_out_3a <- mediate(med_fit, out_fit, boot = TRUE, treat="x", mediator="m", sims = 1000, data=md)
summary(med_out_3a)

#Getting confidence interval for Mediated Effect
ci_3a_delta <- medci(mu.x=coef(summary(med_fit))["x",1],se.x=coef(summary(med_fit))["x",2],mu.y=coef(summary(out_fit))["m",1], se.y=coef(summary(out_fit))["m",2],rho = vcov(out_fit)["x","m"],type = "asymp",alpha = .05) #delta se
ci_3a_delta
ci_3a_bootstrap <- medci(mu.x=coef(summary(med_fit))["x",1],se.x=coef(summary(med_fit))["x",2],mu.y=coef(summary(out_fit))["m",1], se.y=coef(summary(out_fit))["m",2],rho = vcov(out_fit)["x","m"],type = "MC",alpha = .05) #bootstrap se
ci_3a_bootstrap

#med_fit <- lm(m ~ x, data = md)
out_fit <- lm(y3 ~ m*x, data = md) #with interaction
med_out_3b <- mediate(med_fit, out_fit, boot = TRUE, treat="x", mediator="m", sims = 1000, data=md)
summary(med_out_3b)
