#In Reference to...
#https://vimeo.com/harvardcatalyst/review/123625538/7fd460d231

rm(list = ls())
library(lavaan)
#Simulate the Preacher and Hayes hypothetical Cog Therapy -> Failure Outlook -> Life Satisfaction study data
#First, assume the following:
#M = b0mx + .8186*X + e3
#Y = b0yx + .7640*X + e1
#Y = b0ymx +.4334*X + .4039*M + e2
#n = 30

#For the moment, just get some data, courtesy of Dr. Bard
#Will try to fine tune later to match up with (preacher & hayes 2004 data)
mySeed <- 7866
set.seed(mySeed)
simN <- 3000
X <- rbinom(n=simN,size=1,prob=.5)
M <- (0) + .8186*X + rnorm(n=simN)*sqrt(1.25)
Y <- (-.24)*1 + .4334*X + .4039*M + rnorm(n=simN)*sqrt(1)

#Setting aside data for examination/recording
myData <- data.frame(list(Y=Y,M=M,X=X))
head(myData)
cor(myData[,c(3:1)])
#write.csv(myData, "dsCausMed.csv")

#Regression Equations/Models
mMX<-lm(M~X)
mYXM<-lm(Y~X+M+X*M)

#Getting CDE, NDE, NIE.
summary(mMX)
summary(mYXM)

#Exposure variable levels
X0<-0
X1<-1

#CDE(x,x*;m)
BY_X<-as.numeric(mYXM$coefficients[2])
BY_XM<-as.numeric(mYXM$coefficients[4])
CDE<-(BY_X+BY_XM)*(X1-X0)
CDE

#NDE(x,x*;x*)
BM_int<-as.numeric(mMX$coefficients[1])
BM_X<-as.numeric(mMX$coefficients[2])
NDE<-(BY_X+BY_XM*(BM_int+BM_X*X0))*(X1-X0)
NDE

#NIE(x, x*; x)
BY_M<-as.numeric(mYXM$coefficients[3])
NIE<-(BY_M*BM_X + BY_XM*BM_X*X1)*(X1-X0)
NIE

#Proportion Mediated
#NOTE: Only sensical if NIE and NDE are in the same direction
PM<-NIE/(NDE+NIE)
PM

#Total Effect:
TE<-NDE+NIE
TE
