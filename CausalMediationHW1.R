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

summary(lm(M~X))
summary(lm(Y~X+M+X*M))

summary(mMX)
summary(mYXM)

#Exposure variable levels
X0<-0
X1<-1

#Mediation variable
M<-1

#CDE(x,x*;m)
ThetaX<-as.numeric(mYXM$coefficients[2])
ThetaXM<-as.numeric(mYXM$coefficients[4])

CDE<-(ThetaX+ThetaXM)*(X0-X1)
CDE

#NDE(x,x*;x*)
Beta0<-as.numeric(mMX$coefficients[1])
Beta1<-as.numeric(mMX$coefficients[2])
NDE<-(ThetaX+ThetaXM*(Beta0+Beta1*X1))*(X0-X1)
NDE

#NIE(x, x*; x)
ThetaM<-as.numeric(mYXM$coefficients[3])
NIE<-(ThetaM*Beta1 + ThetaXM*Beta1*X0)*(X0-X1)
NIE

#Proportion Mediated
#NOTE: Only sensical if NIE and NDE are in the same direction
PM<-NIE/(NDE+NIE)
PM
