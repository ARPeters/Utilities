######################
#Vignette from:
######################

#https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf

######################
#Loading packages
######################
library(coxme)
library(survival)
library(kinship2)

######################
#So...
######################

#Get dataset with no missingness, apparently?
minnfemale <- minnbreast[minnbreast$sex == 'F' & !is.na(minnbreast$sex),]

#Simple model
fit1 <- coxme(Surv(endage, cancer)~I(parity>0) + (1|famid), data=minnfemale, subset=(proband==0))


#Range of std values; note: must be non-zero
estvar <- seq(.2, .6, length=15)^2

#Create empty vector of integers; not sure why 15
loglik <- double(15)

for(i in 1:15){
  tfit <- coxme(Surv(endage, cancer)~I(parity>0) + (1|famid), data=minnfemale, subset=(proband==0), vfixed=estvar[i])
  loglik[i] <- 2*diff(tfit$loglik)[1]
  }

plot(sqrt(estvar), loglik, xlab="Std of the random effect", ylab="2 * loglik")
abline(h=2*diff(fit1$loglik)[1] - qchisq(.95, 1), lty=2)

temp <- 2*diff(fit1$loglik)[1] - loglik

approx(temp[1:8], sqrt(estvar[1:8]), xout=3.84)$y

######################
#What have we learned:
######################

#This gets the confidence interval of standard deviation of random effects.
#NOT FIXED EFFECTS.
