######################################
# Setting up
######################################
rm(list=ls())
setwd("/Users/Andrew/Documents/Github/Utilities")

library(maps)

######################################
# Messing around
######################################

dsCity<-c("Oklahoma city", "Tulsa", "Norman")

okmap<-map('county', 'oklahoma', fill = FALSE, col = palette())
okmap$names

okcounties<-substr(okmap$names, 10, 30)
okcounties

ds<-county.fips
head(ds)
ds$state<-ds$polyname
ds$state<-substr(ds$state, 1, 8)
ds<-ds[ds$state=="oklahoma", ]

head(ds)
