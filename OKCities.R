######################################
# Setting up
######################################
rm(list=ls())
setwd("C:/Users/Andrew/Documents/GitHub/Utilities")

######################################
# Reading in datasets
######################################

# Reading in List of cities by state and zip
# https://www.zip-codes.com/state/ok.asp

ds1<-read.csv("OKCitiesZipCounty.csv")


# County by Income
# "SELECTED ECONOMIC CHARACTERISTICS 2006-2010 American Community Survey 5-Year Estimates". U.S. Census Bureau. Retrieved 2012-11-25.
# "Profile of General Population and Housing Characteristics: 2010 Demographic Profile Data". U.S. Census Bureau. Archived from the original on March 5, 2014. Retrieved 2012-11-25.
ds2<-read.csv("CountyxIncome.csv")


# Zip codes by income, sorta.
# https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-2014-zip-code-data-soi

ds3<-read.csv("IRSIncomeTax.csv")
