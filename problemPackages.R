#TO install problem packages
# This code is apparently magic.

repos <- getOption('repos')
repos["CRAN"] <- "http://cran.rstudio.org"
options(repos = repos)
install.packages('cvTools')
library(cvTools)
