# installing/loading the package:
if(!require(installr)) { 
  install.packages("installr"); require(installr)} #load / install+load installr
install.packages("installr")
# using the package:
# this will start the updating process of your R installation.  It will check for newer versions, and
# if one is available, will guide you through the decisions you'd need to make.
<<<<<<< HEAD
updateR()
=======

library(installr)

install.packages("stringr")
library(stringr)

updateR()

#Testing...
>>>>>>> origin/master
