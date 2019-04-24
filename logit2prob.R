logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  prob <- round((prob*100), 3)
  return(paste0(prob, " percent", sep=" "))
}

