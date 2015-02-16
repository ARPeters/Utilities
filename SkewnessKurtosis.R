#I don't trust any of these. 

mykurtosis <- function(x) {  
  m4 <- mean((x-mean(x))^4) 
  kurt <- (m4/(sd(x)^4))-3  
  kurt
}

myskewness <-  function(x) {
  m3 <- mean((x-mean(x))^3)
  skew <- m3/(sd(x)^3)
  skew
}


myskewness.adjusted <-  function(x) {
  m3 <- mean((x-mean(x))^3)
  skew <- m3/(sd(x)^3)
  N <-length(x)
  df <- (N-1)
  skew <- skew*(sqrt(N*df))/df
  skew
}