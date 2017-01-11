rDisattenuated<-function(x,y, xrel, yrel){
  test<-cor.test(x,y)
  r<-as.numeric(test$estimate)
  disattenuated<-(r)/sqrt(xrel*yrel)
  return(disattenuated)
}
