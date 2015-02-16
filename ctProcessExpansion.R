#Unfinished. 


require(plyr)



ctProcessExpansion<- function(d, subj, stat, survTime){
  subject<-d$subject
  status<-d$stat
  survt<-d$survTime
  
  eventTimes <- unique(d$survt[d$status==1])
  eventTimes <- eventTimes(order(eventTimes))

  ctProcessExpanded <- ddply(.data=ds,.variables=.(subject),.fun = createPTable)
  return(ctProcessExpanded)
}


createPTable <- function(d){  
  dNew <- d
  for(i in 1:length(eventTimes)){
    dNew[i,] <- d
    dNew[i,"r"] <- i
    dNew[i,"tr"] <- eventTimes[i]
    dNew[i,"dir"] <- ifelse(i==1,min(d$survt,eventTimes[i]),min(d$survt,eventTimes[i]) - eventTimes[i-1])
    dNew[i,"yir"] <- 0
    if(d$survt <= eventTimes[i]) {
      if(d$status %in% 1) dNew[i,"yir"] <- 1
      break      
    }      
  }    
  return(dNew)
}

