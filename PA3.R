best <- function(state, outcome) {
  ## Read outcome data
  yy <- read.csv("outcome-of-care-measures.csv" , colClasses = "character" ,na.strings = "Not Available")
  outcomeCol <- integer()
  ##checking if state and outcome are valid
  states  <- unique(yy[,"State"])
  if(length(grep(state , states)) == 0)
  {
    print("State is not valid")
    return(0)
  }
  if(tolower(outcome) != "heart attack" && tolower(outcome) != "heart failure" && tolower(outcome) != "pneumonia")
  {
    print("Outcome is not valid, valid states :" + states)
    return(0)
  }
  else{
    if(tolower(outcome) == "heart attack")
         outcomeCol = 11
    if(tolower(outcome) == "heart failure")
      outcomeCol = 17
    if(tolower(outcome) == "pneumonia")
      outcomeCol = 23
    
  }
  yy[,outcomeCol] <- as.numeric(yy[,outcomeCol])
  yyState <- yy[yy$State == state,] 
  yyClean <- yyState[!is.na(yyState[,outcomeCol]),]
  ##column Heart.Attack11 , Heart.Failure17 , Pneumonia23
  
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day death
    ## rate
  
 
  minimum <-yyClean[yyClean[,outcomeCol] == min(yyClean[,outcomeCol]),]
  Name<- character()
  if(nrow(minimum) > 1)
  {
    minimum[order(minimum$Hospital.Name),]
    Name <- minimum[order(minimum$Hospital.Name),][1]
  }
  else{
    Name <- minimum$Hospital.Name
  }
  
  Name
  
  
  
}