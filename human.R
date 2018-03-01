# human "CLASS"

human.check <- function(suggestions, key){
  
  feedback <- dplyr::left_join(x = suggestions, y = key, by = "ID") %>%
    dplyr::select(ID, prob, reason, type, isDataError)
  
  return(feedback)
}

human.evaluate <- function(feedback){
  return(mean(feedback$isDataError))
}

human.correct <- function(inputData, feedback, solutions){
  for(i in 1: nrow(feedback)){
    roww <- feedback[i,]
    if(roww$isDataError == 1){
      idx     <- which(inputData$ID == roww$ID)
      idxSol  <- which(solutions$ID == roww$ID)  
      
      inputData$ID[idx] <- solutions$ID[idxSol]
      inputData$date[idx] <- solutions$date[idxSol]
      inputData$currentOutstanding[idx] <- solutions$currentOutstanding[idxSol]
      inputData$discountRate[idx] <- solutions$discountRate[idxSol]
      inputData$maturity[idx] <- solutions$maturity[idxSol]
      inputData$marketValuation[idx] <- solutions$marketValuation[idxSol]
    }
  }
  return(inputData)
}