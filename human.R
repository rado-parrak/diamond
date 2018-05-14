# human "CLASS"

human.check <- function(keys, step){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  
  aux<- dplyr::filter(suggestionContainer, stepID == step) %>%
    dplyr::left_join(y = keys, by = c('recordID' = 'ID')) %>%
    dplyr::select(colnames(suggestionContainer), isDataError) %>%
    dplyr::mutate(feedbackNew = ifelse(is.na(feedback), isDataError, NA)) %>%
    dplyr::mutate(feedback = feedbackNew) %>%
    dplyr::select(-one_of("isDataError", "feedbackNew"))
  
  if(step > 1){
    suggestionContainer <<- rbind(dplyr::filter(suggestionContainer, stepID < step), aux)
  } else{
    suggestionContainer <<- aux
  }

  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function at step: ', step))
}

human.evaluate <- function(step){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  return(mean(suggestionContainer$feedback))
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
}

human.correct <- function(inputData, solutions, step){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  
  thingsToCorrect <- dplyr::filter(suggestionContainer, stepID == step, feedback == 1)
  if(nrow(thingsToCorrect)>0){
    for(i in 1: nrow(thingsToCorrect)){
      roww <- thingsToCorrect[i,]
      toLog(where = roww$recordID, what = "Correcting record with ID")
      
      idx     <- which(inputData$ID == roww$recordID)
      idxSol  <- which(solutions$ID == roww$recordID)  
      
      inputData$ID[idx] <- solutions$ID[idxSol]
      inputData$date[idx] <- solutions$date[idxSol]
      inputData$currentOutstanding[idx] <- solutions$currentOutstanding[idxSol]
      inputData$discountRate[idx] <- solutions$discountRate[idxSol]
      inputData$maturity[idx] <- solutions$maturity[idxSol]
      inputData$marketValuation[idx] <- solutions$marketValuation[idxSol]
      
    }
  }
  
  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function at step: ', step))
  return(inputData)
}