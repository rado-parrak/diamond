# human "CLASS"

human.check <- function(suggestions, keys, step){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  aux <- dplyr::left_join(x = suggestions, y = key, by = c('recordID' = 'ID')) %>%
    dplyr::select(colnames(suggestions), isDataError) %>%
    dplyr::mutate(feedbackNew = ifelse(is.na(feedback), isDataError, NA)) %>%
    dplyr::mutate(feedback = feedbackNew) %>%
    dplyr::select(-one_of("isDataError", "feedbackNew"))
  
  suggestions <<- aux
  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function at step: ', step))
}

human.evaluate <- function(step){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  return(mean(suggestions$feedback))
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
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