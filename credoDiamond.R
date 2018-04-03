# CREDO DIAMOND "CLASS"
library(dplyr)

addToSuggestionContainer <- function(suggestions){
  suggestionContainer <<- rbind(suggestionContainer, suggestions)
}

generateSuggestions <- function(inputData, rules,step, type){
  
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  if(type == "RuleBased"){
    toLog(where = "Start type: RuleBased", what = paste0('Step: ', step))
    
    suggestions <- NULL
    for(i in 1:nrow(rules)){
      toLog(where = "type: RuleBased", what = paste0('Start rule: ', i))
      
      rule      <- rules[i,]
      minn      <- rule$min
      maxx      <- rule$max
      
      checkData <- dplyr::select(inputData, ID, toCheck = eval(as.character(rule$name))) %>%
        dplyr::filter(toCheck < minn | toCheck > maxx)
      
      if(nrow(checkData) > 0){
        aux <- checkData %>% 
          select(recordID = ID) %>%
          mutate(stepID = step
                 , reason = paste0(rule$name, " ", rule$type)
                 , level = "RuleBased"
                 , score = 1
                 , feedback = NA)
        
        suggestions <- rbind(suggestions, aux)
      }
      toLog(where = "type: RuleBased", what = paste0('End rule: ', i))
    }
    toLog(where = "End type: RuleBased", what = paste0('Step: ', step))
  }
  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function at step: ', step))
  
  return(suggestions)
}

credoDiamond.suggest <- function(inputData, rules, settings, step){
  # Login
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  suggestions <- NULL
  
  if(suggestRuleBased){
    toLog(where = 'suggestRuleBased', what = paste0('Step: ', step))
    suggestions <- generateSuggestions(inputData, rules, step, type = "RuleBased")
    if(is.null(suggestions)){
      suggestRuleBased    <<- FALSE
      suggestUnsupervised <<- TRUE
    } else{
      addToSuggestionContainer(suggestions)
    }
  }
  if(suggestUnsupervised){
    toLog(where = 'suggestUnspervised', what = paste0('Step: ', step))
    suggestUnsupervised <<- FALSE
    suggestSupervised   <<- TRUE
  }
  if(suggestSupervised){
    toLog(where = 'suggestSupervised', what = paste0('Step: ', step))
    suggestSupervised   <<- FALSE
  }
  if(!(suggestRuleBased & suggestUnsupervised & suggestSupervised)){
    stopSuggesting <<- TRUE
  }
  
  
  # Logout
  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function at step: ', step))
  
  return(suggestions)
}



