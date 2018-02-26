# CREDO DIAMOND "CLASS"

credoDiamond.suggest <- function(inputData, rules, feedback){
  
  suggestionContainer <- NULL
  suggestions         <- NULL
  
  # --------------------------------------------------------
  # ---           1) check the hard-rules                ---
  # --------------------------------------------------------
  for(i in 1:nrow(rules)){
    rule      <- rules[i,]
    minn      <- rule$min
    maxx      <- rule$max
    checkData <- dplyr::select(inputData, ID, toCheck = eval(as.character(rule$name))) %>%
      dplyr::filter(toCheck < minn | toCheck > maxx)
    
    if(nrow(checkData) > 0){
      suggestions <- checkData %>% select(ID) %>% mutate(prob = 1, reason = paste0(rule$name, " ", rule$type), type = "TYPE 1")
      suggestionContainer <- rbind(suggestionContainer, suggestions)
    }

  }
  
  # --------------------------------------------------------
  # ---                   2) ML-based                    ---
  # --------------------------------------------------------
  if(nrow(filter(suggestionContainer, type == "TYPE 1")) == 0){
    
  }
  
  return(suggestionContainer)
} 