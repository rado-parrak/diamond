# CREDO DIAMOND "CLASS"

credoDiamond.suggest <- function(inputData, rules, feedback, settings){
  
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
  if(!is.null(suggestionContainer)){
    if(nrow(filter(suggestionContainer, type == "TYPE 1")) == 0){
      
      dataWithClusters    <- credoDiamond.cluster(inputData)
      nSuggestedClusters  <- length(unique(dataWithClusters$clusters))
      if(nSuggestedClusters > 2){
        # select a representative from the smallest "N" clusters
        
      }
      
    }    
  }

  # --------------------------------------------------------
  # ---           3) Order and return                    ---
  # --------------------------------------------------------
  if(!is.null(suggestionContainer)){
    suggestionContainer <- suggestionContainer %>% 
      dplyr::arrange(desc(prob)) %>% 
      dplyr::filter(!(ID %in% archive$feedbacks$ID))
    suggestionContainer <- suggestionContainer[1:settings$nSugestions,]
  }
  return(suggestionContainer)
}

credoDiamond.archive <- function(round, feedback, precision){
  
  if(is.null(archive)){
    archive[["feedbacks"]] <- data.frame()
    archive[["precisionHistory"]] <- data.frame(round = 0
                                                , precision = 0
                                                , hits = 0
                                                , hitRate = 0)
    
  }
  
  aux <- feedback %>% dplyr::mutate(round = round)
  
  archive$feedbacks <<- rbind(archive$feedbacks, aux)
  archive$precisionHistory <<- rbind(archive$precisionHistory, data.frame(round = round
                                                                          , precision = precision
                                                                          , hits = (archive$precisionHistory$hits[length(archive$precisionHistory$hits)] + nrow(dplyr::filter(feedback, isDataError == 1))) 
                                                                          , hitRate = nrow(dplyr::filter(feedback, isDataError == 1)) / nrow(feedback) ))
  
}

credoDiamond.clusters <- function(inputData, nClusters){
  
  
  
}
