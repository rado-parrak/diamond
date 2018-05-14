# CREDO DIAMOND "CLASS"
library(dplyr)
library(cluster)
library(factoextra)
library(fpc)

addToSuggestionContainer <- function(suggestions){
  suggestionContainer <<- rbind(suggestionContainer, suggestions)
}

findClosest <- function(clustData, db, settings, inputData){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function.'))
  
  medoids <- cbind(clustData, cluster = db$cluster) %>% 
    dplyr::group_by(cluster) %>% 
    dplyr::summarise(median_currentOutstanding = median(currentOutstanding)
                     , median_marketValuation = median(marketValuation)
                     , median_discountRate = median(discountRate)
                     , median_maturity = median(maturity))
  
  clustData <- cbind(clustData, cluster = db$cluster)
  euc_dist <- NULL
  for(i in 1:nrow(clustData)){
    a           <- as.numeric(medoids[which(clustData$cluster[i] == medoids$cluster),2:5])
    b           <- as.numeric(clustData[i,1:4])
    euc_dist[i] <- sqrt(sum((a-b)^2))
  }
  
  clustData <- cbind(clustData, euc_dist, ID = inputData$ID)
  s <- clustData %>% 
    dplyr::group_by(cluster) %>% 
    dplyr::top_n(-settings$nSugestions, euc_dist)  
  
  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function.'))
  return(s)
}

generateSuggestions <- function(inputData, rules,step, type, settings){
  toLog(where = as.character(match.call()[[1]]), what = paste0('Entering function at step: ', step))
  # ============================================ RULE BASED =============================================
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
  
  # ============================================ UNSUPERVISED =============================================
  if(type == "Unsupervised"){
    toLog(where = "Start type: Unsupervised", what = paste0('Step: ', step))
    if(nrow((dplyr::filter(suggestionContainer, level == "Unsupervised", feedback == 1))) > 0){ # continuing from a feedback on 'seed' anomaly.
      
      print("I am groot!")
      
    } else{ # 'seed' suggest for collective anomaly
      suggestions <- NULL
      toLog(where = "START | Looking for seed suggestion for collective anomaly", what = paste0('Step: ', step))
      
      # get rid of data from previous 'seed-clusters' attempts
      inputData <- dplyr::filter(inputData, !(ID %in% suggestionContainer[suggestionContainer$level == 'Unsupervised',]$recordID))
      
      # find seed clusters:
      clustData <- inputData %>%
        dplyr::select(currentOutstanding, discountRate, maturity, marketValuation) %>%
        na.omit()
      
      for(i in 1:ncol(clustData))
        clustData[,i] <- (clustData[,i] - min(clustData[,i])) / (max(clustData[,i]) - min(clustData[,i]) + settings$eps)
      
      lookForSeedClusters <- TRUE
      seedTry <- 0
      while(lookForSeedClusters & (seedTry <= settings$nTry)){
        set.seed(21+seedTry)
        db <- fpc::dbscan(clustData, eps = settings$clustEps, MinPts = settings$clustMinPoints)  
        
        # summarize
        sumTable <- as.data.frame(table(db$cluster))
        for(i in 1:nrow(sumTable)){
          toLog(where = paste0("seedTry: ",seedTry," | Cluster ", as.character(sumTable$Var1[i])," | "), what = paste0('Number of instances: ', as.character(sumTable$Freq[i])))
        }
        
        if(nrow(sumTable) > 1){ #found at least 2 clusters
            lookForSeedClusters <- FALSE
            
            # suggest closest to "Nsuggestions" closest to centroids
            s <- findClosest(clustData, db, settings, inputData)    
            
            # add to the overall 'suggestion' container
            if(nrow(s) > 0){
              suggestions <- data.frame(recordID = s$ID
                                , stepID = step
                                , reason = paste0("Medoid of cluster: ", as.character(s$cluster))
                                , level = "Unsupervised"
                                , score = NA
                                , feedback = NA)
          } 
        }
       seedTry <- seedTry+1  
      }      
      
      toLog(where = "END |Looking for seed suggestion for a collective anomaly", what = paste0('Step: ', step))
    } 
    toLog(where = "End type: Unsupervised", what = paste0('Step: ', step))
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
    suggestions <- generateSuggestions(inputData, rules, step, type = "RuleBased", settings)
    if(is.null(suggestions)){
      suggestRuleBased    <<- FALSE
      suggestUnsupervised <<- TRUE
    } else{
      addToSuggestionContainer(suggestions)
    }
  }
  if(suggestUnsupervised){
    toLog(where = 'suggestUnspervised', what = paste0('Step: ', step))
    suggestions <- generateSuggestions(inputData, rules, step, type = "Unsupervised", settings)
    if(is.null(suggestions)){
      suggestUnsupervised <<- FALSE
      suggestSupervised   <<- TRUE
    } else{
      addToSuggestionContainer(suggestions)
    }
  }
  if(suggestSupervised){
    toLog(where = 'suggestSupervised', what = paste0('Step: ', step))
    suggestSupervised   <<- FALSE
  }
  
  if(!(suggestRuleBased | suggestUnsupervised | suggestSupervised)){
    stopSuggesting <<- TRUE
  }
  # Logout
  toLog(where = as.character(match.call()[[1]]), what = paste0('Exiting function at step: ', step))
}



