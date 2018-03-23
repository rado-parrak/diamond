# main script for simulating the data checking process: human data validator <-> machine
# by: Radovan Parrak
rm(list = ls())


# --- SETUP ---
source("credoDiamond.R")
source("human.R")
source("utils.R")
require(dplyr)

load("dataQualityInputData.RData")
load("dataQualityKey.RData")
load("dataQualitySolutions.RData")

maxIterations = 10

uniRules <- data.frame()
uniRules <- rbind(uniRules, data.frame(name = "currentOutstanding", min = 0,  max = 5000, type = "| out-of-bounds"))
uniRules <- rbind(uniRules, data.frame(name = "discountRate", min = 0,  max = 1, type = "| out-of-bounds"))
uniRules <- rbind(uniRules, data.frame(name = "maturity", min = 0,  max = 360, type = "| out-of-bounds"))

settings <- list()
settings["nSugestions"] <- 1

# Global vars: (dirty but R...)
archive             <<- NULL #global var!
logg                <<- data.frame() #global var
suggestRuleBased    <<- TRUE
suggestUnsupervised <<- FALSE
suggestSupervised   <<- FALSE
stopSuggesting      <<- FALSE
suggestionContainer <<- data.frame()

# -------------

feedback          <- NULL
precisionHistory  <- NULL
averageHitRate    <- 1
rround            <- 0
  
while(!stopSuggesting){
  rround <- rround + 1
  print(paste0(" ---- ROUND : ", rround, " -----"))
  
  # 1) CREDO DIAMOND: Suggest
  suggestions <- credoDiamond.suggest(inputData, uniRules, feedback, settings, rround)

  # 2) HUMAN: Check & Feedback
  #feedback    <- human.check(suggestions, key)
  #precision   <- human.evaluate(feedback)
  
  # 3) CREDO DIAMOND: Archive & Learn
  #credoDiamond.archive(rround, feedback, precision)
  #averageHitRate <- mean(archive$precisionHistory$hitRate[2:(rround+1)])
  
  # 4) HUMAN: Correct
  #inputData   <- human.correct(inputData, feedback, solutions)   
}


