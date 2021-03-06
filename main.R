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
settings["nSugestions"]     <- 1
settings["eps"]             <- 0.000001
settings["clustMinEps"]     <- 0.1
settings["clustMaxEps"]     <- 0.3
settings["clustMinPoints"]  <- 3
settings["gridSize"]        <- 5 # number of trials for 'seed clustering'

# Global vars: (dirty but R...)
logg                <<- data.frame() #global var
suggestRuleBased    <<- TRUE
suggestUnsupervised <<- FALSE
suggestSupervised   <<- FALSE
stopSuggesting      <<- FALSE
suggestionContainer <<- data.frame()

# --------- MAIN WHILE-LOOP --------- 
rround            <- 0

while(!stopSuggesting){
  rround <- rround + 1
  print(paste0(" ---- ROUND : ", rround, " -----"))
  
  # 1) CREDO DIAMOND: Suggest
  credoDiamond.suggest(inputData, uniRules, settings, rround)
  # 2) HUMAN: Check & Feedback
  human.check(key, rround)
  print(paste0("Precision at step ", as.character(rround), " was: ", as.character(human.evaluate(rround),".")))
  # 3) HUMAN: Corrects
  inputData <- human.correct(inputData, solutions, rround)
}


