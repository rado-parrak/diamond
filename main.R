# main script for simulating the data checking process: human data validator <-> machine
# by: Radovan Parrak
rm(list = ls())


# --- SETUP ---
source("credoDiamond.R")
require(dplyr)

load("dataQualityInputData.RData")
load("dataQualityKey.RData")

maxIterations = 10

uniRules <- data.frame()
uniRules <- rbind(uniRules, data.frame(name = "currentOutstanding", min = 0,  max = 5000, type = "| out-of-bounds"))
uniRules <- rbind(uniRules, data.frame(name = "discountRate", min = 0,  max = 1, type = "| out-of-bounds"))
uniRules <- rbind(uniRules, data.frame(name = "maturity", min = 0,  max = 360, type = "| out-of-bounds"))

settings <- 
# -------------

feedback <- NULL
precisionHistory <- c(0)

suggestions <- credoDiamond.suggest(inputData, uniRules, feedback, settings)
feedback    <- human.check(suggestions)
precision   <- human.evaluate(suggestions)

precisionHistory <- c(precisionHistory, precision)
precisionGain <- precisionHistory[length(precisionHistory)] - precisionHistory[length(precisionHistory)-1]