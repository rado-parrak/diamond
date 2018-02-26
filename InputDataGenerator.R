# Input Data Generator
# By Radovan Parrak
# The idea is to generate a simple data using a known DGP with some data errors both
# (a) univariate error
# (b) errors not spotable in the univariate setting


rm(list=ls())
library(dplyr)
library(ggplot2)
library(lattice)
library(akima)
library(rgl)
library(plotly)
library(grid)
# -------------------------------------------------
# Settings:

nData       <- 1000
nErrorType  <- 5 
set.seed(21)

# -------------------------------------------------

# --- A: Correct Data ---
correctData <- data.frame( ID = sample(1:(nData*2),nData,replace=F)
                          , date = as.Date("2017-12-31")
                          , currentOutstanding = rlnorm(nData, meanlog = 6, sdlog = 1) 
                          , discountRate = rnorm(nData, mean = 0.02, sd = 0.01)
                          , maturity = sample(1:360,nData,replace=T))
correctData$currentOutstanding[correctData$currentOutstanding > 5000] <- 5000
correctData$discountRate[correctData$discountRate > 1] <- 1
correctData$discountRate[correctData$discountRate < 0] <- 0
correctData <- mutate(correctData, marketValuation = (currentOutstanding/(1+discountRate)^(maturity/12)) + runif(nData, min = -100, max = 100))
# --- B: Data errors ---
# B.i) Univariate
errorsUniv <- data.frame()
# B.i.i) negative current outstanding
for(i in 1:nErrorType){
  eId <- paste0("eu_type1_", i)
  co  <- runif(1, min = - 100, max = - 5)
  dr  <- runif(1, min = 0.023, max = 0.03)
  m   <- floor(runif(1, min = 1, max = 360))
    
  errorsUniv <- rbind(errorsUniv, data.frame(ID = eId, date = as.Date("2017-12-31"), currentOutstanding = co, discountRate = dr, maturity = m))
}
# B.i.ii) negative EIR
for(i in 1:nErrorType){
  eId <- paste0("eu_type2_", i)
  co  <- runif(1, min = 0, max = 5000)
  dr  <- runif(1, min = - 0.023, max = -0.0003)
  m   <- floor(runif(1, min = 1, max = 360))
  
  errorsUniv <- rbind(errorsUniv, data.frame(ID = eId, date = as.Date("2017-12-31"), currentOutstanding = co, discountRate = dr, maturity = m))
}

errorsUniv <- mutate(errorsUniv, marketValuation = (currentOutstanding/(1+discountRate)^(maturity/12)) + runif(nrow(errorsUniv), min = -5, max = 5))
# B.ii) Multi-dimensional inconsistencies
errorsMulti <- data.frame()
for(i in 1:nErrorType){
  eId <- paste0("em_type1_", i)
  co  <- runif(1, min = min(correctData$currentOutstanding), max = min(correctData$currentOutstanding)*1.5)
  dr  <- runif(1, min = max(correctData$discountRate)*0.8, max = max(correctData$discountRate))
  m   <- floor(runif(1, min = max(correctData$maturity)*0.8, max = max(correctData$maturity)))
  mv  <- runif(1, min = 4000, max = 5000)
  errorsMulti <- rbind(errorsMulti, data.frame(ID = eId, date = as.Date("2017-12-31"), currentOutstanding = co, discountRate = dr, maturity = m, marketValuation = mv))
}

# Binding
inputData <- rbind(correctData, errorsUniv)
inputData <- rbind(inputData, errorsMulti)
save(inputData, file = "dataQualityInputData.RData")

key <- mutate(inputData, isDataError = ifelse(grepl("e",ID), 1, 0))
save(key, file = "dataQualityKey.RData")




# Visualisation
# i) univariate
# ggplot(data=correctData, aes(currentOutstanding)) + 
#   geom_histogram(breaks=seq(0, 5000, by =500), 
#                  col="red")
# 
# ggplot(data=correctData, aes(discountRate)) + 
#   geom_histogram(breaks=seq(0, 0.2, by =0.01), 
#                  col="red")

# ii) multi-variate
nSurf <- 50
xSurf <- seq(0, max(correctData$maturity), length.out = nSurf)
ySurf <- seq(0, max(correctData$currentOutstanding), length.out = nSurf)
zSurf <- matrix(data = NA, nrow = nSurf, ncol = nSurf, byrow = FALSE)

for(i in 1:nSurf){
  for(j in 1:nSurf){
    zSurf[[i,j]] <- ySurf[i]/(1+mean(correctData$discountRate))^(xSurf[j]/12)
  }
}

plot_ly(x = xSurf, y = ySurf, z = zSurf, type = "surface") %>% 
  add_trace( x = ~as.numeric(correctData$maturity)
            , y = ~as.numeric(correctData$currentOutstanding)
            , z = ~as.numeric(correctData$marketValuation)
            , mode = "markers"
            , type = "scatter3d", 
            marker = list(size = 3, color = "blue", symbol = 104)) %>%
  add_trace( x = ~as.numeric(errorsMulti$maturity)
             , y = ~as.numeric(errorsMulti$currentOutstanding)
             , z = ~as.numeric(errorsMulti$marketValuation)
             , mode = "markers"
             , type = "scatter3d", 
             marker = list(size = 5, color = "red", symbol = 104))


