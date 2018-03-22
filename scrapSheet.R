# scrapsheet

library("cluster")
library("factoextra")
library("magrittr")
library("fpc")
library("plotly")

eps <- 0.0000001

clustData <- inputData %>%
  dplyr::select(currentOutstanding, discountRate, maturity, marketValuation) %>%
  na.omit()

for(i in 1:ncol(clustData))
  clustData[,i] <- (clustData[,i] - min(clustData[,i])) / (max(clustData[,i]) - min(clustData[,i]) + eps)

set.seed(21)
db <- fpc::dbscan(clustData, eps = 0.5, MinPts = 5)

plotData <- inputData %>% dplyr::mutate(cluster = db$cluster)

plot_ly(x = ~plotData$currentOutstanding
        , y = ~plotData$maturity
        , z = ~plotData$marketValuation
        , type = "scatter3d"
        , mode = "markers"
        , color = ~as.factor(plotData$cluster))