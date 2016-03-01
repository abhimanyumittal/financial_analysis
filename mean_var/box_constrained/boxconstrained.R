library(fPortfolio)
library(tseries)
stock1 = read.csv("Book3.csv")
stock1 <- stock1[rev(rownames(stock1)),]
lppdata <- stock1[, 1:7]
colnames(lppdata)
boxSpec <- portfolioSpec()
setNFrontierPoints(boxSpec) <- 15
boxConstraints <- c(
  "minW[1:6]=0.1",
  "maxW[1:6]=0.5")
boxFrontier <- portfolioFrontier(
  data = as.timeSeries(lppdata),
  spec = boxSpec,
  constraints = boxConstraints)
print(boxFrontier)


setNFrontierPoints(boxSpec) <- 25
boxFrontier <- portfolioFrontier(data = as.timeSeries(lppdata), spec = boxSpec,
                                   constraints = boxConstraints)
tailoredFrontierPlot(object = boxFrontier, mText = "MV Portfolio - Box
Constraints",
                       risk = "Cov")



weightsPlot(boxFrontier)
text <- "MV Portfolio - Box Constrained Portfolio"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(boxFrontier)
covRiskBudgetsPlot(boxFrontier)