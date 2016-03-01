library(fPortfolio)
library(tseries)
stock1 = read.csv("Book3.csv")
stock1 <- stock1[rev(rownames(stock1)),]
lppdata <- stock1[, 1:7]
colnames(lppdata)

groupSpec <- portfolioSpec()
setNFrontierPoints(groupSpec) <- 7
groupConstraints <- c("minsumW[c(3,4)]=0.3",
                        "maxsumW[c(2,5)]=0.5")
groupFrontier <- portfolioFrontier(
  data = as.timeSeries(lppdata),
  spec = groupSpec,
  constraints = groupConstraints)
print(groupFrontier)

groupSpec <- portfolioSpec()
setNFrontierPoints(groupSpec) <- 25
groupFrontier <- portfolioFrontier(data = as.timeSeries(lppdata), spec = groupSpec,
                                     constraints = groupConstraints)
tailoredFrontierPlot(object = groupFrontier, mText = "MV Portfolio - Group
Constraints",
                       risk = "Cov")



weightsPlot(groupFrontier, mtext = FALSE)
text <- "MV Portfolio - Group Constrained Portfolio"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(groupFrontier, mtext = FALSE)
covRiskBudgetsPlot(groupFrontier, mtext = FALSE)