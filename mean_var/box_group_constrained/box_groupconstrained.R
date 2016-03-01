library(fPortfolio)
library(tseries)
stock1 = read.csv("Book3.csv")
stock1 <- stock1[rev(rownames(stock1)),]
lppdata <- stock1[, 1:7]
colnames(lppdata)
boxgroupSpec <- portfolioSpec()
setNFrontierPoints(boxgroupSpec) <- 15
boxgroupConstraints <- c(boxConstraints,
                           groupConstraints)
boxgroupFrontier <- portfolioFrontier(
  data = as.timeSeries(lppdata),
  spec = boxgroupSpec,
  constraints = boxgroupConstraints)
print(boxgroupFrontier)




boxgroupSpec <- portfolioSpec()
setNFrontierPoints(boxgroupSpec) <- 100
boxgroupConstraints <- c(boxConstraints,
                         groupConstraints)
boxgroupFrontier <- portfolioFrontier(
  data = as.timeSeries(lppdata),
  spec = boxgroupSpec,
  constraints = boxgroupConstraints)
tailoredFrontierPlot(
  object = boxgroupFrontier,
  mText = "MV Portfolio - Box/Group Constraints",
  risk = "Cov")



weightsPlot(boxgroupFrontier, mtext = FALSE)
text <- "MV Portfolio - Box/Group Constrained Portfolio"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(boxgroupFrontier, mtext = FALSE)
covRiskBudgetsPlot(boxgroupFrontier, mtext = FALSE)


