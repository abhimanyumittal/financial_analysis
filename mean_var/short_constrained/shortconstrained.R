library(fPortfolio)
library(tseries)
stock1 = read.csv("Book3.csv")
stock1 <- stock1[rev(rownames(stock1)),]
lppdata <- stock1[, 1:7]
colnames(lppdata)
shortSpec <- portfolioSpec()
setNFrontierPoints(shortSpec) <- 5
setSolver(shortSpec) <- "solveRshortExact"
shortFrontier <- portfolioFrontier(data = as.timeSeries(lppdata),spec = shortSpec,constraints = "Short")
print(shortFrontier)


setNFrontierPoints(shortSpec) <- 25
shortFrontier <- portfolioFrontier(data = as.timeSeries(lppdata), spec = shortSpec,
                                     constraints = "Short")
tailoredFrontierPlot(object = shortFrontier, mText = "MV Portfolio - Short
                       Constraints",
                       risk = "Cov")
weightsPlot(shortFrontier, mtext = FALSE)
text <- "MV Portfolio - Short Constrained Portfolio"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(shortFrontier, mtext = FALSE)
covRiskBudgetsPlot(shortFrontier, mtext = FALSE)



