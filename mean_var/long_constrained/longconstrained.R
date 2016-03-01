library(fPortfolio)
library(tseries)
stock1 = read.csv("Book3.csv")
stock1 <- stock1[rev(rownames(stock1)),]
lppdata <- stock1[, 1:7]
colnames(lppdata)
lppSpec <- portfolioSpec()
setNFrontierPoints(lppSpec) <- 5
longFrontier <- portfolioFrontier(as.timeSeries(lppdata),lppSpec,constraints = "LongOnly")
print(longFrontier)
longFrontier <- portfolioFrontier(lppdata)
plot(longFrontier,xlim=c(0, 1000), ylim=c(0, 2500))



setNFrontierPoints(lppSpec) <- 25
longFrontier <- portfolioFrontier(as.timeSeries(lppdata), lppSpec)
tailoredFrontierPlot(object = longFrontier, mText = "MV Portfolio - LongOnly
Constraints",risk = "Cov")



weightsPlot(longFrontier, mtext = FALSE)
text <- "Mean-Variance Portfolio - Long Only Constraints"
mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
weightedReturnsPlot(longFrontier, mtext = FALSE)
covRiskBudgetsPlot(longFrontier, mtext = FALSE)



par(mfrow = c(1, 1))
set.seed(1953)
frontierPlot(object = longFrontier, pch = 19, xlim = c(10,500), cex = 0.5)
monteCarloPoints(object = longFrontier, mcSteps = 1000, pch = 19,cex = 0.5)
twoAssetsLines(object = longFrontier, col = "orange", lwd = 2)
frontier <- frontierPoints(object = longFrontier)
lines(frontier, col = "red", lwd = 2)


