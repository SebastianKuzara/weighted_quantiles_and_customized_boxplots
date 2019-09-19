
setwd("~/Documents/praca/projekty/weighted_quantiles_and_customized_boxplots")

library(data.table)
source("./weighted_quantiles/weighted_quantiles.R")
source("./customized_boxplots/customized_boxplots.R")

set.seed(123)
dt <- data.table(
  Values = sample(1:100, 40),
  Weights = rgamma(40, 2) * 10^3
)
dt[1:3, Weights:=Weights*100]
dt[, Weights:=round(Weights, 0)]
dt
xxx <- copy(dt)
setorder(xxx, Values)
xxx[,WC:=cumsum(Weights)/sum(Weights)]
xxx

plot(density(dt$Weights))
a <- weightedQuantiles(x = dt$Values, weights = dt$Weights)
a

# BP ----------------------------------------------------------------------

summarydata<-list(stats=matrix(c(1,2,3,4,5),5,1), n=10)
summarydata<-list(stats=matrix(c(3.4,1,1,4,5),5,1), n=10)
bxp(summarydata)
