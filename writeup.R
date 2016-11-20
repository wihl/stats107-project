# Load libraries
library(ggplot2)
library(quantmod)
library(knitr)
#
# Import List of Securities
#
loadRData = function(filename) {
  # Source: http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  load(filename)
  get(ls()[ls()!="filename"])
}
fromDate = "2000-11-13"
toDate   = "2016-11-13"
funds    = read.csv("data/vanguard.csv",header = T, stringsAsFactors = F)
# Load from previously cached download
stockDataEnv= loadRData("data/stockData.RData")
fundData = mget(funds$Ticker,stockDataEnv)
#
# Generate Summary Statistics
#
for (i in 1:nrow(funds)) {
  sym = funds$Ticker[i]
  data = eval(parse(text=paste("fundData$",sym,sep="")))
  if(!is.null(data)){
    # we have the stockdata; Calculate summary statistics
    funds$startDate[i] = as.Date(index(data[1]))
    funds$endDate[i] = as.Date(index(last(data[])))
    startPrice = as.numeric(Ad(data[1]))
    endPrice = as.numeric(Ad(last(data[])))
    funds$totalRet[i] = (endPrice - startPrice) / startPrice
    dailyRets = dailyReturn(Ad(data))
    funds$stdDev[i] = sd(dailyRets)
    funds$avgRet[i] = mean(dailyRets)
    funds$Sharpe[i] = funds$avgRet[i] / funds$stdDev[i]
    # CAGR
    years =  as.numeric((as.Date(funds$endDate[i]) -
                           as.Date(funds$startDate[i])))/365
    funds$CAGR[i] = ((endPrice / startPrice)^(1/years)) - 1

  }
}
#
# Generate Tables of Top Performers
#
colsToDisplay = c("Ticker","Fund.Name","Expenses","CAGR","Sharpe")
kable(head(funds[order(-funds$Sharpe),colsToDisplay]), caption="Best Sharpe Ratio",
      row.names = F,digits=2)

kable(head(funds[order(funds$Sharpe),colsToDisplay]), caption="Worst Sharpe Ratio",
      row.names = F,digits=2)

kable(head(funds[order(-funds$CAGR),colsToDisplay]), caption="Best CAGR",
      row.names = F,digits=2)

kable(head(funds[order(funds$CAGR),colsToDisplay]), caption="Worst CAGR",
      row.names = F,digits=2)

#
# Show histograms with base VFIAX case
#
par(mfrow=c(3,1), par=rep(4,2))

# CAGR
hist(funds$CAGR,main="CAGR (all funds)",xlab="")
abline(v=funds[funds$Ticker=="VFIAX",]$CAGR,col="blue")

# Standard Deviation
hist(funds$stdDev,main="Standard Deviation (all funds)",xlab="")
abline(v=funds[funds$Ticker=="VFIAX",]$stdDev,col="blue")

# Sharpe Ratio
hist(funds$Sharpe,main="Sharpe Ratio (all funds)",xlab="")
abline(v=funds[funds$Ticker=="VFIAX",]$Sharpe,col="blue")
#
# Compare claimed Expense Ratios vs. Actual Expense Ratios
#
getSymbols("SPY", src="yahoo", from=as.Date(fromDate),to=toDate,adjust=T)
p0 = as.numeric(Ad(SPY[1]))
p1 = as.numeric(Ad(last(SPY[])))
tot = (p1 - p0)/p0

# Compound expenses over an expected 16 year period
expectedVFIAX = round((1.0005 ^ 16 - 1) * 100, 2)
expectedVFINX = round((1.0015 ^ 16 - 1) * 100, 2)
# Determine the percentage difference in total return 
spyOverVFIAX = (tot - funds[funds$Ticker=="VFIAX",]$totalRet)/ funds[funds$Ticker=="VFIAX",]$totalRet
spyOverVFINX = (tot - funds[funds$Ticker=="VFINX",]$totalRet)/ funds[funds$Ticker=="VFINX",]$totalRet
pctOverVFIAX = round(spyOverVFIAX / expectedVFIAX * 100,1)
pctOverVFINX = round(spyOverVFINX / expectedVFINX * 100,1)

years =  as.numeric((as.Date(funds[funds$Ticker=="VFIAX",]$endDate)) -
                     as.Date(funds[funds$Ticker=="VFIAX",]$startDate  ))/365
spy.CAGR =  (p1 / p0)^(1/years)  - 1

# Plot Difference in Adjusted Price based on Expense Ratio
plot(Ad(SPY), main="S&P 500 vs. VFIAX, VFINX",ylab="Adjusted Price")
lines(Ad(fundData$VFIAX),col="blue")
lines(Ad(fundData$VFINX),col="red")
legend("bottomright",c("SPY","VFIAX","VFINX"),fill=c("black","blue","red"))
## 
## 
