# Load libraries
library(ggplot2)
library(quantmod)
library(knitr)
library(stockPortfolio) # Alternative to quantmod
library(reshape2)
library(quadprog) # Quadratic programming library


# dot product
"%.%" <- function(x,y) sum(x*y)
# Calculate percent ready for display
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# Constants
fromDate = "2000-11-13"
toDate   = "2016-11-13"
startCash = 100000
#
# Import List of Securities
#
loadRData = function(filename) {
  # Source: http://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
  load(filename)
  get(ls()[ls()!="filename"])
}
funds    = read.csv("data/vanguard.csv",header = T, stringsAsFactors = F)
# Load from previously cached download
stockDataEnv= loadRData("data/stockData.RData")
fundData = mget(funds$Ticker,stockDataEnv)
# Clean some specific missing or extra values
fundData$VBMFX = fundData$VBMFX[index(fundData$VBMFX) != '2001-11-22']

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
    funds$startPrice[i] = as.numeric(Ad(data[1]))
    funds$endPrice[i] = as.numeric(Ad(last(data[])))
    funds$totalRet[i] = (funds$endPrice[i] - funds$startPrice[i]) / funds$startPrice[i]
    dailyRets = dailyReturn(Ad(data))
    funds$stdDev[i] = sd(dailyRets)
    funds$avgRet[i] = mean(dailyRets)
    funds$Sharpe[i] = funds$avgRet[i] / funds$stdDev[i]
    # CAGR
    years =  as.numeric((as.Date(funds$endDate[i]) -
                           as.Date(funds$startDate[i])))/365
    funds$CAGR[i] = ((funds$endPrice[i] / funds$startPrice[i])^(1/years)) - 1

  }
}
# Store some VFIAX data in separate variables for convenience
VFIAX.CAGR = funds[funds$Ticker=="VFIAX","CAGR"]
VFIAX.stdDev = funds[funds$Ticker=="VFIAX","stdDev"]
VFIAX.startPrice = funds[funds$Ticker=="VFIAX","startPrice"]
VFIAX.endPrice = funds[funds$Ticker=="VFIAX","endPrice"]
VFIAX.Sharpe = funds[funds$Ticker=="VFIAX","Sharpe"]
VFIAX.totReturn =funds[funds$Ticker=="VFIAX","totalRet"]

#
# Generate Tables of Top Performers
#
colsToDisplay = c("Ticker","Fund.Name","Expenses","CAGR","Sharpe")
kable(head(funds[order(-funds$Sharpe),colsToDisplay],n=3), caption="Best Sharpe Ratio",
      row.names = F,digits=2)

kable(head(funds[order(funds$Sharpe),colsToDisplay],n=3), caption="Worst Sharpe Ratio",
      row.names = F,digits=2,n=3)

kable(head(funds[order(-funds$CAGR),colsToDisplay],n=3), caption="Best CAGR",
      row.names = F,digits=2,n=3)

kable(head(funds[order(funds$CAGR),colsToDisplay],n=3), caption="Worst CAGR",
      row.names = F,digits=2,n=3)

#
# Show histograms with base VFIAX case
#
par(mfrow=c(3,1))

# CAGR
hist(funds$CAGR,main="CAGR (all funds)",xlab="",breaks=20)
abline(v=VFIAX.CAGR,col="blue")

# Standard Deviation
hist(funds$stdDev,main="Standard Deviation of Returns (all funds)",xlab="",breaks=20)
abline(v=VFIAX.stdDev,col="blue")

# Sharpe Ratio
hist(funds$Sharpe,main="Sharpe Ratio (all funds)",xlab="",breaks=20)
abline(v=VFIAX.Sharpe,col="blue")
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
spyOverVFIAX = round((tot - funds[funds$Ticker=="VFIAX",]$totalRet)/ funds[funds$Ticker=="VFIAX",]$totalRet*100,1)
spyOverVFINX = round((tot - funds[funds$Ticker=="VFINX",]$totalRet)/ funds[funds$Ticker=="VFINX",]$totalRet*100,1)
pctOverVFIAX = round((expectedVFIAX - spyOverVFIAX) / expectedVFIAX * 100,1)
pctOverVFINX = round((expectedVFINX - spyOverVFINX) / expectedVFINX * 100,1)

years =  as.numeric((as.Date(funds[funds$Ticker=="VFIAX",]$endDate)) -
                     as.Date(funds[funds$Ticker=="VFIAX",]$startDate  ))/365
spy.CAGR =  (p1 / p0)^(1/years)  - 1

# Plot Difference in Adjusted Price based on Expense Ratio
plot(Ad(SPY), main="S&P 500 vs. VFIAX, VFINX",ylab="Adjusted Price")
lines(Ad(fundData$VFINX),col="red",lwd=.1)
lines(Ad(fundData$VFIAX),col="blue",lwd=.5)
legend("bottomright",c("SPY","VFIAX","VFINX"),fill=c("black","blue","red"))
#
# Compare VFIAX to VTHRX over equivalent time periods.
#
# Since VTHRX is younger than VFIAX, recalculate CAGR on matching dates
sDate = as.Date(funds[funds$Ticker=="VTHRX",]$startDate)
startPrice = as.numeric(Ad(fundData$VFIAX[sDate]))
endPrice = as.numeric(Ad(last(fundData$VFIAX[])))
totRet = (endPrice - startPrice) / startPrice
years = as.numeric((as.Date(funds[funds$Ticker=="VTHRX",]$endDate) -
                           as.Date(funds[funds$Ticker=="VTHRX",]$startDate)))/365
CAGR = ((endPrice / startPrice)^(1/years)) - 1
f1 = as.numeric(Ad(fundData$VFIAX[paste0(sDate,"::"),])) / startPrice
f2 = as.numeric(Ad(fundData$VTHRX)) / as.numeric(Ad(fundData$VTHRX[1]))
plot(index(fundData$VTHRX),f1,type="l",main="VTHRX vs. VFIAX (Normalized)",xlab="",ylab="Adjusted Price",col="red")
lines(index(fundData$VTHRX),f2,col="blue",lwd=1)
abline(h=1)
legend("bottomright",c("VFIAX","VTHRX"),fill=c("red","blue"))
#
# Find a better single fund that beats the market with higher CAGR and lower volatility
#
betterFunds = c("VFIAX")
for (i in 1:nrow(funds)) {
  if (funds$CAGR[i] > (1.2 * VFIAX.CAGR)) {
    if (funds$stdDev[i] < VFIAX.stdDev) {
      if (funds$startDate[i] < as.Date("2006-01-01")) {
        #cat(funds$Ticker[i]," ",funds$Fund.Name[i],"\n")
        betterFunds = c(betterFunds, funds$Ticker[i])
      }
    }
  }
}

colsToDisplay = c("Ticker","Fund.Name","startDate","Expenses","CAGR","stdDev")
bf = funds[funds$Ticker %in% betterFunds,colsToDisplay]
bf$startDate = as.Date(bf$startDate)
kable(rbind(head(bf[order(-bf$CAGR),],n=10), tail(bf[order(-bf$CAGR),],n=1)),
        caption="Top 10 Overall better funds than VFIAX",
      row.names = F,digits=3)

f1 = as.numeric(Ad(fundData$VFIAX)) / as.numeric(Ad(fundData$VFIAX[1]))
f2 = as.numeric(Ad(fundData$VASVX)) / as.numeric(Ad(fundData$VASVX[1]))
f3 = as.numeric(Ad(fundData$VGHCX)) / as.numeric(Ad(fundData$VGHCX[1]))
f4 = as.numeric(Ad(fundData$VWESX)) / as.numeric(Ad(fundData$VWESX[1]))
f5 = as.numeric(Ad(fundData$VHGEX)) / as.numeric(Ad(fundData$VHGEX[1]))

plot(index(fundData$VFIAX),f2,type="l",main="VFIAX vs. Better Funds (Normalized)",xlab="",ylab="Adjusted Price",col="blue")
lines(index(fundData$VFIAX),f1,col="black",lwd=0.5)
lines(index(fundData$VFIAX),f3,col="red",lwd=0.5)
lines(index(fundData$VFIAX),f4,col="green",lwd=0.5)
lines(index(fundData$VFIAX),f5,col="orange",lwd=0.5)
abline(h=1)
legend("topleft",c("VFIAX","VASVX","VGHCX","VWESX","VHGEX"),fill=c("black","blue","red","green","orange"))
#
# Compare against a diversified set of funds recommended by Vanguard
#
suggestions = c("VTSMX","VGTSX","VBMFX","GMHBX")
getSymbols("GMHBX", src="yahoo", from=as.Date(fromDate),to=toDate,adjust=T)
weights = c(.36,.24,.28,.12)
# Build covariance matrix
returns = cbind(dailyReturn(fundData$VTSMX),dailyReturn(fundData$VGTSX),dailyReturn(fundData$VBMFX),dailyReturn(GMHBX))
# delete one extra row from VBMFX
returns = returns[index(returns) !='2001-11-22']
names(returns) = suggestions
cov.mat = cov(returns)  # annualized
risk.p = sqrt(t(weights) %*% cov.mat %*% weights)
shares = rep(0,length(suggestions))
startValue = rep(0,length(suggestions))
endValue = rep(0,length(suggestions))
CAGR = rep(0,length(suggestions))
# Find total return on three known funds
for (i in 1:length(suggestions)-1) {
  startValue[i] = startCash * weights[i]
  shares[i] = startValue[i] / funds[funds$Ticker==suggestions[i],"startPrice"]
  endValue[i] = shares[i] * funds[funds$Ticker==suggestions[i],"endPrice"]
}
# special case for GMHBX
startValue[4] = startCash * weights[4]
shares[4] = startCash * weights[4] / as.numeric(Ad(first(GMHBX)))
endValue[4] = shares[4] * Ad(last(GMHBX))
years =  as.numeric((as.Date(funds[funds$Ticker=="VFIAX","endDate"]) -
                           as.Date(funds[funds$Ticker=="VFIAX","startDate"])))/365
for (i in 1:length(suggestions)) {
  CAGR[i] = 100*(((endValue[i] / startValue[i])^(1/years)) - 1)
}
port = cbind(startValue, endValue,CAGR)
port.CAGR = 100*((sum(endValue) / startCash)^(1/years) - 1)
port = rbind(port, c(sum(startValue), sum(endValue), port.CAGR ))
port = rbind(port, c(startCash,startCash * (1+VFIAX.totReturn), VFIAX.CAGR*100))
rownames(port) = c(suggestions,"Total Portfolio","VFIAX")
colnames(port) = c("Start Value","End Value","CAGR")
port.prices = cbind(as.numeric(Ad(fundData$VTSMX)), as.numeric(Ad(fundData$VGTSX)),
                    as.numeric(Ad(fundData$VBMFX)), as.numeric(Ad(GMHBX)))

port.value = sweep(port.prices, MARGIN=2, shares,'*')
kable(port, caption="Vanguard Recommended Portfolio Growth", digits=2)

f1 = as.numeric(Ad(fundData$VFIAX)) / as.numeric(Ad(fundData$VFIAX[1]))
f2 = port.prices[,1] / port.prices[1,1]
f3 = port.prices[,2] / port.prices[1,2]
f4 = port.prices[,3] / port.prices[1,3]
f5 = port.prices[,4] / port.prices[1,4]
f6 = rowSums(port.value) / startCash

plot(index(fundData$VFIAX),f2,type="l",main="VFIAX vs. Recommend Portfolio (Normalized)",xlab="",ylab="Adjusted Price",col="blue")
lines(index(fundData$VFIAX),f1,col="black",lwd=0.5)
lines(index(fundData$VFIAX),f3,col="purple",lwd=0.5)
lines(index(fundData$VFIAX),f4,col="green",lwd=0.5)
lines(index(fundData$VFIAX),f5,col="orange",lwd=0.5)
lines(index(fundData$VFIAX),f6,col="red",lwd=2.0)
abline(h=1)
legend("topleft",c("VFIAX","VTSMX","VGTSX","VBMFX","GMHBX","Portfolio"),fill=c("black","blue","purple","green","orange","red"))

ef.startdate = "2013-09-01"
ef.enddate = "2016-09-01"

ef.stocks <-c(
  "VTSMX" = .36,
  "VGTSX" = .24,
  "VBMFX" = .28,
  "GMHBX" = .12
  )

ef.stockReturns <- stockPortfolio::getReturns(names(ef.stocks), freq ="day",get="overlapOnly", start = ef.startdate,end = ef.enddate)


eff.frontier <- function (returns, short="no", max.allocation=NULL,
 risk.premium.up=.5, risk.increment=.005){
 # return argument should be a m x n matrix with one column per security
 # short argument is whether short-selling is allowed; default is no (short
 # selling prohibited)max.allocation is the maximum % allowed for any one
 # security (reduces concentration) risk.premium.up is the upper limit of the
 # risk premium modeled (see for loop below) and risk.increment is the
 # increment (by) value used in the for loop
 
 covariance <- cov(returns)
 print(covariance)
 n <- ncol(covariance)
 
 # Create initial Amat and bvec assuming only equality constraint
 # (short-selling is allowed, no allocation constraints)
 Amat <- matrix (1, nrow=n)
 bvec <- 1
 meq <- 1
 
 # Then modify the Amat and bvec if short-selling is prohibited
 if(short=="no"){
 Amat <- cbind(1, diag(n))
 bvec <- c(bvec, rep(0, n))
 }
 
 # And modify Amat and bvec if a max allocation (concentration) is specified
 if(!is.null(max.allocation)){
 if(max.allocation > 1 | max.allocation <0){
 stop("max.allocation must be greater than 0 and less than 1")
 }
 if(max.allocation * n < 1){
 stop("Need to set max.allocation higher; not enough assets to add to 1")
 }
 Amat <- cbind(Amat, -diag(n))
 bvec <- c(bvec, rep(-max.allocation, n))
 }
 
 # Calculate the number of loops
 loops <- risk.premium.up / risk.increment + 1
 loop <- 1
 
 # Initialize a matrix to contain allocation and statistics
 # This is not necessary, but speeds up processing and uses less memory
 eff <- matrix(nrow=loops, ncol=n+3)
 # Now I need to give the matrix column names
 colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
 
 # Loop through the quadratic program solver
 for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
 dvec <- colMeans(returns) * i # This moves the solution along the EF
 sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
 eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
 eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
 eff[loop,"sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
 eff[loop,1:n] <- sol$solution
 loop <- loop+1
 }
 
 return(as.data.frame(eff))
}
 
# Run the eff.frontier function based on no short and 50% alloc. restrictions
eff <- eff.frontier(returns=ef.stockReturns$R, short="no", max.allocation=.50,
 risk.premium.up=1, risk.increment=.001)
 
# Find the optimal portfolio
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
eff.min.variance = eff[eff$Std.Dev==min(eff$Std.Dev),]

# graph efficient frontier
# Start with color scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"
 
ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
 geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
 color=ealred, size=5) +
 annotate(geom="text", x=eff.optimal.point$Std.Dev,
 y=eff.optimal.point$Exp.Return,
 label=paste("Risk: ",
 round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
 round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
 round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
 hjust=0, vjust=1.2) +
 ggtitle("Efficient Frontier\nof Optimal Vanguard Portfolio") +
 labs(x="Risk (standard deviation of portfolio)", y="Return") +
 theme(panel.background=element_rect(fill=eallighttan),
 text=element_text(color=ealdark),
 plot.title=element_text(size=24, color=ealred))
## Minimum Variance
# The original Buffet Bet was the market vs. a hedge fund consisting of a basket of funds. In retrospect,
# it is not surprising that the market is beating the hedge fund. The primary purpose of the hedge fund
# is the eponymous hedging - that is reducing risk. Hedge funds outperform the market during downtimes
# but underperform over the long term. Their primary raison d'être is captial preservation. This strategy
# will be mimicked by selecting a minimum variance portfolio. The portfolio will be compared against 
# market downturn periods vs. long term.

#source('zivot_code.R')
#
# Minimum Variance portfolio
#
# We are now going to use the Zivot code to also build the efficient frontier
# 
# nFunds = nrow(funds)
# cov.mat = matrix(0,nrow=nFunds, ncol=nFunds)
# colnames(cov.mat) = funds$Ticker
# rownames(cov.mat) = funds$Ticker
# 
# # We have to build the variance-covariance matrix manually rather than using
# # the cov() function because the date lengths are not the same.
# # This code is not efficient as each covariance is calculated twice. It still takes
# # just a few seconds to run. 
# # TODO: move this into the caching R script
# for (i in 1:nFunds) { # nrow(funds)
#   for (j in 1:nFunds) {
#     # start with later date of either
#     sDate = as.Date(max(funds$startDate[i],funds$startDate[j]))
#     r.i = eval(parse(text = paste("fundData$", funds$Ticker[i], sep = "")))
#     r.i = dailyReturn(Ad(r.i[paste0(sDate,"::"),]))
#     r.j = eval(parse(text = paste("fundData$", funds$Ticker[j], sep = "")))
#     r.j = dailyReturn(Ad(r.j[paste0(sDate,"::"),]))
#     # http://r.789695.n4.nabble.com/Finding-the-correlation-coefficient-of-two-stocks-td3246992.html
#     m = merge(r.i,r.j)
#     cov.mat[i,j] = cov(m[,1],m[,2],use="pairwise.complete.obs")
#   }
# }
# 
# TODO. Should we use Zivot? Is there a better way? How can we find optimal weights?
#returns = cbind(dailyReturn(fundData$VTSMX),dailyReturn(fundData$VGTSX),dailyReturn(fundData$VBMFX),dailyReturn(GMHBX))
#names(returns) = tickers
#risk.p = sqrt(t(weights) %*% cov.mat %*% weights)
# returns = c()
# for (i in 1:nFunds) { # nrow(funds)
#   r = eval(parse(text = paste("fundData$", funds$Ticker[i], sep = "")))
#   returns = cbind(returns, dailyReturn(Ad(r)))
# }
# colnames(returns) = funds$Ticker
# cov.mat2 = cov(returns,use="pairwise.complete.obs")
# colnames(cov.mat2) = funds$Ticker
# rownames(cov.mat2) = funds$Ticker
# 
# cor.mat = cor(returns,use="pairwise.complete.obs")
# colnames(cor.mat) = funds$Ticker
# rownames(cor.mat) = funds$Ticker
# source('zivot_code.R')
# rk.free = 0.005
# er = funds$CAGR
# gmin.port = globalMin.portfolio(er,cov.mat) 
# summary(gmin.port, risk.free = rk.free)
## 
## 
