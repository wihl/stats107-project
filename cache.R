# Download and cache stock symbol data
# Inspired from http://gekkoquant.com/2012/06/01/stock-data-download-saving-r/
library(quantmod)
fromDate = "2000-11-13"
toDate   = "2016-11-13"
funds    = read.csv("data/vanguard.csv",header = T, 
                    colClasses = c("character","character","numeric"))
stocksLst = funds$Ticker
savefilename <- "data/stockdata.RData" #The file to save the data in
startDate = as.Date(fromDate) #Specify what date to get the prices from
maxretryattempts <- 5 #If there is an error downloading a price how many times to retry

#Load the list of ticker symbols from a csv, each row contains a ticker
stockData <- new.env() #Make a new environment for quantmod to store data in
nrstocks = length(stocksLst) #The number of stocks to download

#Download all the stock data
for (i in 1:nrstocks){
  for(t in 1:maxretryattempts){
    
    tryCatch(
      {
        #This is the statement to Try
        #Check to see if the variables exists
        #NEAT TRICK ON HOW TO TURN A STRING INTO A VARIABLE
        #SEE  http://www.r-bloggers.com/converting-a-string-to-a-variable-name-on-the-fly-and-vice-versa-in-r/
        if(!is.null(eval(parse(text=paste("stockData$",stocksLst[i],sep=""))))){
          #The variable exists so dont need to download data for this stock
          #So lets break out of the retry loop and process the next stock
          #cat("No need to retry")
          break
        }
        
        #The stock wasnt previously downloaded so lets attempt to download it
        cat("(",i,"/",nrstocks,") ","Downloading ", stocksLst[i] , "\t\t Attempt: ", t , "/", maxretryattempts,"\n")
        getSymbols(stocksLst[i], env = stockData, src = "yahoo", 
                   from = startDate, to=toDate, adjust=T)
      }
      #Specify the catch function, and the finally function
      , error = function(e) print(e))
  }
}

#Lets save the stock data to a data file
tryCatch(
  {
    save(stockData, file=savefilename)
    cat("Sucessfully saved the stock data to %s",savefilename)
  }
  , error = function(e) print(e))

