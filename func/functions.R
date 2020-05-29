######################
#####  General
######################
# library(PortfolioAnalytics)
library(tseries)

performanceSummary <- function(rts) {
  perf1 = Return.annualized(rts)*100
  perf2 = StdDev.annualized(rts)*100
  perf3 = VaR(rts)*100
  perf4 = as.data.frame(maxDrawdown(rts))*100
  colnames(perf4) = colnames(rts)
  # colnames(perf4) = "V1"
  perf5 = SharpeRatio(rts)
  perf6 = CalmarRatio(rts)
  perf7 = SortinoRatio(rts)
  perf8 = as.data.frame(skewness(na.remove(rts)))
  perf9 = as.data.frame(kurtosis(na.remove(rts)))
  colnames(perf8) = colnames(perf9) = colnames(rts)
  # colnames(perf8) = colnames(perf9) = "V1"
  sumTable = rbind(perf1, perf2, perf3, perf4, perf5, perf6, perf7, perf8, perf9)
  sumTable = round(sumTable, 4)
  rownames(sumTable) = c("Annualized Return (%)", "Annualized Volatility (%)", "VaR (p=.95) (%)",
                         "Maximum Drawdown (%)", "StdDev Sharpe (Rf=0%, p=95%)", "VaR Sharpe (Rf=0%, p=95%)",
                         "ES Sharpe (Rf=0%, p=95%)", "Calmar Ratio","Sortino Ratio (MAR = 0%)",
                         "Skewness", "Kurtosis")
  return(sumTable)
}


multiPerformance <- function(funds) {
  sumTable = as.data.frame(matrix(0,11,1))
  rownames(sumTable) = c("Annualized Return (%)", "Annualized Volatility (%)", "VaR (p=.95) (%)",
                         "Maximum Drawdown (%)", "StdDev Sharpe (Rf=0%, p=95%)", "VaR Sharpe (Rf=0%, p=95%)",
                         "ES Sharpe (Rf=0%, p=95%)", "Calmar Ratio","Sortino Ratio (MAR = 0%)",
                         "Skewness", "Kurtosis")
  for (i in 1:dim(funds)[2]) {
    tmp = performanceSummary(funds[,i])
    sumTable = cbind(sumTable, tmp)
  }
  sumTable = sumTable[,-1]
  return(sumTable)
}

finalPerformance <- function(rts, section) {
  Returns = rts
  colnames(Returns) = section
  return(performanceSummary(Returns))
  
}

######################
#####  Section 1
######################

cta_one_shot <- function(symbolsCTA, fastSMA, slowSMA,
                         stopLimLong, stopLimShort,
                         mktdata,
                         init_date = "2014-01-01",
                         start_date = "2014-01-01",
                         end_date = "2019-07-12",
                         init_equity = 10000000,
                         portfolio.st = "Port.Luxor",
                         account.st = "Acct.Luxor",
                         strategy.st = "Strat.Luxor") {
  
  tradeSize = init_equity/length(symbolsCTA)
  
  osFixedDollar <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
  {
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- round(tradeSize/ClosePrice,-2)
    return(orderqty)
  }
  
  osFixedDollarShort <- function(...){-osFixedDollar(...)}
  
  
  osInvestAll <- function (data, timestamp, orderqty, ordertype, 
                           orderside, equity, portfolio, symbol, ruletype, ..., initEq) {
    datePos <- format(timestamp,"%Y-%m-%d %H:%M:%OS")
    
    datePos <- strptime(c(datePos), format = "%Y-%m-%d %H:%M:%OS", tz = 
                          "UTC") + 86400 #for daily data
    
    updatePortf(Portfolio=portfolio,Symbol=symbol,Dates=paste0(start(data), 
                                                               "/", datePos))
    # After updating portfolio profit, we can extract the Net.Trading.PL 
    
    trading_pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
    
    equity <- initEq + trading_pl
    # ClosePrice <- getPrice(data, prefer = "Close")[datePos]
    ClosePrice <- (mktdata[,4])[datePos]
    # test = getPrice(x[[1]])
    # SPY = x[[1]]
    # test1 = SPY$SPY.Close
    # test2 = SPY[,4]
    
    UnitSize <- as.numeric((equity / ClosePrice))
    UnitSize1 <- round(UnitSize, digits = 8)
    ifelse(is.na(UnitSize1),0,UnitSize1)
  }
  
  
  osInvestAllShort <- function(...) { -osInvestAll(...)}
  
  .orderqty = tradeSize
  .txnfees=.threshold=0
  
  rm.strat(portfolio.st)
  rm.strat(account.st)
  rm.strat(strategy.st)
  
  currency("USD")
  stock(symbolsCTA, 
        currency = "USD", 
        multiplier = 1)
  
  initPortf(name = portfolio.st,
            symbols = symbolsCTA,
            initDate = init_date)

  initAcct(name = account.st,
           portfolios = portfolio.st,
           initDate = init_date,
           initEq = init_equity)
  
  initOrders(portfolio = portfolio.st,
             symbols = symbolsCTA,
             initDate = init_date)
  
  strategy(strategy.st, store = TRUE)
  
  ### Set up Inidcator
  # We use a fast and a slow Simple Moving Average **(SMA)** in the underlying asset we are looking at as two indicators to compare against each other to decide entry for long and short positions. The speed of the fast/slow averages are kept the same across assets  assets in the portfolio, but there is no reason this needs to be the case. The speed could be asset specific with a bit of programming ((ie ***SMA*** windows are the same for every asset ) )It may be something you might look to optimize).
  
  ################################################
  # SMA speeds can be modified here
  # fastSMA=15
  # slowSMA=45
  ################################################
  
  # indicators are set here -> do not touch! ######
  add.indicator(strategy = strategy.st,
                name = "SMA",
                arguments = list(x = Cl(mktdata), 
                                 n = fastSMA),
                label = "nFast")
  
  add.indicator(strategy = strategy.st, 
                name = "SMA", 
                arguments = list(x = Cl(mktdata), 
                                 n = slowSMA), 
                label = "nSlow")
  ######################################################
  
  
  ### Set up signal -Do not touch! ##############################################################
  add.signal(strategy = strategy.st,
             name="sigCrossover",
             arguments = list(columns = c("nFast", "nSlow"),
                              relationship = "gte"),
             label = "long")
  
  add.signal(strategy = strategy.st,
             name="sigCrossover",
             arguments = list(columns = c("nFast", "nSlow"),
                              relationship = "lt"),
             label = "short")
  ### Set up signal -Do not touch! ^^^^^^^^^^######################################
  
  
  
  
  ### Set up Rules
  # Once we have indicators we can add rules we can set up a trading strategy. We need to size the trade so we use the amount of equity we have divided by the number of assets we are working with from our data pull. In this exercise we use the same trade size whether going long or short. We do not update the trade size for changes in our trading account equity.
  
  
  #### Change trade rules here for all assets ###########
  
  # Trade sizes for long/short
  
  # tradeSize=init_equity/length(symbolsCTA)  # allocate capital equally to trade strategies
  # .orderqty=tradeSize
  # .txnfees=.threshold=0
  # stopLimLong=.04
  # stopLimShort=.015
  #######################################################

  ######  Add rules here - Do not touch! ###########################################

  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          orderside='short',
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE
           ),
           type='exit',
           label='Exit2LONG'
  )
  
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='short', sigval=TRUE,
                          orderside='long' ,
                          ordertype='market',
                          orderqty='all',
                          TxnFees=.txnfees,
                          replace=TRUE
           ),
           type='exit',
           label='Exit2SHORT'
  )
  
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='long' , sigval=TRUE,
                          orderside='long' ,
                          ordertype='stoplimit', prefer='High', threshold=.threshold,
                          osFUN = osFixedDollar,
                          tradeSize=tradeSize,
                          replace=FALSE
           ),
           type='enter',
           label='EnterLONG'
  )
  
  add.rule(strategy.st, name='ruleSignal',
           arguments=list(sigcol='short', sigval=TRUE,
                          orderside='short',
                          ordertype='stoplimit', prefer='Low', threshold=-.threshold,
                          osFUN = osFixedDollarShort,
                          replace=FALSE
           ),
           type='enter',
           label='EnterSHORT'
  )
  
  add.rule(strategy.st,name='ruleSignal',
           arguments = list(sigcol="long", sigval=TRUE,
                            replace=FALSE,
                            orderside='long',
                            ordertype='stoplimit',
                            tmult=TRUE,
                            threshold=stopLimLong,
                            orderqty='all',
                            orderset='ocolong'
           ),
           type='chain', parent="EnterLONG",
           label='StopLossLong',
           enabled=TRUE
  )
  
  add.rule(strategy.st,name='ruleSignal',
           arguments = list(sigcol="short", sigval=TRUE,
                            replace=FALSE,
                            orderside='short',
                            ordertype='stoplimit',
                            tmult=TRUE,
                            threshold=stopLimShort,
                            orderqty='all',
                            orderset='ocolong'
           ),
           type='chain', parent="EnterSHORT",
           label='StopLossShort',
           enabled=TRUE
  )
  
  ######  Add rules here - Do not touch! ##########################################
  ######^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^##########################################
  
  ### Get the Results - run to end w/o changing unless you want different output
  # Now run the rules/signal/strategies against the data and get the results 
  results_file <- paste("results", strategy.st, "RData", sep = ".")
  if( file.exists(results_file) ) {
    file.remove(results_file)}
  
  
  
  results <- applyStrategy(strategy.st, portfolios = portfolio.st, 
                           mktdata = mktdata,
                           verbose=FALSE)
  updatePortf(portfolio.st, Prices = mktdata[,4], verbose=F)
  updateAcct(account.st)
  updateEndEq(account.st)
  final_acct <- getAccount(account.st)
  end_eq <- final_acct$summary$End.Eq
  returns <- Return.calculate(end_eq, method="discrete")
  
  ### Save returns as CTAreturns to remember below
  CTAreturns<- Return.calculate(end_eq, method="log")
  return(CTAreturns)
}
# charts.PerformanceSummary(CTAreturns, colorset = rich6equal,
#                           main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
# charts.PerformanceSummary(CTAreturns, colorset = rich6equal,
#                           main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
# charts.PerformanceSummary(test, colorset = rich6equal,
#                           main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
# charts.PerformanceSummary(test2, colorset = rich6equal,
#                           main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)




######################
#####  Section 4
######################
factorMomentum <- function(data, back, hold, topNum) {
  
  topNum = min(topNum, dim(data)[2])

  finalFund = data.frame()
  stepMoment = seq(from = (back+1), to = (dim(data)[1]-hold+1), by = hold)
  for (i in stepMoment) {
    moment = as.numeric(data[(i-1),]) - as.numeric(data[(i-back),])
    toSelect = order(moment, decreasing = TRUE)[1:topNum]
    tmp = data[(i:(i+hold-1)),toSelect]
    colnames(tmp) = rep("", topNum)
    finalFund = rbind(finalFund, as.data.frame(tmp))
  }
  
  if (stepMoment[length(stepMoment)] != dim(data)[1]-hold+1) {
    i = stepMoment[length(stepMoment)] + hold
    moment = as.numeric(data[(i-1),]) - as.numeric(data[(i-back),])
    toSelect = order(moment, decreasing = TRUE)[1:topNum]
    tmp = data[(i:(dim(data)[1])),toSelect]
    colnames(tmp) = rep("", topNum)
    finalFund = rbind(finalFund, as.data.frame(tmp))
  }
  meanFund = apply(finalFund, 1, mean)
  xtsFund = xts(meanFund, order.by = index(data)[(back+1):(dim(data)[1])])
  return(xtsFund)
}



