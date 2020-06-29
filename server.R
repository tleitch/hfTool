source('./func/functions.R')
library(quantmod)
library(ahf)
library(tibble)
library(quantstrat)
library(FinancialInstrument)


shinyServer(function(input, output, session){
  
  stockData <- new.env()
  
  ######################
  #####  Section 1
  ######################
  
  # choose columns to display
  output$mytable1 <- DT::renderDataTable(
    data.frame(edhec2[paste(input$dateRange,collapse = '/') , input$show_vars, drop = FALSE]),
    options = list(scrollX = TRUE)
  )
  
  output$cvalues <- renderPrint({
    dt = edhec2[paste(input$dateRange,collapse = '/') , input$show_vars, drop = FALSE]
    dataframe = data.frame(matrix(ncol = length(input$show_vars), nrow = 0))
    names(dataframe) = input$show_vars
    
    if ("Sharpe Ratio" %in% input$criteria){
      dataframe["Sharpe Ratio",] = SharpeRatio(dt)
    }
    
    if ("Sortino Ratio" %in% input$criteria){
      dataframe["Sortino Ratio",] = SortinoRatio(dt)
    }
    
    if ("CAPM beta" %in% input$criteria){
      dataframe["CAPM beta",] = CAPM.beta(dt,edhec2[,"SPY"])
    }
    
    if ("Kurtosis" %in% input$criteria){
      dataframe["Kurtosis",] = kurtosis(dt)
    }
    if ("Skewness" %in% input$criteria){
      dataframe["Skewness",] = skewness(dt)
    }
    if ("Calmar Ratio" %in% input$criteria){
      dataframe["Calmar Ratio",] = CalmarRatio(dt)
    }
    
    if ("Jensen’s alpha" %in% input$criteria){
      dataframe["Jensen’s alpha",] = CAPM.alpha(dt,edhec2[,"SPY"])
    }
    
    if ("Omega ratio" %in% input$criteria){
      dataframe["Omega ratio",] = Omega(dt,edhec2[,"SPY"])
    }
    
    if ("Bull beta" %in% input$criteria){
      dataframe["Bull beta",] = CAPM.beta.bull(dt,edhec2[,"SPY"])
    }
    
    if ("Bear beta" %in% input$criteria){
      dataframe["Bear beta",] = CAPM.beta.bear(dt,edhec2[,"SPY"])
    }
    
    knitr::kable(dataframe)
    
  })
  
  ######################
  #####  Section 2
  ######################
  
  ### Reset the time input if start == end
  observeEvent(input$date_range,{
    if(input$date_range[1] == input$date_range[2]){
      updateSliderTextInput(session,"date_range",selected = c(date_choices[1],date_choices[length(date_choices)]))
    }
  })
  
  observeEvent(input$asset_sec1,{
    data = hot_to_r(input$asset_sec1)
    tickers = paste(data$Asset, sep = ",")
    tickers = tickers[-which(tickers == "")]
    updateSelectInput(session, 'ind', choices = tickers)
    updateTextInput(session, 'downloadtxt', value = 'Click Download to download data')
  })
  
  observeEvent(input$download, {
    Settingup_port(input, output, session)
  })
  
  ### Input for assets' tickers and parameters
  
  output$asset_sec1 <- renderRHandsontable({
    DF = data.frame(
      Asset= c("SPY", "AGG", "SLV", "LQD", rep("",26)),
      stringsAsFactors = FALSE)
    
    colnames(DF) = c("Asset")
    
    rhandsontable(DF, width = 250, height = 300) %>%
      hot_col("Asset", allowInvalid = TRUE)
  })
  
  Settingup_port <- function(input, output, session){
    withProgress(message = 'Downloading Data', value = 10, {
      data = hot_to_r(input$asset_sec1)
      tickers <<- paste(data$Asset, sep = ",")
      tickers <<- tickers[-which(tickers == "")]
      
      init_date <<- input$date_range_sec1[1]
      start_date <<- input$date_range_sec1[1]
      end_date <<- input$date_range_sec1[2]
      
      for (i in 1:length(tickers)) {
        sym = getSymbols(Symbols = tickers[i],
                         from = start_date,
                         to = end_date,
                         auto.assign = FALSE)
        
        assign(tickers[i], sym, globalenv())
      }
    })
    
    withProgress(message = 'Constructing portfolio', value = 10, {
      stock(tickers,
            currency = "USD",
            multiplier = 1)
      
      rm.strat(portfolio.st)
      rm.strat(account.st)
      rm.strat(strategy.st)
      
      initPortf(name = portfolio.st,
                symbols = tickers,
                initDate = init_date)
      
      initAcct(name = account.st,
               portfolios = portfolio.st,
               initDate = init_date,
               initEq = init_equity)
      
      initOrders(portfolio = portfolio.st,
                 symbols = tickers,
                 initDate = init_date)
      
      strategy(strategy.st, store = TRUE)
      
      # SMA speed can be modified here
      fastSMA <<- input$fastSMA
      slowSMA <<- input$slowSMA
      
      add.indicator(strategy = strategy.st,
                    name = "SMA",
                    arguments = list(x = quote(Cl(mktdata)), 
                                     n = fastSMA),
                    label = "nFast")
      add.indicator(strategy = strategy.st, 
                    name = "SMA", 
                    arguments = list(x = quote(Cl(mktdata)), 
                                     n = slowSMA), 
                    label = "nSlow")
      
      #### Change signals here ##########
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
      
      #### Change trade rules here for all assets ###########
      # Trade sizes for long/short
      tradeSize=init_equity/length(tickers) # allocate capital equally to trade strategies
      longSize=shortSize=1
      
      add.rule(strategy = strategy.st,
               name = "ruleSignal",
               arguments = list(sigcol = "long",
                                sigval = TRUE,
                                orderqty = longSize,
                                ordertype = "market",
                                orderside = "long", 
                                osFUN= IKTrading::osMaxDollar,
                                tradeSize=tradeSize,
                                maxSize=tradeSize,
                                prefer = "High", 
                                TxnFees = 0, 
                                replace = FALSE),
               type = "enter",
               label = "EnterLONG",verbose=F)
      # Exit Long Rule
      add.rule(strategy.st, 
               name = "ruleSignal", 
               arguments = list(sigcol = "long", 
                                sigval = TRUE, 
                                orderside = "short", 
                                ordertype = "market", 
                                orderqty = "all", 
                                TxnFees = 0, 
                                replace = TRUE), 
               type = "exit", 
               label = "Exit2LONG")
      #### SHORT rules
      # Enter Short
      add.rule(strategy.st,
               name = "ruleSignal",
               arguments = list(sigcol = "short",
                                sigval = TRUE,
                                orderqty = -shortSize,
                                osFUN= IKTrading::osMaxDollar,
                                tradeSize= -tradeSize,
                                maxSize= -tradeSize,
                                ordertype = "market",
                                orderside = "short", 
                                replace = FALSE, 
                                TxnFees = 0, 
                                prefer = "Low"),
               type = "enter",
               label = "EnterSHORT")
      # Exit Short
      add.rule(strategy.st, 
               name = "ruleSignal", 
               arguments = list(sigcol = "short", 
                                sigval = TRUE, 
                                orderside = "long", 
                                ordertype = "market", 
                                orderqty = "all", 
                                TxnFees = 0, 
                                replace = TRUE), 
               type = "exit", 
               label = "Exit2SHORT")
      
      results_file <<- paste("results", strategy.st, "RData", sep = ".")
      if( file.exists(results_file) ) {
        file.remove(results_file)}
      results <<- applyStrategy(strategy.st, portfolios = portfolio.st,verbose=FALSE)
      updatePortf(portfolio.st,verbose=F)
      updateAcct(account.st)
      updateEndEq(account.st)
      
      ### Now get Total Portfolio Results ####
      final_acct <<- getAccount(account.st)
      end_eq <<- final_acct$summary$End.Eq
      returns <<- Return.calculate(end_eq, method="log")
    })
  }
  
  output$ind_summary <- renderPrint({
    validate(need(input$update, "Click Analyze to get the result"))
    input$update
    isolate({
      tstats <- tradeStats(portfolio.st)
      knitr::kable(t(tstats))
    })
  })
  
  ### Plot data for section 2
  output$bt_sec1 = renderPlot({
    validate(need(input$update, "Click Analyze to get the result"))
    input$update
    isolate({
      chart.Posn(portfolio.st, Symbol = input$ind, 
                 TA = "add_SMA(n = fastSMA, col = 4); add_SMA(n = slowSMA, col = 2)")
    })## isolate
  })
  
  output$port_summary <- renderPrint({
    validate(need(input$update, "Click Analyze to get the result"))
    input$update
    isolate({
      tmp = rbind(SharpeRatio(returns,annualize=T),SortinoRatio(returns))
      rbind(tmp, CalmarRatio(returns))
    })
  })
  
  ### Plot data for section 2
  output$portf1 = renderPlot({
    validate(need(input$update, "Click Analyze to get the result"))
    input$update
    isolate({
      charts.PerformanceSummary(returns, colorset = bluefocus, main = "Strategy Performance")
    })## isolate
  })
  
  
  ######################
  #####  Section 3
  ######################
  
  ### input assets for section 3
  output$asset_sec3 <- renderRHandsontable({
    DF = data.frame(
      Asset= c("SPY", "AGG", "SHV", "LQD", rep("",26)),
      LowerBound = c(-100,-100,-100,-100,rep("",26)),
      UpperBound = c(100,100,100,100,rep("",26)),
      # stopLimLong = c(4,5,6,7,rep("",26)),
      # stopLimShort = c(1.5,2,3,4,rep("",26)),
      stringsAsFactors = FALSE)
    
    colnames(DF) = c("Asset", "Lower Bound (%)", "Upper Bound (%)")
    
    rhandsontable(DF, width = 400, height = 347) %>%
      hot_col("Asset", allowInvalid = TRUE)
  })
  
  ### input factors for section 3
  factor3table <- reactiveValues(
    factors = df_sec3
  )
  
  ### get pre-select rows
  preSelect <- reactiveValues(sel = c(1,3))
  
  ### show factors as DT
  dtWrapper <- reactive({
    presel = preSelect$sel
    datatable(factor3table$factors,
              selection = list(mode = "multiple", selected = presel, target = "row"),
              editable = TRUE)
  })
  
  output$factors_sec3 <- DT::renderDataTable({
    dtWrapper()
  })
  
  
  ### Select pre-prepared factors
  
  observeEvent(input$model_sec3, {
    if (input$model_sec3 != "User Define") {
      preSelect$sel = switch(input$model_sec3,
                             "CAPM" = c(1),
                             "FF3" = c(1,2,3),
                             c())
    }
    # factor3table$factors[2,2] = 100
  }, ignoreInit = TRUE)
  
  ### Make the factors loading limit editable
  observeEvent(input$factors_sec3_cell_edit, {
    factor3table$factors[input$factors_sec3_cell_edit$row, input$factors_sec3_cell_edit$col] <<- input$factors_sec3_cell_edit$value
  })
  
  ### get clcl data for optimization
  fetchdata_sec3 <- reactive({
    if(input$fetch_sec3==0){return()} #confirming button click
    
    isolate({
      input$fetch_sec3
      
      data = hot_to_r(input$asset_sec3)
      tickers = paste(data$Asset, sep = ",")
      tickers = tickers[-which(tickers == "")]
      
      init_date = input$date_range_sec3[1]
      start_date = input$date_range_sec3[1]
      end_date = input$date_range_sec3[2]
      clRange=paste0(init_date,"/",end_date)
      x = list()
      # tickers = c("SPY", "AGG", "SHV", "LQD")
      
      for (i in 1:length(tickers)) {
        getSymbols(Symbols = tickers[i], 
                   src = "yahoo", 
                   index.class = "POSIXct",
                   from = start_date, 
                   to = end_date, 
                   auto.assign = TRUE,
                   env = stockData,
                   adjust = TRUE)
        # getSymbols(i, env = .GlobalEnv, from = "1999-12-31")
        # x[[i]] = ClCl(get(tickers[i]))[clRange]
        x[[i]] = ClCl(get(tickers[i], stockData))[clRange]
      }
      
      # factorCoefs=data.frame()
      # rets=data.frame()
      
    })
    # Data = get(tickers[1], stockData)
    # test = cta_one_shot(tickers[1], 15, 45, .04, .015)
    return(x)
  })
  
  ### prepare for factor optimization
  factorOptPrep <- reactive({
    if(input$backtest_go3==0){return()} #confirming button click
    
    isolate({
      
      input$backtest_go3
      
      data = hot_to_r(input$asset_sec3)
      tickers = paste(data$Asset, sep = ",")
      tickers = tickers[-which(tickers == "")] # get tickers
      
      init_date = input$date_range_sec3[1]
      start_date = input$date_range_sec3[1]
      end_date = input$date_range_sec3[2]
      clRange=paste0(init_date,"/",end_date) # get time range
      
      clclData = fetchdata_sec3() # clcl data
      sel = input$factors_sec3_rows_selected # get selected rows
      factorData = factorDataGlobal[,sel] # only get needed factors
      
      factorCoefs=data.frame()
      rets=data.frame()
      # rets = xts()
      for(i in (1:length(tickers))){
        clcl = clclData[[i]]
        idx=intersect(as.Date(index(clcl)),as.Date(index(factorData)))
        fit=lm(clcl[as.Date(idx)]~factorData[as.Date(idx)])
        tmp = as.matrix(fit$coefficients)
        tmp[1] = tickers[i]
        tmp = as.data.frame(t(tmp))
        colnames(tmp) = c("Asset", colnames(factorData))
        factorCoefs=rbind(factorCoefs, tmp)
        rets=cbind(rets,clcl)
        colnames(rets)=c(colnames(rets[,-ncol(rets)]),tickers[i])
      }
      rownames(factorCoefs)=c()
      rets = rets[clRange]
      # rets = xts(rets, order.by = as.Date(index(rets)))
    })
    # return(factorData)
    return(list(factorCoefs = factorCoefs,
                rets = rets))
  })
  
  ### factor optimization
  factorOpt <- reactive({
    if(input$backtest_go3==0){return()} #confirming button click
    
    isolate({
      input$backtest_go3
      
      data = hot_to_r(input$asset_sec3)
      tickers = paste(data$Asset, sep = ",")
      tmp2del = which(tickers == "")
      tickers = tickers[-tmp2del]
      data = data[-tmp2del,]
      
      box1 = as.numeric(data[,2])/100
      box2 = as.numeric(data[,3])/100
      
      factorDf = factor3table$factors[input$factors_sec3_rows_selected,]
      lower = as.numeric(factorDf[,2])
      upper = as.numeric(factorDf[,3])
      
      lev = strsplit(input$leverage, ",")
      lev1 = as.numeric(lev[[1]][1])
      lev2 = as.numeric(lev[[1]][2])
      
      factorCoefs = factorOptPrep()$factorCoefs
      rets = factorOptPrep()$rets
      
      pspec <- portfolio.spec(assets = colnames(rets))
      
      #' Box constraint for position size where we can either be fully short or long an asset - do not edit first 3 in min, edit 4th if you only have 2 alternative asset specialties
      lo_constr <- box_constraint(assets = pspec$assets, min = box1, max = box2)
      ##############
      
      #' Position limit constraints, number of long and short positions allowed, which we set to any
      pl_constr <- position_limit_constraint(assets = pspec$assets, max_pos = ncol(rets), max_pos_short = ncol(rets))
      lev_constr <- weight_sum_constraint(min_sum = lev1, max_sum = lev2)
      
      mf=as.matrix(factorCoefs[,-1])
      mf <- apply(mf, 2, function(x) as.numeric(as.character(x)))
      rownames(mf)=factorCoefs$Asset
      
      exp_constr <- factor_exposure_constraint(assets=pspec$assets, B=mf, lower=lower, upper=upper)
      
      #' Set objective to maximize return.
      ret_obj <- return_objective(name="mean")
      var_obj <- portfolio_risk_objective(name="var")
      
      # Use ROI instead of pso optimizer because it is faster and more stable.
      # Optimization here will only consider period when there is no missing data.
      opta <- optimize.portfolio(R=rets[complete.cases(rets)], portfolio=pspec,
                                 constraints=list(lev_constr, lo_constr, pl_constr,exp_constr),
                                 objectives=list(ret_obj,var_obj), trace=FALSE,maxSR=TRUE,
                                 optimize_method="ROI",verbose = FALSE)
      
      wgts = opta$weights
      
      totalFund = apply(rets, 1, function(x) sum(x*wgts))
      totalFund = xts(totalFund, order.by = index(rets))
    })
    # return(opta)
    return(totalFund)
  })
  
  output$bt_sec3 = renderPlot({
    validate(need(input$backtest_go3, "")
             
    )
    input$backtest_go3
    isolate({
      portfolioReturns = factorOpt()
      charts.PerformanceSummary(portfolioReturns, colorset = rich6equal,
                                main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
    })
  })
  
  
  
  
  # output$tttest = renderPrint(input$factors_sec3_rows_selected)
  # output$tttest = renderPrint(backtest_sec4()[1:10])
  # output$tttest2 = renderPrint(factorOpt()[1:10])
  # as.numeric(input$leverage)
  # output$tttest2 = renderPrint(input$factors_sec3_cell_edit)
  # output$tttest2 = renderPrint(factor3table$factors[input$factors_sec3_rows_selected,])
  
  
  ############## section 4
  backtest_sec4 <- reactive({
    
    if(input$backtest_go4==0){return()}
    isolate({
      input$backtest_go4
      
      start_date = input$date_range_sec4[1]
      end_date = input$date_range_sec4[2]
      clRange=paste0(start_date,"/",end_date)
      
      data = factorDataGlobal[clRange]
      back = as.numeric(input$back_4)
      hold = as.numeric(input$hold_4)
      topNum = as.numeric(input$num_4)
      finalFund = factorMomentum(data, back, hold, topNum)
    })
    return(finalFund)
  })
  
  output$bt_sec4 = renderPlot({
    validate(need(input$backtest_go4, "")
             
    )
    input$backtest_go4
    isolate({
      portfolioReturns = backtest_sec4()
      charts.PerformanceSummary(portfolioReturns, colorset = rich6equal,
                                main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
    })
  })
  
  output$tttest = renderPrint(factorOpt())
  # output$tttest2 = renderPrint(factorOpt()[1:10])
  
  output$performance_sec5 <- renderRHandsontable({
    
    # input$backtest_go
    # input$backtest_go
    input$backtest_go3
    input$backtest_go4
    
    isolate({

      # 
      # CTAreturns = backtest_sec1()
      # Portfolio = xts(apply(CTAreturns, 1, mean), order.by = index(CTAreturns))
      # colnames(Portfolio) = "Section 1"
      # sec1Ret = performanceSummary(Portfolio)
      sumTable = as.data.frame(matrix(0,11,1))
      rownames(sumTable) = c("Annualized Return (%)", "Annualized Volatility (%)", "VaR (p=.95) (%)",
                             "Maximum Drawdown (%)", "StdDev Sharpe (Rf=0%, p=95%)", "VaR Sharpe (Rf=0%, p=95%)",
                             "ES Sharpe (Rf=0%, p=95%)", "Calmar Ratio","Sortino Ratio (MAR = 0%)",
                             "Skewness", "Kurtosis")
      tmp = tryCatch({
        CTAreturns = backtest_sec1()
        if (dim(CTAreturns)[2] == 1) {
          tmp = performanceSummary(CTAreturns)
        } else {
          Portfolio = xts(apply(CTAreturns, 1, mean), order.by = index(CTAreturns))
          CTAreturns = cbind(Portfolio, CTAreturns)
          tmp = multiPerformance(CTAreturns)
        }
        tmp
      }, error = function(e)
        data.frame(Section_1 = matrix(0,11,1)))

      sumTable = cbind(sumTable, tmp)[,1:2]
      colnames(sumTable) = c("x", "Section_1")

      # sumTable = sumTable[,-1]
      # sumTable = sumTable[,-1]
      # sec1Ret = sumTable$Portfolio
      
      # 
      # sec3Returns = factorOpt()
      # colnames(sec3Returns) = "Section 3"
      sec3Ret = tryCatch(finalPerformance(factorOpt(), "Section_3"), error = function(e)
        data.frame(Section_3 = matrix(0,11,1))
        )
      sec4Ret = tryCatch(finalPerformance(backtest_sec4(), "Section_4"), error = function(e) 
        data.frame(Section_4 = matrix(0,11,1))
      )

      # sec4Returns = backtest_sec4()
      # colnames(sec4Returns) = "Section 4"
      # sec4Ret = performanceSummary(sec4Returns)
      # sec4Ret = finalPerformance(backtest_sec4(), "Section_4")
      # 
      # 
      
      # sumTable = cbind(sumTable, sec3Ret)[,1:2]
      # colnames(sumTable) = c("x", "Section_3")
      # sumTable = cbind(sumTable, sec4Ret)
      
      sumTable = cbind(sumTable, sec3Ret, sec4Ret)
      # sumTable = sumTable[,2:3]
      sumTable = sumTable[,2:4]
      # sumTable = sumTable[,-1]
      # sumTable = cbind(sec3Ret, sec4Ret)
      # sumTable = sec1Ret

      
      sumTable = sumTable %>% rownames_to_column("Performance Analysis")
      rhandsontable(sumTable, 
                    # width = "150%",
                    height = 1100,
                    readOnly = TRUE) %>% 
        hot_rows(rowHeights = 30) %>%
        hot_cols(colWidths = c(400,150,150,150)) %>%
        hot_table(highlightRow = TRUE)
      
      
      
      # 
      # CTAreturns = backtest_sec1()
      # if (dim(CTAreturns)[2] == 1) {
      #   sumTable = performanceSummary(CTAreturns)
      # } else {
      #   Portfolio = xts(apply(CTAreturns, 1, mean), order.by = index(CTAreturns))
      #   CTAreturns = cbind(Portfolio, CTAreturns)
      #   sumTable = multiPerformance(CTAreturns)
      # }
      # sumTable = sumTable %>% rownames_to_column("Performance Analysis")
      # rhandsontable(sumTable, 
      #               # width = 800, height = 300, 
      #               readOnly = TRUE) %>% 
      #   hot_table(highlightRow = TRUE)
      
    })
    


      
  })
  
  
  
})