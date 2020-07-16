source('./func/functions.R')
library(quantmod)
library(tibble)


shinyServer(function(input, output, session){
  
  stockData <- new.env()
  
  ######################
  #####  Section 1
  ######################
  
  ### Reset the time input if start == end
  
  observeEvent(input$date_range,{
    if(input$date_range[1] == input$date_range[2]){
      updateSliderTextInput(session,"date_range",selected = c(date_choices[1],date_choices[length(date_choices)]))
    }
  })
  
  ### Input for assets' tickers and parameters
  
  output$asset_sec1 <- renderRHandsontable({
    DF = data.frame(
      Asset= c("SPY", "AGG", "SOYB", "LQD", rep("",26)),
      Allocation=c(20,30,15,35,rep("",26)),
      fastSMA = c(9,11,13,12,rep("",26)),
      slowSMA = c(30,25,27,33,rep("",26)),
      stopLimLong = c(2,2,3,3,rep("",26)),
      stopLimShort = c(2,3,2,4,rep("",26)),
      stringsAsFactors = FALSE)
    
    colnames(DF) = c("Asset", "Allocation","Fast_Moving_Average", "Slow_Moving_Average", "Stop_Loss_Long(%)", "Stop_Loss_Short(%)")
    
    rhandsontable(DF, width = 700, height = 300) %>%
      hot_col("Asset", allowInvalid = TRUE)
  })
  
  
  ### Fetch data for section 1
  
  fetchdata_sec1 <- reactive({
    if(input$fetch_sec1==0){return()} #confirming button click
    print("here 2")
    isolate({
      input$fetch_sec1
      
      data = hot_to_r(input$asset_sec1)
      tickers = paste(data$Asset, sep = ",")
      tickers = tickers[-which(tickers == "")]
      print("here")
      print(tickers)
      
      init_date = input$date_range_sec1[1]
      start_date = input$date_range_sec1[1]
      end_date = input$date_range_sec1[2]
      x = list()
      for (i in 1:length(tickers)) {
        x[[i]] = getSymbols(Symbols = tickers[i], 
                            src = "yahoo", 
                            index.class = "POSIXct",
                            from = start_date, 
                            to = end_date, 
                            auto.assign = FALSE,
                            env = NULL,
                            adjust = TRUE)
        tmp=OHLC(x[[i]])
        print(head(tmp))
      }
    })
    
    return(x)
    
  })
  
  ### get the log return for each asset
  output$tickers<-NULL
  
  output$symSelector<- renderUI({
    #selectInput("variable1", "Choose Option:", as.list(camps)) 
    data = hot_to_r(input$asset_sec1)
    tickers = paste(data$Asset, sep = ",")
    tmp2del = which(tickers == "")
    tickers = tickers[-tmp2del]
    selectInput("sym", "Choose a dataset:",choices = tickers,selected=tickers[2])
  })
  
  backtest_sec1 <- reactive({
    input$backtest_go
    isolate({
      data = hot_to_r(input$asset_sec1)
      tickers = paste(data$Asset, sep = ",")
      tmp2del = which(tickers == "")
      tickers = tickers[-tmp2del]
      print(tickers)
      data = data[-tmp2del,]
      print(data)
      stockPool = fetchdata_sec1()
      print(head(stockPool))
      
      CTAreturn = xts()
      for (i in 1:(dim(data)[1])) {
        tmpReturn = cta_one_shot(data[i,1],
                                 as.numeric(data[i,3]),
                                 as.numeric(data[i,4]),
                                 as.numeric(data[i,5])/100,
                                 as.numeric(data[i,6])/100,
                                 stockPool[[i]],
                                 init_equity = as.numeric(data[i,2])*1000000,
                                 init_date = input$date_range_sec1[1],
                                 start_date = input$date_range_sec1[1],
                                 end_date = input$date_range_sec1[2])
        colnames(tmpReturn) = data[i,1]
        CTAreturn = cbind(CTAreturn, tmpReturn*as.numeric(data[i,2])/sum(as.numeric(data[,2])))
      }
    })
    return(CTAreturn)
    
  })
  
  
  
  backtestAsset_sec1 <- reactive({
    input$backtest_go
    isolate({
      data = hot_to_r(input$asset_sec1)
      tickers = paste(data$Asset, sep = ",")
      tmp2del = which(tickers == "")
      tickers = tickers[-tmp2del]
      data = data[-tmp2del,]
      print(data)
      stockPool = fetchdata_sec1()
      
      SYMreturn = xts()
      i=which(input$sym==tickers)
      tmpReturn = cta_one_shot(data[i,1],
                               as.numeric(data[i,3]),
                               as.numeric(data[i,4]),
                               as.numeric(data[i,5])/100,
                               as.numeric(data[i,6])/100,
                               stockPool[[i]],
                               init_equity = as.numeric(data[i,2])*1000000,
                               init_date = input$date_range_sec1[1],
                               start_date = input$date_range_sec1[1],
                               end_date = input$date_range_sec1[2])
      colnames(tmpReturn) = data[i,1]
      # SYMreturn = cbind(SYMreturn, tmpReturn)
      
      
    })
    return(tmpReturn)
    
  })
  
  
  pureTest <- reactive({
    if(input$fetch_sec1==0){return()} #confirming button click
    print("here 1")
    isolate({
      input$fetch_sec1
      data = hot_to_r(input$asset_sec1)
      tickers = paste(data$Asset, sep = ",")
      Data = get(tickers[1], stockData)
      Data
    })
    
  })
  # fetchdata_sec1
  # output$tttest = renderPrint(fetchdata_sec1()[[2]])
  # output$tttest2 = renderPrint(backtest_sec1()[1:5,])
  
  # output$tttest2 = renderPrint(backtest_sec1()[(dim(backtest_sec1())[1]),])
  
  ### graph the backtest result
  
  output$bt_sec1 = renderPlot({
    validate(need(input$backtest_go, "")
             
    )
    input$backtest_go
    isolate({
      CTAreturns = backtest_sec1()
      portfolioReturns = xts(apply(CTAreturns, 1, mean), order.by = index(CTAreturns))
      charts.PerformanceSummary(portfolioReturns, colorset = rich6equal,
                                main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
      # charts.PerformanceSummary(CTAreturns, colorset = rich6equal,ylim = c(min(CTAreturns), max(CTAreturns)),
      #                           main = "Strategy Performance", legend.loc = "bottomleft", cex.legend=1.15)
      
    })
    # if(is.null(CTAreturns)) {
    #   return(NULL)
    # }
  })
  
  output$btAsset_sec1 = renderPlot({
    validate(need(input$backtest_go, "")
             
    )
    input$backtest_go
    isolate({
      CTAreturns = backtestAsset_sec1()
      portfolioReturns = xts(apply(CTAreturns, 1, mean), order.by = index(CTAreturns))
      charts.PerformanceSummary(portfolioReturns, colorset = rich6equal,
                                main = paste("Strategy Performance",colnames(CTAreturns[1])), legend.loc = "bottomleft", cex.legend=1.15)
    })
  })
  
  
  ### generate summary table to compare performance
  
  output$performance_sec1 <- renderRHandsontable({
    input$backtest_go
    isolate({
      CTAreturns = backtest_sec1()
      if (dim(CTAreturns)[2] == 1) {
        sumTable = performanceSummary(CTAreturns)
      } else {
        Portfolio = xts(apply(CTAreturns, 1, mean), order.by = index(CTAreturns))
        CTAreturns = cbind(Portfolio, CTAreturns)
        sumTable = multiPerformance(CTAreturns)
      }
      sumTable = sumTable %>% rownames_to_column("Performance Analysis")
      rhandsontable(sumTable,
                    # width = 800, height = 300,
                    readOnly = TRUE) %>%
        hot_table(highlightRow = TRUE)
      
    })
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
      
      tickers_global <<- tickers
      
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
                   env = stockData,
                   auto.assign = TRUE,
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
      
      i = 1
      clcl = clclData[[i]]
      idx=intersect(as.Date(index(clcl)),as.Date(index(factorData)))
      fit=lm(clcl[as.Date(idx)]~factorData[as.Date(idx)])
      tmp = as.matrix(fit$coefficients)
      tmp[1] = tickers[i]
      tmp = as.data.frame(t(tmp))
      colnames(tmp) = c("Asset", colnames(factorData))
      factorCoefs=rbind(factorCoefs, tmp)
      rets = clcl
      colnames(rets)=c(colnames(rets[,-ncol(rets)]),tickers[i])
      
      # rets = xts()
      for(i in (2:length(tickers))){
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
      opta <<- optimize.portfolio(R=rets[complete.cases(rets)], portfolio=pspec,
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
                                main = paste("Strategy Performance of", paste(factor3table$factors[input$factors_sec3_rows_selected,1], collapse = " / ")), legend.loc = "bottomleft", cex.legend=1.15)
    })
  })
  
  output$weight_table = renderTable({
    validate(need(input$backtest_go3, "")
             
    )
    input$backtest_go3
    isolate({
      t = cbind(Asset = tickers_global, Weight = opta$weights)
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