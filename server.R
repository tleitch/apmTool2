
source('./func/am_helper.R')
source('./func/shiny_helper.R')

my_colors = brewer.pal(6, "Blues")

shinyServer(function(input, output, session){

  ###############
  ##  Static html pages
  ###############

  output$disclaimer = renderUI(includeHTML("./html/disclaimer.html"))
  output$abt = renderUI(includeHTML("./html/about.html"))
  output$measures= renderUI(withMathJax(includeHTML("./html/measures.html")))


  # output$simutest <- renderPrint(input$simuWay)



  ###############
  ##  Theory Page
  ###############

  # Render graphs for Theory part (ggplot comes from global.R)
  output$graph1 =renderPlotly(g1)
  output$graph2 =renderPlotly(g2)
  output$graph3 =renderPlotly(g3)
  output$graph4 =renderPlotly(g4)



  ###############
  ##  Allocation Page
  ###############
  stockData <- new.env()

  getTicker <- reactive({
    fulltext = paste(input$pp1, input$pp2, input$pp3, input$pp4, input$pp5, input$pp6, input$pp7, input$pp8, input$pp9, input$pp10, input$pp11, input$pp12)
    fulltext = str_trim(fulltext)
    tickers = strsplit(fulltext, " ")[[1]]
    if (length(tickers) < 12) {tickers[(length(tickers)+1):12] = "NA"}
    tickers[1:12]
  })


  dataInput <- reactive({
    if(input$update==0){return()} #confirming button click
    isolate({
      input$update
      # tickers = getTicker()
      text = paste(input$pp1, input$pp2, input$pp3, input$pp4, input$pp5, input$pp6, input$pp7, input$pp8, input$pp9, input$pp10, input$pp11, input$pp12)
      text = str_trim(text)
      tickers = strsplit(text, " ")[[1]]
      # tickers = strsplit(input$data, ",")[[1]]
      # getSymbols(tickers, src = "yahoo", env=stockData, from = "1999-12-31")
      for (i in tickers) {
        if (i != "NA") {
          tryCatch(getSymbols(i, env = stockData, from = "1999-12-31"),
                   error = function(e) {getSymbols(i, src = "FRED",
                                                   from = "1999-12-31", env = stockData)})
        }
      }

      Data <- data.frame()

      # validate(need(input$data != "", label = "stock"))
      validate(need(input$pp1 != "", label = "stock"))
      for (i in 1:length(tickers)) {

        tmp = getCol(get(tickers[i], pos=stockData), tickers[i])
        Data <- cbind(Data, tmp)
      }
      Data = na.fill(Data, 0)
      Data
    })
  })


  #Weights (make sure that sliders are mutually dependent and weights add up to 1)
  # Initialize portfolio weights
  # port_weight = reactiveValues(weight=append(rep(1/6,6), rep(0,4))) # naive diversification
  port_weight = reactiveValues(weight=append(rep(1/8,8), rep(0,2))) # naive diversification
  # simu = reactiveValues(mu = 0)

  currSum = reactive({
    if (input$auto == "TRUE") {
      sum = "100%"
    } else {
      sum = input$p1+input$p2+input$p3+input$p4+input$p5+input$p6+input$p7+input$p8+input$p9+input$p10+input$p11+input$p12
      sum = round(sum*100, digits = 4)
      sum = paste(sum, "%", sep = "")
    }
    # sum = paste("The total current weight is: ", sum, sep = "")
    sum = paste("Total weight: ", sum, sep = "")
  })

  output$currentsum <- renderText(currSum())
  # If any of the sliders change, then recalculate other weight weights to satisfy sum to 1 constraint
  observers = list(
    observeEvent(input$p1,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers) #This function comes from shinyhelper.R
                     port_weight$weight = updateweight(port_weight$weight, input$p1, 1)
                     resumeMany(observers) #This function comes from shinyhelper.R
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p1, 1)
                   }
                 }
    ),
    observeEvent(input$p2,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p2, 2)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p2, 2)
                   }
                 }
    ),
    observeEvent(input$p3,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p3, 3)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p3, 3)
                   }
                 }
    ),
    observeEvent(input$p4,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p4, 4)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p4, 4)
                   }
                 }
    ),
    observeEvent(input$p5,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p5, 5)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p5, 5)
                   }
                 }
    ),
    observeEvent(input$p6,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p6, 6)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p6, 6)
                   }
                 }
    ),
    observeEvent(input$p7,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p7, 7)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p7, 7)
                   }
                 }
    ),
    observeEvent(input$p8,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p8, 8)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p8, 8)
                   }
                 }
    ),
    observeEvent(input$p9,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p9, 9)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p9, 9)
                   }
                 }
    ),
    observeEvent(input$p10,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p10, 10)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p10, 10)
                   }
                 }
    ),
    observeEvent(input$p11,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p11, 11)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p11, 11)
                   }
                 }
    ),
    observeEvent(input$p12,
                 {
                   if (input$auto == "TRUE") {
                     suspendMany(observers)
                     port_weight$weight = updateweight(port_weight$weight, input$p12, 12)
                     resumeMany(observers)
                   } else {
                     port_weight$weight = outflowControl(port_weight$weight, input$p12, 12)
                   }
                 }
    )
  )



  output$p1ui = renderUI({
    wghtsliderInput("p1", port_weight$weight[1], label = NULL) #This function comes from shinyhelper.R
  })
  output$p2ui = renderUI({
    wghtsliderInput("p2", port_weight$weight[2], label = NULL)
  })
  output$p3ui = renderUI({
    wghtsliderInput("p3", port_weight$weight[3], label = NULL)
  })
  output$p4ui = renderUI({
    wghtsliderInput("p4", port_weight$weight[4], label = NULL)
  })
  output$p5ui = renderUI({
    wghtsliderInput("p5", port_weight$weight[5], label = NULL)
  })
  output$p6ui = renderUI({
    wghtsliderInput("p6", port_weight$weight[6], label = NULL)
  })
  output$p7ui = renderUI({
    wghtsliderInput("p7", port_weight$weight[7], label = NULL)
  })
  output$p8ui = renderUI({
    wghtsliderInput("p8", port_weight$weight[8], label = NULL)
  })
  output$p9ui = renderUI({
    wghtsliderInput("p9", port_weight$weight[9], label = NULL)
  })
  output$p10ui = renderUI({
    wghtsliderInput("p10", port_weight$weight[10], label = NULL)
  })
  output$p11ui = renderUI({
    wghtsliderInput("p11", port_weight$weight[11], label = NULL)
  })
  output$p12ui = renderUI({
    wghtsliderInput("p12", port_weight$weight[12], label = NULL)
  })


  #Date slider
  #If min date and max date are the same - reset the slider
  observeEvent(input$date_range,{
    if(input$date_range[1] == input$date_range[2]){
      updateSliderTextInput(session,"date_range",selected = c(date_choices[1],date_choices[length(date_choices)]))
    }
  })


  #Allocation pie chart
  output$graph5 = renderPlotly({


    alloc = data.frame(wght = port_weight$weight, asset = showManyTickers(getTicker()[1:12]))

    g5 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=280, height=280) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))

    g5

  })

  #############################################
  # Perform backtesting
  # Functions are in shiny_helper.R
  #############################################

  # Backtest data
  # bt_data = reactive({bt_port(df, as.Date(input$date_range[1]), as.Date(input$date_range[2]), port_weight$weight, input$rebalance)})

  bt_data = reactive({
    df2 = dataInput()
    bt_port(df2, as.Date(input$date_range[1]), as.Date(input$date_range[2]), port_weight$weight, input$rebalance, df_full)
  })

  opt_weights = reactive({
    #Calculate target risk and return
    bt_df = bt_data()
    target_ret = mean(bt_df$Portfolio) * 250
    target_risk = sd(bt_df$Portfolio) * sqrt(250)

    #Extract dataframe for dates
    from = as.Date(input$date_range[1])
    to = as.Date(input$date_range[2])

    df = as.data.frame(dataInput())
    df_tmp = df %>% rownames_to_column("date") %>%
      filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")

    # Calculate inputs for optimization
    returns = xts(df_tmp, order.by = as.Date(row.names(df_tmp)))
    mean_ret = apply(df_tmp, 2, mean) * 250
    cov_matrix = cov(df_tmp) * 250

    #Find optimal weights
    #opt_w_ret = findEfficientFrontier.Return(returns, target_ret)

    tickers = getTicker()
    numTicker = 12
    while (tickers[numTicker] == "NA") {
      numTicker = numTicker-1
    }

    opt_w_ret = findEfficientFrontier.ReturnALT(mean_ret, cov_matrix, target_ret, numTicker)

    opt_w_risk = findEfficientFrontier.Risk(mean_ret, cov_matrix, target_risk, numTicker)

    #Return a dataframe
    opt_df = data.frame(OptRet = opt_w_ret, OptRisk = opt_w_risk)
    return (opt_df)


  })


  #Plot backtest compound return
  output$graph6 = renderPlotly({
    validate(need(input$go, "")
    )
    input$go

    isolate({  ### To let weights settle

      bt_df = bt_data()
      print(head(bt_df))
      #Calculate compound return
      bt_df = bt_df %>%
        gather(key="Asset", value="Return", -date) %>%
        group_by(Asset) %>%
        arrange(date) %>%
        # mutate(cumRet = cumprod(1+Return) - 1) %>%
        mutate(cumRet = cumsum(Return)) %>%
        select(date, Asset, cumRet) %>%
        spread(key=Asset, value=cumRet)


      plot_ly(bt_df, x = ~date, y = ~Portfolio, type = "scatter", mode = "line", name = "Portfolio",
              line = list(color = "Steelblue3", width = 2), width = 700, height = 400) %>%
        add_trace(y= ~SP500, name = "SP500",
                  line = list(color = "black", width = 2)) %>%
        add_trace(y= ~R60T10C30, name = "S&P500:60%, CorpBonds:30%, Treasury:10%",
                  line = list(color = "gray", width = 2)) %>%
        layout(xaxis = list(title = "", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
               yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE, tickformat = "%"),
               legend = list(orientation = "h", x = 0.1, y=1.2),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)')
    })
  })

  #Create backtest preformance stats

  output$bt_table1 = renderTable(digits =2, {
    validate(need(input$go, "")
    )
    input$go

    isolate({
      #Select data
      ret_df = bt_data()

      ret_df = ret_df %>% rename(Mixed = R60T10C30) %>%
        select(date, Portfolio, SP500, Mixed)

      rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))


      #Calculate performance measures
      perf_df = data.frame(Measure = c("Return (annualized), %","Risk (annualized), %","Sharpe","Sortino","Beta","Treynor"))
      perf_df$Portfolio = unlist(calcPortMeasures(ret_df$Portfolio, ret_df$SP500, rf_range$rf))
      perf_df$SP500 = unlist(calcPortMeasures(ret_df$SP500, ret_df$SP500, rf_range$rf))
      perf_df$Mixed = unlist(calcPortMeasures(ret_df$Mixed, ret_df$SP500, rf_range$rf))

      perf_df[1:2, c("Portfolio","SP500","Mixed")] = round(perf_df[1:2, c("Portfolio","SP500","Mixed")] * 100, 2)

      return (perf_df)
      # return (ret_df[1:5,])
    })
  })



  ###########
  ##  Plots for comparison
  ############

  #Current allocation
  output$graph7 = renderPlotly({
    alloc = data.frame(wght = port_weight$weight, asset = showManyTickers(getTicker()[1:12]))

    g7 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=300, height=300) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))

    g7

  })

  #Same return
  output$graph8 = renderPlotly({

    validate(need(input$go, "")
             )

    opt_w = opt_weights()

    optRet2 = append(opt_w$OptRet, rep(0, 12 - length(opt_w$OptRet)))

    # alloc = data.frame(wght = opt_w$OptRet, asset = c("SP500","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
    alloc = data.frame(wght = optRet2, asset = getTicker()[1:12])


    # g8 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
    g8 = plot_ly(alloc, labels = ~showManyTickers(getTicker()[1:12]), values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=300, height=300) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))

    g8

  })

  #Same Risk
  output$graph9 = renderPlotly({
    validate(need(input$go, "")
    )
    opt_w = opt_weights()

    optRisk2 = append(opt_w$OptRisk, rep(0, 10 - length(opt_w$OptRisk)))

    # alloc = data.frame(wght = opt_w$OptRisk, asset = c("SP500","EuropeStocks","EMStocks","Treasury","CorpBond","RealEstate"))
    alloc = data.frame(wght = optRisk2, asset = getTicker()[1:10])

    # showManyTickers
    # g9 = plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
    g9 = plot_ly(alloc, labels = ~showManyTickers(getTicker()[1:10]), values = ~wght, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 insidetextfont = list(color = '#000'),
                 hoverinfo = 'text',
                 text = ~paste(round(wght,4)*100, ' %'),
                 marker = list(colors = my_colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 showlegend = FALSE, width=300, height=300) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor='rgba(0,0,0,0)',
             plot_bgcolor='rgba(0,0,0,0)',
             margin = list(b = 0, l = 0, t = 0))

    g9

  })


  ###########
  ## Comparison with the optimal portfolio
  #####


  opt_data = reactive({

    #Get backtesting data
    port_ret = bt_data()

    #Get optimal weights
    opt_w = opt_weights()

    #Extract dataframe for dates
    from = as.Date(input$date_range[1])
    to = as.Date(input$date_range[2])

    df = as.data.frame(dataInput())

    opt_port(df, from, to, opt_w, port_ret)  # Comes from shiny_helper.R


  })



  ########
  ##  Graphs for optimal portfollios
  ########

  #Plot backtest compound return
  output$graph10 = renderPlotly({
    validate(need(input$go, "")
    )
    input$go

    isolate({  ### To let weights settle

      bt_df = opt_data()

      #Calculate compound return
      bt_df = bt_df %>%
        gather(key="Asset", value="Return", -date) %>%
        group_by(Asset) %>%
        arrange(date) %>%
        mutate(cumRet = cumsum(Return)) %>%
        # mutate(cumRet = cumprod(1+Return) - 1) %>%
        select(date, Asset, cumRet) %>%
        spread(key=Asset, value=cumRet)

      #Plot
      plot_ly(bt_df, x = ~date, y = ~Portfolio, type = "scatter", mode = "line", name = "Portfolio",
              line = list(color = "Steelblue3", width = 2), width = 700, height = 400) %>%
        add_trace(y= ~OptRet, name = "Similar Return",
                  line = list(color = "black", width = 2)) %>%
        add_trace(y= ~OptRisk, name = "Similar Risk",
                  line = list(color = "gray", width = 2)) %>%
        layout(xaxis = list(title = "", showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
               yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE, tickformat = "%"),

               legend = list(orientation = "h", x = 0.1, y=1.2),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor='rgba(0,0,0,0)',
               margin = list(b = 20, l = 20, t = 30))
    })
  })



  #Plot backtest compound return
  output$graph11 = renderPlot({
    validate(need(input$go2, "")
    )
    input$go2

    isolate({  ### To let weights settle

      ret_df = opt_data()

      ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk)

      rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))

      portf= calcPortMeasures(ret_df$Portfolio, ret_df$SP500, rf_range$rf)
      mu = getSimuMean()$mu1/100
      # mu = ifelse(is.na(input$simu12), portf$AvRet, input$simu12/100)
      prices = asset.paths(100, mu, portf$StDev, 250, periods = 1:40)
      prices = rbind(rep(100,250), prices)
      if(is.null(input$table3)) return(NULL)
      liabs=hot_to_r(input$table3)
      liabs[1,2] = liabs[1,2] - 100
      liabs=apply(liabs,2,function(column) cumsum(column))
      prices[1:length(liabs[,3]),]=prices[1:length(liabs[,3]),]-liabs[,3]+liabs[,2]

      # prices[1:length(liabs[,2]),]=prices[1:length(liabs[,2]),]-liabs[,2]
      has.neg <- apply(prices, 2, function(column) any(column < 0))
      fneg= apply(prices, 2, function(column) which(column<=0)[1])
      if(sum(has.neg)==0){has.neg=1;fneg=0}

      sumCash = sum((hot_to_r(input$table3))[,2])
      yup = 4*sumCash
      ydn = -.5*sumCash

      # cash = getcash()$cash11
      # yup = 4*sum(cash[cash>0])
      # ydn = -.5*sum(cash[cash>0])
      #
      matplot(prices, type='l', xlab='Years', ylab='Prices',
              main='Asset vs Liability - Selected Allocation',
              sub=paste("probability of shortfall =", round(sum(has.neg)/ncol(prices),2), "Shortfall Time=",sum(fneg,na.rm=T)/sum(has.neg)),
              ylim = c(ydn,yup))

      abline(0, 0, lwd = 4, col = "black")
    })
  })

  output$graph12 = renderPlot({
    validate(need(input$go2, "")
    )
    input$go2

    isolate({  ### To let weights settle

      ret_df = opt_data()

      ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk)

      rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))

      portf= calcPortMeasures(ret_df$Same.Return, ret_df$SP500, rf_range$rf)
      mu = getSimuMean()$mu2/100
      # mu = ifelse(is.na(input$simu22), portf$AvRet, input$simu22/100)
      prices = asset.paths(100, mu, portf$StDev, 250, periods = 1:40)
      prices = rbind(rep(100,250), prices)
      if(is.null(input$table3)) return(NULL)
      liabs=hot_to_r(input$table3)
      liabs[1,2] = liabs[1,2] - 100
      liabs=apply(liabs,2,function(column) cumsum(column))

      prices[1:length(liabs[,3]),]=prices[1:length(liabs[,3]),]-liabs[,3]+liabs[,2]
      # prices[1:length(liabs[,2]),]=prices[1:length(liabs[,2]),]-liabs[,2]
      has.neg <- apply(prices, 2, function(column) any(column < 0))
      fneg= apply(prices, 2, function(column) which(column<=0)[1])

      print(sum(fneg,na.rm=T)/length(fneg))
      if(sum(has.neg)==0){has.neg=1;fneg=0}

      sumCash = sum((hot_to_r(input$table3))[,2])
      yup = 4*sumCash
      ydn = -.5*sumCash

      # cash = getcash()$cash11
      # yup = 4*sum(cash[cash>0])
      # ydn = -.5*sum(cash[cash>0])

      matplot(prices, type='l', xlab='Years', ylab='Prices',
              main='Asset vs Liability - Similar Return',
              sub=paste("probability of shortfall =", round(sum(has.neg)/ncol(prices),2), "Shortfall Time=",sum(fneg,na.rm=T)/sum(has.neg)),
              ylim = c(ydn,yup))
      # abline(0, 0, lwd = 5, col = "white")
      abline(0, 0, lwd = 4, col = "black")
    })
  })

  output$graph13 = renderPlot({
    validate(need(input$go2, "")
    )
    input$go2

    isolate({  ### To let weights settle

      ret_df = opt_data()

      ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk)

      rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))

      portf= calcPortMeasures(ret_df$Same.Risk, ret_df$SP500, rf_range$rf)
      mu = getSimuMean()$mu3/100
      # mu = ifelse(is.na(input$simu32), portf$AvRet, input$simu32/100)
      prices = asset.paths(100, mu, portf$StDev, 250, periods = 1:40)
      prices = rbind(rep(100,250), prices)
      if(is.null(input$table3)) return(NULL)
      liabs=hot_to_r(input$table3)
      liabs[1,2] = liabs[1,2] - 100
      liabs=apply(liabs,2,function(column) cumsum(column))

      prices[1:length(liabs[,3]),]=prices[1:length(liabs[,3]),]-liabs[,3]+liabs[,2]
      # prices[1:length(liabs[,2]),]=prices[1:length(liabs[,2]),]-liabs[,2]
      # print(dim(prices))
      has.neg <- apply(prices, 2, function(column) any(column < 0))
      # print((has.neg))
      fneg= apply(prices, 2, function(column) which(column<=0)[1])
      if(sum(has.neg)==0){has.neg=1;fneg=0}

      sumCash = sum((hot_to_r(input$table3))[,2])
      yup = 4*sumCash
      ydn = -.5*sumCash

      # cash = getcash()$cash11
      # yup = 4*sum(cash[cash>0])
      # ydn = -.5*sum(cash[cash>0])

      matplot(prices, type='l', xlab='Years', ylab='Prices',
              main='Asset vs Liability - Similar Risk',
              sub=paste("probability of shortfall =", round(sum(has.neg)/ncol(prices),2), "Shortfall Time=",sum(fneg,na.rm=T)/sum(has.neg)),
              ylim = c(ydn,yup))

      abline(0, 0, lwd = 4, col = "black")
    })
  })

  ## Opt Portfolio comparison table
  output$bt_table2 = renderTable(digits=2, {
    validate(need(input$go, "")
    )
    input$go

    isolate({
      #Select data
      ret_df = opt_data()

      ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk)

      rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))


      #Calculate performance measures
      perf_df = data.frame(Measure = c("Return (annualized), %","Risk (annualized), %","Sharpe","Sortino","Beta","Treynor"))
      perf_df$Portfolio = unlist(calcPortMeasures(ret_df$Portfolio, ret_df$SP500, rf_range$rf))
      Portfolio = (calcPortMeasures(ret_df$Portfolio, ret_df$SP500, rf_range$rf))
      print(Portfolio)
      perf_df$Same.Return = unlist(calcPortMeasures(ret_df$Same.Return, ret_df$SP500, rf_range$rf))
      perf_df$Same.Risk = unlist(calcPortMeasures(ret_df$Same.Risk, ret_df$SP500, rf_range$rf))

      perf_df = perf_df %>% select(Measure, Portfolio, Same.Return, Same.Risk) %>% rename(Similar.Return = Same.Return,
                                                                                          Similar.Risk = Same.Risk)

      perf_df[1:2, c("Portfolio","Similar.Return","Similar.Risk")] = round(perf_df[1:2, c("Portfolio","Similar.Return","Similar.Risk")] * 100, 2)


      return (perf_df)
    })
  })


  getMeanStd <- reactive({

    ret_df = opt_data()

    ret_df = ret_df %>% rename(Same.Return=OptRet, Same.Risk = OptRisk)

    rf_range = rf%>% filter(as.Date(date) >= as.Date(input$date_range[1]) & as.Date(date) <= as.Date(input$date_range[2]))

    # if(is.null(input$table3)) return(NULL)
    # liabs=hot_to_r(input$table3)
    # liabs=apply(liabs,2,function(column) cumsum(column))

    portf= calcPortMeasures(ret_df$Portfolio, ret_df$SP500, rf_range$rf)
    mu1 = portf$AvRet
    std1 = portf$StDev

    portf= calcPortMeasures(ret_df$Same.Return, ret_df$SP500, rf_range$rf)
    mu2 = portf$AvRet
    std2 = portf$StDev

    portf= calcPortMeasures(ret_df$Same.Risk, ret_df$SP500, rf_range$rf)
    mu3 = portf$AvRet
    std3 = portf$StDev

    return(list(mu1 = mu1, mu2 = mu2, mu3 = mu3,
                std1 = std1, std2 =std2, std3 = std3))

  })

  # getcash <- reactive({
  #
  #   simubase = getMeanStd()
  #   # liabs = simubase$liabs
  #   mu1 = getSimuMean()$mu1/100
  #   # mu1 = ifelse(is.na(input$simu12), simubase$mu1, input$simu12/100)
  #   prices = asset.paths(100, mu1, simubase$std1, 250, periods = 1:40)
  #   prices = rbind(rep(100,250), prices)
  #
  #   # prices[1:length(liabs[,2]),]=prices[1:length(liabs[,2]),]-liabs[,2]
  #   cash1 = rowMeans(prices)
  #   cash1 = c(100, diff(cash1))
  #
  #   mu2 = getSimuMean()$mu2/100
  #   # mu2 = ifelse(is.na(input$simu22), simubase$mu2, input$simu22/100)
  #   prices = asset.paths(100, mu2, simubase$std2, 250, periods = 1:40)
  #   prices = rbind(rep(100,250), prices)
  #
  #   # prices[1:length(liabs[,2]),]=prices[1:length(liabs[,2]),]-liabs[,2]
  #   cash2 = rowMeans(prices)
  #   cash2 = c(100, diff(cash2))
  #
  #   mu3 = getSimuMean()$mu3/100
  #   # mu3 = ifelse(is.na(input$simu32), simubase$mu3, input$simu32/100)
  #   prices = asset.paths(100, mu3, simubase$std3, 250, periods = 1:40)
  #   prices = rbind(rep(100,250), prices)
  #
  #   # prices[1:length(liabs[,2]),]=prices[1:length(liabs[,2]),]-liabs[,2]
  #   cash3 = rowMeans(prices)
  #   cash3 = c(100, diff(cash3))
  #
  #   return(list(cash11 = cash1, cash22 = cash2, cash33 = cash3))
  # })

  output$simu11 <- renderPrint({
    validate(need(input$go, "")
    )
    getSimuMean()$mu0})
  # output$simu21 <- renderPrint(getMeanStd()$mu2*100)
  # output$simu31 <- renderPrint(getMeanStd()$mu3*100)

  output$graph11_2 = renderPlot({
    validate(need(input$go2, "")
    )
    input$go2

    isolate({
      # data = as.data.frame(cbind(1:41, getcash()$cash11, -getLiab()))
      data = as.data.frame(cbind(1:41, (hot_to_r(input$table3))[,2], -(hot_to_r(input$table3))[,3]))
      colnames(data) = c("year", "cashIn", "cashout")
      ggplot() +
        geom_bar(data = data, aes(x = year, y = cashIn, fill = cashIn < 0), stat = "identity") +
        geom_bar(data = data, aes(x = year, y = cashout, fill = cashout < 0), stat = "identity") +
        scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("#619cff", "#F8766D")) +
        theme_classic() + ggtitle("Cashflows") + theme(plot.title = element_text(hjust = 0.5))

      # ggplot(data, aes(x = year, y = cashflows)) +
      #   geom_bar(aes(fill = cashflows < 0), stat = "identity") +
      #   scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("gray", "red")) +
      #   theme_classic() + ggtitle("Cashflows : Selected Allocation") + theme(plot.title = element_text(hjust = 0.5))
    })
  }, height = 300)

  # initialize
  customLiab=c(0,rep(120,40))
  customCash=c(20000,rep(0,40))
  alm=list()
  alm$customLiab=customLiab
  alm$customCash=customCash

  # observeEvent(input$getAlm, {
  #   load("customAlm.RData")
  #   print(alm)
  # })
  # observeEvent(input$saveAlm, {
  #   save(alm,file="customAlm.RData")
  # })


  getLiab <- reactive({
    switch (input$simuWay,
            "Recently Retired" = c(0,rep(120,40)),
            "Pre Retired" = c(0,rep(0,10),rep(75,30)),
            "Couple and Young Kids" = c(0,rep(0,7),rep(50,6),rep(0,11),rep(85,16)),
            "Cook County"=c(0,871 , 929, 991, 1058, 1129, 1204, 1285, 1371, 1463, 1561, 1666, 1777, 1896, 2023, 2159, 2303, 2458, 2622, 2798, 2986, 3186, 3399, 3627, 3870,
                            4129,  4405,  4701,  5016,  5352,  5710,  6093,  6501,  6937,  7401,  6800,  6400,  6000,  5500,5000 ,4000 ),
            c(0,rep(0,5),rep(5,4),rep(0,11),rep(9,20)),
            "Custom"= alm$customLiab
    )
  })

  getCash <- reactive({
    switch (input$simuWay,
            "Recently Retired" = c(20000,rep(0,40)),
            "Pre Retired" = c(150,rep(45,10),rep(0,30)),
            "Couple and Young Kids" = c(120,rep(35,40)),
            "Cook County" = c(9116,749 , 409, 420, 431, 442, 453, 465, 477, 490, 503, 516, 529, 543, 557, 571, 586, 601, 617, 633, 650, 666, 684, 702, 720, 739, 758, 777, 798, 818, 840, 862, 884, 907,
                              930, 955, 979, 1005, 1031, 1058, 1085),
            c(100,rep(0,5),rep(5,4),rep(0,11),rep(9,20)),
            "Custom" = alm$customCash
    )
  })


  # output$debug = renderPrint(hot_to_r(input$table3)[1,2])

  output$table3 <- renderRHandsontable({

    # input$go2

    nYears=40


    DF = data.frame(year = 0:nYears,
                    # cashIn = c(100,rep(0,4),rep(4,5),rep(0,9),rep(3,8), rep(0,14)),
                    cashIn = getCash(),
                    cashOut = getLiab(),
                    stringsAsFactors = FALSE)

    rhandsontable(DF, width = 300, height = 1000, rowHeaders = NULL) %>%
      hot_col("cashOut", allowInvalid = TRUE)
  })



  getHis <- reactive({
    data = as.data.frame(dataInput())
    from = as.Date(input$date_range[1])
    to = as.Date(input$date_range[2])
    df_range = data %>% rownames_to_column("date") %>%
      filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")
    colMeans(df_range)*250
  })

  getSimuMean <- reactive({
    rtn = hot_to_r(input$table5)
    rtn0 = rtn[1][[1]]
    rtn = rtn[2][[1]]
    wgts = opt_weights()

    mu0 = simulationMean(rtn0, port_weight$weight)
    mu1 = simulationMean(rtn, port_weight$weight)
    mu2 = simulationMean(rtn, wgts$OptRet)
    mu3 = simulationMean(rtn, wgts$OptRisk)
    return(list(mu0 = mu0, mu1 = mu1, mu2 = mu2, mu3 = mu3))
  })



  # output$tttest = renderPrint(getSimuMean())
  output$simu12 <- renderPrint({
    validate(need(input$go, "")
    )
    getSimuMean()$mu1
  })


  output$table5 <- renderRHandsontable({
    col1 = getHis()*100

    DF = data.frame(Historical = col1,
                    Expected = col1
                    )

    rhandsontable(DF, width = 200, height = 260) %>%
      hot_col("Historical", readOnly = TRUE)

  })


})
