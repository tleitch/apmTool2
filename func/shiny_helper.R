######################################
# Miscellaneous functions 
###


## Funcions to balance weight
#Updates weights if changed

updateweight = function(oldweight, new, i) {
  # if (ticker[i] == "NA") {new == 0}
  if (new==oldweight[i]) {
    oldweight
  } else if (new==1){
    newweight = rep(0,10)
    oldweight = oldweight
    new = 0.9999
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(1-new)
    newweight[i] = new
    newweight
  } else {
    newweight = rep(0,10)
    oldweight = oldweight
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(1-new)
    newweight[i] = new
    newweight
  }
}


outflowControl = function(oldweight, new, i) {
  newweight = oldweight
  cursum = sum(oldweight[-i])
  newweight[i] = min((1-cursum), new)
  newweight
}


showTicker <- function(ticker) {
  # switch (ticker,
  #         "SPY" = "SP500",
  #         "PRESX" = "EuropeStocks",
  #         "EEM" = "EMStocks",
  #         "DGS10" = "Treasury",
  #         "LQD" = "CorpBonds",
  #         "IYR" = "RealEstate",
  #         "PSP" = "PrivateEquity",
  #         "DFGBX" = "GlobalBond",
  #         ticker
  # )
  ticker
}

# showTicker <- function(ticker) {
#   if (ticker == "SPY") {
#     "S&P 500"
#   } else if (ticker == "DGS10") {
#     "Treasury"
#   } else {
#     ticker
#   }
# }

showManyTickers <- function(ticker) {
  for (i in 1:length(ticker)) {
    ticker[i] = showTicker(ticker[i])
  }
  ticker
}

getCol <- function(price, ticker) {
  if (dim(price)[2] != 1) {
    retCol = diff(log(Ad(price)))
    retCol = retCol[2:length(retCol)]
    colnames(retCol) = ticker
  } else {
    retCol = log(1+price/100)/250
    retCol = retCol[index(retCol) >= "2000-01-03",1]
    colnames(retCol) = ticker
  }
  retCol
}

# updateweight = function(oldweight, new, i) {
#   if (new==oldweight[i]) {
#     oldweight
#   } else if (new==1){
#     newweight = rep(1e-10,10)
#     newweight[i] = new
#     newweight
#   } else {
#     before = c(1:i)
#     newweight = rep(0,10)
#     newweight[before] = oldweight[before]
#     newweight[i] = new
#     beforesum = sum(newweight[before])
#     if (beforesum >= 1) {
#       newweight[before] = newweight[before]/beforesum
#     } else {
#       lastsum = 1 - beforesum
#       newweight[-before] = oldweight[-before]/(sum(oldweight[-before]) + 1e-10) * lastsum
#     }
#     newweight
#   }
# }

# suspend and resume a list of observers
suspendMany = function(observers) invisible(lapply(observers, function(x) x$suspend()))
resumeMany = function(observers) invisible(lapply(observers, function(x) x$resume()))

# function to change sliderInput
wghtsliderInput = function(inputId,value, label, submitted=FALSE) {
  if (!submitted)
    sliderInput(inputId=inputId,
                value=value,
                label=label,
                min=0,
                max=1,
                ticks=FALSE)
}



### Function to perform backtesting

bt_port = function(df, from, to, wght, rebalance, dfzero){
  
  # wipe out weight for "NA"
  i = 10
  while (wght[i] <= 1e-7 && i > (dim(df)[2])) {
    wght = wght[-i]
    i = i-1
  }
  # Create a dataframe with portfolio and benchmark returns
  
  df = as.data.frame(df)
  
  df_tmp = df %>% mutate(date = as.Date(row.names(df)))
  df_tmp2 = dfzero %>% mutate(date = as.Date(row.names(dfzero)))
  
  # Portfolio return
  port_ret = data.frame(calcPortReturn(df, from, to, wght, rebalance))
  
  port_ret$date = as.Date(row.names(port_ret))
  
  port_ret = rename(port_ret, Portfolio = RetPort)
  
  # 60/30/10 Portfolio
  sixty_port = data.frame(calcPortReturn(dfzero, from, to,
                                         wght = c(0.6, 0, 0, 0.1, 0.3, 0, 0, 0), rebalance))
  
  sixty_port$date = as.Date(row.names(sixty_port))
  sixty_port = rename(sixty_port, R60T10C30 = RetPort)
  
  # Merge into one df
  port_ret = merge(port_ret, df_tmp2[,c("SP500","date")], by = "date", all.x = TRUE)
  port_ret = merge(port_ret, sixty_port, by = "date", all.x = TRUE)
  
  
  return(port_ret)
}


#### Function to find optimal portfolios
opt_port = function(df, from, to, opt_w, port_ret){

  
  #Get portfolio  returns
  port_ret = port_ret %>% select(date, Portfolio, SP500)
  
  df_tmp = df %>% rownames_to_column("date") %>%
    filter(as.Date(date)>=from & as.Date(date) <= to) %>% column_to_rownames("date")
  
  #Same return portfolio
  opt_ret = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRet, rebalance = "Never" , geometric = TRUE))
  # opt_ret = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRet, rebalance = "Never" , geometric = FALSE))
  # opt_ret = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRet, rebalance = "Never"))
  opt_ret$date = as.Date(row.names(opt_ret))
  
  #Same risk portfolio
  opt_risk = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRisk, rebalance = "Never", geometric = TRUE))
  # opt_risk = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRisk, rebalance = "Never", geometric = FALSE))
  # opt_risk = data.frame(calcPortReturn(df_tmp, from, to, opt_w$OptRisk, rebalance = "Never"))
  opt_risk$date = as.Date(row.names(opt_risk))
  
  
  #Combine into one dataframe
  port_ret = merge(port_ret, opt_ret, by = "date", all.x = TRUE)
  port_ret = merge(port_ret, opt_risk, by = "date", all.x = TRUE)
  port_ret$date = as.Date(port_ret$date)
  
  
  #Change names
  colnames(port_ret) = c("date","Portfolio","SP500" , "OptRet","OptRisk")
  
  print(head(port_ret))
  return(port_ret)
}


simulationMean <- function(rtn, wgt) {
  simRtn = exp(rtn/100)-1
  totalSim = sum(wgt[1:length(rtn)]*simRtn)
  log(totalSim+1)*100
}

