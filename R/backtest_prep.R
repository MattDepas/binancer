#' Set backtest settings
#'
#' @param indicator
#' @param MACD_fast
#' @param MACD_slow
#' @param MACD_nSig
#' @param RSI_n
#' @param min_momentum_count
#' @param interval
#' @param start_time
#' @param end_time
#' @param initial_value
#' @param min_trade
#'
#' @return
#' @export backtest_settings
#'
#' @examples
#'
#'
#'
settings <- new.env()

interval_secs <- new.env()

backtest_settings <- function(indicator = "MACD",
                              MACD_fast = 12,
                              MACD_slow = 26,
                              MACD_nSig = 9,
                              RSI_n = 14,
                              min_momentum_count = 50,
                              interval = c("1h"),
                              start_time = Sys.Date()-90,
                              end_time = Sys.Date(),
                              base_currency = "BTC",
                              initial_value = 1,
                              min_trade = 0.0002){
  
  #Indicator type
  settings$indicator <- indicator
  
  #MACD settings:
  settings$nFast	 <-   MACD_fast #Number of periods for fast moving average.
  settings$nSlow	 <-   MACD_slow #Number of periods for slow moving average.
  settings$nSig    <-   MACD_nSig #Number of periods for signal moving average.
  
  #RSI settings
  settings$n <- RSI_n #Number of periods for moving averages.
  
  #Trade frequency and historical data
  settings$interval    <- interval  #c("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
  settings$start_time  <- start_time
  settings$end_time    <- end_time
  
  #interval date check functionality
  interval_secs$interval <- c("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w")
  interval_secs$seconds  <- c(60,180,300,900,1800,3600,7200,14400,21600,28800,43200,86400,259200,604800)
  
  
  settings$date_check  <- start_time
  
  #Initial portfolio value & base currency
  settings$base_currency <- base_currency
  settings$initial_value <- initial_value
  
  #Binance trade rules
  settings$min_trade <- min_trade #minimum trade amount stated by binance
  settings$min_momentum_count <- min_momentum_count #The amount of the market required to eb showing positive momentum to trade
  
}

#' Check the settings specified for backtesting
#'
#' @return
#' @export backtest_settings_check
#'
backtest_settings_check <- function() {
  
  cat("\nBelow are the backtesting specifications in place:
      \nIndicator: ",paste0(settings$indicator),"\n",
      dplyr::case_when(settings$indicator=="MACD" ~ paste0("\nMACD Settings:\nnFast: ",settings$nFast,"\nnSlow: ",settings$nSlow,"\nnSig: ",settings$nSig),
                       settings$indicator=="RSI" ~ paste0("\nRSI Settings:\nn: ",settings$n)),
      "\n\nTrade frequency and historical data:",
      "\nStart time: ",paste0(settings$start_time),
      "\nEnd time: ",paste0(settings$end_time),
      "\nFrequency: ",paste0(settings$interval),
      "\n\nBase currency & initial portfolio value",
      "\nBase currency: ",paste0(settings$base_currency),
      "\nInitial portfolio value (in base currency): ",paste0(settings$initial_value),
      "\n\nTrading rules in place: ",
      "\nMinimum trade amount: ",paste0(settings$min_trade),
      "\nMinimum positive momentum count: ",paste0(settings$min_momentum_count))
}





#------------------------------------------------------------------------------------------------------------------------------------------------#




#' Fetch historical coin data
#' @description This function serves to pull historical market data for a given symbol
#' @usage coin_data_importer<-function(symbol, interval = interval, start_time = start_time, end_time = end_time)
#' @param symbol The binance market of interest (e.g. the Ethereum-Bitcoin market "ETHBTC")
#' @param interval The interval at which historical data is pulled ("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
#' @param start_time The starting date of the historical data
#' @param end_time The end date of the historical data
#' @importFrom magrittr "%>%"
#' @importFrom stringr "str_sub"
#' @importFrom dplyr "select" "arrange" "mutate" "distinct" "ungroup" "group_by"
#' @return
#' @export coin_data_importer
#'

coin_data_importer<-function(symbol, interval = interval, start_time = start_time, end_time = end_time){
  
  coin <- symbol
  
  coin_data <- data.frame(binance_klines(as.character(coin),interval = interval, start_time = start_time, end_time = end_time)) %>%
    dplyr::select(symbol, close_time, open, high, low, close, trades, volume) %>%
    dplyr::arrange(symbol,close_time) %>%
    #obtain distinct
    # mutate(date = str_sub(as.character(close_time),1,10))%>%
    # distinct(symbol, date, .keep_all = TRUE)%>%
    #end
    dplyr::ungroup()%>%
    dplyr::group_by(symbol)
  
  coin_data_revised<-data.frame(coin_data$symbol,
                                coin_data$close_time,
                                coin_data$close,
                                coin_data$volume)%>%
    dplyr::rename(time = "coin_data.close_time",
                  close = "coin_data.close",
                  volume = "coin_data.volume")
  
  return(coin_data_revised)
  
}


#' Fetch historical coin data
#' @description This function serves to pull all historical market data that
#' @usage historical(symbol, interval = "1d", start_time = "2021-01-01", end_time = NULL)
#' @param symbol The binance market of interest (e.g. the Ethereum-Bitcoin market "ETHBTC")
#' @param interval The interval at which historical data is pulled ("1m", "3m", "5m", "15m", "30m", "1h", "2h", "4h", "6h", "8h", "12h", "1d", "3d", "1w", "1M")
#' @param start_time The starting date of the historical data
#' @param end_time The end date of the historical data
#' @param base_currency The base currency being used for trading (e.g. "BTC", "ETC")
#' @importFrom magrittr "%>%"
#' @importFrom stringr "str_sub"
#' @importFrom purrr "map" "possibly"
#' @importFrom dplyr "rename" "filter" "arrange" "mutate"
#' @importFrom tidyr "unnest"
#' @return historical_data
#' @export historical
#'
#'

historical <- function(interval = settings$interval, start_time = settings$start_time, end_time = settings$end_time, base_currency = settings$base_currency){
  
  error_frame<-data.frame(NA,NA,NA,NA)
  colnames(error_frame)<-c("coin_data.symbol","time","close","volume")
  
  historical_data<-data.frame(binancer::binance_symbols()) %>%
    dplyr::rename('coins' = binancer..binance_symbols..)%>%
    dplyr::filter(stringr::str_sub(coins,-3,-1) == base_currency) %>%
    dplyr::arrange(coins)%>%
    dplyr::mutate(datasets = purrr::map(as.character(coins),
                                        possibly(~coin_data_importer(symbol = .x,
                                                                     interval = interval,
                                                                     start_time = start_time,
                                                                     end_time = end_time),
                                                 otherwise = error_frame,
                                                 quiet = TRUE)
    )
    )
  
  historical_data<-historical_data %>%
    tidyr::unnest()%>%
    dplyr::rename(symbol="coin_data.symbol")%>%
    ungroup()%>%
    group_by(symbol)%>%
    mutate(timedif=as.numeric(difftime(time, dplyr::lag(time, 1), units = "secs")))
  
  return(historical_data)
}



#------------------------------------------------------------------------------------------------------------------------------------------------------




#' Generate trade indicator information
#'
#' @return historical_data
#' @export generate_indicator
#'
#' @examples
#'

generate_indicator <- function(historical_data = historical_data, indicator = settings$indicator){
  
  if(indicator=="MACD"){
    
    matrix_macd<-as.matrix(c("insufficient data"))
    colnames(matrix_macd)<-"macd"
    
    df_indicator <-  historical_data%>%
      dplyr::arrange(symbol,time)%>%
      dplyr::group_by(symbol)%>%
      dplyr::mutate(close_change = (close - dplyr::lag(close,1))/dplyr::lag(close,1),
                    close_change_lead = (dplyr::lead(close,1) - close)/close,
                    macd = as.numeric(tryCatch(TTR::MACD(close,
                                                         nFast = settings$nFast,
                                                         nSlow = settings$nSlow,
                                                         nsig = settings$nSig),
                                               error=function(err) matrix_macd)[,1]))%>%
      dplyr::ungroup()%>%
      dplyr::group_by(time)%>%
      dplyr::mutate(macd_great0 = dplyr::if_else(as.numeric(macd)>0,
                                                 as.numeric(macd),
                                                 0),
                    macd_great0 = dplyr::if_else(is.na(macd_great0),
                                                 0,
                                                 macd_great0),
                    macd_weighting = dplyr::if_else(macd_great0/sum(macd_great0)>0,
                                                    macd_great0/sum(macd_great0),
                                                    0),
                    macd_weighting = dplyr::if_else(is.na(macd_weighting),
                                                    0,
                                                    macd_weighting))
  }
  
  if(indicator=="RSI"){
    
    df_indicator <-  historical_data%>%
      dplyr::arrange(symbol,time)%>%
      dplyr::group_by(symbol)%>%
      dplyr::mutate(rsi=as.numeric(tryCatch(TTR::RSI(close,
                                                     n = settings$n),
                                            error=function(err) as.numeric("na"))))%>%
      dplyr::ungroup()%>%
      dplyr::group_by(time)%>%
      dplyr::mutate(rsi_great50 = dplyr::if_else(as.numeric(rsi)>50,
                                                 as.numeric(rsi),
                                                 0),
                    rsi_great50 = dplyr::if_else(is.na(rsi_great50),
                                                 0,
                                                 rsi_great50),
                    rsi_weighting = dplyr::if_else(rsi_great50/sum(rsi_great50)>0,
                                                   rsi_great50/sum(rsi_great50),
                                                   0),
                    rsi_weighting = dplyr::if_else(is.na(rsi_weighting),
                                                   0,
                                                   rsi_weighting))
    
  }
  
  return(df_indicator)
  
  
  # if(settings$indicator=="RSI"){
  # historical_data}
  
  # if(settings$indicator=="RSI"){
  #   historical_data}
  #
  # if(settings$indicator=="RSI"){
  #   historical_data}
  
}











#----------------------------------------------------------------------------------------------------------------------------------------------------------


#' conduct backtest
#'
#' @param data
#' @param indicator
#' @param initial_value
#' @param starting_date
#' @param end_date
#' @param min_momentum_count
#' @param fees
#'
#' @return
#' @export backtest
#'
#' @examples
#'
#'
#'

backtest <- function(data = data,
                     indicator = settings$indicator,
                     initial_value = settings$initial_value,
                     starting_date = settings$start_time,
                     end_date = settings$end_time,
                     min_trade = settings$min_trade,
                     min_momentum_count = settings$min_momentum_count,
                     fees = 0.001){
  #-------------------------------- Backtest portfolio weightings -----------------------
  if(indicator=="MACD"){
    
    start<-Sys.time()
    
    #create coin dataset
    coin_data <- data %>%
      
      dplyr::select(time,symbol,close,close_change,close_change_lead,macd,macd_weighting) %>%
      
      dplyr::arrange(symbol,time)%>%
      dplyr::group_by(symbol)%>%
      #create values to be generated
      dplyr::mutate(proposed_position = 0,
                    lagged_actual_position = 0,
                    trade = 0,
                    trade_rule1_indicator = 0,
                    sign_test = 0,
                    tradeshare_samesign = 0,
                    revised_trade = 0,
                    actual_position = 0,
                    portfolio_share = 0)%>%
      
      # dplyr::filter(time>= starting_date & time<= end_date)%>%
      
      dplyr::ungroup() %>%
      dplyr::group_by(time) %>%
      dplyr::mutate(close_change = ifelse(is.na(close_change),0,close_change))
    
    #create total portfolio dataset
    total <- data %>%
      dplyr::select(time) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      #set starting BTC amount and set up checker variables
      dplyr::mutate(symbol = "Total Portfolio",
                    value = initial_value,
                    redist_trade_val = 0,
                    revised_trade_total = 0,
                    momentum_count = 0,
                    macd_weighting_total = 0)#%>%
    # dplyr::filter(time>= starting_date & time<= end_date)
    
    #create list of times
    timelist <- data%>%
      dplyr::select(time) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time)#%>%
    # dplyr::filter(time>= starting_date & time<= end_date)
    
    
    # run backtesting loop
    for (i in 1:(nrow(timelist)-1)){
      
      timeperiod<- timelist$time[i]
      
      portval <- total$value[i]
      
      momentum_count<-as.numeric(nrow(coin_data %>%
                                        dplyr::ungroup() %>%
                                        dplyr::filter(time == timeperiod)%>%
                                        dplyr::filter(macd>0)))
      
      coin_data <- coin_data %>%
        dplyr::ungroup()%>%
        dplyr::group_by(symbol) %>%
        dplyr::mutate(proposed_position =      dplyr::if_else(time==timeperiod,
                                                              dplyr::case_when(momentum_count>=min_momentum_count ~ portval*macd_weighting,
                                                                               TRUE ~ 0),
                                                              proposed_position),
                      lagged_actual_position = dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(time==timelist$time[1],
                                                                             0,
                                                                             dplyr::lag(actual_position,1)*(1+close_change)),
                                                              lagged_actual_position),
                      trade =                  dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(is.na(proposed_position - lagged_actual_position),
                                                                             0,
                                                                             proposed_position - lagged_actual_position),
                                                              trade),
                      trade_rule1_indicator =  dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(abs(trade)>min_trade,
                                                                             1,
                                                                             0),
                                                              trade_rule1_indicator))
      
      
      
      redist_trade_val <- coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod & !is.na(trade)) %>%
        dplyr::summarise(sum(trade[trade_rule1_indicator==0]))%>%
        dplyr::rename(redist_val="sum(trade[trade_rule1_indicator == 0])")
      redist_val <- redist_trade_val$redist_val[1]
      
      coin_data <- coin_data %>%
        dplyr::ungroup()%>%
        dplyr::group_by(time) %>%
        dplyr::mutate(sign_test           =    dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(sign(trade)==sign(redist_val),
                                                                             1,
                                                                             0),
                                                              sign_test),
                      tradeshare_samesign =    dplyr::if_else(time==timeperiod,
                                                              dplyr::case_when(sign_test ==1 & trade_rule1_indicator == 1 & proposed_position!=0 ~ trade/sum(trade[trade_rule1_indicator==1 & sign_test==1 & proposed_position!=0]),
                                                                               TRUE                                                              ~ 0),
                                                              tradeshare_samesign),
                      revised_trade =          dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(trade_rule1_indicator==1,
                                                                             trade+(tradeshare_samesign*redist_val),
                                                                             0),
                                                              revised_trade),
                      actual_position =        dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(is.na(lagged_actual_position+revised_trade),
                                                                             0,
                                                                             (lagged_actual_position+revised_trade)-fees*revised_trade),
                                                              actual_position),
                      portfolio_share =        dplyr::if_else(time==timeperiod,
                                                              100*(actual_position/portval),
                                                              portfolio_share))
      
      
      port_val_calc <- coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod) %>%
        dplyr::summarise(sum(actual_position*(1+close_change_lead)))%>%
        dplyr::rename(value="sum(actual_position * (1 + close_change_lead))")
      
      revised_trade_total <- coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod) %>%
        dplyr::summarise(sum(revised_trade))%>%
        dplyr::rename(revised_trade_total="sum(revised_trade)")
      revised_trade_total <- revised_trade_total$revised_trade_total[1]
      
      weighting_check<-coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod) %>%
        dplyr::summarise(sum(macd_weighting))%>%
        dplyr::rename(value="sum(macd_weighting)")
      
      
      #revise portfolio value by adding the diff or removing the diff between trade amount and raw_weights_resdist_port_val_change_revised
      
      total$redist_trade_val[i] <- redist_val
      total$revised_trade_total[i] <- revised_trade_total
      total$momentum_count[i] <- momentum_count
      total$macd_weighting_total[i] <- weighting_check$value[1]
      total$value[i+1] <- dplyr::if_else(momentum_count<min_momentum_count,
                                         total$value[i],
                                         port_val_calc$value[1])
      
      
      message(paste0("Rotation ",i,"/",nrow(timelist)," completed"))
      gc()
      
    }
    
    
    
  }
  
  if(indicator=="RSI"){
    
    print(class(starting_date))
    print(class(end_date))
    
    start<-Sys.time()
    
    #create coin dataset
    coin_data <- data %>%
      
      dplyr::select(time,symbol,close,close_change,close_change_lead,rsi,rsi_weighting) %>%
      
      dplyr::arrange(symbol,time)%>%
      dplyr::group_by(symbol)%>%
      #create values to be generated
      dplyr::mutate(proposed_position = 0,
                    lagged_actual_position = 0,
                    trade = 0,
                    trade_rule1_indicator = 0,
                    sign_test = 0,
                    tradeshare_samesign = 0,
                    revised_trade = 0,
                    actual_position = 0,
                    portfolio_share = 0)%>%
      
      # dplyr::filter(time>= starting_date & time<= end_date)%>%
      
      dplyr::ungroup() %>%
      dplyr::group_by(time) %>%
      dplyr::mutate(close_change = ifelse(is.na(close_change),0,close_change))
    
    #create total portfolio dataset
    total <- data %>%
      dplyr::select(time) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time) %>%
      #set starting BTC amount and set up checker variables
      dplyr::mutate(symbol = "Total Portfolio",
                    value = initial_value,
                    redist_trade_val = 0,
                    revised_trade_total = 0,
                    momentum_count = 0,
                    rsi_weighting_total = 0)#%>%
    # dplyr::filter(time>= starting_date & time<= end_date)
    
    #create list of times
    timelist <- data%>%
      dplyr::select(time) %>%
      dplyr::distinct() %>%
      dplyr::arrange(time)#%>%
    # dplyr::filter(time>= starting_date & time<= end_date)
    
    
    # run backtesting loop
    for (i in 1:(nrow(timelist)-1)){
      
      timeperiod<- timelist$time[i]
      
      portval <- total$value[i]
      
      momentum_count<-as.numeric(nrow(coin_data %>%
                                        dplyr::ungroup() %>%
                                        dplyr::filter(time == timeperiod)%>%
                                        dplyr::filter(rsi>0)))
      
      coin_data <- coin_data %>%
        dplyr::ungroup()%>%
        dplyr::group_by(symbol) %>%
        dplyr::mutate(proposed_position =      dplyr::if_else(time==timeperiod,
                                                              dplyr::case_when(momentum_count>=min_momentum_count ~ portval*rsi_weighting,
                                                                               TRUE ~ 0),
                                                              proposed_position),
                      lagged_actual_position = dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(time==timelist$time[1],
                                                                             0,
                                                                             dplyr::lag(actual_position,1)*(1+close_change)),
                                                              lagged_actual_position),
                      trade =                  dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(is.na(proposed_position - lagged_actual_position),
                                                                             0,
                                                                             proposed_position - lagged_actual_position),
                                                              trade),
                      trade_rule1_indicator =  dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(abs(trade)>min_trade,
                                                                             1,
                                                                             0),
                                                              trade_rule1_indicator))
      
      
      
      redist_trade_val <- coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod & !is.na(trade)) %>%
        dplyr::summarise(sum(trade[trade_rule1_indicator==0]))%>%
        dplyr::rename(redist_val="sum(trade[trade_rule1_indicator == 0])")
      redist_val <- redist_trade_val$redist_val[1]
      
      coin_data <- coin_data %>%
        dplyr::ungroup()%>%
        dplyr::group_by(time) %>%
        dplyr::mutate(sign_test           =    dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(sign(trade)==sign(redist_val),
                                                                             1,
                                                                             0),
                                                              sign_test),
                      tradeshare_samesign =    dplyr::if_else(time==timeperiod,
                                                              dplyr::case_when(sign_test ==1 & trade_rule1_indicator == 1 & proposed_position!=0 ~ trade/sum(trade[trade_rule1_indicator==1 & sign_test==1 & proposed_position!=0]),
                                                                               TRUE                                                              ~ 0),
                                                              tradeshare_samesign),
                      revised_trade =          dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(trade_rule1_indicator==1,
                                                                             trade+(tradeshare_samesign*redist_val),
                                                                             0),
                                                              revised_trade),
                      actual_position =        dplyr::if_else(time==timeperiod,
                                                              dplyr::if_else(is.na(lagged_actual_position+revised_trade),
                                                                             0,
                                                                             lagged_actual_position+revised_trade),
                                                              actual_position),
                      portfolio_share =        dplyr::if_else(time==timeperiod,
                                                              100*(actual_position/portval),
                                                              portfolio_share))
      
      
      port_val_calc <- coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod) %>%
        dplyr::summarise(sum(actual_position*(1+close_change_lead)))%>%
        dplyr::rename(value="sum(actual_position * (1 + close_change_lead))")
      
      revised_trade_total <- coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod) %>%
        dplyr::summarise(sum(revised_trade))%>%
        dplyr::rename(revised_trade_total="sum(revised_trade)")
      revised_trade_total <- revised_trade_total$revised_trade_total[1]
      
      weighting_check<-coin_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(time == timeperiod) %>%
        dplyr::summarise(sum(rsi_weighting))%>%
        dplyr::rename(value="sum(rsi_weighting)")
      
      
      #revise portfolio value by adding the diff or removing the diff between trade amount and raw_weights_resdist_port_val_change_revised
      
      total$redist_trade_val[i] <- redist_val
      total$revised_trade_total[i] <- revised_trade_total
      total$momentum_count[i] <- momentum_count
      total$rsi_weighting_total[i] <- weighting_check$value[1]
      total$value[i+1] <- dplyr::if_else(momentum_count<min_momentum_count,
                                         total$value[i],
                                         port_val_calc$value[1])
      
      
      
    }
    
    
  }
  
  
  end<-Sys.time()
  
  time_dif<<-start-end
  
  coin_data<<-coin_data
  total<<-total
  timelist<<-timelist
  
}
 