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
                              base_currency = "BUSD",
                              initial_value = 1,
                              min_trade = 5){
  
  
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
 
  return(as.list(settings))
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
    dplyr::select(symbol, close_time, open, high, low, close, trades, volume, quote_asset_volume) %>%
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
                                coin_data$volume,
                                coin_data$quote_asset_volume)%>%
    dplyr::rename(time = "coin_data.close_time",
                  close = "coin_data.close",
                  volume = "coin_data.volume",
                  quote_asset_volume = "coin_data.quote_asset_volume")
  
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
    dplyr::filter(stringr::str_sub(coins,-4,-1) == base_currency) %>%
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

generate_indicator <- function(historical_data = historical_data){
  
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
  
  return(df_indicator)
  
}









