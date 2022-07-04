
#' generate empty dataset in backtest
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
#' @export gen_coindata_empty
#' @examples
#'
#'
#'

gen_coindata_empty <- function(data){

  #create coin dataset
  x <- data %>%
    
    dplyr::select(time,symbol,close,close_change,close_change_lead,macd,macd_weighting) %>%
    
    dplyr::arrange(symbol,time)%>%
    dplyr::group_by(symbol)%>%
    #create values to be generated
    dplyr::mutate(proposed_position = 0,
                  lagged_actual_position = 0,
                  trade = 0,
                  trade_rule1_indicator = 0,
                  dust_indicator = 0,
                  sign_test = 0,
                  tradeshare_samesign = 0,
                  revised_trade = 0,
                  actual_position = 0,
                  portfolio_share = 0,
                  )%>%
    
    # dplyr::filter(time>= starting_date & time<= end_date)%>%
    
    dplyr::ungroup() %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(close_change = ifelse(is.na(close_change),0,close_change))
  
  return(x)
  }

gen_total_empty <- function(data, initial){
  
  #create total portfolio dataset
  x <- data %>%
    dplyr::select(time) %>%
    dplyr::distinct() %>%
    dplyr::arrange(time) %>%
    #set starting BTC amount and set up checker variables
    dplyr::mutate(symbol = "Total Portfolio",
                  value = initial,
                  redist_trade_val = 0,
                  revised_trade_total = 0,
                  momentum_count = 0,
                  macd_weighting_total = 0)#%>%
  # dplyr::filter(time>= starting_date & time<= end_date)
  return(x)
}

gen_time<-function(data){
  x <- data%>%
  dplyr::select(time) %>%
  dplyr::distinct() %>%
  dplyr::arrange(time)
  
  return(x)
}



#' first round of coin dataset in backtest
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
#'
#' @examples
#'
#'
#'

coindata_firstloop<-function(data, 
                             portval = portval, 
                             min_momentum_count = min_momentum_count,
                             min_trade = min_trade,
                             timeperiod = timeperiod,
                             i=i){
  
  x <- data %>%
    dplyr::ungroup()%>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      
      # if momentum in the market is greater than the minimum, let the proposed amount of coin 
      # equal its macd_weighting by the current portfolio value, otherwise, the proposed holding of all coins is 0
      proposed_position =      dplyr::if_else(time==timeperiod,
                                              dplyr::case_when(momentum_count>=min_momentum_count ~ portval*macd_weighting,
                                                               TRUE ~ 0),
                                              proposed_position),
      
      # this is the value of the holding in the previous period for each coin multiplied by the growth rate to the current period
      lagged_actual_position = dplyr::if_else(time==timeperiod,
                                              dplyr::if_else(i==1,
                                                             0,
                                                             dplyr::lag(actual_position,1)*(1+close_change)),
                                              lagged_actual_position),
      
      # the trade amount is the proposed position minus the lagged actual position
      trade =                  dplyr::if_else(time==timeperiod,
                                              dplyr::if_else(is.na(proposed_position - lagged_actual_position),
                                                             0,
                                                             proposed_position - lagged_actual_position),
                                              trade),
      
      # the trade rule indicator is 1 if the trade amount above is greater than the minimum amount, otherwise equal to 0          
      trade_rule1_indicator =  dplyr::if_else(time==timeperiod,
                                              dplyr::if_else(abs(trade)>min_trade,
                                                             1,
                                                             0),
                                              trade_rule1_indicator),
      
      # equal to 1 if a dust conversion is required
      dust_indicator      =    dplyr::if_else(time==timeperiod,
                                              dplyr::if_else(trade_rule1_indicator == 0,
                                                             if_else(proposed_position<lagged_actual_position,
                                                                     1,
                                                                     0),
                                                             0),
                                              dust_indicator))
  
  return(x)
  
}