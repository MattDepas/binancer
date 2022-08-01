
#' generate empty dataset in backtest
#'
#' @param data
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

gen_total_empty <- function(data,
                            initial=parent.frame()$initial_value){
  
  #create total portfolio dataset
  x <- data %>%
    dplyr::select(time) %>%
    dplyr::distinct() %>%
    dplyr::arrange(time) %>%
    #set starting BTC amount and set up checker variables
    dplyr::mutate(symbol = "Total Portfolio",
                  value = initial,
                  redist_trade_val = 0,
                  dust_val = 0,
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
#' @export coindata_firstloop
#'
#'

coindata_firstloop<-function(data, 
                             portval_loop1 = parent.frame()$portval,
                             momentum_count_loop1 = parent.frame()$momentum_count,
                             min_momentum_count_loop1 = parent.frame()$min_momentum_count,
                             min_trade_loop1 = parent.frame()$min_trade,
                             timeperiod_loop1 = parent.frame()$timeperiod,
                             i_loop1 = parent.frame()$i){
  
  if (is.null(data)) {
    stop('data is null')
  }
  if (is.null(portval_loop1)) {
    stop('portval_loop1 is null')
  }
  if (is.null(min_momentum_count_loop1)) {
    stop('min_momentum_count_loop1 is null')
  }
  if (is.null(min_trade_loop1)) {
    stop('min_trade_loop1 is null')
  }
  if (is.null(timeperiod_loop1)) {
    stop('timeperiod_loop1 is null')
  }
  if (is.null(i_loop1)) {
    stop('i_loop1 is null')
  }

  x <- data %>%
    dplyr::ungroup()%>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(
      
      # if momentum in the market is greater than the minimum, let the proposed amount of coin 
      # equal its macd_weighting by the current portfolio value, otherwise, the proposed holding of all coins is 0
      proposed_position =      dplyr::if_else(time==timeperiod_loop1,
                                              dplyr::case_when(momentum_count_loop1>=min_momentum_count_loop1 ~ portval_loop1*macd_weighting,
                                                               TRUE ~ 0),
                                              proposed_position),
      
      # this is the value of the holding in the previous period for each coin multiplied by the growth rate to the current period
      lagged_actual_position = dplyr::case_when(time==timeperiod_loop1 & i_loop1==1 ~ 0,
                                                time==timeperiod_loop1 & i_loop1!=1 ~ dplyr::lag(actual_position,1)*(1+close_change),
                                                TRUE ~ lagged_actual_position),
      
      # the trade amount is the proposed position minus the lagged actual position
      trade =                  dplyr::case_when(time==timeperiod_loop1 & is.na(proposed_position - lagged_actual_position) ~ 0,
                                                time==timeperiod_loop1 & !is.na(proposed_position - lagged_actual_position) ~ proposed_position - lagged_actual_position,
                                                TRUE ~ trade),
      
      # the trade rule indicator is 1 if the trade amount above is greater than the minimum amount, otherwise equal to 0          
      trade_rule1_indicator =  dplyr::case_when(time==timeperiod_loop1 & abs(trade)>min_trade_loop1 ~ 1,
                                                time==timeperiod_loop1 & abs(trade)<min_trade_loop1 ~ 0,
                                                TRUE ~ trade_rule1_indicator),
      
      # equal to 1 if a dust conversion is required
      dust_indicator      =    dplyr::case_when(time==timeperiod_loop1 & lagged_actual_position > min_trade_loop1 ~ 1,
                                                time==timeperiod_loop1 & lagged_actual_position < min_trade_loop1 & lagged_actual_position!=0 ~ 0,
                                                time==timeperiod_loop1 & lagged_actual_position < min_trade_loop1 & lagged_actual_position==0 ~ 1,
                                                TRUE ~ dust_indicator))
  
  return(x)
  
}