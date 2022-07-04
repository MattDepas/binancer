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
    start<-Sys.time()
    
    #create coin dataset

    coin_data <- gen_coindata_empty(data)
    if(!exists("coin_data")){
      stop("gen_coindata_empty function error")}
      
    total <- gen_total_empty(data, initial = initial_value)
    if(!exists("total")){
      stop("gen_total_empty function error")}
      
    timelist <- gen_time(data)
    if(!exists("timelist")){
      stop("gen_time function error")}
      
    
    # run backtesting loop
    for (i in 1:(nrow(timelist)-1)){
      
      timeperiod<- timelist$time[i]
      
      portval <- total$value[i]
      
      momentum_count<-as.numeric(nrow(coin_data %>%
                                        dplyr::ungroup() %>%
                                        dplyr::filter(time == timeperiod)%>%
                                        dplyr::filter(macd>0)))
      
      # This function calculates the rows "proposed position", "lagged actual position", "trade" and "trade rule indicator".
      # These columns say how much you should hold, what you are currently holding, how much the trade amount should be in theory
      # and whether or not that trade amount exceeds the minimum required amount
      coin_data <- coindata_firstloop(coin_data, 
                                      portval = portval, 
                                      min_momentum_count = min_momentum_count,
                                      min_trade = min_trade,
                                      i=i) 
      
      #this is the amount of redistributed trade val
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
    
    
   
  end<-Sys.time()
  
  runtime<-start-end
  
  message("This backtest took ",runtime,"seconds")
  
  return(list(coin_data,total,time_list))
  
}
