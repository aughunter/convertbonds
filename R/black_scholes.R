#' Black Schiles Model function
#' Calculating Function Using the Black-Schiles Option Pricing Model
#'
#' @param mode Two calculation methods, respectively 1 and 2
#' @param current_price Current price of convertible bonds
#' @param stock_price Positive stock price
#' @param conver_price Conversion price
#' @param stock_var Standard deviation of annualized rate of return for underlying stocks
#' @param time Expiration time (annualized remaining period)
#' @param interest_rate Risk-free continuous compound interest rate
#' @param netdebt_value Pure debt value
#'
#' @return Option value per share(numeric)
#' @export
#'
#' @examples result<-black_schiles(mode=1,current_price=122.82,
#'  stock_price=5.9,conver_price=5.43,stock_var=0.2616,time=1.353,
#'  interest_rate=0.018482, netdebt_value=104.05)
#'
black_schiles <- function(mode=1,current_price, stock_price,conver_price,stock_var,time,interest_rate, netdebt_value) {

  d1= ( log(stock_price/conver_price) + (interest_rate + 0.5*stock_var^2)*time ) /(  stock_var *sqrt(time))
  d2= ( log(stock_price/conver_price) + (interest_rate - 0.5*stock_var^2)*time ) /(  stock_var *sqrt(time))

  if( mode==1 ){
    C=stock_price*stats::pnorm(d1) - conver_price*exp(1)^(-interest_rate*time)*stats::pnorm(d2)
        }

  if( mode==2 ){
    C=stock_price*stats::pnorm(d1) - conver_price*exp(-interest_rate*time)*stats::pnorm(d2)
       }

   return(  C )
}


