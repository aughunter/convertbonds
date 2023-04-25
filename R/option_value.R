#' Option_value functuon
#' Calculate the four value comparisons:Option value of convertible bond,Theoretical value of convertible bonds (pure bond value + option value),The difference between the theoretical price of convertible bonds and the current price,The ratio of the difference between the theoretical price of convertible bonds and the current price
#' @param value_per Option value per share(numeric)
#' @param current_price Current price of convertible bonds
#' @param conver_price Conversion price
#' @param netdebt_value Pure debt value
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' option_value( value_per=1.02,current_price=122.82,conver_price=5.43,netdebt_value=104.05  )
option_value<- function( value_per,current_price,conver_price,netdebt_value  ){

  ##Option value of convertible bond
  a=100/conver_price*value_per

  ##Theoretical value of convertible bonds (pure bond value + option value)
  b=netdebt_value+100/conver_price*value_per

  ##The difference between the theoretical price of convertible bonds and the current price
  c=current_price-(netdebt_value+100/conver_price*value_per)

  ##The ratio of the difference between the theoretical price of convertible bonds and the current price
  d=(current_price-(netdebt_value+100/conver_price*value_per) )/current_price

  print( paste("Option value of convertible bond: " ,  a,sep="")  )
  print( paste("Theoretical value of convertible bonds (pure bond value + option value): " ,  b,sep="")  )
  print( paste("The difference between the theoretical price of convertible bonds and the current price: " ,c,sep="")  )
  print( paste("The ratio of the difference between the theoretical price of convertible bonds and the current price: " ,d,sep="")  )
}


