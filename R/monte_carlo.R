#' Monte Carlo simulation function
#' Predicting Theoretical Value of Options per Share Using Monte Carlo Simulations
#' @param I number ofsimulation
#' @param M number of time steps
#' @param S_0 The initial price of the underlying stock
#' @param Time Simulate paths over time intervals
#' @param K Exercise price (conversion price)
#' @param r risk free rate
#' @param sigma Volatility (Standard Deviation of Return)
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' monte_carlo(I=10000,M=50,S_0=5.9,K=5.43,T=1.353,r=0.018482,sigma=0.2616)
monte_carlo<- function(I,M,S_0,K,Time,r,sigma){
  #time enterval
  dt=Time/M
  start=Sys.time()

  S=list()
  for (i in 1:I) {
    path=list(  )

    for (t in 1:(M+1)) {
      if( t==1  ){
        path[1]<-S_0

      }else{
        z=stats::rnorm( 1,mean=0.0, sd=1.0)
        b1<- as.numeric(path[t-1]  )
        S_t= b1*exp((r-0.5*sigma**2)*dt + sigma*sqrt(dt)*z)
        path[t]<-S_t
      }

    }
    #print(i)
    S[[i]]<- path
  }

  # Calculate the present value of the option
  he<- 0
  for (p in S) {
    show<- as.numeric( p[M+1]  )
    he=he+ max( ( show -K),0  )
  }
  #discount
  C_0=exp(-r*T)*he/I

  total_time=Sys.time()-start

  print( paste("European Option value: " ,C_0,sep="")  )
  print( paste("total time is seconds: " ,total_time,sep="")  )

}


