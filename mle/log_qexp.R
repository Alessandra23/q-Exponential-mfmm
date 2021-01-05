log.qexp <- function(values,x){
  
  n <- length(x)
  mu <- values[1]
  theta <- values[2]
  
  return(-(n*log((theta+1)/(theta*mu))-(theta+2)*sum(log(1+x/(mu*theta)))))
}
