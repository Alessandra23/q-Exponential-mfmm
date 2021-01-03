#' Generate samples
#' 
#' 
#' @param N Numeric. Number of samples
#' @param n Numeric. Sample size of each sample
#' @param theta Numeric. True value of theta
#' @param mu Numeric. True value of mu
#' 
#' @example G.samples(N = 10, n = 2, theta = 1, mu = 2)
#' 
#' 

qexp.samples <- function(N,n,theta,mu){
  result <- t(replicate(N, rtsal(n,shape = theta+1, scale = mu*theta)))
  return(result)
}


