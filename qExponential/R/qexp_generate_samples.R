#' qexp.samples
#'
#' @description Generate samples
#'
#'
#' @param N Numeric. Number of samples
#' @param n Numeric. Sample size of each sample
#' @param theta Numeric. True value of theta
#' @param mu Numeric. True value of mu
#'
#' @return A matrix of order N x n, where each row is a sample of size n.
#'
#' @example qexp.samples(N = 10, n = 2, theta = 1, mu = 2)
#'
#' @export
#'

qexp.samples <- function(N,n,theta,mu){
  samples <- t(replicate(N, rtsal(n,shape = theta+1, scale = mu*theta)))
  return(samples)
}

